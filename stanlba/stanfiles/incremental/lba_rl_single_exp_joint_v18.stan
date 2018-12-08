//v2: this incorporates reward prediction error as the only behavioral model parameter for which covariance is calculated.
//v3: this incorporates changes to priors that I have experimented with in the rl models.
//v4: ??? ran something on Friday 29 but not sure what it was.
//v5: additional changes including a refinement of alpha_pr adding a correlation matrix to go with the covariance matrix. Can compare with rl_single_exp_joint_v6.stan.
//v6 fixed up a substantial error in joint model standardization present in v5.
//v7 allowed an arbitrary number of deltas.
//v8 Fixed up a substantial error with the construction of the reward prediction error calculation.
//v9 Replaces RPE with simple correct response. This effectively makes it a "parallel model" because the behavioral parameters and joint parameters are independent in this model.
//    Also have changed the estimation of mean ThetaDeltas to simple calculation of the mean, because we're not actually interested in knowing them
//    means of these values.
//v10 goes back to RPE, but keeps the change to avoid estimation of mean ThetaDeltas, because this seems unnecessary.
//v11 adds expected value into the model.
//v13 skips v12 and adds a log probability.
functions{
     
     real lba_pdf(real t, real b, real A, real v, real s){
          //PDF of the LBA model
          
          real b_A_tv_ts;
          real b_tv_ts;
          real term_1;
          real term_2;
          real term_3;
          real term_4;
          real pdf;
          
          b_A_tv_ts = (b - A - t*v)/(t*s);
          b_tv_ts = (b - t*v)/(t*s);
          term_1 = v*Phi_approx(b_A_tv_ts);
          term_2 = s*exp(normal_lpdf(b_A_tv_ts | 0,1)); 
          term_3 = v*Phi_approx(b_tv_ts);
          term_4 = s*exp(normal_lpdf(b_tv_ts | 0,1)); 
          pdf = (1/A)*(-term_1 + term_2 + term_3 - term_4);
          
          return pdf;
     }
     
     real lba_cdf(real t, real b, real A, real v, real s){
          //CDF of the LBA model
          
          real b_A_tv;
          real b_tv;
          real ts;
          real term_1;
          real term_2;
          real term_3;
          real term_4;
          real cdf;	
          
          b_A_tv = b - A - t*v;
          b_tv = b - t*v;
          ts = t*s;
          term_1 = b_A_tv/A * Phi_approx(b_A_tv/ts);	
          term_2 = b_tv/A   * Phi_approx(b_tv/ts);
          term_3 = ts/A     * exp(normal_lpdf(b_A_tv/ts | 0,1)); 
          term_4 = ts/A     * exp(normal_lpdf(b_tv/ts | 0,1)); 
          cdf = 1 + term_1 - term_2 + term_3 - term_4;
          
          return cdf;
          
     }
     
     real lba_log(real response_time, int response, real k, real A, vector v, real s, real tau){
          
          real t;
          real b;
          real cdf;
          real pdf;		
          real prob;
          real out;
          real prob_neg;
          real prob_neg_over_v[num_elements(v)];

          b = A + k;
          // for (i in 1:rows(RT)){	
             t = response_time - tau;
             if(t > 0){			
                  cdf = 1;
                  
                  for(j in 1:num_elements(v)){
                       if(response == j){
                            pdf = lba_pdf(t, b, A, v[j], s);
                       }else{	
                            cdf = (1-lba_cdf(t, b, A, v[j], s)) * cdf;
                       }
                  }
                  // prob_neg = 1;
                  // for(j in 1:num_elements(v)){
                  //      prob_neg = Phi_approx(-v[j]/s) * prob_neg;    
                  // }
                  for(j in 1:num_elements(v)){
                       prob_neg_over_v[j] = Phi_approx(-v[j]/s);    
                    }
                  prob_neg = prod(prob_neg_over_v);
                  
                  prob = pdf*cdf;		
                  prob = prob/(1-prob_neg);	
                  if(prob < 1e-10){
                       prob = 1e-10;				
                  }
                  
             }else{
                  prob = 1e-10;			
             }		

          out = log(prob);
          //print("lba lprob:",out)
          return out;		
     }
     
    vector lba_rng(real k, real A, vector v, real s, real tau){
          
          int get_pos_drift;	
          int no_pos_drift;
          int get_first_pos;
          vector[num_elements(v)] drift;
          int max_iter;
          int iter;
          real start[num_elements(v)];
          real ttf[num_elements(v)];
          int resp[num_elements(v)];
          real rt;
          vector[2] pred;
          real b;
          
          //try to get a positive drift rate
          get_pos_drift = 1;
          no_pos_drift = 0;
          max_iter = 1000;
          iter = 0;
          while(get_pos_drift){
               for(j in 1:num_elements(v)){
                    drift[j] = normal_rng(v[j],s);
                    if(drift[j] > 0){
                         get_pos_drift = 0;
                    }
               }
               iter = iter + 1;
               if(iter > max_iter){
                    get_pos_drift = 0;
                    no_pos_drift = 1;
               }	
          }
          //if both drift rates are <= 0
          //return an infinite response time
          if(no_pos_drift){
               pred[1] = -1;
               pred[2] = -1;
          }else{
               b = A + k;
               for(i in 1:num_elements(v)){
                    //start time of each accumulator	
                    start[i] = uniform_rng(0,A);
                    //finish times
                    ttf[i] = (b-start[i])/drift[i];
               }
               //rt is the fastest accumulator finish time	
               //if one is negative get the positive drift
               resp = sort_indices_asc(ttf);
               ttf = sort_asc(ttf);
               get_first_pos = 1;
               iter = 1;
               while(get_first_pos){
                    if(ttf[iter] > 0){
                         pred[1] = ttf[iter] + tau;
                         pred[2] = resp[iter]; 
                         get_first_pos = 0;
                    }
                    iter = iter + 1;
               }
          }
          return pred;	
     }
}

data{
   int<lower=1> LENGTH;
   int<lower=2> NUM_CHOICES;
   real<lower=0> A;
   vector[LENGTH] response_time;
   int response[LENGTH];
   int required_choice[LENGTH];//the choice which would be reinforced on each round.
   int cue[LENGTH];
     
  //////neural model
  int DELTA_N;
  int THETA_N;
  matrix[LENGTH,DELTA_N] neural_data;
  
  ////////////\begin{joint model machinery}
  vector[THETA_N+DELTA_N] td_mu_prior;
  vector[THETA_N+DELTA_N] td_sd_prior;
  
  ////////////\end{joint model machinery}   
}
transformed data{
  ////////////\begin{joint model machinery}
  ////////////\end{joint model machinery}
  int THETA_rpe=1;
  int THETA_ev=2;
  //int THETA_oc=1;
  int TD_rpe=THETA_rpe;
  //int TD_oc=THETA_oc;
  
  //int THETA_N=THETA_oc;
  int TD_ev=THETA_ev;
  int TD_N=DELTA_N+THETA_N;
  
  vector[TD_N] zeros = rep_vector(0,TD_N);
  
  real<lower=0> s = 1;

  matrix[LENGTH,NUM_CHOICES] choice_outcomes;
  for (i in 1:LENGTH){
    //if the required choice was chosen then 
    //assign the value of 1 to it and distribute the value of -1 across all alternatives.
    //or if the required choice was not chosen, 
    //assign the value of -1 to it and distribute the value of 1 across all alternatives.
    for (j in 1:NUM_CHOICES){
      if(j==response[i]){
        choice_outcomes[i,j]=((response[i]==required_choice[i])-0.5)*2;
      }else{
        choice_outcomes[i,j]=-((response[i]==required_choice[i])-0.5)*2/(NUM_CHOICES-1);
      }
    }
  }
}

parameters {
  real alpha_pr;
  real k_pr;//relative threshold
  real tau_pr;//non-decision time
  
  // ////////////\begin{joint model machinery}
  //vector[TD_N] td_mu;
  cholesky_factor_corr[TD_N] L_Omega;
  vector<lower=0>[TD_N] L_sigma;
  // ////////////\end{joint model machinery}
}

transformed parameters {
  real <lower=0,upper=1> alpha;
  real<lower=0> k;
  real<lower=0> tau;
  
  // ////////////\begin{joint model machinery}
  matrix[TD_N, TD_N] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  matrix[TD_N, TD_N] Sigma = L_Sigma * L_Sigma';
  // ////////////\end{joint model machinery}
  
  alpha = inv_logit(alpha_pr);
  k = exp(k_pr);
  tau = exp(tau_pr);
}

model {
  matrix[max(cue), NUM_CHOICES] exp_val = rep_matrix(0,max(cue), NUM_CHOICES);
  real pred_err;
  vector[LENGTH] run_pred_err_c2;
  vector[LENGTH] trial_expected_val;
  vector[LENGTH] actual_outcome;
  real outcome;
  vector[NUM_CHOICES] v;
  matrix[LENGTH,NUM_CHOICES] v_record;
  // ////////////\begin{joint model machinery}
  vector[TD_N] td_var[LENGTH];
  matrix[LENGTH, TD_N] theta_delta;
  vector[TD_N] td_mean;
  vector[TD_N] td_sd;
  // ////////////\end{joint model machinery}
  
  //these use VERY weak priors because we are going to use this data to set priors for future analyses
  //so it's important that we don't unduly bias analysis at this level.
  //this priors works out to ROUGHLY a uniform distribution between 0 and 1 when transformed.
  alpha_pr ~ normal(0,1.5);//weak prior, agnostic about learning rate, but still biased to low learning rates.
    //as a compromise between an absolute 0 learning rate (-Inf) and a learning rate flat in the middle of the possible distribution (0).
  
  k_pr ~ normal(log(.5),2); //other thing we could do with these is replace them with half-normals as in the Annis paper.
  //A ~ normal(.5,1)T[0,];
  //tau_pr ~ normal(log(.5),1);//normal(.5,.5)T[0,];
  tau_pr ~ normal(log(.5),2);
  //now we need to loop through the trials, modelin
  //so how do we model choices in this context?
  for (i in 1:LENGTH){//loop through timesteps.
    //the EXPECTED value for this choice, which *should* always be positive(?) but will difer in degree of how positive you'd expect.
    
    trial_expected_val[i] = exp_val[cue[i],response[i]];
    
    for(j in 1:NUM_CHOICES){
      v[j]=logit(exp_val[cue[i],j]/4+0.75);//We could simplify this right down to just passing in the expected value. Should we???
      v_record[i,j]=v[j];
      //if j was the reinforced choice and it was the response value,
      pred_err=choice_outcomes[i,j]-exp_val[cue[i],j]; 
      
      if(j==response[i]){
        //recorded reward prediction error has to be the prediction of *what the subject thought would happen*,
        //i.e., the RPE for the particular choice they made. churr.
        run_pred_err_c2[i] = pred_err;
      }
      
      exp_val[cue[i],j] = exp_val[cue[i],j] + alpha*pred_err;
      
      //the value of the outcome obtained for this choice.
      actual_outcome[i]=choice_outcomes[i,response[i]];
    }
    
    //i don't know what to do with non-resposne time...but let's work that out later.
    response_time[i] ~ lba(response[i],k,A,v,s,tau);
  }
  ////////////\begin{joint model machinery}
   
  //transfer the one theta into the theta-delta matrix, transforming it at the same time.
  theta_delta[:,THETA_rpe]=logit(run_pred_err_c2/4+0.5);
  //theta_delta[:,THETA_oc]=(actual_outcome-mean(actual_outcome))./sd(actual_outcome);
  theta_delta[:,THETA_ev]=logit(trial_expected_val/2+0.5);
  //maybe this could make a difference, could check??
  
  //transfer the deltas into the theta-delta matrix.
  for (d_i in 1:DELTA_N){
    theta_delta[:,THETA_N+d_i]=neural_data[:, d_i];
  }
  
  // //In order to operate a faster model, we don't estimate these means; we just calculate them.
  // //I've got no interest in actually calculating means here, so I think that's the best approach.
  for (tdi in 1:TD_N){
    td_mean[tdi] = mean(theta_delta[:,tdi]);
    td_sd[tdi] = sd(theta_delta[:,tdi]);
  }
  // 
  //standardize the variance.
  for (i in 1:LENGTH){
    //td_var[i,:] = (to_vector(theta_delta[i,:]) - td_mu) ./ td_sd;
    td_var[i,:] = (to_vector(theta_delta[i,:]) - td_mean) ./ td_sd;
  }
  
  //predict the variance from the remaining information.
  L_Omega ~ lkj_corr_cholesky(.000000000001);
  L_sigma ~ cauchy(0,1); //these yield standard deviations of each individual value.
  td_var ~ multi_normal_cholesky(zeros,L_Sigma);
  ////////////\end{joint model machinery}

    
}
