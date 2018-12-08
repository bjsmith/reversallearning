//rl_single_exp_joint
//this does NOT use the lba model. For comparison purposes it is included anyway, but it's unused.
//it still does a joint model on reward prediction error.
//the reason I did this is that the lba_rl model for some reason has very low alphas
//this was the case in the other stan model too, and I want to know why!
//v3: center-biased priors for alpha and beta.
//v5: change in beta prior to be closer to what we empirically saw in the parallel model.
//v6: standardize DeltaThetas. Comparable to lba_rl_single_exp_joint_v5.stan
//v7: introduce new neural deltas.
//(skipped v8)
//v9: does a full log likelihood calculation, and allows measurement of EVs and RPEs
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
     
     
  ////////////\begin{joint model machinery}
  vector[6] td_mu_prior;
  vector[6] td_sd_prior;
  
  ////////////\end{joint model machinery}   
  //////neural model
  int DELTA_N;
  matrix[LENGTH,DELTA_N] neural_data;
  
  // int DELTA_accumbens_lh;// = 3;
  // int DELTA_accumbens_rh;// = 4;
  // int DELTA_ofc_lh;// = 5;
  // int DELTA_ofc_rh;// = 6;
}
transformed data{
  ////////////\begin{joint model machinery}
  ////////////\end{joint model machinery}
  int THETA_rpe=1;
  int THETA_ev=2;
  //int TD_rpe=THETA_ev;
  int THETA_N=THETA_ev;
  //int TD_ev=THETA_ev;
  // int TD_accumbens_lh = DELTA_accumbens_lh+THETA_rpe;
  // int TD_accumbens_rh = DELTA_accumbens_rh+THETA_rpe;
  // int TD_ofc_lh = DELTA_ofc_lh+THETA_rpe;
  // int TD_ofc_rh = DELTA_ofc_rh+THETA_rpe;
  int TD_N=THETA_N + DELTA_N;
  
  vector[TD_N] zeros = rep_vector(0,TD_N);
  
  real<lower=0> s = 1;

  matrix[LENGTH,NUM_CHOICES] choice_outcomes;
  for (i in 1:LENGTH){
    //if the required choice was chosen then assign the value of 1 to it and distribute the value of -1 across all alternatives.
    //or if the required choice was not chosen, assign the value of -1 to it and distribute the value of 1 across all alternatives.
    for (j in 1:NUM_CHOICES){
      if(j==response[i]){
        choice_outcomes[i,j]=((response[i]==required_choice[i])-0.5)*2;
      }else{
        choice_outcomes[i,j]=-((response[i]==required_choice[i])-0.5)*2/(NUM_CHOICES-1);
      }
    }
  }
  
  // print("matrix of choice outcomes generated from required_choice and response input data:")
  // print(choice_outcomes)
  // 
  
}

parameters {
  real alpha_pr;
  // real k_pr;
  // real tau_pr;
  real beta_pr;
  
  // 
  // ////////////\begin{joint model machinery}
  vector[TD_N] td_mu;
  cholesky_factor_corr[TD_N] L_Omega;
  vector<lower=0>[TD_N] L_sigma;
  // ////////////\end{joint model machinery}
}

transformed parameters {
  real <lower=0,upper=1> alpha;
  real<lower=0> beta;
  //real<lower=0> k;
  //real<lower=0> tau;
  
  // ////////////\begin{joint model machinery}
  matrix[TD_N, TD_N] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  matrix [TD_N,TD_N] Sigma = L_Sigma * L_Sigma';
  // 
  // ////////////\end{joint model machinery}
  
  alpha = inv_logit(alpha_pr);
  //k = exp(k_pr);
  //tau = exp(tau_pr);
  beta = exp( beta_pr);
  
}

model {
  matrix[max(cue),NUM_CHOICES] exp_val = rep_matrix(0,max(cue),NUM_CHOICES);
  real pred_err;
  vector[LENGTH] run_pred_err_c2;
  vector[LENGTH] trial_expected_val;
  real outcome;
  vector[NUM_CHOICES] v;
  
  // ////////////\begin{joint model machinery}
  vector[TD_N] td_var[LENGTH];
  matrix[LENGTH, TD_N] theta_delta;
  vector[TD_N] td_sd;
  // ////////////\end{joint model machinery}
  
  //these use VERY weak priors because we are going to use this data to set priors for future analyses
  //so it's important that we don't unduly bias analysis at this level.
  alpha_pr ~ normal(0,1.5);//a uniform prior in the logit
  //out of desparation I'm going to set the prior to a value I know makes sense from 
  //pure behavioral models.
    //as a compromise between an absolute 0 learning rate (-Inf) and a learning rate flat in the middle of the possible distribution (0).
  beta_pr ~ normal(0,3);
  
  // k_pr ~ normal(log(.5),5); 
  // tau_pr ~ normal(log(.5),3);//normal(.5,.5)T[0,];
  //now we need to loop through the trials, modelin
  //so how do we model choices in this context?
  
  for (i in 1:LENGTH){//loop through timesteps.
    
    //the EXPECTED value for this choice, which *should* always be positive(?) but will difer in degree of how positive you'd expect.
    
    trial_expected_val[i] = exp_val[cue[i],response[i]];
    
    response[i] ~ categorical_logit( to_vector(exp_val[cue[i],]) * beta);
    
    for(j in 1:NUM_CHOICES){
      //v[j]=logit(exp_val[cue[i],j]/4+0.75);
      //if j was the reinforced choice and it was the response value,
      pred_err=choice_outcomes[i,j]-exp_val[cue[i],j]; 
      
      
      exp_val[cue[i],j] = exp_val[cue[i],j] + alpha*pred_err;
      //for occam's razor, I'm going to avoid any transformation from expected value to drift rate. we'll treat expected value as drift rate exactly!
      
    }
    //we're going to do prediction error just for the last choice
    run_pred_err_c2[i] = pred_err;
    
    //i don't know what to do with non-resposne time...but let's work that out later.
    //response_time[i] ~ lba(response[i],k,A,v,s,tau);
  }
  // print("run_pred_err_c2:");
  // print(run_pred_err_c2);
  // print("trial_expected_val:");
  // print(trial_expected_val);
  
  ////////////\begin{joint model machinery}
  // 
  
  theta_delta[:,THETA_rpe]=logit(run_pred_err_c2/4+0.5);
  //theta_delta[:,THETA_oc]=(actual_outcome-mean(actual_outcome))./sd(actual_outcome);
  theta_delta[:,THETA_ev]=logit(trial_expected_val/2+0.5);
  //maybe this could make a difference, could check??
  
  //transfer the deltas into the theta-delta matrix.
  for (d_i in 1:DELTA_N){
    theta_delta[:,THETA_N+d_i]=neural_data[:, d_i];
  }
  // 
  // theta_delta[:,THETA_rpe]=logit(run_pred_err_c2/4+0.5);
  // // theta_delta[:,THETA_ev]=logit(trial_expected_val/2+0.5);
  // theta_delta[:,TD_accumbens_lh]=neural_data[:, DELTA_accumbens_lh];
  // theta_delta[:,TD_accumbens_rh]=neural_data[:, DELTA_accumbens_rh];
  // theta_delta[:,TD_ofc_lh]=neural_data[:, DELTA_ofc_lh];
  // theta_delta[:,TD_ofc_rh]=neural_data[:, DELTA_ofc_rh];
  // // 
  // //estimate the means for each of our parameters
  // //These should be zero already but just to make sure.
  for (tdi in 1:TD_N){
    td_mu[tdi] ~ normal(td_mu_prior[tdi],td_sd_prior[tdi]);
    td_sd[tdi] = sd(theta_delta[:,tdi]);
  }
  // 
  //standardize the variance.
  for (i in 1:LENGTH){
    td_var[i,:] = (to_vector(theta_delta[i,:]) - td_mu)./td_sd;
  }
  //predict the variance from the remaining information.
  L_Omega ~ lkj_corr_cholesky(1);
  //print(L_Omega)
  L_sigma ~ cauchy(0,1); //these yield standard deviations of each individual value.
  td_var ~ multi_normal_cholesky(zeros,L_Sigma);
  ////////////\end{joint model machinery}

    
}
generated quantities{
  #so I think this works by...for every value we describe the probability of getting it 
  #given the specific parameter values.
  
  matrix[max(cue), NUM_CHOICES] exp_val = rep_matrix(0,max(cue), NUM_CHOICES);
  real pred_err;
  vector[LENGTH] run_pred_err_c2;
  vector[LENGTH] trial_expected_val;
  vector[LENGTH] y_hat;
  
  //matrix[LENGTH,NUM_CHOICES] v_record;
  // ////////////\begin{joint model machinery}
  // vector[TD_N] td_var[LENGTH];
  // matrix[LENGTH, TD_N] theta_delta;
  // vector[TD_N] td_mean;
  // vector[TD_N] td_sd;
  
  real log_lik = 0;
  
  // inv_mat_var = rep_matrix(((1) ./ sqrt(diagonal(Sigma))),TD_N);
  // y_corr = inv_mat_var' .* Sigma .* inv_mat_var;
  // ////////////\end{joint model machinery}
  
  ////////
  
  
  for (i in 1:LENGTH){//loop through timesteps.
    
    //the EXPECTED value for this choice, which *should* always be positive(?) but will difer in degree of how positive you'd expect.
    
    trial_expected_val[i] = exp_val[cue[i],response[i]];
    
    #response[i] ~ categorical_logit( to_vector(exp_val[cue[i],]) * beta);
    log_lik= log_lik + categorical_logit_lpmf( response[i] |  to_vector(exp_val[cue[i],]) * beta);
          
    # Posterior prediction
    y_hat[i] = categorical_rng( softmax(to_vector(exp_val[cue[i],]) * beta));
    
    for(j in 1:NUM_CHOICES){
      //if j was the reinforced choice and it was the response value,
      pred_err=choice_outcomes[i,j]-exp_val[cue[i],j];
      exp_val[cue[i],j] = exp_val[cue[i],j] + alpha*pred_err;
      
    }
    //we're going to do prediction error just for the last choice
    run_pred_err_c2[i] = pred_err;
    
    //i don't know what to do with non-resposne time...but let's work that out later.
    //response_time[i] ~ lba(response[i],k,A,v,s,tau);
  }
  // 
  // 
  // theta_delta[:,THETA_rpe]=logit(run_pred_err_c2/4+0.5);
  // theta_delta[:,THETA_ev]=logit(trial_expected_val/2+0.5);
  // 
  // for (d_i in 1:DELTA_N){
  //   theta_delta[:,THETA_N+d_i]=neural_data[:, d_i];
  // }
  // 
  // for (tdi in 1:TD_N){
  //   td_mean[tdi] = mean(theta_delta[:,tdi]);
  //   td_sd[tdi] = sd(theta_delta[:,tdi]);
  // }
  // 
  // for (i in 1:LENGTH){
  //   td_var[i,:] = (to_vector(theta_delta[i,:]) - td_mean) ./ td_sd;
  // }
  //now, we don't necessarily need this....but we do need to say....
  //given our Sigma matrix, what is the probability of td_var?
  //L_sigma ~ cauchy(0,1); //these yield standard deviations of each individual value.
  //log_lik = log_lik + multi_normal_cholesky_lpdf(td_var | zeros, L_Sigma);
  //maybe?
  
  
}

