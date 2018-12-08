//rl_single_exp_joint
//this does NOT use the lba model. For comparison purposes it is included anyway, but it's unused.
//it still does a joint model on reward prediction error.
//the reason I did this is that the lba_rl model for some reason has very low alphas
//this was the case in the other stan model too, and I want to know why!
//v3: center-biased priors for alpha and beta.
//v5: change in beta prior to be closer to what we empirically saw in the parallel model.
//v6: standardize DeltaThetas. Comparable to lba_rl_single_exp_joint_v5.stan
//v7: introduce new neural deltas.
//v8: What would an actor-critic model look like?
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
     
     real lba_log(real response_time, int chosen_option, real k, real A, vector v, real s, real tau){
          
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
                       if(chosen_option == j){
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
          //return an infinite chosen_option time
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
   int<lower=2> NUM_OPTIONS;
   real<lower=0> A;
   vector[LENGTH] response_time;
   int chosen_option[LENGTH];
   int required_option[LENGTH];//the choice which would be reinforced on each round.
   int cue[LENGTH];
     
     
  ////////////\begin{joint model machinery}
  vector[5] td_mu_prior;
  vector[5] td_sd_prior;
  
  ////////////\end{joint model machinery}   
  //////neural model
  int DELTA_N;
  matrix[LENGTH,DELTA_N] neural_data;
  
  int DELTA_accumbens_lh;// = 3;
  int DELTA_accumbens_rh;// = 4;
  int DELTA_ofc_lh;// = 5;
  int DELTA_ofc_rh;// = 6;
}
transformed data{
  ////////////\begin{joint model machinery}
  ////////////\end{joint model machinery}
  int THETA_rpe=1;
  //int THETA_ev=0;
  int TD_rpe=THETA_rpe;
  int THETA_N=THETA_rpe;
  //int TD_ev=THETA_ev;
  int TD_accumbens_lh = DELTA_accumbens_lh+THETA_rpe;
  int TD_accumbens_rh = DELTA_accumbens_rh+THETA_rpe;
  int TD_ofc_lh = DELTA_ofc_lh+THETA_rpe;
  int TD_ofc_rh = DELTA_ofc_rh+THETA_rpe;
  int TD_N=THETA_N + DELTA_N;
  
  vector[TD_N] zeros = rep_vector(0,TD_N);
  
  real<lower=0> s = 1;

  matrix[LENGTH,NUM_OPTIONS] option_outcomes;
  for (i in 1:LENGTH){
    //we should maybe CHANGE this to allow for negative reward and punishment values
    //but as a default, we start the process assuming that a rewarded chosen_option r=1;
    //not rewarded or punished=0
    //if the required choice was chosen then assign the value of 1 to it and distribute the value of -1 across all alternatives.
    //or if the required choice was not chosen, assign the value of -1 to it and distribute the value of 1 across all alternatives.
    for (j in 1:NUM_OPTIONS){
      if(j==chosen_option[i]){
        //option_outcomes[i,j]=((chosen_option[i]==required_option[i])-0.5)*2;
        option_outcomes[i,j]=(chosen_option[i]==required_option[i])*1;
      }else{
        option_outcomes[i,j]=(chosen_option[i]==required_option[i])*1;
      }
    }
  }
  
  // print("matrix of choice outcomes generated from required_option and chosen_option input data:")
  // print(option_outcomes)
  // 
  
}

parameters {
  real alpha_pr;
  // real k_pr;
  // real tau_pr;
  real beta_pr;
  real beta_G_pr;
  real beta_N_pr;
  // 
  // ////////////\begin{joint model machinery}
  vector[TD_N] td_mu;
  cholesky_factor_corr[TD_N] L_Omega;
  vector<lower=0>[TD_N] L_sigma;
  // ////////////\end{joint model machinery}
}

transformed parameters {
  real <lower=0,upper=1> alpha;
  real <lower=0,upper=1> alpha_G;
  real <lower=0,upper=1> alpha_N;
  real<lower=0> beta;
  real<lower=0> beta_G;
  real<lower=0> beta_N;
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
  beta_G = exp( beta_G_pr);
  beta_N = exp( beta_N_pr);
}

model {
  
  //this should be on a scale of [0,1] and start at 0.5, which should represent neutral
  //we may need to re-assess scaling.
  matrix[max(cue),NUM_OPTIONS] exp_val = rep_matrix(0.5,max(cue),NUM_OPTIONS);
  real pred_err;
  vector[LENGTH] run_pred_err_c2;
  vector[LENGTH] trial_expected_val;
  real outcome;
  vector[NUM_OPTIONS] v;
  
  // ////////////\begin{joint model machinery}
  vector[TD_N] td_var[LENGTH];
  matrix[LENGTH, TD_N] theta_delta;
  vector[TD_N] td_sd;
  // ////////////\end{joint model machinery}
  // ////////////\begin{actor-critic}
  real theta;
  matrix[max(cue),NUM_OPTIONS] G_a = rep_matrix(1,max(cue),NUM_OPTIONS);
  matrix[max(cue),NUM_OPTIONS] N_a = rep_matrix(1,max(cue),NUM_OPTIONS);
  vector [NUM_OPTIONS] Act_a;
  // ////////////\end{actor-critic}
  //these use VERY weak priors because we are going to use this data to set priors for future analyses
  //so it's important that we don't unduly bias analysis at this level.
  alpha_pr ~ normal(0,1.5);//a uniform prior in the logit
  //out of desparation I'm going to set the prior to a value I know makes sense from 
  //pure behavioral models.
    //as a compromise between an absolute 0 learning rate (-Inf) and a learning rate flat in the middle of the possible distribution (0).
  beta_pr ~ normal(0,3);
  beta_G_pr ~ normal(0,3);
  beta_N_pr ~ normal(0,3);
  
  // k_pr ~ normal(log(.5),5); 
  // tau_pr ~ normal(log(.5),3);//normal(.5,.5)T[0,];
  //now we need to loop through the trials, modelin
  //so how do we model choices in this context?
  
  for (i in 1:LENGTH){//loop through timesteps.
    
    //the EXPECTED value for this choice, which *should* always be positive(?) but will difer in degree of how positive you'd expect.
    
    trial_expected_val[i] = exp_val[cue[i],chosen_option[i]];
    
    //chosen_option[i] ~ categorical_logit( to_vector(exp_val[cue[i],]) * beta);
    
    for(j in 1:NUM_OPTIONS){
      
      //prediction error: calculated on previous CRITIC value.
      pred_err=option_outcomes[i,j]-exp_val[cue[i],j]; 
      //critic learning.
      exp_val[cue[i],j] = exp_val[cue[i],j] + alpha*pred_err;
      //"critic values are represented in the ventral striatum"
      
      //actor learning.
      G_a[cue[i],j] = G_a[cue[i],j] + alpha_G*G_a[cue[i],j]*pred_err;
      N_a[cue[i],j] = N_a[cue[i],j] + alpha_N*N_a[cue[i],j]*-pred_err;
      
      //policy
      Act_a[j] = beta_G*G_a[cue[i],j] - beta_N*N_a[cue[i],j];
      
    }
    
    // chosen_option[i] ~ categorical_logit( 
    //   e^Act_a[chosen_option[i]]/
    //   sum(e^(Act_a)));
      //we no longer have beta because we have separate beta_G and beta_N
    chosen_option[i] ~ categorical_logit(Act_a);
    
    //response_time[i] = mean(response_time) + 1/(1+e^(Act_a[chosen_option[i]]-theta));
    #we could solve for theta then calculate log probability of theta.
    theta~normal(Act_a[chosen_option[i]] -log(1/(response_time[i]-mean(response_time))-1),1);
    //we're going to do prediction error just for the last choice
    run_pred_err_c2[i] = pred_err;
    
    //i don't know what to do with non-resposne time...but let's work that out later.
    //response_time[i] ~ lba(chosen_option[i],k,A,v,s,tau);
  }

  
  ////////////\begin{joint model machinery}
  // 
  theta_delta[:,THETA_rpe]=logit(run_pred_err_c2/4+0.5);
  // theta_delta[:,THETA_ev]=logit(trial_expected_val/2+0.5);
  theta_delta[:,TD_accumbens_lh]=neural_data[:, DELTA_accumbens_lh];
  theta_delta[:,TD_accumbens_rh]=neural_data[:, DELTA_accumbens_rh];
  theta_delta[:,TD_ofc_lh]=neural_data[:, DELTA_ofc_lh];
  theta_delta[:,TD_ofc_rh]=neural_data[:, DELTA_ofc_rh];
  // 
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
  matrix[TD_N,TD_N] y_corr;
  matrix[TD_N,TD_N] inv_mat_var;
  
  inv_mat_var = rep_matrix(((1) ./ sqrt(diagonal(Sigma))),TD_N);
  y_corr = inv_mat_var' .* Sigma .* inv_mat_var;
}

