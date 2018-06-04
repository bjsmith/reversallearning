parameters {
  ////////////////////
  //GROUP LEVEL
  real subj_mu[3];
  real<lower=0> subj_sigma[3];
  
  ////////////////////
  //SUBJECT LEVEL
  real alpha_pr[N];
  real k_pr[N];
  real tau_pr[N];
}

transformed parameters {
  real <lower=0,upper=1> alpha[N];
  real<lower=0> k[N];
  real<lower=0> tau[N];
  
  alpha = inv_logit(alpha_pr);
  k = exp(k_pr);
  tau = exp(tau_pr);
}

model {
  real exp_val[N,max(cue),NUM_CHOICES];
  real pred_err;
  real outcome;
  
  ////////////////////
  //GROUP LEVEL
  
  //priors for mean of subject params
  subj_mu[1] ~ normal(-3,3);
  subj_mu[2] ~ normal(log(.5),1);
  subj_mu[3] ~ normal(log(.5),0.5);
  
  subj_sigma[1] ~ cauchy(0,5); 
  subj_sigma[2] ~ cauchy(0,3); 
  subj_sigma[3] ~ cauchy(0,2);
  
  
  ////////////////////
  //SUBJECT LEVEL
  exp_val = rep_array(0,N,max(cue),NUM_CHOICES);
  
  alpha_pr ~ normal(subj_mu[PARID_alpha],subj_sigma[PARID_alpha]);
  k_pr ~ normal(subj_mu[PARID_lba_k],subj_sigma[PARID_lba_k]);
  tau_pr ~ normal(subj_mu[PARID_lba_tau],subj_sigma[PARID_lba_tau]);
  
  for (t in 1:NUM_TRIALS){//loop through timesteps.
    for(j in 1:NUM_CHOICES){
      #reinforcement learning
      pred_err=choice_outcomes[t,j]-exp_val[trial_runid[t],cue[t],j]; 
      exp_val[trial_runid[t],cue[t],j] = exp_val[trial_runid[t],cue[t],j] + alpha[trial_runid[t]]*pred_err;
    }
    #user-defined linear ballistic accumulator function based on that written by Annis, Miller, & Palmeri (2017)
    response_time[t] ~ lba(response[t],k[trial_runid[t]],A,to_vector(exp_val[trial_runid[t],cue[t],]),s,tau[trial_runid[t]]);
    
  }
  
    
}

