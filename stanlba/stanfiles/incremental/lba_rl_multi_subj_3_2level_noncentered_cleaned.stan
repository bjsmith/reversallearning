parameters {
  ////////////////////
  //GROUP LEVEL
  real subj_mu[3];
  real<lower=0> subj_sigma[3];
  
  ////////////////////
  //SUBJECT LEVEL
  real alpha_pr_var[N];
  real k_pr_var[N];
  real tau_pr_var[N];
}

transformed parameters {
  
  real alpha_pr[N];
  real k_pr[N];
  real tau_pr[N];
  
  real<lower=0,upper=1> alpha[N];
  real<lower=0> k[N];
  real<lower=0> tau[N];
  
  for (r in 1:N){
    alpha_pr[r] = subj_sigma[1]*  alpha_pr_var[r] + subj_mu[1];
    k_pr[r] =     subj_sigma[2]*  k_pr_var[r]     + subj_mu[2];
    tau_pr[r] =   subj_sigma[3]*  tau_pr_var[r]   + subj_mu[3];
  }
  
  alpha = inv_logit(alpha_pr);
  k = exp(k_pr);
  tau = exp(tau_pr);
}

model {
  real exp_val[N,max(cue),NUM_CHOICES];
  real pred_err;
  real outcome;
  vector[NUM_CHOICES] v;

  ////////////////////
  //GROUP LEVEL
  
  //priors for mean of subject params
  subj_mu[1] ~ normal(-3,3);
  subj_mu[2] ~ normal(log(.5),1);
  subj_mu[3] ~ normal(log(.5),0.5);
  
  //priors for deviation of subject params from their mean.
  subj_sigma[1] ~ cauchy(0,5); 
  //these have lower prior SDs because our priors for them originally, from Palmeri et al., were lower.
  subj_sigma[2] ~ cauchy(0,3); 
  subj_sigma[3] ~ cauchy(0,2);
  
  
  ////////////////////
  //SUBJECT LEVEL
  exp_val = rep_array(0,N,max(cue),NUM_CHOICES);
  
  alpha_pr_var ~ normal(0,1);
  k_pr_var ~ normal(0,1);
  tau_pr_var ~ normal(0,1);

  for (t in 1:NUM_TRIALS){//loop through timesteps.
    for(j in 1:NUM_CHOICES){
      v[j]=logit(exp_val[cue[i],j]/4+0.75);
      #reinforcement learning
      pred_err=choice_outcomes[t,j]-exp_val[trial_runid[t],cue[t],j]; 
      exp_val[trial_runid[t],cue[t],j] = exp_val[trial_runid[t],cue[t],j] + alpha[trial_runid[t]]*pred_err;
    }
    #user-defined linear ballistic accumulator function based on that written by Annis, Miller, & Palmeri (2017)
    response_time[t] ~ lba(response[t],k[trial_runid[t]],A,v,s,tau[trial_runid[t]]);
  }
}

