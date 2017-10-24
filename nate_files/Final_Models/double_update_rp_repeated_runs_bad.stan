data {
  int<lower=1> N;#number of subjects
  int<lower=1> T; #number of trials (max)
  int<lower=1> R; #number of runs (max)
  int<lower=1,upper=T> Tsubj[N]; #count of trials for each subject.
  int<lower=1,upper=144> N_cues[N];
  int<lower=0,upper=2> choice[N,T];
  int<lower=0,upper=144> cue[N,T];
  int trial[N,T];
  int cue_pos[N,T];
  int subjid[N,T];
  int cor_resp[N,T];
  int cue_freq[N,T];
  real outcome[N,T];
  int<lower=0,upper=2> outcome_type[N,T];
    #zero outcome types are outcome types where there is no choice.
    #e.g., at the end of a trial
    #OUTCOME_TYPE_REW = 1; #can't actually define them here but we can emember what they are.
    #OUTCOME_TYPE_PUN = 2;
  int<lower=0> run_id[N,T];
  #RUN1=1;
  #RUN2=2;
  #as for outcomes, we will make the trial count not be per run, and we won't add an extra dimension to most data
  #we will just include an extra variable recording which run a trial is from.
  #perhaps this is not hte most intuitive? but it is best to be consistent with how we handled outcome_type
}
transformed data {
}
parameters {
# Declare all parameters as vectors for vectorizing
  # Hyper(group)-parameters
  vector[2] mu_p[2];
  vector<lower=0>[2] sigma[2];
  #this is an array of vectors; each vector represents data for reward or punishment.
  #see Stan ref manual Section5.8 for a helpful example of how to index arrays of vectors.
  vector[2] mu_p_rm[2,R-1];
  vector<lower=0>[2] sigma_rm[2,R-1];
  #2D array of vectors; each vector represents data for reward or punishment.
  

  # Subject-level raw parameters (for Matt trick)
  vector[N] alpha_rew_pr;   # learning rate for rewards
  vector[N] alpha_pun_pr;   # learning rate for punishment
  vector[N] beta_rew_pr;  # inverse temperature
  vector[N] beta_pun_pr;  # inverse temperature
  vector[N] alpha_rew_pr_run_multiplier[R-1]; 
  vector[N] alpha_pun_pr_run_multiplier[R-1];
  vector[N] beta_rew_pr_run_multiplier[R-1];
  vector[N] beta_pun_pr_run_multiplier[R-1];
}

transformed parameters {
  # Transform subject-level raw parameters
  vector<lower=0,upper=1>[N] alpha_rew;
  vector<lower=0,upper=1>[N] alpha_pun;
  vector<lower=0,upper=5>[N] beta_rew;
  vector<lower=0,upper=5>[N] beta_pun;

  vector<lower=0,upper=1>[N] alpha_rew_run_multiplier[R-1];
  vector<lower=0,upper=1>[N] alpha_pun_run_multiplier[R-1];
  vector<lower=0,upper=5>[N] beta_rew_run_multiplier[R-1];
  vector<lower=0,upper=5>[N] beta_pun_run_multiplier[R-1];


  for (s in 1:N) {
    alpha_rew[s]  = Phi_approx( mu_p[1, 1] + sigma[1, 1] * alpha_rew_pr[s] );
    beta_rew[s]   = Phi_approx( mu_p[1, 2] + sigma[1, 2] * beta_pun_pr[s] ) * 5;
    
    alpha_pun[s]  = Phi_approx( mu_p[2, 1] + sigma[2, 1] * alpha_rew_pr[s] );
    beta_pun[s]   = Phi_approx( mu_p[2, 2] + sigma[2, 2] * beta_pun_pr[s] ) * 5;
    #I am not sure we need to transform the multipliers? They stand on their own, perhaps...
    for (r in 1:(R-1)){
      alpha_rew_run_multiplier[r,s]  = Phi_approx( mu_p_rm[1, r, 1] + sigma_rm[1, r, 1] * alpha_rew_pr_run_multiplier[r,s] );
      beta_rew_run_multiplier[r,s]   = Phi_approx( mu_p_rm[1, r, 2] + sigma_rm[1, r, 2] * beta_pun_pr_run_multiplier[r,s] ) * 5;
      
      alpha_pun_run_multiplier[r,s]  = Phi_approx( mu_p_rm[2, r, 1] + sigma_rm[2, r, 1] * alpha_rew_pr_run_multiplier[r,s] );
      beta_pun_run_multiplier[r,s]   = Phi_approx( mu_p_rm[2, r, 2] + sigma_rm[2, r, 2] * beta_pun_pr_run_multiplier[r,s] ) * 5;    
    }
  }
}

model {
  # Hyperparameters
  for (ot in 1:2){#cycle through outcome types
    mu_p[ot]  ~ normal(0, 1);
    sigma[ot] ~ cauchy(0, 5);
    
    for (r in 1:(R-1)){
      mu_p_rm[ot,r]  ~ normal(1, 1);
      sigma_rm[ot,r] ~ cauchy(0, 5);
    }
  }
  
  
  # individual parameters
  alpha_rew_pr  ~ normal(0,1);
  alpha_pun_pr  ~ normal(0,1);
  beta_rew_pr   ~ normal(0,1);
  beta_pun_pr   ~ normal(0,1);

  #these are set with mean of 1 because I am treating them as *multipliers*
  #an array of run modifiers. The first element has modifiers for run2;  the second for run 3, and so on; all relative to run 1
  #of course in this dataset we only have two runs, but the design here will be extensible for more than two runs :-)
  for (r in 1:(R-1)){
    alpha_rew_pr_run_multiplier[r]  ~ normal(1,1);
    alpha_pun_pr_run_multiplier[r]  ~ normal(1,1);
    beta_rew_pr_run_multiplier[r]   ~ normal(1,1);
    beta_pun_pr_run_multiplier[r]   ~ normal(1,1);
  }
  
  for (s in 1:N) {
    # Define values
    matrix[144,2] ev; 
      #matrix dim 1 represents iterations of each trial (reward or punishment)
      #matrix dim 2 represents the choice [left or right] that each value represents.
    real PEnc; # fictitious prediction error (PE-non-chosen)
    real PE;         # prediction error
    
    # Initialize values
      ev[,1] = rep_vector(0, 144); # initial ev values
      ev[,2] = rep_vector(0, 144); # initial ev values
    
    for (t in 1:(Tsubj[s])) {
      # compute action probabilities
      if (choice[s,t]!=0) {
        real alpha_s=0;
        real beta_s=0;
        real alpha_pr_run_multiplier_s=1;
        real beta_pr_run_multiplier_s=1;
        if(outcome_type[s,t]==1){
          alpha_s=alpha_rew[s];
          beta_s=beta_rew[s];
          if(run_id[s,t]>1){#only run if run_id is greater than 1; otherwise we fix the multiplier to 1, by default.
            alpha_pr_run_multiplier_s=alpha_rew_pr_run_multiplier[run_id[s,t]-1,s];
            beta_pr_run_multiplier_s=beta_rew_pr_run_multiplier[run_id[s,t]-1,s];
          }
        }else if(outcome_type[s,t]==2){
          alpha_s=alpha_pun[s];
          beta_s=beta_pun[s];
          if(run_id[s,t]>1){#only run if run_id is greater than 1; otherwise we fix the multiplier to 1, by default.
            alpha_pr_run_multiplier_s=alpha_pun_pr_run_multiplier[run_id[s,t]-1,s];
            beta_pr_run_multiplier_s=beta_pun_pr_run_multiplier[run_id[s,t]-1,s];
          }
        }else{
          reject("invalid outcome_type for s ",s," and t", t,". Dividing by zero to halt")
          #outcome_type[s,t]=1/0
        }
        #print("s ",s,"; and t ", t)
        choice[s,t] ~ categorical_logit( to_vector(ev[cue[s,t],]) * beta_s*beta_pr_run_multiplier_s );
        # prediction error
        PE   =  outcome[s,t] - ev[cue[s,t],choice[s,t]];
        PEnc = -outcome[s,t] - ev[cue[s,t],3-choice[s,t]];
  
        # value updating (learning)
        ev[cue[s,t],3-choice[s,t]] = ev[cue[s,t],3-choice[s,t]] + alpha_s * PEnc * alpha_pr_run_multiplier_s;
        ev[cue[s,t],choice[s,t]] = ev[cue[s,t],choice[s,t]] + alpha_s * PE * alpha_pr_run_multiplier_s;
      }
    }
  }
}

generated quantities {
  # For group level parameters
  real<lower=0,upper=1> mu_alpha_rew;
  real<lower=0,upper=5> mu_beta_rew;
  real<lower=0,upper=1> mu_alpha_pun;
  real<lower=0,upper=5> mu_beta_pun;
  vector[R-1] mu_alpha_rew_run_multiplier; 
  vector[R-1] mu_alpha_pun_run_multiplier;
  vector[R-1] mu_beta_rew_run_multiplier;
  vector[R-1] mu_beta_pun_run_multiplier;
  

  # For log likelihood calculation
  real log_lik[N];
  
  # For posterior predictive check
  real y_hat[N,T];
  real p_trial[N,T];
  real p_subjID[N,T];
  real p_cor_res[N,T];
  real p_cue_pos[N,T];
  real p_cue_freq[N,T];
  real p_choice[N,T];
  real p_outcome[N,T];
  
  #we have to initialize these values, becasue we are getting an error otherwise.
  p_trial = rep_array(0,N,T);
  p_subjID = rep_array(0,N,T);
  p_cor_res = rep_array(0,N,T);
  p_cue_pos = rep_array(0,N,T);
  p_outcome = rep_array(0,N,T);
  p_choice = rep_array(0,N,T);
  p_cue_freq = rep_array(0,N,T);

  for (s in 1:N) {
    for (c in 1:T) {
      y_hat[s,c] = 0;
    }
  }

  mu_alpha_rew  = Phi_approx(mu_p[1, 1]);
  mu_beta_rew   = Phi_approx(mu_p[1, 2]) * 5;

  mu_alpha_pun  = Phi_approx(mu_p[2, 1]);
  mu_beta_pun   = Phi_approx(mu_p[2, 2]) * 5;
  
  for (r in 1:(R-1)){
    mu_alpha_rew_run_multiplier[r] = Phi_approx(mu_p_rm[1,r,1]);
    mu_beta_rew_run_multiplier[r] = Phi_approx(mu_p_rm[1,r,2]) * 5;#not sure why we're multiplying by 5 here. 
    mu_alpha_pun_run_multiplier[r] = Phi_approx(mu_p_rm[2,r,1]);
    mu_beta_pun_run_multiplier[r] = Phi_approx(mu_p_rm[2,r,2])*5;
  }
  
  { # local section, this saves time and space
    real alpha_s=0;
    real beta_s=0;
    real alpha_run_multiplier_s=1;
    real beta_run_multiplier_s=1;
    for (s in 1:N) {
      # Define values
      matrix[144,2] ev;
      real PEnc; # fictitious prediction error (PE-non-chosen)
      real PE;         # prediction error

      # Initialize values
      log_lik[s] = 0;
      ev[,1] = rep_vector(0, 144); # initial ev values
      ev[,2] = rep_vector(0, 144); # initial ev values
      
      for (t in 1:(Tsubj[s])) {#loops through all the trials for each subject.
        p_trial[s,t] = trial[s,t];
        p_subjID[s,t] = subjid[s,t];
        p_cor_res[s,t] = cor_resp[s,t];
        p_cue_pos[s,t] = cue_pos[s,t];
        p_outcome[s,t] = outcome[s,t];
        p_choice[s,t] = choice[s,t];
        p_cue_freq[s,t] = cue_freq[s,t];
        if (choice[s,t]!=0) {
          if(run_id[s,t]==1){
            #these four are initialized above to save time but should be reset here.
            alpha_s=0;
            beta_s=0;
            alpha_run_multiplier_s=1;
            beta_run_multiplier_s=1;
          }
          if(outcome_type[s,t]==1){
            alpha_s=alpha_rew[s];
            beta_s=beta_rew[s];
            if(run_id[s,t]>1){#only run if run_id is greater than 1; otherwise we leave the multiplier to default to 1.
              alpha_run_multiplier_s=alpha_rew_run_multiplier[run_id[s,t]-1,s];
              beta_run_multiplier_s=beta_rew_run_multiplier[run_id[s,t]-1,s];
            }
          }else if(outcome_type[s,t]==2){
            alpha_s=alpha_pun[s];
            beta_s=beta_pun[s];
            if(run_id[s,t]>1){#only run if run_id is greater than 1; otherwise we leave the multiplier to default to 1.
              alpha_run_multiplier_s=alpha_pun_run_multiplier[run_id[s,t]-1,s];
              beta_run_multiplier_s=beta_pun_run_multiplier[run_id[s,t]-1,s];
            }
          }else{
            reject("invalid outcome_type for s ",s," and t", t,". Dividing by zero to halt")
            #outcome_type[s,t]=1/0
          }
          # Iterate log-likelihood
          log_lik[s] = log_lik[s] + categorical_logit_lpmf( choice[s,t] |  to_vector(ev[cue[s,t],]) * beta_s*beta_run_multiplier_s);
          
          # Posterior prediction
          y_hat[s,t] = categorical_rng( softmax(to_vector(ev[cue[s,t],]) * beta_s*beta_run_multiplier_s));
          
          # prediction error
          PE   =  outcome[s,t] - ev[cue[s,t],choice[s,t]];
          PEnc = -outcome[s,t] - ev[cue[s,t],3-choice[s,t]];
    
          # value updating (learning)
          ev[cue[s,t],3-choice[s,t]] = ev[cue[s,t],3-choice[s,t]] + alpha_s*alpha_run_multiplier_s * PEnc;
          ev[cue[s,t],choice[s,t]] = ev[cue[s,t],choice[s,t]] + alpha_s*alpha_run_multiplier_s * PE;
        }
      }
    }
  }
}
