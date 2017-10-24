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
  int<lower=1,upper=2> outcome_type[N,T]; #zero outcome types are outcome types where there is no choice.
  #OUTCOME_TYPE_REW = 1; #can't actually define them here but we can emember what they are.
  #OUTCOME_TYPE_PUN = 2;
  int<lower=1> run_id[N,T];
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
  vector[2] mu_p_rm[2,R];
  vector<lower=0>[2] sigma_rm[2,R];
  #2D array of vectors; each vector represents data for reward or punishment.
  

  # Subject-level raw parameters (for Matt trick)
  vector[N] alpha_rew_pr;   # learning rate for rewards
  vector[N] alpha_pun_pr;   # learning rate for punishment
  vector[N] beta_rew_pr;  # inverse temperature
  vector[N] beta_pun_pr;  # inverse temperature
  matrix[N,R] alpha_rew_pr_run_multiplier; 
  matrix[N,R] alpha_pun_pr_run_multiplier;
  matrix[N,R] beta_rew_pr_run_multiplier;
  matrix[N,R] beta_pun_pr_run_multiplier;
  #although there are as many values in the run dimension as there are runs
  #we will constrain the first value to 1.
}

transformed parameters {
  # Transform subject-level raw parameters
  vector<lower=0,upper=1>[N] alpha_rew;
  vector<lower=0,upper=1>[N] alpha_pun;
  vector<lower=0,upper=5>[N] beta_rew;
  vector<lower=0,upper=5>[N] beta_pun;

  matrix<lower=0,upper=1>[N,R] alpha_rew_run_multiplier;
  matrix<lower=0,upper=1>[N,R] alpha_pun_run_multiplier;
  matrix<lower=0,upper=5>[N,R] beta_rew_run_multiplier;
  matrix<lower=0,upper=5>[N,R] beta_pun_run_multiplier;


  for (i in 1:N) {
    alpha_rew[i]  = Phi_approx( mu_p[1, 1] + sigma[1, 1] * alpha_rew_pr[i] );
    beta_rew[i]   = Phi_approx( mu_p[1, 2] + sigma[1, 2] * beta_pun_pr[i] ) * 5;
    
    alpha_pun[i]  = Phi_approx( mu_p[2, 1] + sigma[2, 1] * alpha_rew_pr[i] );
    beta_pun[i]   = Phi_approx( mu_p[2, 2] + sigma[2, 2] * beta_pun_pr[i] ) * 5;
    #I am not sure we need to transform the multipliers? They stand on their own, perhaps...
    alpha_rew_run_multiplier[i,1]=1;
    beta_rew_run_multiplier[i,1]=1;
    alpha_pun_run_multiplier[i,1]=1;
    beta_pun_run_multiplier[i,1]=1;
    for (r in 2:R){
    alpha_rew_run_multiplier[i,r]  = Phi_approx( mu_p_rm[1, r, 1] + sigma_rm[1, r, 1] * alpha_rew_pr_run_multiplier[i,r] );
    beta_rew_run_multiplier[i,r]   = Phi_approx( mu_p_rm[1, r, 2] + sigma_rm[1, r, 2] * beta_pun_pr_run_multiplier[i,r] ) * 5;
    
    alpha_pun_run_multiplier[i,r]  = Phi_approx( mu_p_rm[2, r, 1] + sigma_rm[2, r, 1] * alpha_rew_pr_run_multiplier[i,r] );
    beta_pun_run_multiplier[i,r]   = Phi_approx( mu_p_rm[2, r, 2] + sigma_rm[2, r, 2] * beta_pun_pr_run_multiplier[i,r] ) * 5;    }
  }
}

model {
  # Hyperparameters
  for (ot in 1:2){#cycle through outcome types
    mu_p[ot]  ~ normal(0, 1);
    sigma[ot] ~ cauchy(0, 5);
    
    #first run multiplier is not estimated; it's fixed at 1. Although this might seem less efficient (storing a dummy variable!), I think it makes the output more intuitive.
    mu_p_rm[ot,1] = 1;
    sigma_rm[ot,1] = 1;
    for (r in 2:R){
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
  alpha_rew_pr_run_multiplier  ~ normal(1,1);
  alpha_pun_pr_run_multiplier  ~ normal(1,1);
  beta_rew_pr_run_multiplier   ~ normal(1,1);
  beta_pun_pr_run_multiplier   ~ normal(1,1);

  for (i in 1:N) {
    #whatever we sampled the run multipliers to, constrain the *first* multiplier to 1 exactly.
    #because we don't need or want the multiplier for run 1 to vary.
    alpha_rew_pr_run_multiplier[N,1] = 1;
    alpha_pun_pr_run_multiplier[N,1] = 1;
    beta_rew_pr_run_multiplier[N,1] = 1;
    beta_pun_pr_run_multiplier[N,1] = 1;
    
    # Define values
    matrix[72,2] ev; 
      #matrix dim 1 represents iterations of each trial (reward or punishment)
      #matrix dim 2 represents the choice [left or right] that each value represents.
    real PEnc; # fictitious prediction error (PE-non-chosen)
    real PE;         # prediction error
    
    # Initialize values
      ev[,1] = rep_vector(0, 72); # initial ev values
      ev[,2] = rep_vector(0, 72); # initial ev values
    
    for (t in 1:(Tsubj[i])) {
      # compute action probabilities
      if (choice[i,t]!=0) {
        vector[N] alpha=rep_vector(0,N);
        vector[N] beta=rep_vector(0,N);
        matrix[N,R] alpha_pr_run_multiplier=rep_matrix(0,N,R);
        matrix[N,R] beta_pr_run_multiplier=rep_matrix(0,N,R);
        if(outcome_type[i,t]==1){
          alpha=alpha_rew;
          beta=beta_rew;
          alpha_pr_run_multiplier=alpha_rew_pr_run_multiplier;
          beta_pr_run_multiplier=beta_rew_pr_run_multiplier;
        }else if(outcome_type[i,t]==2){
          alpha=alpha_pun;
          beta=beta_pun;
          alpha_pr_run_multiplier=alpha_pun_pr_run_multiplier;
          beta_pr_run_multiplier=beta_pun_pr_run_multiplier;
        }else{
          reject("invalid outcome_type for i ",i," and t", t,". Dividing by zero to halt")
          #outcome_type[i,t]=1/0
        }
        #print("i ",i,"; and t ", t)
        choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta[i]*beta_pr_run_multiplier[i,run_id[i,t]] );
        # prediction error
        PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
        PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
  
        # value updating (learning)
        ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha[i] * PEnc * alpha_pr_run_multiplier[i,run_id[i,t]];
        ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[i] * PE * alpha_pr_run_multiplier[i,run_id[i,t]];
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
  vector[R] mu_alpha_rew_pr_run_multiplier; 
  vector[R] mu_alpha_pun_pr_run_multiplier;
  vector[R] mu_beta_rew_pr_run_multiplier;
  vector[R] mu_beta_pun_pr_run_multiplier;
  

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
  
  mu_alpha_rew_pr_run_multiplier[1] = 1;
  mu_alpha_pun_pr_run_multiplier[1] = 1;
  mu_beta_rew_pr_run_multiplier[1] = 1;
  mu_beta_pun_pr_run_multiplier[1] = 1;
  for (r in 2:R){
    mu_alpha_rew_pr_run_multiplier[r] = Phi_approx(mu_p_rm[1,r,1]);
    mu_beta_rew_pr_run_multiplier[r] = Phi_approx(mu_p_rm[1,r,2]) * 5;#not sure why we're multiplying by 5 here. 
    mu_alpha_pun_pr_run_multiplier[r] = Phi_approx(mu_p_rm[2,r,1]);
    mu_beta_pun_pr_run_multiplier[r] = Phi_approx(mu_p_rm[2,r,2])*5;
  
  }
  


  { # local section, this saves time and space
    for (i in 1:N) {
      # Define values
      matrix[72,2] ev;
      real PEnc; # fictitious prediction error (PE-non-chosen)
      real PE;         # prediction error

      # Initialize values
      log_lik[i] = 0;
      ev[,1] = rep_vector(0, 72); # initial ev values
      ev[,2] = rep_vector(0, 72); # initial ev values
      
      vector[N] alpha=rep_vector(0,N);
      vector[N] beta=rep_vector(0,N);
      
      vector[R] alpha_run_multiplier=rep_vector(1,R); 
      vector[R] beta_run_multiplier=rep_vector(1,R); 
      for (t in 1:(Tsubj[i])) {#loops through all the trials for each subject.
        p_trial[i,t] = trial[i,t];
        p_subjID[i,t] = subjid[i,t];
        p_cor_res[i,t] = cor_resp[i,t];
        p_cue_pos[i,t] = cue_pos[i,t];
        p_outcome[i,t] = outcome[i,t];
        p_choice[i,t] = choice[i,t];
        p_cue_freq[i,t] = cue_freq[i,t];
        if (choice[i,t]!=0) {
          
          if(outcome_type[i,t]==1){
            alpha=alpha_rew;
            beta=beta_rew;
            alpha_run_multiplier=alpha_rew_run_multiplier;
            beta_run_multiplier=beta_rew_run_multiplier;
          }else if(outcome_type[i,t]==2){
            alpha=alpha_pun;
            beta=beta_pun;
            alpha_run_multiplier=alpha_pun_run_multiplier;
            beta_run_multiplier=beta_pun_run_multiplier;
          }else{
            reject("invalid outcome_type for i ",i," and t", t,". Dividing by zero to halt")
            #outcome_type[i,t]=1/0
          }
          # Iterate log-likelihood
          log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i,t] |  to_vector(ev[cue[i,t],]) * beta[i]*beta_run_multiplier);
          
          # Posterior prediction
          y_hat[i,t] = categorical_rng( softmax(to_vector(ev[cue[i,t],]) * beta[i]));
          
          # prediction error
          PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
          PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
    
          # value updating (learning)
          ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha[i]*alpha_run_multiplier * PEnc;
          ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[i]*alpha_run_multiplier * PE;
        }
      }
    }
  }
}
