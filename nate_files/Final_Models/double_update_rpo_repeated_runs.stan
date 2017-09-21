data {
  int<lower=1> N;#number of subjects
  int<lower=1> T; #number of trials (max)
  int<lower=1> R; #number of runs (max)
  int<lower=1,upper=T> Tsubj[N]; #count of trials for each subject.
  int<lower=1,upper=100> N_cues[N];
  int<lower=0,upper=2> choice[N,T];
  int<lower=0,upper=100> cue[N,T];
  int trial[N,T];
  int cue_pos[N,T];
  int subjid[N,T];
  int cor_resp[N,T];
  int cue_freq[N,T];
  real outcome[N,T];
  int<lower=0,upper=2> outcome_type[N,T];
  #zero outcome types are outcome types where there is no choice.
  #e.g., at the end of a trial
  int<lower=0> run_id[N,T];
  #RUN1=1;
  #RUN2=2;
  #as for outcomes, we will make the trial count not be per run, and we won't add an extra dimension to most data
  #we will just include an extra variable recording which run a trial is from.
  #perhaps this is not hte most intuitive? but it is best to be consistent with how we handled outcome_type
}
transformed data {
  int OT_REW=1;
  int OT_PUN=2;
  int OT_MIN=1;
  int OT_MAX=2;#these two are for use in a for loop; apparently we can't use an array for a for loop here.
  #define these here for easier reading below!
}
parameters {
# Declare all parameters as vectors for vectorizing
  # Hyper(group)-parameters
  vector[2] mu_p[2];
  vector<lower=0>[2] sigma[2];
  #this is an array of vectors; each vector represents data for reward or punishment.
  #each value in vector represents a different parameter.
  
  #see Stan ref manual Section5.8 for a helpful example of how to index arrays of vectors.
  vector[2] mu_p_rm[2,R-1];
  vector<lower=0>[2] sigma_rm[2,R-1];
  #2D array of vectors; each vector represents data for reward or punishment.
  

  # Subject-level raw parameters (for Matt trick)
  matrix[N,2] alpha_pr;   # learning rate for rewards, punishment
  matrix[N,2] beta_pr;  # inverse temperature for rewards,punishment
  matrix[N,2] alpha_pr_rm[R-1]; 
  #vector[N] alpha_pun_pr_rm[R-1];
  matrix[N,2] beta_pr_rm[R-1];
  #vector[N] beta_pun_pr_rm[R-1];
}

transformed parameters {
  # Transform subject-level raw parameters
  real<lower=0,upper=1> alpha[N,R,2];
  #real<lower=0,upper=1> alpha_pun[N,R];
  real<lower=0,upper=5> beta[N,R,2];
  #real<lower=0,upper=5>[N,R] beta_pun;

  #so, the overall learning rate needs to be constrained between 0 and 1 or it doesn't make sense within the theory.
  #so we need to either:
  #1. express that constraint with the run modifier included, or
  #2. use a run modifier that will somehow not push the distribution past 1.
    #I think option 2. is probably not credible
    #So we need to do option 1, as Nathan has suggested.
    #
  for (s in 1:N) {
    #run 1
    alpha[s,1,OT_REW]  = Phi_approx( mu_p[OT_REW, 1] + sigma[OT_REW, 1] * alpha_pr[s,OT_REW] );
    beta[s,1,OT_REW]   = Phi_approx( mu_p[OT_REW, 2] + sigma[OT_REW, 2] * beta_pr[s,OT_REW] ) * 5;
    
    alpha[s,1,OT_PUN]  = Phi_approx( mu_p[OT_PUN, 1] + sigma[OT_PUN, 1] * alpha_pr[s,OT_PUN] );
    beta[s,1,OT_PUN]   = Phi_approx( mu_p[OT_PUN, 2] + sigma[OT_PUN, 2] * beta_pr[s,OT_PUN] ) * 5;
    #I am not sure we need to transform the multipliers? They stand on their own, perhaps...
    for (r in 2:R){
      #Nate's suggestion
      alpha[s,r,OT_REW]  = Phi_approx( 
        mu_p[OT_REW, 1] + sigma[OT_REW, 1] * alpha_pr[s,OT_REW] + 
        mu_p_rm[OT_REW,r-1,1] + sigma_rm[OT_REW, r-1, 1] * alpha_pr_rm[r-1,s,OT_REW]);
      beta[s,r,OT_REW]  = Phi_approx( 
        mu_p[OT_REW, 2] + sigma[OT_REW, 2] * beta_pr[s,OT_REW] + 
        mu_p_rm[OT_REW,r-1,2] + sigma_rm[OT_REW, r-1, 2] * beta_pr_rm[r-1,s,OT_REW]);
      
      alpha[s,r,OT_PUN]  = Phi_approx( 
        mu_p[OT_PUN, 1] + sigma[OT_PUN, 1] * alpha_pr[s,OT_PUN] + 
        mu_p_rm[OT_PUN,r-1,1] + sigma_rm[OT_PUN, r-1, 1] * alpha_pr_rm[r-1,s,OT_PUN]);
      beta[s,r,OT_PUN]  = Phi_approx( 
        mu_p[OT_PUN, 2] + sigma[OT_PUN, 2] * beta_pr[s,OT_PUN] + 
        mu_p_rm[OT_PUN,r-1,2] + sigma_rm[OT_PUN, r-1, 2] * beta_pr_rm[r-1,s,OT_PUN]);
    }
  }
}

model {
  real alpha_s;
  real beta_s;
  real alpha_rm_s;
  real beta_rm_s;
  matrix[100,2] ev; 
  
  # Hyperparameters
  for (ot in OT_MIN:OT_MAX){#cycle through outcome types
    mu_p[ot]  ~ normal(0, 1);
    sigma[ot] ~ cauchy(0, 5);
    
    for (r in 1:(R-1)){
      mu_p_rm[ot,r]  ~ normal(0, 1);
      sigma_rm[ot,r] ~ cauchy(0, 5);
    }
  }
  
  # individual parameters
  alpha_pr[,1]  ~ normal(0,1);
  alpha_pr[,2]  ~ normal(0,1);
  beta_pr[,1]   ~ normal(0,1);
  beta_pr[,2]   ~ normal(0,1);
  #beta_pun_pr   ~ normal(0,1);

  #an array of run modifiers. The first element has modifiers for run2;  the second for run 3, and so on; all relative to run 1
  #of course in this dataset we only have two runs, but the design here will be extensible for more than two runs :-)
  for (r in 1:(R-1)){
    alpha_pr_rm[r,,1]  ~ normal(0,1);
    alpha_pr_rm[r,,2]  ~ normal(0,1);
    #alpha_pun_pr_rm[r]  ~ normal(0,1);
    beta_pr_rm[r,,1]   ~ normal(0,1);
    beta_pr_rm[r,,2]   ~ normal(0,1);
    #beta_pun_pr_rm[r]   ~ normal(0,1);
  }
  
  for (s in 1:N) {
    # Define values
    
      #matrix dim 1 represents iterations of each trial (reward or punishment)
      #matrix dim 2 represents the choice [left or right] that each value represents.
    real PEnc; # fictitious prediction error (PE-non-chosen)
    real PE;         # prediction error
    
    # Initialize values
      #ev[,1] = rep_vector(0, 100); # initial ev values
      #ev[,2] = rep_vector(0, 100); # initial ev values
      ev = rep_matrix(0, 100, 2); # initial ev values
    
    for (t in 1:(Tsubj[s])) {
      # compute action probabilities
      if (choice[s,t]!=0) {
        // alpha_s=0;
        // beta_s=0;
        // alpha_rm_s=0;
        // beta_rm_s=0;
        alpha_s=alpha[s,run_id[s,t],outcome_type[s,t]];
        beta_s=beta[s,run_id[s,t],outcome_type[s,t]];
/*        if(outcome_type[s,t]==OT_REW){
          alpha_s=alpha_rew[s,run_id[s,t]];
          beta_s=beta_rew[s,run_id[s,t]];
        }else if(outcome_type[s,t]==OT_PUN){
          alpha_s=alpha_pun[s,run_id[s,t]];
          beta_s=beta_pun[s,run_id[s,t]];
        }else{
          reject("invalid outcome_type for s ",s," and t", t,". Dividing by zero to halt")
        }
*/        #print("s ",s,"; and t ", t)
        choice[s,t] ~ categorical_logit( to_vector(ev[cue[s,t],]) * beta_s );
        # prediction error
        PE   =  outcome[s,t] - ev[cue[s,t],choice[s,t]];
        PEnc = -outcome[s,t] - ev[cue[s,t],3-choice[s,t]];
  
        # value updating (learning)
        ev[cue[s,t],3-choice[s,t]] = ev[cue[s,t],3-choice[s,t]] + alpha_s * PEnc;
        ev[cue[s,t],choice[s,t]] = ev[cue[s,t],choice[s,t]] + alpha_s * PE;
      }
    }
  }
}

generated quantities {
  # For group level parameters
  matrix<lower=0,upper=1>[R,2] mu_alpha;
  matrix<lower=0,upper=5>[R,2] mu_beta;
  #vector<lower=0,upper=1>[R] mu_alpha_pun;
  #vector<lower=0,upper=5>[R] mu_beta_pun;
  // vector[R-1] mu_alpha_rew_rm; 
  // vector[R-1] mu_alpha_pun_rm;
  // vector[R-1] mu_beta_rew_rm;
  // vector[R-1] mu_beta_pun_rm;
  
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

  mu_alpha[1,OT_REW]  = Phi_approx(mu_p[OT_REW, 1]);
  mu_beta[1,OT_REW]   = Phi_approx(mu_p[OT_REW, 2]) * 5;

  mu_alpha[1,OT_PUN]  = Phi_approx(mu_p[OT_PUN, 1]);
  mu_beta[1,OT_PUN]   = Phi_approx(mu_p[OT_PUN, 2]) * 5;
  
  for (r in 2:R){
    mu_alpha[r,OT_REW] = Phi_approx( mu_p[OT_REW, 1] + mu_p_rm[OT_REW,r-1,1]);
    mu_beta[r,OT_REW] = Phi_approx(  mu_p[OT_REW, 2] + mu_p_rm[OT_REW,r-1,2]) * 5;#not sure why we're multiplying by 5 here. 
    mu_alpha[r,OT_PUN] = Phi_approx( mu_p[OT_PUN, 1] + mu_p_rm[OT_PUN,r-1,1]);
    mu_beta[r,OT_PUN] = Phi_approx(  mu_p[OT_PUN, 2] + mu_p_rm[OT_PUN,r-1,2])*5;
  }
  
  { # local section, this saves time and space
    #Cues ARE specific to:
    #-runs
    #-reward and punishment tasks?
    #in other words, no cue is used across more than one run or RP. Therefore we can store EVs in this manner, without separately specifying which 
    #run or task they pertain to, so long as we have the cue.
    real alpha_s;
    real beta_s;
    #set these to 0 by default. They will take on some other value if we are not looking at Run 1.
    real alpha_rm_s=0;
    real beta_rm_s=0;
    matrix[100,2] ev;#one row for each of the two options for each choice.
    
    for (s in 1:N) {
      # Define values
      
      real PEnc; # fictitious prediction error (PE-non-chosen)
      real PE;         # prediction error

      # Initialize values
      log_lik[s] = 0;
      ev = rep_matrix(0, 100, 2); # initial ev values
      #ev[,2] = rep_vector(0, 100); # initial ev values
      
      for (t in 1:(Tsubj[s])) {#loops through all the trials for each subject.
        p_trial[s,t] = trial[s,t];
        p_subjID[s,t] = subjid[s,t];
        p_cor_res[s,t] = cor_resp[s,t];
        p_cue_pos[s,t] = cue_pos[s,t];
        p_outcome[s,t] = outcome[s,t];
        p_choice[s,t] = choice[s,t];
        p_cue_freq[s,t] = cue_freq[s,t];
        if (choice[s,t]!=0) {
          alpha_s=alpha[s,run_id[s,t],outcome_type[s,t]];
          beta_s=beta[s,run_id[s,t],outcome_type[s,t]];
          // if(outcome_type[s,t]==OT_REW){
          //   
          // }else if(outcome_type[s,t]==OT_PUN){
          //   alpha_s=alpha_pun[s,run_id[s,t]];
          //   beta_s=beta_pun[s,run_id[s,t]];
          // }else{
          //   reject("invalid outcome_type for s ",s," and t", t,". Dividing by zero to halt")
          //   #outcome_type[s,t]=1/0
          // }
          # Iterate log-likelihood
          #print(beta_rm_s)
          log_lik[s] = log_lik[s] + categorical_logit_lpmf( choice[s,t] |  to_vector(ev[cue[s,t],]) * beta_s);
          
          # Posterior prediction
          y_hat[s,t] = categorical_rng( softmax(to_vector(ev[cue[s,t],]) * beta_s));
          
          # prediction error
          PE   =  outcome[s,t] - ev[cue[s,t],choice[s,t]];
          PEnc = -outcome[s,t] - ev[cue[s,t],3-choice[s,t]];
    
          # value updating (learning)
          ev[cue[s,t],3-choice[s,t]] = ev[cue[s,t],3-choice[s,t]] + alpha_s * PEnc;
          ev[cue[s,t],choice[s,t]] = ev[cue[s,t],choice[s,t]] + alpha_s * PE;
        }
      }
    }
  }
}
