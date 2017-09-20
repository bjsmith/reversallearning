data {
  int<lower=1> N;#subject N
  int<lower=1> T; #Trial T
  int<lower=1,upper=T> Tsubj[N]; #count of trials for each subject.
  int<lower=1,upper=72> N_cues[N];
  int<lower=0,upper=2> choice[N,T];
  int<lower=0,upper=72> cue[N,T];
#zeros appear in the trial variable where there was no actual event to pay attention to.
#I am not sure why this is a problem, though.
#I don't think there are any zeros past the TSubj limit, and this stan script doesn't use any past that point either.
  int trial[N,T];
  int cue_pos[N,T];
  int subjid[N,T];
  int cor_resp[N,T];
  int cue_freq[N,T];
  real outcome[N,T];
  int<lower=0,upper=2> outcome_type[N,T]; #zero outcome types are outcome types where there is no choice.
  #OUTCOME_TYPE_REW = 1; #can't actually define them here but we can emember what they are.
  #OUTCOME_TYPE_PUN = 2;
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

  # Subject-level raw parameters (for Matt trick)
  vector[N] alpha_rew_pr;   # learning rate for rewards
  vector[N] alpha_pun_pr;   # learning rate for punishment
  vector[N] beta_rew_pr;  # inverse temperature
  vector[N] beta_pun_pr;  # inverse temperature
}

transformed parameters {
  # Transform subject-level raw parameters
  vector<lower=0,upper=1>[N] alpha_rew;
  vector<lower=0,upper=1>[N] alpha_pun;
  vector<lower=0,upper=5>[N] beta_rew;
  vector<lower=0,upper=5>[N] beta_pun;

  reject("The code below is faulty - beta_rew and beta_pun and alpha_pun and alpha_rew are confused. Don't run!")
  for (i in 1:N) {
    alpha_rew[i]  = Phi_approx( mu_p[1, 1] + sigma[1, 1] * alpha_rew_pr[i] );
    beta_rew[i]   = Phi_approx( mu_p[1, 2] + sigma[1, 2] * beta_pun_pr[i] ) * 5;
    
    alpha_pun[i]  = Phi_approx( mu_p[2, 1] + sigma[2, 1] * alpha_rew_pr[i] );
    beta_pun[i]   = Phi_approx( mu_p[2, 2] + sigma[2, 2] * beta_pun_pr[i] ) * 5;
  }
}

model {
  # Hyperparameters
  for (ot in 1:2){#cycle through outcome types
    mu_p[ot]  ~ normal(0, 1);
    sigma[ot] ~ cauchy(0, 5);
  }
  
  # individual parameters
  // alpha_pr  ~ normal(0,1);
  // beta_pr   ~ normal(0,1);
  alpha_rew_pr  ~ normal(0,1);
  alpha_pun_pr  ~ normal(0,1);
  beta_rew_pr   ~ normal(0,1);
  beta_pun_pr   ~ normal(0,1);

  for (i in 1:N) {
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
        if(outcome_type[i,t]==1){
          alpha=alpha_rew;
          beta=beta_rew;
        }else if(outcome_type[i,t]==2){
          alpha=alpha_pun;
          beta=beta_pun;
        }else{
          reject("invalid outcome_type for i ",i," and t", t,". Dividing by zero to halt")
          #outcome_type[i,t]=1/0
        }
        #print("i ",i,"; and t ", t)
        choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta[i] );
        # prediction error
        PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
        PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
  
        # value updating (learning)
        
          ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha[i] * PEnc;
          ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[i] * PE;
        
          
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

  for (r in 1:N) {
    for (c in 1:T) {
      y_hat[r,c] = 0;
    }
  }

  mu_alpha_rew  = Phi_approx(mu_p[1, 1]);
  mu_beta_rew   = Phi_approx(mu_p[1, 2]) * 5;

  mu_alpha_pun  = Phi_approx(mu_p[2, 1]);
  mu_beta_pun   = Phi_approx(mu_p[2, 2]) * 5;


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
      


      for (t in 1:(Tsubj[i])) {#loops through all the trials for each subject.
        p_trial[i,t] = trial[i,t];
        p_subjID[i,t] = subjid[i,t];
        p_cor_res[i,t] = cor_resp[i,t];
        p_cue_pos[i,t] = cue_pos[i,t];
        p_outcome[i,t] = outcome[i,t];
        p_choice[i,t] = choice[i,t];
        p_cue_freq[i,t] = cue_freq[i,t];
        if (choice[i,t]!=0) {
          vector[N] alpha=rep_vector(0,N);
          vector[N] beta=rep_vector(0,N);
          if(outcome_type[i,t]==1){
            alpha=alpha_rew;
            beta=beta_rew;
          }else if(outcome_type[i,t]==2){
            alpha=alpha_pun;
            beta=beta_pun;
          }else{
            reject("invalid outcome_type for i ",i," and t", t,". Dividing by zero to halt")
            #outcome_type[i,t]=1/0
          }
          # Iterate log-likelihood
          log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i,t] |  to_vector(ev[cue[i,t],]) * beta[i]);
          
          # Posterior prediction
          y_hat[i,t] = categorical_rng( softmax(to_vector(ev[cue[i,t],]) * beta[i]));
          
          # prediction error
          PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
          PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
    
          # value updating (learning)
          ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha[i] * PEnc;
          ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[i] * PE;
        }
      }
    }
  }
}
