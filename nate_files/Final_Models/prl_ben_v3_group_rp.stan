data {
  int<lower=1> N; // number of subjects
  int<lower=1> Gr_N; // number of subject groups
  int<lower=1> T; // number of trials
  int<lower=1,upper=T> Tsubj[N];  //trials per subject???
  int<lower=1,upper=T> subjGr[N]; // group identifier
  int<lower=1,upper=72> N_cues[N];
  int<lower=0,upper=2> choice[N,T];
  int<lower=0,upper=72> cue[N,T];
  real outcome[N,T];
  int<lower=1,upper=2> outcome_type[N,T];
  #OUTCOME_TYPE_REW = 1; #can't actually define them here but we can emember what they are.
  #OUTCOME_TYPE_PUN = 2;
  
}
transformed data {
}
parameters {
# Declare all parameters as vectors for vectorizing
  # Hyper(group)-parameters
  real mu_p[2, 2, Gr_N];
  #first dimension denotes parameters
  #second dimension denotes reward and punishment (respectively) 
  #third dimension denotes groups.
  
  real<lower=0> sigma[2, 2, Gr_N];
  #first dimension denotes parameters
  #second dimension denotes reward and punishment (respectively) 
  #third dimension denotes groups.
  
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

  for (i in 1:N) {
    int g_i = subjGr[i];
    alpha_rew[i]  = Phi_approx( mu_p[1, 1, g_i] + sigma[1, 1, g_i] * alpha_rew_pr[i] );
    alpha_pun[i]  = Phi_approx( mu_p[1, 2, g_i] + sigma[1, 2, g_i] * alpha_pun_pr[i] );
    
    beta_rew[i]   = Phi_approx( mu_p[2, 1, g_i] + sigma[2, 1, g_i] * beta_rew_pr[i] ) * 5;
    beta_pun[i]   = Phi_approx( mu_p[2, 2, g_i] + sigma[2, 2, g_i] * beta_pun_pr[i] ) * 5;
  }
}

model {
  # Hyperparameters
  for (i_p in 1:2){
    for (i_g in 1:Gr_N){
      mu_p[i_p,i_g]  ~ normal(0, 1);
      sigma[i_p,i_g] ~ cauchy(0, 1);
    }
  }

  # individual parameters
  alpha_rew_pr  ~ normal(0,1);
  alpha_pun_pr  ~ normal(0,1);
  beta_rew_pr   ~ normal(0,1);
  beta_pun_pr   ~ normal(0,1);

  for (i in 1:N) {
    # Define values
    matrix[72,2] ev; #Dim 1: trial number, Dim 2:???
    real PEnc; # fictitious prediction error (PE-non-chosen)
    real PE;         # prediction error
    #real ev_chosen; #obsolete
    #real theta;  #obsolete

    # Initialize values
    ev[,1] = rep_vector(0, 72); # initial ev values
    ev[,2] = rep_vector(0, 72); # initial ev values
    
    #these no longer used.
    #theta_rew = pow(3, beta_rew[i]) - 1;
    #theta_pun = pow(3, beta_pun[i]) - 1;

    for (t in 1:(Tsubj[i])) {
      # value updating (learning)
      #we define different alpha and beta value for the trial
      #depnding on whether the outcome type for the trial is Reward or Punishment [avoidance]
      real alpha_val = 0;
      real beta_val = 0;
      if(outcome_type[i,t]==1){
        alpha_val = alpha_rew[i];
        beta_val = beta_rew[i];
      }else if (outcome_type[i,t]==2){
        alpha_val = alpha_pun[i];
        beta_val = beta_rew[i];
      }
      
      # compute action probabilities
      choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta_val );

      # prediction error
      PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
      PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];

      # Store chosen EV for fictive updating
      #ev_chosen = ev[cue[i,t],choice[i,t]];

      ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha_val * PEnc;
      ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha_val * PE;
    }
  }
}

generated quantities {
  # For group level parameters
  real<lower=0,upper=1> mu_alpha_rew[Gr_N];
  real<lower=0,upper=1> mu_alpha_pun[Gr_N];
  real<lower=0,upper=5> mu_beta_rew[Gr_N];
  real<lower=0,upper=5> mu_beta_pun[Gr_N];

  # For log likelihood calculation
  real log_lik[N];
  
  # For posterior predictive check
  real y_hat[N,T];
  for (r in 1:N) {
    for (c in 1:T) {
      y_hat[r,c] = 0;
    }
  }
  
  for (g in 1:Gr_N){#for each group
    mu_alpha_rew[g]  = Phi_approx(mu_p[1, 1, g]);
    mu_alpha_pun[g]  = Phi_approx(mu_p[1, 2, g]);
    mu_beta_rew[g]   = Phi_approx(mu_p[2, 1, g]) * 5;
    mu_beta_pun[g]   = Phi_approx(mu_p[2, 2, g]) * 5;
  }
  

  { # local section, this saves time and space
    for (i in 1:N) {
      # Define values
      matrix[72,2] ev;
      real PEnc; # fictitious prediction error (PE-non-chosen)
      real PE;         # prediction error
      real ev_chosen;
      #real theta; #obsolete

      # Initialize values
      log_lik[i] = 0;
      ev[,1] = rep_vector(0, 72); # initial ev values
      ev[,2] = rep_vector(0, 72); # initial ev values
      #theta = pow(3, beta[i]) - 1; #obsolete

      for (t in 1:(Tsubj[i])) {
        #we use a different alpha and beta value for the trial
        #depnding on whether the outcome type for the trial is Reward or Punishment [avoidance]
        real alpha_val = 0;
        real beta_val = 0;
        if(outcome_type[i,t]==1){
          alpha_val = alpha_rew[i];
          beta_val = beta_rew[i];
        }else if (outcome_type[i,t]==2){
          alpha_val = alpha_pun[i];
          beta_val = beta_rew[i];
        }
      
        # Iterate log-likelihood
        log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i,t] |  to_vector(ev[cue[i,t],]) * beta_val);
        
        # Posterior prediction
        y_hat[i,t] = categorical_rng( softmax(to_vector(ev[cue[i,t],]) * beta_val));

        # prediction error
        PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
        PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
  
        # Store chosen EV for fictive updating
        ev_chosen = ev[cue[i,t],choice[i,t]];
  
        # value updating (learning)
        ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha_val * PEnc;
        ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha_val * PE;
      }
    }
  }
}
