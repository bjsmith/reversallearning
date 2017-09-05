data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1,upper=T> Tsubj[N];
  int<lower=1,upper=36> N_cues[N];
  int<lower=0,upper=2> choice[N,T];
  int<lower=0,upper=36> cue[N,T];
  int reversal[N,T];
  int cue_freq[N,T];
  int cue_pos[N,T];
  int subjid[N,T];
  int cor_resp[N,T];
  real outcome[N,T];
}
transformed data {
}
parameters {
# Declare all parameters as vectors for vectorizing
  # Hyper(group)-parameters
  vector[3] mu_p;
  vector<lower=0>[3] sigma;

  # Subject-level raw parameters (for Matt trick)
  vector[N] A_pr;   # learning rate
  vector[N] AR_pr;   # learning rate
  vector[N] beta_pr;  # inverse temperature
}

transformed parameters {
  # Transform subject-level raw parameters
  vector<lower=0,upper=1>[N] A;
  vector<lower=0,upper=1>[N] AR;
  vector<lower=0,upper=10>[N] beta;

  for (i in 1:N) {
    A[i]  = Phi_approx( mu_p[1] + sigma[1] * A_pr[i] );
    AR[i]  = Phi_approx( mu_p[2] + sigma[2] * AR_pr[i] );
    beta[i]   = Phi_approx( mu_p[3] + sigma[3] * beta_pr[i] ) * 10;
  }
}

model {
  # Hyperparameters
  mu_p  ~ normal(0, 1);
  sigma ~ cauchy(0, 1);

  # individual parameters
  A_pr  ~ normal(0,1);
  AR_pr  ~ normal(0,1);
  beta_pr   ~ normal(0,1);

  for (i in 1:N) {
    # Define values
    vector[36] lr_vec;
    matrix[36,2] ev;
    real PEnc; # fictitious prediction error (PE-non-chosen)
    real PE;         # prediction error

    # Initialize values
    ev[,1] = rep_vector(0, 36); # initial ev values
    ev[,2] = rep_vector(0, 36); # initial ev values
    lr_vec = rep_vector(A[i], 36); 

    for (t in 1:(Tsubj[i])) {
      # compute action probabilities
      if (choice[i,t]!=0) {
        choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta[i] );
        # prediction error
        PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
  
        # value updating (learning)
        if (reversal[i,t]==1) {
          lr_vec[cue[i,t]] = AR[i]; 
        }
        # value updating (learning)
        ev = ev * lr_vec[cue[i,t]];
        ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + outcome[i,t];
      } else {
        ev = ev * lr_vec[cue[i,t]];
      }
    }
  }
}

generated quantities {
  # For group level parameters
  real<lower=0,upper=1> mu_A;
  real<lower=0,upper=1> mu_AR;
  real<lower=0,upper=10> mu_beta;

  # For log likelihood calculation
  real log_lik[N];
  
  # For posterior predictive check
  real y_hat[N,T];
  real p_subjID[N,T];
  real p_cor_res[N,T];
  real p_cue_pos[N,T];
  real p_cue_freq[N,T];
  real p_choice[N,T];
  real p_outcome[N,T];
  for (r in 1:N) {
    for (c in 1:T) {
      y_hat[r,c] = 0;
    }
  }

  mu_A  = Phi_approx(mu_p[1]);
  mu_AR  = Phi_approx(mu_p[2]);
  mu_beta   = Phi_approx(mu_p[3]) * 10;

  { # local section, this saves time and space
    for (i in 1:N) {
      # Define values
      vector[36] lr_vec;
      matrix[36,2] ev;
      real PEnc; # fictitious prediction error (PE-non-chosen)
      real PE;         # prediction error

      # Initialize values
      log_lik[i] = 0;
      ev[,1] = rep_vector(0, 36); # initial ev values
      ev[,2] = rep_vector(0, 36); # initial ev values
      lr_vec = rep_vector(A[i], 36); 

      for (t in 1:(Tsubj[i])) {
        p_subjID[i,t] = subjid[i,t];
        p_cor_res[i,t] = cor_resp[i,t];
        p_cue_pos[i,t] = cue_pos[i,t];
        p_outcome[i,t] = outcome[i,t];
        p_choice[i,t] = choice[i,t];
        p_cue_freq[i,t] = cue_freq[i,t];
        if (choice[i,t]!=0) {
          # Iterate log-likelihood
          log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i,t] |  to_vector(ev[cue[i,t],]) * beta[i]);
          # Posterior prediction
          y_hat[i,t] = categorical_rng( softmax(to_vector(ev[cue[i,t],]) * beta[i]));
          
          # prediction error
          PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
    
          # value updating (learning)
          if (reversal[i,t]==1) {
            lr_vec[cue[i,t]] = AR[i]; 
          }
          # value updating (learning)
          ev = ev * lr_vec[cue[i,t]];
          ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + outcome[i,t];
        } else {
          ev = ev * lr_vec[cue[i,t]];
        }
      }
    }
  }
}
