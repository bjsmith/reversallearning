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
  vector[2] mu_p;
  vector<lower=0>[2] sigma;

  # Subject-level raw parameters (for Matt trick)
  vector[N] A_pr;   # decay rate
  vector[N] beta_pr;  # inverse temperature
}

transformed parameters {
  # Transform subject-level raw parameters
  vector<lower=0,upper=1>[N] A;
  vector<lower=0,upper=5>[N] beta;

  for (i in 1:N) {
    A[i]  = Phi_approx( mu_p[1] + sigma[1] * A_pr[i] );
    beta[i]   = Phi_approx( mu_p[2] + sigma[2] * beta_pr[i] ) * 5;
  }
}

model {
  # Hyperparameters
  mu_p  ~ normal(0, 1);
  sigma ~ cauchy(0, 1);

  # individual parameters
  A_pr  ~ normal(0,1);
  beta_pr   ~ normal(0,1);

  for (i in 1:N) {
    # Define values
    matrix[36,2] ev;
    real PEnc; # fictitious prediction error (PE-non-chosen)
    real PE;         # prediction error

    # Initialize values
    ev[,1] = rep_vector(0, 36); # initial ev values
    ev[,2] = rep_vector(0, 36); # initial ev values

    for (t in 1:(Tsubj[i])) {
      if (choice[i,t]!=0) {
        # compute action probabilities
        choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta[i] );
  
        # value updating (learning)
        ev = ev * A[i];
        ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + outcome[i,t];
      } else {
        ev = ev * A[i];
      }
    }
  }
}

generated quantities {
  # For group level parameters
  real<lower=0,upper=1> mu_A;
  real<lower=0,upper=5> mu_beta;

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
      p_subjID[r,c] = 0;
      p_cor_res[r,c] = 0;
      p_cue_pos[r,c] = 0;
      p_cue_freq[r,c] = 0;
      p_choice[r,c] = 0;
      p_outcome[r,c] = 0;
    }
  }

  mu_A  = Phi_approx(mu_p[1]);
  mu_beta   = Phi_approx(mu_p[2]) * 5;

  { # local section, this saves time and space
    for (i in 1:N) {
      # Define values
      matrix[36,2] ev;
      real PEnc; # fictitious prediction error (PE-non-chosen)
      real PE;         # prediction error
      real ev_chosen;
      real theta;

      # Initialize values
      log_lik[i] = 0;
      ev[,1] = rep_vector(0, 36); # initial ev values
      ev[,2] = rep_vector(0, 36); # initial ev values

      for (t in 1:(Tsubj[i])) {
        p_subjID[i,t] = subjid[i,t];
        p_cor_res[i,t] = cor_resp[i,t];
        p_cue_pos[i,t] = cue_pos[i,t];
        p_outcome[i,t] = outcome[i,t];
        p_choice[i,t] = choice[i,t];
        p_cue_freq[i,t] = cue_freq[i,t];
        if (choice[i,t]!=0) {
          # Iterate log-likelihood
          log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i,t] |  to_vector(ev[cue[i,t],]) * beta[i] );
    
          # Posterior prediction
          y_hat[i,t] = categorical_rng( softmax(to_vector(ev[cue[i,t],]) * beta[i]));
    
          # value updating (learning)
          ev = ev * A[i];
          ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + outcome[i,t];
        } else {
          ev = ev * A[i];
        }
      }
    }
  }
}
