data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1,upper=T> Tsubj[N];
  int<lower=1,upper=36> N_cues[N];
  int<lower=0,upper=2> choice[N,T];
  int<lower=0,upper=36> cue[N,T];
  int<lower=0,upper=36> cue_freq[N,T];
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
  vector[N] alpha_pr;   # learning rate
  vector[N] beta_pr;  # inverse temperature
  vector[N] bias_pr;  # pr flip
}

transformed parameters {
  # Transform subject-level raw parameters
  vector<lower=0,upper=1>[N] alpha;
  vector<lower=0,upper=5>[N] beta;
  vector[N] bias;

  for (i in 1:N) {
    alpha[i]  = Phi_approx( mu_p[1] + sigma[1] * alpha_pr[i] );
    beta[i]   = Phi_approx( mu_p[2] + sigma[2] * beta_pr[i] ) * 5;
  }
  bias = mu_p[3] + sigma[3] * bias_pr;
}

model {
  # Hyperparameters
  mu_p  ~ normal(0, 1);
  sigma ~ cauchy(0, 1);

  # individual parameters
  alpha_pr  ~ normal(0,1);
  beta_pr   ~ normal(0,1);
  bias_pr  ~ normal(0,1);

  for (i in 1:N) {
    # Define values
    matrix[36,2] ev;
    real PEnc; # fictitious prediction error (PE-non-chosen)
    real PE;         # prediction error

    # Initialize values
    ev[,1] = rep_vector(0, 36); # initial ev values
    ev[,2] = rep_vector(0, 36); # initial ev values

    for (t in 1:(Tsubj[i])) {
      # compute action probabilities
      choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta[i] + bias[i]);

      # prediction error
      PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];

      ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[i] * PE;
    }
  }
}

generated quantities {
  # For group level parameters
  real<lower=0,upper=1> mu_alpha;
  real<lower=0,upper=5> mu_beta;
  real mu_bias;

  # For log likelihood calculation
  real log_lik[N];
  
  # For posterior predictive check
  real y_hat[N,T];
  for (r in 1:N) {
    for (c in 1:T) {
      y_hat[r,c] = 0;
    }
  }

  mu_alpha  = Phi_approx(mu_p[1]);
  mu_beta   = Phi_approx(mu_p[2]) * 5;
  mu_bias  =mu_p[3];

  { # local section, this saves time and space
    for (i in 1:N) {
      # Define values
      matrix[36,2] ev;
      real PEnc; # fictitious prediction error (PE-non-chosen)
      real PE;         # prediction error

      # Initialize values
      log_lik[i] = 0;
      ev[,1] = rep_vector(0, 36); # initial ev values
      ev[,2] = rep_vector(0, 36); # initial ev values

      for (t in 1:(Tsubj[i])) {
        # Iterate log-likelihood
        log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i,t] |  to_vector(ev[cue[i,t],]) * beta[i] + bias[i]);
        
        # Posterior prediction
        y_hat[i,t] = categorical_rng( softmax(to_vector(ev[cue[i,t],]) * beta[i]));

        # prediction error
        PE =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
  
        # value updating (learning)
        ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[i] * PE;
      }
    }
  }
}
