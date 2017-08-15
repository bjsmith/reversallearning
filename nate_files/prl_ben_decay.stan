data {
    int<lower=1> N;
    int<lower=1> T;
    int<lower=1,upper=T> Tsubj[N];
    int<lower=1,upper=44> N_cues[N];
    int<lower=0,upper=2> choice[N,T];
    int<lower=0,upper=44> cue[N,T];
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
    vector[36] ev;
    vector[36] PEnc; # fictitious prediction error (PE-non-chosen)
    real PE;         # prediction error
    real ev_chosen;
    real theta;

    # Initialize values
    ev = rep_vector(0,36); # initial ev values
    theta = pow(3, beta[i]) - 1;

    for (t in 1:(Tsubj[i])) {
      # compute action probabilities
      choice[i,t] ~ bernoulli_logit( ev[cue[i,t]] * theta );

      # value updating (learning)
      ev = ev * A[i];
      ev[cue[i,t]] = ev[cue[i,t]] + outcome[i,t];
    }
  }
}

generated quantities {
  # For group level parameters
  real<lower=0,upper=1> mu_A;
  real<lower=0,upper=5> mu_beta;

  # For log likelihood calculation
  real log_lik[N];

  mu_A  = Phi_approx(mu_p[1]);
  mu_beta   = Phi_approx(mu_p[2]) * 5;

  { # local section, this saves time and space
    for (i in 1:N) {
      # Define values
      vector[36] ev;
      vector[36] PEnc; # fictitious prediction error (PE-non-chosen)
      real PE;         # prediction error
      real ev_chosen;
      real theta;

      # Initialize values
      log_lik[i] = 0;
      ev = rep_vector(0,36); # initial ev values
      theta = pow(3, beta[i]) - 1;

      for (t in 1:(Tsubj[i])) {
        # Iterate log-likelihood
        log_lik[i] = log_lik[i] + bernoulli_logit_lpmf( choice[i,t] |  ev[cue[i,t]] * theta);
  
        # value updating (learning)
        ev = ev * A[i];
        ev[cue[i,t]] = ev[cue[i,t]] + outcome[i,t];
      }
    }
  }
}
