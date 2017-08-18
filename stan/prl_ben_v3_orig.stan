data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1,upper=T> Tsubj[N];
  #int<lower=1,upper=36> N_cues[N]; #
  int<lower=0,upper=2> choice[N,T]; # indicates the choice the subject made
  #int<lower=0,upper=2> correct_choice[N,T]; #Indicates the choice the subject should have made.
  int<lower=0,upper=36> cue[N,T]; #Indicates the image ID for subject N at trial T
  real outcome[N,T]; #indicates 1 for reward, 0 for non-reward
  
}
transformed data {
}
parameters {
  # Declare all parameters as vectors for vectorizing
  # Hyper(group)-parameters
  vector[2] mu_p;
  vector<lower=0>[2] sigma;
  
  # Subject-level raw parameters (for Matt trick)
  vector[N] alpha_pr;   # learning rate
  vector[N] beta_pr;  # inverse temperature
}

transformed parameters {
  # Transform subject-level raw parameters
  vector<lower=0,upper=1>[N] alpha;
  vector<lower=0,upper=5>[N] beta;
  
  for (i in 1:N) {
    alpha[i]  = Phi_approx( mu_p[1] + sigma[1] * alpha_pr[i] );
    beta[i]   = Phi_approx( mu_p[2] + sigma[2] * beta_pr[i] ) * 5;
  }
}

model {
  # Hyperparameters
  mu_p  ~ normal(0, 1);
  sigma ~ cauchy(0, 1);
  
  # individual parameters
  alpha_pr  ~ normal(0,1);
  beta_pr   ~ normal(0,1);
  
  for (i in 1:N) {#for each subject
    # Define values
    matrix[36,2] ev; #image by choice
    real PEnc; # fictitious prediction error (PE-non-chosen)
    real PE;         # prediction error
    real ev_chosen;
    real theta;
    
    # Initialize values
    ev[,1] = rep_vector(0, 36); # initial ev values
    ev[,2] = rep_vector(0, 36); # initial ev values
    #theta = pow(3, beta[i]) - 1;
    
    for (t in 1:(Tsubj[i])) {#for each trial
      # compute action probabilities
      choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta[i] );
      
      # prediction error
      #difference in the predicted and observed outcome.
      PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
      PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
      
      # Store chosen EV for fictive updating
      ev_chosen = ev[cue[i,t],choice[i,t]];
      
      # value updating (learning)
      ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha[i] * PEnc;
      ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[i] * PE;
      print("choice was ");
      print((choice[i,t]));
      print("; outcome was ");
      print((outcome[i,t]));
    }
  }
}

generated quantities {
  # For group level parameters
  real<lower=0,upper=1> mu_alpha;
  real<lower=0,upper=5> mu_beta;
  
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
  
  { # local section, this saves time and space
    for (i in 1:N) {
      # Define values
      matrix[36,2] ev;
      real PEnc; # fictitious prediction error (PE-non-chosen)
      real PE;         # prediction error
      real ev_chosen;
      real theta;
      real beta_cheat;
      real alpha_cheat;
      
      # Initialize values
      log_lik[i] = 0;
      ev[,1] = rep_vector(0, 36); # initial ev values
      ev[,2] = rep_vector(0, 36); # initial ev values
      
      
      for (t in 1:(Tsubj[i])) {
        
        beta_cheat=0;
        # Iterate log-likelihood
        log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i,t] |  to_vector(ev[cue[i,t],]) * (beta[i]+beta_cheat));
        
        # Posterior prediction
        y_hat[i,t] = categorical_rng( softmax(to_vector(ev[cue[i,t],]) * (beta[i]+beta_cheat)));
        
        # prediction error
        PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
        PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
        
        # Store chosen EV for fictive updating
        ev_chosen = ev[cue[i,t],choice[i,t]];
        
        # value updating (learning)]
        
        alpha_cheat=0;
        ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + (alpha[i]+alpha_cheat) * PEnc;
        ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + (alpha[i]+alpha_cheat) * PE;
      }
    }
  }
}


