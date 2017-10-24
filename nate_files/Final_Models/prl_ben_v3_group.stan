data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1> Gr_N; // number of groups
  int<lower=1,upper=T> Tsubj[N];
  int<lower=1,upper=T> subjGr[N]; // group identifier
  int<lower=1,upper=36> N_cues[N];
  int<lower=0,upper=2> choice[N,T];
  int<lower=0,upper=36> cue[N,T];
  int trial[N,T];
  int cue_pos[N,T];
  int subjid[N,T];
  int cor_resp[N,T];
  int cue_freq[N,T];
  real outcome[N,T];
}
transformed data {
}
parameters {
# Declare all parameters as vectors for vectorizing
  # Hyper(group)-parameters
  #real mu_p[Gr_N,2];
  #FIRST  dimension denotes groups.
  #SECOND dimension denotes parameters
  vector[2] mu_p[Gr_N];
  
  
  #real<lower=0> sigma[Gr_N,2];
  #first dimension denotes groups.
  #second dimension denotes parameters
  vector<lower=0>[2] sigma[Gr_N];

  # Subject-level raw parameters (for Matt trick)
  vector[N] alpha_pr;   # learning rate
  vector[N] beta_pr;  # inverse temperature
  
}

transformed parameters {
  # Transform subject-level raw parameters
  vector<lower=0,upper=1>[N] alpha;
  vector<lower=0,upper=5>[N] beta;

  for (i in 1:N) {
    int g_i = subjGr[i]; #the SubjectGroup index for this subject.
    alpha[i]  = Phi_approx( mu_p[g_i, 1] + sigma[g_i, 1] * alpha_pr[i] );
    beta[i]   = Phi_approx( mu_p[g_i, 2] + sigma[g_i, 2] * beta_pr[i] ) * 5;
  }
}

model {
  
  # Hyperparameters
  // for (i_p in 1:2){
  //   for (i_g in 1:Gr_N){
  //     mu_p[i_p,i_g]  ~ normal(0, 1); #original just sampled without the loop
  //     sigma[i_p,i_g] ~ cauchy(0, 5); #but with 2D array I don't think that is possible.
  //   }
  // }
    for (i_g in 1:Gr_N){
      mu_p[i_g]  ~ normal(0, 1); #original just sampled without the loop
      sigma[i_g] ~ cauchy(0, 5); #but with 2D array I don't think that is possible.
    }
  
  # individual parameters
  alpha_pr  ~ normal(0,1);
  beta_pr   ~ normal(0,1);

  for (i in 1:N) {
    # Define values
    matrix[36,2] ev;
    real PEnc; # fictitious prediction error (PE-non-chosen)
    real PE;         # prediction error
    #real ev_chosen;
    #real theta;

    # Initialize values
    ev[,1] = rep_vector(0, 36); # initial ev values
    ev[,2] = rep_vector(0, 36); # initial ev values
    #theta = pow(3, beta[i]) - 1;

    for (t in 1:(Tsubj[i])) {
      # compute action probabilities
      if (choice[i,t]!=0) {
	      choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta[i] );
	
   	   	# prediction error
      	PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
      	PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];

     	  # Store chosen EV for fictive updating
      	#ev_chosen = ev[cue[i,t],choice[i,t]];
      	#we don't actually need this.

      	# value updating (learning)
      	ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha[i] * PEnc;
      	ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[i] * PE;
      }
    }
  }
}

generated quantities {
  # For group level parameters
  real<lower=0,upper=1> mu_alpha[Gr_N];
  real<lower=0,upper=5> mu_beta[Gr_N];

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
    mu_alpha[g]  = Phi_approx(mu_p[g, 1]);
    mu_beta[g]   = Phi_approx(mu_p[g, 2]) * 5;
  }
  

  { # local section, this saves time and space
    for (i in 1:N) {
      # Define values
      matrix[36,2] ev;
      real PEnc; # fictitious prediction error (PE-non-chosen)
      real PE;         # prediction error
      #real ev_chosen;
      #real theta;

      # Initialize values
      log_lik[i] = 0;
      ev[,1] = rep_vector(0, 36); # initial ev values
      ev[,2] = rep_vector(0, 36); # initial ev values
      #theta = pow(3, beta[i]) - 1;

      for (t in 1:(Tsubj[i])) {
        # Iterate log-likelihood
        log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i,t] |  to_vector(ev[cue[i,t],]) * beta[i]);
        
        # Posterior prediction
        y_hat[i,t] = categorical_rng( softmax(to_vector(ev[cue[i,t],]) * beta[i]));

        # prediction error
        PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
        PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
  
        # Store chosen EV for fictive updating
        #ev_chosen = ev[cue[i,t],choice[i,t]];
  
        # value updating (learning)
        ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha[i] * PEnc;
        ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[i] * PE;
      }
    }
  }
}
