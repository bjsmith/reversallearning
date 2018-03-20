//a fresh adaptation of the double update model
//following Kruschke's textbook guidelines for developing multiple levels.
data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1,upper=T> Tsubj[N];
  int<lower=1,upper=100> N_cues[N];
  int<lower=0,upper=2> choice[N,T];
  int<lower=0,upper=100> cue[N,T];
  int trial[N,T];
  int cue_pos[N,T];
  int subjid[N,T];
  int cor_resp[N,T];
  int cue_freq[N,T];
  real outcome[N,T];
  
  int<lower=1> R; //number of runs (max)
  int<lower=1> R_N[N]; //number of runs for each subject
  int<lower=0> run_id[N,T];
}

transformed data {

}

//how do mu_p[1], alpha_s, and alpha all link together in the basic model?
//mu_p[1] is sampled from a normal distribution
//alpha (subject level) is sampled from a Phi_approximation taking the sum of mu_p (group level) and alpha_s (subject level)
//alpha_s is sampled from a subject level
//alpha and beta are used to model the particular task.

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[2] mu_p;
  vector<lower=0>[2] sigma_p;

  // Subject-level raw parameters (for Matt trick)
  vector[N] alpha_s;   // learning rate
  vector[N] beta_s;  // inverse temperature
  
  // Run level raw parameters
  vector[N] alpha_s_r[R];   // learning rate
  vector[N] beta_s_r[R];  // inverse temperature
  vector<lower=0,upper=1>[N] alpha_s_sigma;
  vector<lower=0,upper=5>[N] beta_s_sigma;
}


transformed parameters {
  // run-level raw parameters, somehow, hopefully!
  vector<lower=0,upper=1>[N] alpha_r[R];
  vector<lower=0,upper=5>[N] beta_r[R];
  
    // subject-level raw parameters
  vector<lower=0,upper=1>[N] alpha;
  vector<lower=0,upper=5>[N] beta;
  vector[N] subj_alpha_s;
  vector[N] subj_beta_s;
  
  subj_alpha_s=mu_p[1] + sigma_p[1] * alpha_s;
  subj_beta_s=mu_p[2] + sigma_p[2] * beta_s;
  
  //we should be able to vectorize the phi approximations across subjects, too
  alpha  = Phi_approx(subj_alpha_s); #not sure whether to use mu_p_r or alpha_r values here.
  beta   = Phi_approx(subj_beta_s) * 5; #not sure whether to use mu_p_r or alpha_r values here.
  
  for (i in 1:N) {
  //iterating per subject because each subject has different numbers of runs.
    for (r in 1:R_N[i]){
      alpha_r[r,i]  = Phi_approx(subj_alpha_s[i]+alpha_s_sigma[i] * alpha_s_r[r,i]); #not sure whether to use mu_p_r or alpha_r values here.
      beta_r[r,i]   = Phi_approx(subj_beta_s[i] +beta_s_sigma[i] .* beta_s_r[r,i]) * 5; #not sure whether to use mu_p_r or alpha_r values here.
    }
  }
}

model {
  int r = -1; //an iterator
  
  // Hyperparameters
  mu_p  ~ normal(0, 1);
  sigma_p ~ cauchy(0, 5);

  // individual parameters
  alpha_s  ~ normal(0,1);
  beta_s   ~ normal(0,1);
  alpha_s_sigma ~ cauchy(0, 5);
  beta_s_sigma ~ cauchy(0, 5);
  
  
  
  //run parameters
  // individual parameters
  for (i in 1:N){
    for (r1 in 1:R_N[i]){
      alpha_s_r[r1,i]  ~ normal(0,1);
      beta_s_r[r1,i]   ~ normal(0,1);
    }
  }
  

  for (i in 1:N) {
    // Define values
    matrix[100,2] ev;
    real PEnc; // fictitious prediction error (PE-non-chosen)
    real PE;         // prediction error

    // Initialize values
    ev[,1] = rep_vector(0, 100); // initial ev values
    ev[,2] = rep_vector(0, 100); // initial ev values

    for (t in 1:(Tsubj[i])) {
      // compute action probabilities
        // NB: In this algorithm, should exploit the fact that 
        // accumulation of updated values occurs independently for runs and outcome_types, i.e.,
        // no images occur across runs or outcome_types
        
      //get the particular run we are dealing with this time.
      r = run_id[i,t]; 
        
      if (choice[i,t]!=0) {
        choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta_r[r,i] );
        #choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta[i] );
        // prediction error
        PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
        PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
  
        // value updating (learning)
        ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha_r[r,i] * PEnc;
        ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha_r[r,i] * PE;
      }
   }
  }
}


generated quantities {

  // For group level parameters
  real<lower=0,upper=1> mu_alpha;
  real<lower=0,upper=5> mu_beta;

  mu_alpha  = Phi_approx(mu_p[1]);
  mu_beta   = Phi_approx(mu_p[2]) * 5;
  

}
