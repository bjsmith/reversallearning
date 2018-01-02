//update Rev3b: The prior version, Rev3a and all prior Rev3, did not use suitable distributions
//Here I've taken care to make sure these distributions are suitable; we'll see where that gets us!
//adaptation using multiple runs, but not usign a non-centered paramterization.
//this version uses simplified within-run distributions. Basically we assume that all subjects have the same
//within-run distribution of scores.
//This is a simplifying assumption I used to avoid building in an extra level, 
//but i think it shouldn't make the model too much more complicated.
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
  int sample_from_prior;
  int subj_level_params;
  
  //multiple runs, multiple reward type extension
  int<lower=1> R; //number of runs (max)
  int<lower=1> R_N[N]; //number of runs for each subject
  int<lower=0> run_id[N,T];
}

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[2] group_pr_mu; //group-level means of subject means
  vector[2] group_pr_sigma; //variance within the group, across subjects
  
  // Subject-level raw parameters. 
  vector[N] alpha_s_pr_mu;   // learning rate, subject average across runs
  vector[N] beta_s_pr_mu;  // inverse temperature, subject average across runs
  //these are not vectors because we make a simplifying assumption they're the same for all subjects.
  real<lower=0> alpha_s_pr_sigma;   // learning rate, subject variance across runs
  real<lower=0> beta_s_pr_sigma;  // inverse temperature, subject variance across runs
  
  // Run level raw parameters
  // using probit transform
  real alpha_pr[N, R];   // learning rate, run estimate
  real beta_pr[N, R];  // inverse temperature, run estimate
}

transformed parameters {
// here is where we take that normally distributed parameter 
// and phi-approximate it into a range.

  // Transform subject-level raw parameters
  real<lower=0,upper=1> alpha[N, R];
  real<lower=0,upper=14> beta[N, R];
  //interacts directly with the trial-level learning
  //drawn from a phi-approximation from group-level mean and deviation multiplied by subject-level parameter

  //run level parameter transform.
  //I am not sure if this is high enough. 
  //This is double what we found in the recent paper using the same model
  for (s in 1:N) {
    for (r in 1:R){
      alpha[s, r]  = Phi_approx( alpha_pr[s, r]);
      beta[s, r]   = Phi_approx( beta_pr[s, r]) * 14; 
    }
  }
}

model {
  
  int run = -1; //an iterator
  
  //mean and variance of the subject mean, i.e., the group level mean and SD
  group_pr_mu ~ normal(0, 1);
  group_pr_sigma ~ cauchy(0, 5); # For the way we're doing this, this might be too large/uninformative.
  alpha_s_pr_sigma ~ cauchy(0, 5);
  beta_s_pr_sigma ~ cauchy(0, 5);

  //subject level.
  for (s in 1:N){
    #alpha_s_pr_mu[s] ~ normal(0, 1);# ~ normal(group_pr_mu[1],group_pr_sigma[1]);
    #beta_s_pr_mu[s] ~ normal(0, 1);# ~ normal(group_pr_mu[2],group_pr_sigma[2]);
    alpha_s_pr_mu[s] ~ normal(group_pr_mu[1],group_pr_sigma[1]);
    beta_s_pr_mu[s] ~ normal(group_pr_mu[2],group_pr_sigma[2]);
    //run level
    #for (r in 1:R_N[s]){
    for (r in 1:R){
      //remember, because we made the simplifying assumption that every subject has the same run-level variance,
      //we just use the same alpha_s_sigma value for every subject :-)
      alpha_pr[s,r] ~ normal(alpha_s_pr_mu[s],alpha_s_pr_sigma);
      beta_pr[s,r] ~ normal(beta_s_pr_mu[s],beta_s_pr_sigma);
    }
  }
  
  for (s in 1:N) {
    // Define values
    matrix[100,2] ev;
    real PEnc; // fictitious prediction error (PE-non-chosen)
    real PE;         // prediction error

    // Initialize values
    ev[,1] = rep_vector(0, 100); // initial ev values
    ev[,2] = rep_vector(0, 100); // initial ev values

    for (t in 1:(Tsubj[s])) {
      // compute action probabilities
        // NB: In this algorithm, should exploit the fact that 
        // accumulation of updated values occurs independently for runs and outcome_types, i.e.,
        // no images occur across runs or outcome_types
        
      //get the particular run we are dealing with this time.
      run = run_id[s,t]; 
        
      if (choice[s,t]!=0) {
        if(sample_from_prior!=1){
          choice[s,t] ~ categorical_logit( to_vector(ev[cue[s,t],]) * beta[s, run] );
        
          // prediction error
          PE   =  outcome[s,t] - ev[cue[s,t],choice[s,t]];
          PEnc = -outcome[s,t] - ev[cue[s,t],3-choice[s,t]];
    
          // value updating (learning)
          ev[cue[s,t],3-choice[s,t]] = ev[cue[s,t],3-choice[s,t]] + alpha[s, run] * PEnc;
          ev[cue[s,t],choice[s,t]] = ev[cue[s,t],choice[s,t]] + alpha[s, run] * PE;
        }
        else{
          print("sampling from prior; data ignored.")
        }
      }
    }
  }
}

generated quantities {
  // For group level parameters
  real<lower=0,upper=1> group_mu_alpha;
  real<lower=0,upper=14> group_mu_beta;
  real<lower=0,upper=1> group_sigma_alpha;
  real<lower=0,upper=14> group_sigma_beta;
  
  vector[N] alpha_s_mu;
  vector[N] beta_s_mu;
  real alpha_s_sigma;
  real beta_s_sigma;
  
  //group level
  group_mu_alpha  = Phi_approx(group_pr_mu[1]);
  group_mu_beta   = Phi_approx(group_pr_mu[2]) * 14;
  
  //sigma. fuck. how does this translate? Perhaps because sigma exists within the normal space we can simply transform it using the phi approximation, too?
  group_sigma_alpha  = Phi_approx(group_pr_sigma[1]);
  group_sigma_beta   = Phi_approx(group_pr_sigma[2]) * 14;
  
  if(subj_level_params==1){
    //subject level parameters. 
    alpha_s_mu = Phi_approx(alpha_s_pr_mu);
    beta_s_mu = Phi_approx(beta_s_pr_mu);

    alpha_s_sigma = Phi_approx(alpha_s_pr_sigma);
    beta_s_sigma = Phi_approx(beta_s_pr_sigma);
  }
}
