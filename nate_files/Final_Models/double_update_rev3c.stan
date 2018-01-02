//update Rev3b: The prior version, Rev3a and all prior Rev3, did not use suitable distributions
//Here I've taken care to make sure these distributions are suitable; we'll see where that gets us!
//adaptation using multiple runs, but not usign a non-centered paramterization.
//this version uses simplified within-run distributions. Basically we assume that all subjects have the same
//within-run distribution of scores.
//This is a simplifying assumption I used to avoid buliding in an extra level, 
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
  
  //multiple runs, multiple reward type extension
  int<lower=1> R; //number of runs (max)
  int<lower=1> R_N[N]; //number of runs for each subject
  int<lower=0> run_id[N,T];
}

transformed data {
}

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[2] group_pr_mu; //group-level means of subject means
  vector[2] group_pr_sigma; //variance within the group, across subjects
  
  // Subject-level raw parameters. 
  vector[N] alpha_s_mu_pr;   // learning rate, subject average across runs
  vector[N] beta_s_mu_pr;  // inverse temperature, subject average across runs
  //these are not vectors because we make a simplifying assumption they're the same for all subjects.
  real alpha_s_sigma;   // learning rate, subject variance across runs
  real beta_s_sigma;  // inverse temperature, subject variance across runs
  
  // Run level raw parameters
  // using probit transform
  vector[N] alpha_pr[R];   // learning rate, run estimate
  vector[N] beta_pr[R];  // inverse temperature, run estimate
}

transformed parameters {
// here is where we take that normally distributed parameter 
// and phi-approximate it into a range.

  // Transform subject-level raw parameters
  vector<lower=0,upper=1>[N] alpha;
  vector<lower=0,upper=5>[N] beta;
  #interacts directly with the trial-level learning
  #drawn from a phi-approximation from group-level mean and deviation multiplied by subject-level parameter

  for (i in 1:N) {
    alpha[i]  = Phi_approx( alpha_pr[i] );
    beta[i]   = Phi_approx( beta_pr[i] ) * 14; 
      #I am not sure if this is high enough. 
      #This is double what we found in the recent paper using the same model
  }
}

model {
  
  int run = -1; //an iterator
  
  //mean and variance of the subject mean, i.e., the group level mean and SD
  group_pr_mu ~ norm(0, 1);
  group_pr_sigma ~ cauchy(0, 7); # now this is of course going to be transformed.

  #subject level.
  for (s in 1:N){
    alpha_s_mu_pr[s] = Phi_approx( group_pr_mu[1] + norm(0,group_pr_sigma[1]);
    beta_s_mu_pr[s] = Phi_approx( group_pr_mu[2] + norm(0,group_pr_sigma[2]);
  }
  
  #run level
  
  
  
  alpha_s_kappa~cauchy(0,5);//cauchy(s_sigma_g_mu[1],s_sigma_g_sigma[1]);
  beta_s_kappa~cauchy(0,5);
  
  for (s in 1:N){//reparameterize this across subjects if possible
  //only generate values within the range of runs for this subject.
    for (r in 1:R_N[s]){
      alpha[r,s] ~ beta(alpha_s_mu[s]*(alpha_s_kappa)+1, (1-alpha_s_mu[s])*(alpha_s_kappa)+1);
      beta[r,s] ~ beta(beta_s_mu[s]*(beta_s_kappa)+1, (1-beta_s_mu[s])*(beta_s_kappa)+1);
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
      run = run_id[i,t]; 
        
      if (choice[i,t]!=0) {
        if(sample_from_prior!=1){
          choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta[run,i] );
        
          // prediction error
          PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
          PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
    
          // value updating (learning)
          ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha[run,i] * PEnc;
          ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[run,i] * PE;
        }
        else{
          print("sampling from prior; data ignored.")
        }
      }
    }
  }
}
