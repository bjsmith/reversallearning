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
  vector[2] s_mu_g_mu; //group-level means of subject means
  #vector<lower=0>[2] s_mu_g_kappa; //variance within the group, across subjects
  vector[2] s_mu_g_kappa; //variance within the group, across subjects
  
  //vector[2] s_sigma_g_mu; //group-level means of subject variance
  //vector<lower=0>[2] s_sigma_g_sigma; //group-level variance of subject variance
  
  // Subject-level raw parameters. 
  vector[N] alpha_s_mu;   // learning rate, subject average across runs
  vector[N] beta_s_mu;  // inverse temperature, subject average across runs
  //these are not vectors because we make a simplifying assumption they're the same for all subjects.
  real alpha_s_kappa;   // learning rate, subject variance across runs
  real beta_s_kappa;  // inverse temperature, subject variance across runs
  
  // Run level raw parameters
  vector[N] alpha[R];   // learning rate, run estimate
  vector[N] beta[R];  // inverse temperature, run estimate
}


transformed parameters {

}

model {
  
  int run = -1; //an iterator
  
  //mean and variance of the subject mean, i.e., the group level mean and SD
  s_mu_g_mu[1]~beta(1,1);
  s_mu_g_mu[2]~cauchy(0,7);
    #This is effectively a uniform distribution between 0 and 1.
    #This is what we want for alpha.
    #For beta we want beta to be up to a higher value???
  #Not sure of the right scale to use here.
  #we probably want a fairly uninormative prior for the SD of the beta distribution
  #what would that be?
  s_mu_g_var[1]~beta(2,2)*.28867;#SD between 0 and a uniform distribution.
  
  //mean and variance of the subject-level variance
  //we're assuming constant subject-level variance so no need for this.
  //s_sigma_g_mu~normal(0,1);
  //s_sigma_g_sigma~cauchy(0,5);//maybe this should be lower...
  
  //sample the subject means
  
  
  for (s in 1:N){
    #this looks complicated and htere must be a better way.
    #Is there not another distribution that will take thse 'natively'?
    #like a beta distriubtion
    #possibly inverse_logit or phi_approx. Got to look into thsi a bit!
    #really need to dig in and understand distributions. Might be worth reading up a bit. Is there a primer online?
    real alpha=(s_mu_g_mu[1]*s_mu_g_mu[1]*(1-s_mu_g_mu[1])/s_mu_g_var[1]^2-1);
    real beta=(1-s_mu_g_mu[1])*s_mu_g_mu[1]*(1-s_mu_g_mu[1]/s_mu_g_var[1]^2-1);
    alpha_s_mu[s]~beta(alpha,beta);
    alpha=s_mu_g_mu[2]*s_mu_g_mu[2]*(1-s_mu_g_mu[2])/s_mu_g_var[2]^2-1;
    beta=(1-s_mu_g_mu[2])*s_mu_g_mu[2]*(1-s_mu_g_mu[2]/s_mu_g_var[2]^2-1);
    beta_s_mu[s]~beta(alpha,beta);
  }
  
  //sample the subject standard deviations
  //alpha_s_sigma~cauchy(0,s_mu_g_sigma[1])//cauchy(s_sigma_g_mu[1],s_sigma_g_sigma[1]);
  //beta_s_sigma~cauchy(0,s_sigma_g_sigma[2]);
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
