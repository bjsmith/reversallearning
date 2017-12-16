//adaptation using multiple runs, but not usign a non-centered paramterization.
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
  
  #multiple runs, multiple reward type extension
  int<lower=1> R; //number of runs (max)
  int<lower=1> R_N[N]; //number of runs for each subject
    #let's represent each reward and punishment as its own run within the collection of runs
    #which makes 4 runs altogether
    #unless this makes the model partially identified, but I dont' really expect that to be a problem.
    #or not. Because reward and punishment also always occur in separate runs
    #so if we count 4 runs then there is less code we have to write to distinguish between outcome_types.
  #and can we represent a trial's Run and whether that trial is Rew or Punishment in the same way? That seems best...
  int<lower=0> run_id[N,T];
}

transformed data {}

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  // vector[2] s_mu_g_mu; //group-level means of subject means
  // vector<lower=0>[2] s_mu_g_sigma; //variance within the group, across subjects
  // 
  // vector[2] s_sigma_g_mu; //group-level means of subject variance
  // vector<lower=0>[2] s_sigma_g_sigma; //group-level variance of subject variance
  // 
  // // Subject-level raw parameters. 
  // vector[N] alpha_s_mu;   // learning rate, subject average across runs
  // vector[N] beta_s_mu;  // inverse temperature, subject average across runs
  // vector[N] alpha_s_sigma;   // learning rate, subject variance across runs
  // vector[N] beta_s_sigma;  // inverse temperature, subject variance across runs
  // 
  // Run level raw parameters
  vector[N] alpha[R];   // learning rate, run estimate
  vector[N] beta[R];  // inverse temperature, run estimate
}

transformed parameters {}

model {
  for (s in 1:N){
    for (r in 1:R_N[s]){
        alpha[r,s]~normal(0,1);
        beta[r,s]~normal(0,1);
    }
  }
  // for (s in 1:N){#reparameterize this across subjects if possible
  //   alpha~normal(alpha_s_mu[s],alpha_s_sigma[s]);
  //   beta~normal(beta_s_mu[s],beta_s_sigma[s]);
  // }
  // 
  // 
  // //sample the subject means
  // alpha_s_mu~norm(s_mu_g_mu[1],s_mu_g_sigma[1]);
  // beta_s_mu~norm(s_mu_g_mu[2],s_mu_g_sigma[2]);
  // 
  // //sample the subject standard deviations
  // alpha_s_sigma~cauchy(s_sigma_g_mu[1],s_sigma_g_sigma[1]);
  // beta_s_sigma~cauchy(s_sigma_g_mu[2],s_sigma_g_sigma[2]);
  // 
  // //mean and variance of the subject mean, i.e., the group level mean and SD
  // s_mu_g_mu~normal(0,1);
  // s_mu_g_sigma~cauchy(0,5);
  // 
  // //mean and variance of the subject-level variance
  // s_sigma_g_mu~normal(0,1);
  // s_sigma_g_sigma~cauchy(0,5);//maybe this should be lower...

  int r = -1; //an iterator

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
        choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta[r,i] );
        #choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta[i] );
        // prediction error
        PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
        PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
  
        // value updating (learning)
        ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha[r,i] * PEnc;
        ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[r,i] * PE;
        // ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha[i] * PEnc;
        // ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha[i] * PE;
      }
   }
  }
}

generated quantities {
  // For group level parameters
  real<lower=0,upper=1> mu_alpha;
  real<lower=0,upper=5> mu_beta;
  // 
  mu_alpha  = Phi_approx(alpha);
  mu_beta   = Phi_approx(beta) * 5;
}
