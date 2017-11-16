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
  
  #multiple runs, multiple reward type extension
  int<lower=1> R; //number of runs (max)
    #let's represent each reward and punishment as its own run within the collection of runs
    #which makes 4 runs altogether
    #unless this makes the model partially identified, but I dont' really expect that to be a problem.
    #or not. Because reward and punishment also always occur in separate runs
    #so if we count 4 runs then there is less code we have to write to distinguish between outcome_types.
  #and can we represent a trial's Run and whether that trial is Rew or Punishment in the same way? That seems best...
  int<lower=0> run_id[N,T];
}

transformed data {

}

//how do mu_p[1], alpha_s, and alpha all link together in the basic model?
//mu_p[1] is sampled from a normal distribution
//alpha (subject level) is sampled from a Phi_approximation taking the sum of mu_p (group level) and alpha_s (subject level)
//alpha_s is sampled from a subject level
//alpha and beta are used to model the particular task.

#so we would want to modify this so alpha_r is used to model the particular task.
#alpha_r would then need t obe sampled from a distribution taking the group level, subject level, and run level data
#this might be done all `at once' or it might be done hierarchically, I'm not sure.
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
  
  
  
  
  //Kruschke likes to draw from distributions directly centered on parameters
  //Nate hasn't designed the two levels like that
  //I've followed Nate's approach here but I think been very principled about how I distribute means and variances
  //we'll see if there's room for it or not!
  

  //how to connect these levels?
  //make alpha_s a function of mu_p_r somehow....
  

  //approximate subject-level means based on subject-level mean plus subject-level variance
  //multiplied by individual subject means
  //none of these parameters are at trial-level. They're all at subject-level or group-level
  //where do we insert trial-level estimates?
  for (i in 1:N) {
    for (r in 1:R){
      alpha_r[r,i]  = Phi_approx( 
        mu_p[1] + 
        sigma_p[1] * alpha_s[i] + 
        alpha_s_sigma[i] * alpha_s_r[r,i]); #not sure whether to use mu_p_r or alpha_r values here.
        #TO DO: add outcome type as a parameter into here...probably just need a mean and variance parameter for outcome_type?
      beta_r[r,i]   = Phi_approx( 
        mu_p[2] + 
        sigma_p[2] * beta_s[i] + 
        beta_s_sigma[i] * beta_s_r[r,i]) * 5; #not sure whether to use mu_p_r or alpha_r values here.
    }
  }//OK, so if we have done this, how do we get the values that are not specified for run, do we simply copy like
  
    #do we need these any more? maybe only as 'transformed parameters'
  for (i in 1:N) {
      alpha[i]  = Phi_approx( mu_p[1] + sigma_p[1] * alpha_s[i]); #not sure whether to use mu_p_r or alpha_r values here.
      beta[i]   = Phi_approx( mu_p[2] + sigma_p[2] * beta_s[i]) * 5; #not sure whether to use mu_p_r or alpha_r values here.
  }
  #from a multi-level modeling principled perspective, is this right? I'm not sure.
  
  #seems **plausible** I guess.
  #But I'm worried about not drawing each level directly from the one above it.
  #might work regardless though. perhaps I should try this out.
  
  #so I guess...
  #subject-level parameters alpha and beta can't be drawn directly from trial level parameters without 
  #somehow inserting run-level and outcome-type parameters
  #two ways to do this. Try to insert the run-level and outcome-type parameters as 'modifiers' or
  #draw run-level parameters from subject-level parameters
  #and draw subject-level parameters from run-level parameters
  #idk man, need to look carefully at how Kruschke does it, 
  #maybe try to attempt both and see what makes the most sense
  #let's start by trying to do a big hierarchy.

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
  for (r1 in 1:R){
    alpha_s_r[r1]  ~ normal(0,1);
    beta_s_r[r1]   ~ normal(0,1);
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
