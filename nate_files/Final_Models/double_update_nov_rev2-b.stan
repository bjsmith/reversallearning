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
  // See stan manual v2.17.0, Page 325, for a discussion on how to use matrices efficiently
  matrix[R,N] alpha_s_r;   // learning rate
  matrix[R,N] beta_s_r;  // inverse temperature
  vector<lower=0,upper=1>[N] alpha_s_sigma;
  vector<lower=0,upper=5>[N] beta_s_sigma;
}


transformed parameters {
  // run-level raw parameters, somehow, hopefully!
  matrix<lower=0,upper=1>[R,N] alpha_r;
  matrix<lower=0,upper=5>[R,N] beta_r;
  
    // subject-level raw parameters
  vector<lower=0,upper=1>[N] alpha;
  vector<lower=0,upper=5>[N] beta;
  vector[N] subj_alpha_s;
  vector[N] subj_beta_s;
  
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
  //we can do elementwise multiplication for thse vectors.
  subj_alpha_s=mu_p[1] + sigma_p[1] * alpha_s;
  subj_beta_s=mu_p[2] + sigma_p[2] * beta_s;
  
  //we should be able to vectorize the phi approximations across subjects, too
  alpha  = Phi_approx(subj_alpha_s); #not sure whether to use mu_p_r or alpha_r values here.
  beta   = Phi_approx(subj_beta_s) * 5; #not sure whether to use mu_p_r or alpha_r values here.
  
  //for (i in 1:N) {
  //we don't need to iterate across subjects because we can do this elementwise!
  //see page 325 of the manual - this is probably NOT going to be efficient.
  for (r in 1:R){
    alpha_r[r] = Phi_approx(subj_alpha_s + alpha_s_r[r] * alpha_s_sigma); #not sure whether to use mu_p_r or alpha_r values here.
      #TO DO: add outcome type as a parameter into here...probably just need a mean and variance parameter for outcome_type?
    beta_r[r]   = Phi_approx(subj_beta_s + beta_s_r[r] * beta_s_sigma) * 5; #not sure whether to use mu_p_r or alpha_r values here.
  }

  //}//OK, so if we have done this, how do we get the values that are not specified for run, do we simply copy like
  
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
      r = run_id[s,t]; 
        
      if (choice[s,t]!=0) {
        choice[s,t] ~ categorical_logit( to_vector(ev[cue[s,t],]) * beta_r[r,s] );
        #choice[s,t] ~ categorical_logit( to_vector(ev[cue[s,t],]) * beta[s] );
        // prediction error
        PE   =  outcome[s,t] - ev[cue[s,t],choice[s,t]];
        PEnc = -outcome[s,t] - ev[cue[s,t],3-choice[s,t]];
  
        // value updating (learning)
        ev[cue[s,t],3-choice[s,t]] = ev[cue[s,t],3-choice[s,t]] + alpha_r[r,s] * PEnc;
        ev[cue[s,t],choice[s,t]] = ev[cue[s,t],choice[s,t]] + alpha_r[r,s] * PE;
        // ev[cue[s,t],3-choice[s,t]] = ev[cue[s,t],3-choice[s,t]] + alpha[s] * PEnc;
        // ev[cue[s,t],choice[s,t]] = ev[cue[s,t],choice[s,t]] + alpha[s] * PE;

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
