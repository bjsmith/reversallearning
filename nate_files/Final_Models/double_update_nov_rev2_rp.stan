//a fresh adaptation of the double update model
//following Kruschke's textbook guidelines for developing multiple levels.
//This version also deals with both reward and punishment separately
//BETTER THAN MULTI-TRACK DRIFTING, YO!
//A little statistical logic is required here
//We want to *constrain* the variance of reward and punishmetn together in some way, right?
//If we simply took an average of the parameers from our reward and punishment runs,
//that wouldn't take advantage of any shrinkage occuring in the reward and punishment space
//so the principled way is to add an extra reward/punishment variable at the run level
//we will need both sigma and mu values for this.
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
  int<lower=0,upper=2> outcome_type[N,T];
  #and can we represent a trial's Run and whether that trial is Rew or Punishment in the same way? That seems best...
  int<lower=0> run_id[N,T];
}

transformed data {
  int OT_REW=1;
  int OT_PUN=2;
  int OT_MIN=1;
  int OT_MAX=2;//these two are for use in a for loop; apparently we can't use an array for a for loop here.
  //define these here for easier reading below!
}

//how do mu_p[1], alpha_pr, and alpha all link together in the basic model?
//mu_p[1] is sampled from a normal distribution
//alpha (subject level) is sampled from a Phi_approximation taking the sum of mu_p (group level) and alpha_pr (subject level)
//alpha_pr is sampled from a subject level
//alpha and beta are used to model the particular task.

#so we would want to modify this so alpha_r is used to model the particular task.
#alpha_r would then need t obe sampled from a distribution taking the group level, subject level, and run level data
#this might be done all `at once' or it might be done hierarchically, I'm not sure.
parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[2] mu_p;
  vector<lower=0>[2] sigma;

  // Subject-level raw parameters (for Matt trick)
  vector[N] alpha_pr;   // learning rate
  vector[N] beta_pr;  // inverse temperature
  
  // Run level raw parameters
  vector[N,R] alpha_r_pr;   // learning rate
  vector[N,R] beta_r_pr;  // inverse temperature
  // Reward/punishment values, also at run-level in the model but we have less of them
  vector[N,OT_MAX-OT_MIN+1] alpha_rp_pr;   // learning rate
  vector[N,OT_MAX-OT_MIN+1] beta_rp_pr;  // inverse temperature
  
  
  
}


transformed parameters {
  // Transform subject-level raw parameters
  vector<lower=0,upper=1>[N] alpha;
  vector<lower=0,upper=5>[N] beta;
  // run-level raw parameters, somehow, hopefully!
  vector<lower=0,upper=1>[N,R] alpha_r;
  vector<lower=0,upper=5>[N,R] beta_r;
  vector<lower=0,upper=1>[N,OT_MAX-OT_MIN+1] alpha_rp;
  vector<lower=0,upper=5>[N,OT_MAX-OT_MIN+1] beta_rp;
  
  #how to connect these levels?
  #make alpha_pr a function of mu_p_r somehow....
  

  //approximate subject-level means based on subject-level mean plus subject-level variance
  //multiplied by individual subject means
  //none of these parameters are at trial-level. They're all at subject-level or group-level
  //where do we insert trial-level estimates?
  for (i in 1:N) {//iterate through subjects AND runs
    for (r in 1:R){
      #we don't need to iterate through reward and punishment because this has already been done iterating through runs.
      alpha_r[i,r]  = Phi_approx( 
        mu_p[1] + 
        sigma[1] * alpha_pr[i] + 
        alpha_pr_sigma[i] * alpha_r_pr[i,r]+
        alpha_pr_sigma_rp[i] * alpha_rp_pr[i,r]); #not sure whether to use mu_p_r or alpha_r values here.
        
      beta_r[i,r]   = Phi_approx( 
        mu_p[2] + 
        sigma[2] * beta_pr[i] + 
        beta_pr_sigma[i] * beta_r_pr[i,r]+
        beta_pr_sigma_rp[i] * beta_rp_pr[i,r]) * 5; #not sure whether to use mu_p_r or alpha_r values here.
    }
  }#OK, so if we have done this, how do we get the values that are not specified for run, do we simply copy like
  
  #do we need these any more? maybe only as 'transformed parameters'
  for (i in 1:N) {
      alpha[i]  = Phi_approx( mu_p[1] + sigma[1] * alpha_pr[i]); #not sure whether to use mu_p_r or alpha_r values here.
      beta[i]   = Phi_approx( mu_p[2] + sigma[2] * beta_pr[i]) * 5; #not sure whether to use mu_p_r or alpha_r values here.
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
  // Hyperparameters
  mu_p  ~ normal(0, 1);
  sigma ~ cauchy(0, 5);

  // individual parameters
  alpha_pr  ~ normal(0,1);
  beta_pr   ~ normal(0,1);
  alpha_pr_sigma ~ cauchy(0, 5);
  beta_pr_sigma ~ cauchy(0, 5);
  alpha_pr_sigma_rp ~ cauchy(0, 5);
  beta_pr_sigma_rp ~ cauchy(0, 5);
  #'random effects' of reward and punishment varying within individuals. I wonder if this is unnecessary? But I *think* it is the most principled way to do this.
  
  // individual parameters
  //run parameters
  alpha_pr_r  ~ normal(0,1);
  beta_pr_r   ~ normal(0,1);
  //reward/punishment parameters
  alpha_pr_rp  ~ normal(0,1);
  beta_pr_rp   ~ normal(0,1);

  int run=0;//create and initialize run indicator.
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
      //we don't need to loop through runs; we just pick the correct one for this particular trial.
      //we also don't need to do anything special for reward vs. punishment here 
      //because those are picked out by the appropriate run.
      r = run_id[i,t]; 
        
        if (choice[i,t]!=0) {
          choice[i,t] ~ categorical_logit( to_vector(ev[cue[i,t],]) * beta_r[i,r] );
          // prediction error
          PE   =  outcome[i,t] - ev[cue[i,t],choice[i,t]];
          PEnc = -outcome[i,t] - ev[cue[i,t],3-choice[i,t]];
    
          // value updating (learning)
          ev[cue[i,t],3-choice[i,t]] = ev[cue[i,t],3-choice[i,t]] + alpha_r[i,r] * PEnc;
          ev[cue[i,t],choice[i,t]] = ev[cue[i,t],choice[i,t]] + alpha_r[i,r] * PE;
      }
   }
  }
}