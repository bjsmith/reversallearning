//rev5a-pain1
//This version extends 5a by adding pain as a moderator to the learning rate; does pain affect the learning rate?
//rev5a:
//previous version had a bug where it was relying on the output of the group_pr_rpdiff_mu to calculate the group reward and punishment values,
//but this value wasn't actually connected to the estimation process

//rev5:
//This builds on rev4 but also supports a separate parameter to differentiate reward and punishment runs.
//Needs to support people who only had one of those two.
//How do we handle this hierarchically?
//A "difference score" would be difficult because not all subjects have reward and not all subjects have punishment runs.
//Really, we need:
//Store the categories of runs for each subject
//separately sample a value for each category
//But this would be difficult where there's only one category of run because you have no way to fix run-type variance at anything sensible
//for subjects with only one run-type.
//I suppose, though, because we're assuming variance is equal across all subjects, that might be OK.

//But I think we made the decision already when we decided to pool all runs.
//So, only for subjects with two run types, we add a parameter that represents the difference between them.
//This parameter will be equal and opposite for the two - it's a value that represents half the gap between trial types
//and will be positive for reward, negative for punishment
//Then we can get a group calculation for that parameter after having calculated it separately for each subject.
data {
  int<lower=1> N; //used to determine length of n(subject) length arrays
  int<lower=1> T; //used to determine lenght of n(trial) length delays. 
  //This is the maximum number of trials, across all runs,` that any subject should have
  //in practice most subjects have the same number of trials, but some subjects have less because data is missing for one or another reason.
  
  int<lower=1,upper=T> Tsubj[N]; //number of trials we actually have for each nth subject. 
  //Used to determine the number of iterations when iterating through each subject's trials.
  
  int<lower=0,upper=2> choice[N,T]; //choice made by subject N at trial T. 
  //These are the choices the model tries to predict so this is definitely used.
  
  int<lower=0,upper=100> cue[N,T]; //cue (image) showed to subject N on trial T.
  //This is very important because subjects' learned values for choice are specific to each cue.
  
  real outcome[N,T]; //The actual outcome (correct=1, incorrect=0, nonresponse=0)
  int sample_from_prior; // flag to tell stan to sample from the prior, i.e., ignore empirical data from choice and outcome.
  //useful for comparison to observe the influence of the data on the model.
  
  //int subj_level_params;
  
  //multiple runs, multiple reward type extension
  int<lower=1> R; //number of runs (max); used when iterating through run values
  int<lower=1> R_N[N]; //number of runs for each subject; used when iterating through subject-level run values.
  int<lower=0> run_id[N,T]; //identifies each particular run for a subject; used because we calculate separate estimate for each run.
  
  
  int<lower=0> run_ot[N,R]; //run outcome type. Indicates whether a particular run is reward or punishment.
  //used because we calculate a difference parameter for each of each subject's parameters 
  //to indicate difference between the two reward runs and the two punishment runs.
  
  real pain_signal[N,T];
  
  //THE FOLLOWING ARE NOT USED. THEY WERE INCLUDED TO GENERATE POSTERIOR SIMULATED DATA WITHIN STAN.
  int trial[N,T];
  int cue_pos[N,T];
  int subjid[N,T];
  int cor_resp[N,T];
  int cue_freq[N,T];
  
  int<lower=1,upper=100> N_cues[N]; //number of unique cues (images) presented to each subject.
  //NOT ACTUALLY USED in this version.
  //Was included for an older version where we actually generated posterior simulated data within stan.

  
  
  

}

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[2] group_pr_mu; //group-level means of subject means
  real group_pr_mu_pain_effect;
  vector<lower=0>[2] group_pr_sigma; //variance within the group, across subjects
  real group_pr_sigma_pain_effect;
  
  //hypers for reward-punishment difference
  vector[2] group_pr_rpdiff_mu; //group-level means of subject means
  vector<lower=0>[2] group_pr_rpdiff_sigma; //variance within the group, across subjects
  
  
  // Subject-level raw parameters. 
  vector[N] alpha_s_pr_mu;   // learning rate, subject average across runs
  vector[N] beta_s_pr_mu;  // inverse temperature, subject average across runs
  vector[N] pain_effect_s_pr_mu;//effect of pain on learning rate
  vector[N] alpha_s_pr_rpdiff_mu;   // learning rate, difference between reward and punishment
  vector[N] beta_s_pr_rpdiff_mu;  // inverse temperature, difference between reward and punishment
  
    //Will be very important to somehow ignore values from subjects where there's no actual difference.

  //these are not vectors because we make a simplifying assumption they're the same for all subjects.
  real<lower=0> alpha_s_pr_sigma;   // learning rate, subject variance across runs
  real<lower=0> beta_s_pr_sigma;  // inverse temperature, subject variance across runs
  real<lower=0> pain_effect_s_pr_sigma;   // learning rate, subject variance across runs
  
  // Run level raw parameters
  // using probit transform
  real alpha_pr[N, R];   // learning rate, run estimate
  real beta_pr[N, R];  // inverse temperature, run estimate
  real pain_effect_pr[N, R];//effect of pain on learning rate
}

transformed parameters {
// here is where we take that normally distributed parameter 
// and phi-approximate it into a range.

  // Transform subject-level raw parameters
  
  real<lower=0,upper=14> beta[N, R];
  //real pain_effect[N, R];
  //interacts directly with the trial-level learning
  //drawn from a phi-approximation from group-level mean and deviation multiplied by subject-level parameter

  
  //run level parameter transform.
  //I am not sure if this is high enough. 
  //This is double what we found in the recent paper using the same model
  for (s in 1:N) {
    for (r in 1:R){
      beta[s, r]   = Phi_approx( beta_pr[s, r]) * 14; 
      //pain_effect[s, r] = pain_effect_pr[s, r];
    }
    for (t in 1:(Tsubj[s])) {
      // alphan[s, t]  = 
      //   Phi_approx(alpha_pr[s, run_id[s,t]] * 
      //   pain_effect[s,run_id[s,t]] * 
      //   pain_signal[s,t]);
      //alphan[s, t]  = Phi_approx(alpha_pr[s, run_id[s,t]]);
    }
  }
}

model {
  int run = -1; //an iterator
  real run_ot_multiplier =0;
  int sub_has_rew_runs = 0;
  int sub_has_pun_runs = 0;
  #real<lower=0,upper=1> alphan[N, T];
  real alphan[N, T];
  
  //mean and variance of the subject mean, i.e., the group level mean and SD
  group_pr_mu ~ normal(0, 1);
  group_pr_mu_pain_effect ~ normal(0, 1);
  group_pr_sigma ~ cauchy(0, 5);
  group_pr_sigma_pain_effect ~ cauchy(0, 5); 
  group_pr_rpdiff_mu ~ normal(0, 1);
  group_pr_rpdiff_sigma ~ cauchy(0, 5);
  alpha_s_pr_sigma ~ cauchy(0, 5);
  pain_effect_s_pr_sigma ~ cauchy(0, 5);
  beta_s_pr_sigma ~ cauchy(0, 5);

  //subject level.
  for (s in 1:N){
    alpha_s_pr_mu[s] ~ normal(group_pr_mu[1],group_pr_sigma[1]);
    beta_s_pr_mu[s] ~ normal(group_pr_mu[2],group_pr_sigma[2]);
    pain_effect_s_pr_mu[s] ~ normal(group_pr_mu_pain_effect,group_pr_sigma_pain_effect);
    
    alpha_s_pr_rpdiff_mu[s] ~ normal(group_pr_rpdiff_mu[1],group_pr_rpdiff_sigma[1]); 
    beta_s_pr_rpdiff_mu[s] ~ normal(group_pr_rpdiff_mu[2],group_pr_rpdiff_sigma[2]); 
    //alpha_s_pr_rpdiff_mu[s] ~ normal(0, 1); 
    //beta_s_pr_rpdiff_mu[s] ~ normal(0, 1); 
    
    //record the kind of runs this subject has
    sub_has_rew_runs = 0;
    sub_has_pun_runs = 0;
    for (r in 1:R){
      if(run_ot[s, r]==1){//1 represents reward
        sub_has_rew_runs = 1;
      }else if (run_ot[s, r]==2){//2 represents punishment
        sub_has_pun_runs = 1;
      }
    }
    
    //run level
    for (r in 1:R){
      //remember, because we made the simplifying assumption that every subject has the same run-level variance,
      //we just use the same alpha_s_sigma value for every subject :-)
      
      if(sub_has_rew_runs && sub_has_pun_runs){
        //this subject has both reward and punishment runs
        //if this run is a reward run, set the multiplier to 1. 
        //If it's a punishment run, set it to -1.
        if(run_ot[s, r]==1){
          run_ot_multiplier = 0.5;
        }else if (run_ot[s, r]==2){
          run_ot_multiplier = -0.5;
        }
      }else{
        //If this subject doen'st have both reward and punishment runs, set the multiplier to 0.
        run_ot_multiplier = 0;
      }
      
      alpha_pr[s,r] ~ normal(alpha_s_pr_mu[s]+run_ot_multiplier*alpha_s_pr_rpdiff_mu[s],alpha_s_pr_sigma);
      beta_pr[s,r] ~ normal(beta_s_pr_mu[s]+run_ot_multiplier*beta_s_pr_rpdiff_mu[s],beta_s_pr_sigma);
      pain_effect_pr[s,r] ~ normal(pain_effect_s_pr_mu[s],pain_effect_s_pr_sigma);
    }
  }
  
  for (s in 1:N) {
    // Define values
    matrix[100,2] ev;
    real PEnc; // fictitious prediction error (PE-non-chosen)
    real PE;         // prediction error
    real pain_signal_s_t;
    

    // Initialize values
    ev[,1] = rep_vector(0, 100); // initial ev values
    ev[,2] = rep_vector(0, 100); // initial ev values

    for (t in 1:(Tsubj[s])) {
      // compute action probabilities
        // NB: In this algorithm, should exploit the fact that 
        // accumulation of updated values occurs independently for runs and outcome_types, i.e.,
        // no images occur across runs or outcome_types
        
      //get the particular run we are dealing with for this trial.
      run = run_id[s,t]; 
        
      if (choice[s,t]!=0) {
        if(sample_from_prior!=1){
          choice[s,t] ~ categorical_logit( to_vector(ev[cue[s,t],]) * beta[s, run] );
          // prediction error
          PE   =  outcome[s,t] - ev[cue[s,t],choice[s,t]];
          PEnc = -outcome[s,t] - ev[cue[s,t],3-choice[s,t]];
          
          #pain_signal_s_t=1+normal_cdf(pain_effect[s,run]*pain_signal[s,t], 0, 1);
          #print(pain_signal_s_t)
          // if(run_ot[s,run]==2){
          //   //pain_signal_s_t = pain_effect[s,run] * pain_signal[s,t];
          //   alpha = Phi_approx(alphan[s,run]);# + pain_signal_s_t);
          // }else{
          alphan[s,t] = Phi_approx(alpha_pr[s,run]+pain_effect_pr[s,run]*pain_signal[s,t]); #reward trial.
          // }
          // value updating (learning)
          ev[cue[s,t],3-choice[s,t]] = ev[cue[s,t],3-choice[s,t]] + alphan[s,t] * PEnc;# * pain_signal_s_t;
          //these pain effects only apply if this is a punishment run (i.e., run2)
          ev[cue[s,t],choice[s,t]] = ev[cue[s,t],choice[s,t]] + alphan[s,t] * PE;# * pain_signal_s_t;
          //these pain effects only apply if this is a punishment run (i.e., run2)
          
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
  real group_mu_pain_effect;
  real group_sigma_pain_effect;
  
  real<lower=0,upper=1> group_rew_mu_alpha;
  real<lower=0,upper=14> group_rew_mu_beta;
  // real<lower=0,upper=1> group_rew_sigma_alpha;
  // real<lower=0,upper=14> group_rew_sigma_beta;
  
  real<lower=0,upper=1> group_pun_mu_alpha;
  real<lower=0,upper=14> group_pun_mu_beta;
  // real<lower=0,upper=1> group_pun_sigma_alpha;
  // real<lower=0,upper=14> group_pun_sigma_beta;
  
  
  // vector[N] alpha_s_mu;
  // vector[N] beta_s_mu;
  // real alpha_s_sigma;
  // real beta_s_sigma;
  
  //group level
  group_mu_alpha  = Phi_approx(group_pr_mu[1]);
  group_mu_beta   = Phi_approx(group_pr_mu[2]) * 14;
  group_mu_pain_effect   = group_pr_mu_pain_effect;
  group_sigma_alpha  = Phi_approx(group_pr_sigma[1]);
  group_sigma_beta   = Phi_approx(group_pr_sigma[2]) * 14;
  group_sigma_pain_effect   = group_pr_sigma_pain_effect;
  
  group_rew_mu_alpha  = Phi_approx(group_pr_mu[1]+0.5*group_pr_rpdiff_mu[1]);
  group_rew_mu_beta   = Phi_approx(group_pr_mu[2]+0.5*group_pr_rpdiff_mu[2]) * 14;
  
  group_pun_mu_alpha  = Phi_approx(group_pr_mu[1]-0.5*group_pr_rpdiff_mu[1]);
  group_pun_mu_beta   = Phi_approx(group_pr_mu[2]-0.5*group_pr_rpdiff_mu[2]) * 14;

  // if(subj_level_params==1){
  //   //subject level parameters. 
  //   alpha_s_mu = Phi_approx(alpha_s_pr_mu);
  //   beta_s_mu = Phi_approx(beta_s_pr_mu);
  // 
  //   alpha_s_sigma = Phi_approx(alpha_s_pr_sigma);
  //   beta_s_sigma = Phi_approx(beta_s_pr_sigma);
  // }
}
