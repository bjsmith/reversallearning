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
  //int subj_level_params;
  
  //multiple runs, multiple reward type extension
  int<lower=1> R; //number of runs (max)
  int<lower=1> R_N[N]; //number of runs for each subject
  int<lower=0> run_id[N,T];
  
  //run outcome type. Indicates whether a particular run is reward or punishment.
  int<lower=0> run_ot[N,R];

}

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[2] group_pr_mu; //group-level means of subject means
  vector<lower=0>[2] group_pr_sigma; //variance within the group, across subjects
  
  //hypers for reward-punishment difference
  vector[2] group_pr_rpdiff_mu; //group-level means of subject means
  vector<lower=0>[2] group_pr_rpdiff_sigma; //variance within the group, across subjects
  
  // Subject-level raw parameters. 
  vector[N] alpha_s_pr_mu;   // learning rate, subject average across runs
  vector[N] beta_s_pr_mu;  // inverse temperature, subject average across runs
  vector[N] alpha_s_pr_rpdiff_mu;   // learning rate, difference between reward and punishment
  vector[N] beta_s_pr_rpdiff_mu;  // inverse temperature, difference between reward and punishment
    //Will be very important to somehow ignore values from subjects where there's no actual difference.

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
  real run_ot_multiplier =0;
  int sub_has_rew_runs = 0;
  int sub_has_pun_runs = 0;
  
  //mean and variance of the subject mean, i.e., the group level mean and SD
  group_pr_mu ~ normal(0, 1);
  group_pr_sigma ~ cauchy(0, 5); 
  group_pr_rpdiff_mu ~ normal(0, 1);
  group_pr_rpdiff_sigma ~ cauchy(0, 5);
  alpha_s_pr_sigma ~ cauchy(0, 5);
  beta_s_pr_sigma ~ cauchy(0, 5);

  //subject level.
  for (s in 1:N){
    alpha_s_pr_mu[s] ~ normal(group_pr_mu[1],group_pr_sigma[1]);
    beta_s_pr_mu[s] ~ normal(group_pr_mu[2],group_pr_sigma[2]);
    
    // alpha_s_pr_rpdiff_mu[s] ~ normal(group_pr_rpdiff_mu[1],group_pr_rpdiff_sigma[1]); 
    // beta_s_pr_rpdiff_mu[s] ~ normal(group_pr_rpdiff_mu[2],group_pr_rpdiff_sigma[2]); 
    alpha_s_pr_rpdiff_mu[s] ~ normal(0, 1); 
    beta_s_pr_rpdiff_mu[s] ~ normal(0, 1); 
    
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
        
      //get the particular run we are dealing with for this trial.
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
  group_sigma_alpha  = Phi_approx(group_pr_sigma[1]);
  group_sigma_beta   = Phi_approx(group_pr_sigma[2]) * 14;
  
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
