
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
  
  //response time
  //real rt[N,T];
  real RT_choice[N, T];
}

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[5] group_pr_mu; //group-level means of subject means
  vector<lower=0>[5] group_pr_sigma; //variance within the group, across subjects
  
  //hypers for reward-punishment difference
  vector[2] group_pr_rpdiff_mu; //group-level means of subject means
  vector<lower=0>[2] group_pr_rpdiff_sigma; //variance within the group, across subjects
  
  // Subject-level raw parameters. 
  //subject average across runs
  vector[N] alpha_s_pr_mu;   // learning rate
  //these are not vectors because we make a simplifying assumption they're the same for all subjects.
  real<lower=0> alpha_s_pr_sigma;   // learning rate, subject variance across runs
  // using probit transform
  real alpha_pr[N, R];   // learning rate, run estimate
}

transformed parameters {
// here is where we take that normally distributed parameter 
// and phi-approximate it into a range.

  // Transform subject-level raw parameters
  real<lower=0,upper=1> alpha[N, R];
  //run level parameter transform.
  //I am not sure if this is high enough. 
  //This is double what we found in the recent paper using the same model
  for (s in 1:N) {
    for (r in 1:R){
      alpha[s, r]  = Phi_approx( alpha_pr[s, r]);
      
    }
  }
}

model {
    int PID_alpha=1;
  //int PID_beta=2;
  int PID_lba_k=2;
  //int PID_lba_A=4;
  //int PID_lba_v=4;
  int PID_lba_tau=3;
  int LBA_CHOICE_COUNT=2;
  
  int run = -1; //an iterator
  real run_ot_multiplier =0;
  int sub_has_rew_runs = 0;
  int sub_has_pun_runs = 0;
  
  //mean and variance of the subject mean, i.e., the group level mean and SD
  group_pr_mu ~ normal(0, 1);
  group_pr_sigma ~ cauchy(0, 5); 
  group_pr_rpdiff_mu ~ normal(0, 1);
  group_pr_rpdiff_sigma ~ cauchy(0, 5);
  #there might be too much opportunity for lba to vary...do we really need LBA rates to vary *per subject* *according to some random distribution*?
  alpha_s_pr_sigma ~ cauchy(0, 3);
  

  //subject level.
  for (s in 1:N){
    alpha_s_pr_mu[s] ~ normal(group_pr_mu[PID_alpha],group_pr_sigma[1]);
    alpha_s_pr_rpdiff_mu[s] ~ normal(group_pr_rpdiff_mu[1],group_pr_rpdiff_sigma[1]); 

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
      //beta_pr[s,r] ~ normal(beta_s_pr_mu[s]+run_ot_multiplier*beta_s_pr_rpdiff_mu[s],beta_s_pr_sigma);
      //lba_k_pr[s,r] ~ normal(lba_k_s_pr_mu[s]+run_ot_multiplier*lba_k_s_pr_rpdiff_mu[s],lba_k_s_pr_sigma);
      //lba_A_pr[s,r] ~ normal(lba_A_s_pr_mu,lba_A_s_pr_sigma);
      //lba_v_pr[s,r] ~ normal(lba_v_s_pr_mu,lba_tau_s_pr_sigma);
      //lba_tau_pr[s,r] ~ normal(lba_tau_s_pr_mu,lba_tau_s_pr_sigma);
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
        
      if (RT_choice[s,t,2]!=0) {#non-response trial.
        if(sample_from_prior!=1){
          #do we need a model simulating time taken here? if it's greater than x delay then we do nothing
          #if it's less than the delay then we act, otherwise we don't.
          choice[s,t] ~ categorical_logit( to_vector(ev[cue[s,t],]) * 1 * rt[s,t] );
          #RT_choice[s,t] ~lba(lba_k, lba_A, lba_v, 1, lba_tau);
          #RT_choice[s,t] ~lba(lba_k, lba_A, v=ev[cue[s,t],], 1, lba_tau);
        
          // prediction error
          PE   =  outcome[s,t] - ev[cue[s,t],RT_choice[s,t,2]];
          PEnc = -outcome[s,t] - ev[cue[s,t],3-RT_choice[s,t,2]];
    
          // value updating (learning)
          ev[cue[s,t],3-RT_choice[s,t,2]] = ev[cue[s,t],3-RT_choice[s,t,2]] + alpha[s, run] * PEnc;
          ev[cue[s,t],RT_choice[s,t,2]] = ev[cue[s,t],RT_choice[s,t,2]] + alpha[s, run] * PE;
        }
        else{
          print("sampling from prior; data ignored.")
        }
      }
    }
  }
}
