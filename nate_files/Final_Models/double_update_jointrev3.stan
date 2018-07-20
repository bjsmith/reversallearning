//jointrev3: For debugging: cut out the covariance calculation; just trying to extract EV and RPE values.
//jointrev2: covariance matrix works across all runs rather than separately for each run.
//jointrev1: rev8 but a joint model.
//rev8: Re-organization of rev5a, prepared to integrate a joint model, but without the joint model active.
//      Should function the same, but requires a different set of inputs.

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
  int<lower=2> NUM_OPTIONS;

  int<lower=1> NUM_SUBJECTS;
  int<lower=1> NUM_RUNS;
  int<lower=1> NUM_TRIALS;
  //int<lower=1> N; //used to determine length of n(subject) length arrays
  //int<lower=1> T; //used to determine lenght of n(trial) length delays. 
  //This is the maximum number of trials, across all runs, that any subject should have
  //in practice most subjects have the same number of trials, but some subjects have less because data is missing for one or another reason.
  
  //int<lower=1,upper=T> Tsubj[N]; //number of trials we actually have for each nth subject. 
  //Used to determine the number of iterations when iterating through each subject's trials.
  
  //vector[NUM_TRIALS] response_time;
  int<lower=0,upper=NUM_OPTIONS> selected_option[NUM_TRIALS];
  //int<lower=0,upper=2> choice[N,T]; //choice made by subject N at trial T. 
  
  int<lower=0,upper=NUM_OPTIONS> required_option[NUM_TRIALS];//the choice which would be
  real outcome[NUM_TRIALS]; //The actual outcome (correct=1, incorrect=0, nonresponse=0) reinforced on each round.
  int cue[NUM_TRIALS];
  //int<lower=0,upper=100> cue[N,T]; //cue (image) showed to subject N on trial T.
  //This is very important because subjects' learned values for choice are specific to each cue.
  
  int<lower=1> run_subjid[NUM_RUNS];
  int<lower=1> run_ot[NUM_RUNS];
  int<lower=1> trial_runid[NUM_TRIALS];

  int DELTA_N;
  matrix[NUM_TRIALS,DELTA_N] neural_data;
  
  int sample_from_prior; // flag to tell stan to sample from the prior, i.e., ignore empirical data from choice and outcome.
  //useful for comparison to observe the influence of the data on the model.
  
  //multiple runs, multiple reward type extension
  // int<lower=1> R; //number of runs (max); used when iterating through run values
  // int<lower=1> R_N[N]; //number of runs for each subject; used when iterating through subject-level run values.
  // int<lower=0> run_id[N,T]; //identifies each particular run for a subject; used because we calculate separate estimate for each run.
  // 
  
  //int<lower=0> run_ot[N,R]; //run outcome type. Indicates whether a particular run is reward or punishment.
  //used because we calculate a difference parameter for each of each subject's parameters 
  //to indicate difference between the two reward runs and the two punishment runs.
  
}
transformed data{
  int THETA_rpe = 1;
  int THETA_ev = 2;
  int THETA_N = 2;
  int TD_N=DELTA_N+THETA_N;
  int<lower=0> run_N_trials[NUM_TRIALS] = rep_array(0,NUM_TRIALS);
  vector[TD_N] zeros = rep_vector(0,TD_N);
  
  int MAX_RUN_LENGTH;
  int MAX_CUE_ID = max(cue);
  
  
  int<lower=1,upper=1> RUNTYPE_REW=1;
  int<lower=2,upper=2> RUNTYPE_PUN=2;
  
  real run_ot_multiplier[NUM_RUNS];
  
  //need to record for all subjects whether they have reward runs, and whether they have punishment runs.
  int sub_has_rew_runs[NUM_SUBJECTS] = rep_array(0,NUM_SUBJECTS);
  int sub_has_pun_runs[NUM_SUBJECTS] = rep_array(0,NUM_SUBJECTS);
  
  //print("max cue ID is ",MAX_CUE_ID);
  
  //iterate through trial_runid, and create run_n_trials out of that, 
  //and ensure trials are correctly grouped by making sure trial always increments by 0 or 1
  run_N_trials[trial_runid[1]]=1;
  if(trial_runid[1]!=1){
    reject("Illegal order of trials: must start with the trials for run 1.");
  }
  for (t in 2:NUM_TRIALS){
    
    run_N_trials[trial_runid[t]]=run_N_trials[trial_runid[t]]+1;
    if((trial_runid[t-1]!=trial_runid[t]) && ((trial_runid[t-1]+1)!=(trial_runid[t]))){
      reject("Illegal order of trials. Trials for each run must be grouped together and groups of runs must be organized in consecutive order of run ID.");
      
    }
    
  }
  
  MAX_RUN_LENGTH=max(run_N_trials);
  
  if(NUM_OPTIONS!=2){
    reject("number of options must be 2");
  }
  
  //go through each run; check its owner, its type, and mark its owner as having a run with that type.
  for (r in 1:NUM_RUNS){
    if(run_ot[r]==RUNTYPE_REW){//1 represents reward
      sub_has_rew_runs[run_subjid[r]] = 1;
    }else if (run_ot[r]==RUNTYPE_PUN){//2 represents punishment
      sub_has_pun_runs[run_subjid[r]] = 1;
    }
  }
  //only can do the next step after we've been through all runs.
  for (r in 1:NUM_RUNS){
    if(sub_has_rew_runs[run_subjid[r]] && sub_has_pun_runs[run_subjid[r]]){
        //this subject has both reward and punishment runs
        //if this run is a reward run, set the multiplier to 1. 
        //If it's a punishment run, set it to -1.
        if(run_ot[r]==1){
          run_ot_multiplier[r] = 0.5;
        }else if (run_ot[r]==2){
          run_ot_multiplier[r] = -0.5;
        }
      }else{
        //If this subject doen'st have both reward and punishment runs, set the multiplier to 0.
        run_ot_multiplier[r] = 0;
      }
  }
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
  vector[NUM_SUBJECTS] alpha_s_pr_mu;   // learning rate, subject average across runs
  vector[NUM_SUBJECTS] beta_s_pr_mu;  // inverse temperature, subject average across runs
  vector[NUM_SUBJECTS] alpha_s_pr_rpdiff_mu;   // learning rate, difference between reward and punishment
  vector[NUM_SUBJECTS] beta_s_pr_rpdiff_mu;  // inverse temperature, difference between reward and punishment
    //Will be very important to somehow ignore values from subjects where there's no actual difference.

  //these are not vectors because we make a simplifying assumption they're the same for all subjects.
  real<lower=0> alpha_s_pr_sigma;   // learning rate, subject variance across runs
  real<lower=0> beta_s_pr_sigma;  // inverse temperature, subject variance across runs
  
  // Run level raw parameters
  // using probit transform
  real alpha_pr[NUM_RUNS];   // learning rate, run estimate
  real beta_pr[NUM_RUNS];  // inverse temperature, run estimate
  
  
  // ////////////\begin{joint model machinery}
  //vector[TD_N] td_mu;
  // cholesky_factor_corr[TD_N] L_Omega;
  // vector<lower=0>[TD_N] L_sigma;
  // ////////////\end{joint model machinery}
}

transformed parameters {
  // here is where we take that normally distributed parameter 
  // and phi-approximate it into a range.

  // Transform subject-level raw parameters
  real<lower=0,upper=1> alpha[NUM_RUNS];
  real<lower=0,upper=14> beta[NUM_RUNS];
  //interacts directly with the trial-level learning
  //drawn from a phi-approximation from group-level mean and deviation multiplied by subject-level parameter

  // ////////////\begin{joint model machinery}
  // matrix[TD_N, TD_N] L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  // matrix[TD_N, TD_N] Sigma = L_Sigma * L_Sigma';
  // ////////////\end{joint model machinery}

  //run level parameter transform.
  //I am not sure if this is high enough. 
  //This is double what we found in the recent paper using the same model
  for (r in 1:NUM_RUNS){//yikes. this won't work for < 4 runs.
    alpha[r]  = Phi_approx( alpha_pr[r]);
    beta[r]   = Phi_approx( beta_pr[r]) * 14; 
  }
}

model {
  int run_first_trial_id;
  int run_last_trial_id;
  int run_sub;
  
  // vector[TD_N] td_var[NUM_TRIALS];
  // matrix[NUM_TRIALS, TD_N] theta_delta;
  // vector[TD_N] td_mean;
  // vector[TD_N] td_sd;
  //   
    
  //mean and variance of the subject mean, i.e., the group level mean and SD
  group_pr_mu ~ normal(0, 1);
  group_pr_sigma ~ cauchy(0, 5); 
  group_pr_rpdiff_mu ~ normal(0, 1);
  group_pr_rpdiff_sigma ~ cauchy(0, 5);
  alpha_s_pr_sigma ~ cauchy(0, 5);
  beta_s_pr_sigma ~ cauchy(0, 5);
  
  
  //set up the TD matrix
  // L_Omega ~ lkj_corr_cholesky(4);
  // L_sigma ~ normal(0,1); //these yield standard deviations of each individual value.

  //subject level.
  for (s in 1:NUM_SUBJECTS){
    alpha_s_pr_mu[s] ~ normal(group_pr_mu[1],group_pr_sigma[1]);
    beta_s_pr_mu[s] ~ normal(group_pr_mu[2],group_pr_sigma[2]);
    
    alpha_s_pr_rpdiff_mu[s] ~ normal(group_pr_rpdiff_mu[1],group_pr_rpdiff_sigma[1]); 
    beta_s_pr_rpdiff_mu[s] ~ normal(group_pr_rpdiff_mu[2],group_pr_rpdiff_sigma[2]); 
    //alpha_s_pr_rpdiff_mu[s] ~ normal(0, 1); 
    //beta_s_pr_rpdiff_mu[s] ~ normal(0, 1); 
  }
  //run level
  run_last_trial_id=0;
  for (r in 1:NUM_RUNS){
    //define values specific to this run.
    //////////////\begin{joint model machinery}
    
    vector[run_N_trials[r]] trial_ev;
    vector[run_N_trials[r]] trial_PE;
    //////////////\end{joint model machinery}
    // Define values
    matrix[MAX_CUE_ID,NUM_OPTIONS] ev;
    real PEnc; // fictitious prediction error (PE-non-chosen)
    real PE;         // prediction error

    run_sub=run_subjid[r];
    //remember, because we made the simplifying assumption that every subject has the same run-level variance,
    //we just use the same alpha_s_sigma value for every subject :-)

    alpha_pr[r] ~ normal(alpha_s_pr_mu[run_sub]+run_ot_multiplier[run_sub]*alpha_s_pr_rpdiff_mu[run_sub],alpha_s_pr_sigma);
    beta_pr[r] ~ normal(beta_s_pr_mu[run_sub]+run_ot_multiplier[run_sub]*beta_s_pr_rpdiff_mu[run_sub],beta_s_pr_sigma);
    
    // Initialize values
    ev[,1] = rep_vector(0, MAX_CUE_ID); // initial ev values
    ev[,2] = rep_vector(0, MAX_CUE_ID); // initial ev values

    //go across the trials for this run.
    //Trials are grouped by runs and are consecutive, so we can do the following in order to access just the runs we want.
    run_first_trial_id = run_last_trial_id+1; //the ID of the first trial of current run is equal to the ID of the last trial of the last run + 1.
    run_last_trial_id = run_last_trial_id+ run_N_trials[r]; //ID of the last of this run is equal to the ID of the last trial of the last run plus number of trials in this run.
    //print(run_first_trial_id, "," ,run_last_trial_id, ",", run_N_trials[r]);
    //then we can iterate through the trials in this run by:
    for (t in run_first_trial_id:run_last_trial_id) {
      int t_within_run=t-run_first_trial_id+1;
      //get the particular run we are dealing with for this trial.
      //run = run_id[s,t]; 
      //runid = trial_runid[t];
      //don't need that because we're iterating through runs
      
      trial_ev[t_within_run] = ev[cue[t],selected_option[t]];
      
      if (selected_option[t]!=0) {
        if(sample_from_prior!=1){
          selected_option[t] ~ categorical_logit( to_vector(ev[cue[t],]) * beta[r] );
        
          // prediction error
          PE   =  outcome[t] - ev[cue[t],selected_option[t]];
          PEnc = -outcome[t] - ev[cue[t],3-selected_option[t]];
          
          trial_PE[t_within_run] = PE;
    
          // value updating (learning)
          ev[cue[t],3-selected_option[t]] = ev[cue[t],3-selected_option[t]] + alpha[r] * PEnc;
          ev[cue[t],selected_option[t]] = ev[cue[t],selected_option[t]] + alpha[r] * PE;
        }
        else{
          print("sampling from prior; data ignored.")
        }
      }
    }

    ///////NEXT THING TO DO: CREATE THE TD MATRIX FOR THIS TRIAL AND SAMPLE FOR IT.
    // theta_delta[run_first_trial_id:run_last_trial_id,THETA_rpe]=logit(trial_PE/4+0.5);
    // theta_delta[run_first_trial_id:run_last_trial_id,THETA_ev]=logit(trial_ev/2+0.5);
    // //transfer the deltas into the theta-delta matrix.
    // for (d_i in 1:DELTA_N){
    //   theta_delta[run_first_trial_id:run_last_trial_id,THETA_N+d_i]=neural_data[run_first_trial_id:run_last_trial_id, d_i];
    // }
    // //standardize within-run.
    // for (tdi in 1:TD_N){
    //   td_mean[tdi] = mean(theta_delta[run_first_trial_id:run_last_trial_id,tdi]);  //separately calculate mean for each ThetaDelta var
    //   td_sd[tdi] = sd(theta_delta[run_first_trial_id:run_last_trial_id,tdi]);      //separately calculate SD for each ThetaDelta var
    // }
    // for (i in run_first_trial_id:run_last_trial_id){
    //   td_var[i,:] = (to_vector(theta_delta[i,:]) - td_mean) ./ td_sd;
    // }
    
  }
    
    // now this theta_delta matrix needs to be the length of the
    // particular number of trials we have. This is going to be

    //go through and estimate the same matrix for each subject.
    // for (tdi in 1:TD_N){
    //   td_mean[tdi] = mean(theta_delta[:,tdi]);  //separately calculate mean for each ThetaDelta var
    //   td_sd[tdi] = sd(theta_delta[:,tdi]);      //separately calculate SD for each ThetaDelta var
    // }
    // //this is a bizarre wey of calculating this isn't it?
    // //standardize the variance.
    // for (i in 1:NUM_TRIALS){
    //   td_var[i,:] = (to_vector(theta_delta[i,:]) - td_mean) ./ td_sd;
    // }
    //print("--");
    //print(td_var);
    // print(zeros);
    // print(L_Sigma);
    //td_var ~ multi_normal_cholesky(zeros,L_Sigma);//sample the ThetaDelta matrix from a cholesky matrix
    ////////////\end{joint model machinery}
  
  
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
  
  // vector[NUM_SUBJECTS] alpha_s_mu;
  // vector[NUM_SUBJECTS] beta_s_mu;
  // real alpha_s_sigma;
  // real beta_s_sigma;
  
  ///////////////////////////////////////////////////////////////
  ///////gather information about covariance
  int run_first_trial_id;
  int run_last_trial_id;
  int run_sub;
  
  //vector[TD_N] td_var[NUM_TRIALS];
  matrix[NUM_TRIALS, TD_N] theta_delta;
  //vector[TD_N] td_mean;
  //vector[TD_N] td_sd;
    
  
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
  ///////////////////////////////////////////////////////////////
  ///////gather information about covariance
  
  
  //run level
  run_last_trial_id=0;
  for (r in 1:NUM_RUNS){
    //define values specific to this run.
    //////////////\begin{joint model machinery}
    
    vector[run_N_trials[r]] trial_ev;
    vector[run_N_trials[r]] trial_PE;
    //////////////\end{joint model machinery}
    // Define values
    matrix[MAX_CUE_ID,NUM_OPTIONS] ev;
    real PEnc; // fictitious prediction error (PE-non-chosen)
    real PE;         // prediction error

    run_sub=run_subjid[r];
    //remember, because we made the simplifying assumption that every subject has the same run-level variance,
    //we just use the same alpha_s_sigma value for every subject :-)

    // Initialize values
    ev[,1] = rep_vector(0, MAX_CUE_ID); // initial ev values
    ev[,2] = rep_vector(0, MAX_CUE_ID); // initial ev values

    //go across the trials for this run.
    //Trials are grouped by runs and are consecutive, so we can do the following in order to access just the runs we want.
    run_first_trial_id = run_last_trial_id+1; //the ID of the first trial of current run is equal to the ID of the last trial of the last run + 1.
    run_last_trial_id = run_last_trial_id+ run_N_trials[r]; //ID of the last of this run is equal to the ID of the last trial of the last run plus number of trials in this run.
    //print(run_first_trial_id, "," ,run_last_trial_id, ",", run_N_trials[r]);
    //then we can iterate through the trials in this run by:
    for (t in run_first_trial_id:run_last_trial_id) {
      int t_within_run=t-run_first_trial_id+1;
      //get the particular run we are dealing with for this trial.
      //run = run_id[s,t]; 
      //runid = trial_runid[t];
      //don't need that because we're iterating through runs
      
      trial_ev[t_within_run] = ev[cue[t],selected_option[t]];
      
      if (selected_option[t]!=0) {
        if(sample_from_prior!=1){
          //selected_option[t] ~ categorical_logit( to_vector(ev[cue[t],]) * beta[r] );
        
          // prediction error
          PE   =  outcome[t] - ev[cue[t],selected_option[t]];
          PEnc = -outcome[t] - ev[cue[t],3-selected_option[t]];
          
          trial_PE[t_within_run] = PE;
    
          // value updating (learning)
          ev[cue[t],3-selected_option[t]] = ev[cue[t],3-selected_option[t]] + alpha[r] * PEnc;
          ev[cue[t],selected_option[t]] = ev[cue[t],selected_option[t]] + alpha[r] * PE;
        }
        else{
          print("sampling from prior; data ignored.");
        }
      }else{
        reject("got a nonselection row but that is not supported for a joint model at this time.");
      }
    }

    ///////NEXT THING TO DO: CREATE THE TD MATRIX FOR THIS TRIAL AND SAMPLE FOR IT.
    theta_delta[run_first_trial_id:run_last_trial_id,THETA_rpe]=trial_PE;
    theta_delta[run_first_trial_id:run_last_trial_id,THETA_ev]=trial_ev;
    //transfer the deltas into the theta-delta matrix.
    for (d_i in 1:DELTA_N){
      theta_delta[run_first_trial_id:run_last_trial_id,THETA_N+d_i]=neural_data[run_first_trial_id:run_last_trial_id, d_i];
    }
    //standardize within-run.
    // for (tdi in 1:TD_N){
    //   td_mean[tdi] = mean(theta_delta[run_first_trial_id:run_last_trial_id,tdi]);  //separately calculate mean for each ThetaDelta var
    //   td_sd[tdi] = sd(theta_delta[run_first_trial_id:run_last_trial_id,tdi]);      //separately calculate SD for each ThetaDelta var
    // }
    // for (i in run_first_trial_id:run_last_trial_id){
    //   td_var[i,:] = (to_vector(theta_delta[i,:]) - td_mean) ./ td_sd;
    // }
    
  }
    
    // // now this theta_delta matrix needs to be the length of the
    // // particular number of trials we have. This is going to be
    // 
    // //go through and estimate the same matrix for each subject.
    // for (tdi in 1:TD_N){
    //   td_mean[tdi] = mean(theta_delta[:,tdi]);  //separately calculate mean for each ThetaDelta var
    //   td_sd[tdi] = sd(theta_delta[:,tdi]);      //separately calculate SD for each ThetaDelta var
    // }
    // //this is a bizarre wey of calculating this isn't it?
    // //standardize the variance.
    // for (i in 1:NUM_TRIALS){
    //   td_var[i,:] = (to_vector(theta_delta[i,:]) - td_mean) ./ td_sd;
    // }

  //print("generating from posterior");
}
