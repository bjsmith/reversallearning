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