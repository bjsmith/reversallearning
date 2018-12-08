

get_priors<-function(informative_priors=FALSE){
  data_to_pass<-get_priors_and_data(informative_priors)
  return(data_to_pass[names(data_to_pass)[grep("priors_",names(data_to_pass))]])
}

get_priors_and_data<-function(informative_priors=FALSE){
  tstart<-Sys.time()
  if(informative_priors){
    data_to_pass<- list(
      NUM_CHOICES=2,
      A=0.01,
      NUM_SUBJECTS=length(unique(multisubj_multirun_moresubs$subid)),
      NUM_TRIALS=dim(multisubj_multirun_moresubs)[1],
      NUM_RUNS=length(unique(multisubj_multirun_moresubs$UniqueRunID)),
      run_subjid=multisubj_multirun_moresubs[,.(RunID=unique(UniqueRunID)),by=ConsecSubId] %>% .[order(RunID),ConsecSubId],
      trial_runid=as.numeric(multisubj_multirun_moresubs$UniqueRunID),
      
      response_time=multisubj_multirun_moresubs$reaction_time,
      response=multisubj_multirun_moresubs$choice,
      required_choice=multisubj_multirun_moresubs$cor_res_Counterbalanced,
      cue=multisubj_multirun_moresubs$cue,
      priors_alpha=alpha_pr_mean,
      priors_alpha_spread=alpha_pr_var,
      priors_alpha_sd_gamma=alpha_run_sigma_gamma,
      priors_alpha_run_sigma_gamma=alpha_run_sigma_gamma,
      # these priors could probably be set even narrower than this, but let's ease into it.
      
      
      priors_lba_k=k_pr_mean,
      priors_lba_k_spread=k_pr_var,
      priors_lba_k_sd_gamma=k_sd_prior*2,
      priors_lba_k_run_sigma_gamma=k_run_sigma_gamma,
      
      priors_lba_tau=tau_pr_mean,
      priors_lba_tau_spread=tau_pr_var,
      priors_lba_tau_sd_gamma=tau_sd_prior*2,
      priors_lba_tau_run_sigma_gamma=tau_run_sigma_gamma
    )
  }else{
    data_to_pass<- list(
      NUM_CHOICES=2,
      A=0.01,
      NUM_SUBJECTS=length(unique(multisubj_multirun_moresubs$subid)),
      NUM_TRIALS=dim(multisubj_multirun_moresubs)[1],
      NUM_RUNS=length(unique(multisubj_multirun_moresubs$UniqueRunID)),
      run_subjid=multisubj_multirun_moresubs[,.(RunID=unique(UniqueRunID)),by=ConsecSubId] %>% .[order(RunID),ConsecSubId],
      trial_runid=as.numeric(multisubj_multirun_moresubs$UniqueRunID),
      
      response_time=multisubj_multirun_moresubs$reaction_time,
      response=multisubj_multirun_moresubs$choice,
      required_choice=multisubj_multirun_moresubs$cor_res_Counterbalanced,
      cue=multisubj_multirun_moresubs$cue,
      
      priors_alpha=-3,
      priors_alpha_spread=3,
      priors_alpha_sd_gamma=5,
      priors_alpha_run_sigma_gamma=4,
      # these priors could probably be set even narrower than this, but let's ease into it.
      
      
      priors_lba_k=log(0.5),
      priors_lba_k_spread=1,
      priors_lba_k_sd_gamma=3,
      priors_lba_k_run_sigma_gamma=2,
      
      priors_lba_tau=log(0.5),
      priors_lba_tau_spread=0.5,
      priors_lba_tau_sd_gamma=2,
      priors_lba_tau_run_sigma_gamma=1
      
    )
  }
  return(data_to_pass)
  
}

