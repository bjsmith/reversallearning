#uses the non-centered models
#and uses empirical priors.
#the informative priors model took 3.8 hours. With 10x the number of subjects, I might estimate 40 hours; 
#with double the number of iterations I'd estimate 80 hours. 
#That's 3-4 days, which seems acceptable for a large model.

source("stanlba/lba_rl_setup.R")
source("stanlba/lba_rl_allsingles_get_results_summary.R")
chains<-min(get_my_preferred_cores(),3)
cores_to_use <- chains
options(mc.cores = cores_to_use)
print(paste0("using ", cores_to_use, " cores."))

#Get a minimal amount of data to test a three level model.
multisubj_multirun_moresubs<-rawdata[subid %in% c(105:115) #& Motivation=="reward" 
                                     & reaction_time>0,
                                    .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                      ConsecSubId=as.integer(as.factor(as.character(subid))),
                                      UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]
unique(multisubj_multirun_moresubs$ConsecSubId)
#hmmm, before we can speedtest, we need to ensure the damn thing actually works.
bseed<-712363934#set.seed(as.numeric(Sys.time())); sample.int(.Machine$integer.max-1000, 1)

warmup_iter=10
iter<-12
print(paste0("warmup_iter: ",warmup_iter))
print(paste0("iter: ",iter))

run_model<-function(model_filename,model_description,filedir="",informative_priors=FALSE){
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
      priors_lba_alpha=alpha_pr_mean,
      priors_lba_alpha_spread=alpha_pr_var,
      # these priors could probably be set even narrower than this, but let's ease into it.
      priors_lba_alpha_sd_gamma=alpha_sd_prior*2,
      
      priors_lba_k=k_pr_mean,
      priors_lba_k_spread=k_pr_var,
      priors_lba_k_sd_gamma=k_sd_prior*2,
      
      priors_lba_tau=tau_pr_mean,
      priors_lba_tau_spread=tau_pr_var,
      priors_lba_tau_sd_gamma=tau_sd_prior*2
      
      
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
      priors_lba_alpha=-3,
      priors_lba_alpha_spread=3,
      priors_lba_k=log(.5),
      priors_lba_k_spread=1,
      priors_lba_tau=log(.5),
      priors_lba_tau_spread=0.5
    )
  }
  rmfit<-stan(file=paste0(stanfiledir,filedir,model_filename,".stan"), 
       #fit=fit_rl_lba_multi_subj_proto1,
       data = data_to_pass,
       warmup = warmup_iter, 
       iter = iter,
       chains = chains, #run as many chains as we have cores to run them, but no more than 12 necessary.
       seed=bseed,
       refresh=5,
       control = list(max_treedepth = 13,adapt_delta=0.9))
  tend<-Sys.time()
  print(tend-tstart)
  file_save_name<-get_fit_desc(use_model = model_filename,descr = model_description,run=c(1,2),
                               model_rp_separately=TRUE,model_runs_separately=TRUE,
                               use_pain=FALSE,fastDebug=FALSE,fileSuffix="",
                               estimation_method=ESTIMATION_METHOD.MCMC,
                               bseed=bseed,warmup_iter = warmup_iter,
                               iterations=iter)
  save(rmfit,file=file_save_name)
  print(rmfit)
  
  return(rmfit)
}
print("running...")


print("Running the INFORMATIVE PRIORS model.")
fit_informative_priors <- run_model("lba_rl_multi_subj_6_3level_empiricalpriors_noncentered","allsubs",filedir="incremental/",informative_priors = TRUE)
# 
# print("------------------------")
print("Running the base model")
fit_weakly_informative_priors <- run_model("lba_rl_multi_subj_6_3level_empiricalpriors_noncentered","allsubs",filedir="incremental/",informative_priors = FALSE)
# 
#save(fit_normalsds,fit_widevariablecauchys,fit_base,file=paste0(dd, "Fits/hierarchical_stanforum_suggestion_results.RData"))
save(fit_informative_priors,fit_weakly_informative_priors,file=paste0(dd, "Fits/informative_priors_test_allsubs_450.RData"))

