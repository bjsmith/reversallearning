#uses the non-centered models
#and uses empirical priors.

source("stanlba/lba_rl_setup.R")

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
bseed<-586651014#set.seed(as.numeric(Sys.time())); sample.int(.Machine$integer.max-1000, 1)

warmup_iter=400
iter<-500
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
      priors_lba_alpha=-3.24,
      priors_lba_alpha_spread=2,
      # these priors could probably be set even narrower than this, but let's ease into it.
      priors_lba_alpha_sd_gamma=1.596763*2,
      
      priors_lba_k=-0.31,
      priors_lba_k_spread=0.1,
      priors_lba_k_sd_gamma=0.2549544*2,
      
      priors_lba_tau=-0.95,
      priors_lba_tau_spread=0.5,
      priors_lba_tau_sd_gamma=0.6146147*2
      
      
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
# print("Running the STANFORUM SUGGESTIONS 20180605 model.")
# fit_normalsds <- run_model("lba_rl_multi_subj_5_3level_normal_sds","15sub",filedir="incremental/")
# 
# print("------------------------")

print("Running the INFORMATIVE PRIORS model.")
fit_informative_priors <- run_model("lba_rl_multi_subj_6_3level_empiricalpriors_noncentered","15sub",filedir="incremental/",informative_priors = TRUE)
# 
# print("------------------------")
print("Running the base model")
fit_weakly_informative_priors <- run_model("lba_rl_multi_subj_6_3level_widevariablecauchys_noncentered","15sub",filedir="incremental/",informative_priors = FALSE)
# 
#save(fit_normalsds,fit_widevariablecauchys,fit_base,file=paste0(dd, "Fits/hierarchical_stanforum_suggestion_results.RData"))
save(fit_informative_priors,fit_weakly_informative_priors,file=paste0(dd, "Fits/informative_priors_test_450.RData"))

#based on intial transition estimates, going from 2s*2r/s=4r to 10s*4r/s=60r has increased the "1000 transitions" estimate from 11 s to 218.13 s. For the final model we'd be hoping to increase iterations by a factor of 10 and subjects by a factor of 15, so our estimate should be that the final model will take 150x as long, 
#i.e., if we aim for a 168 hour limit, this practice should take no longer than about 1 hour.
#considering the pattern we're observing of slower and slower iterations as we move through, we might really want a faster speed than that even.