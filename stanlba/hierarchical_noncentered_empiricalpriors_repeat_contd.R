#so I have a seemingly inexplicable change from the 500 iteration version to the 1000 iteration version and I don't know why.
#The 500 iteration version worked; the 1000 iteration version did not.
#let's re-run them at the same time, using the code tht worked originally, and see if they both still run?
#in analysis "stanlba/hierarchical_noncentered_empiricalpriors_evaluate.R" I ruled out differences in:
# - the text of the model code itself.
# - initial values
# - adapt_delta or any other arguments in stan_args
#remains possible that we passed in different data itself, including different informative priors.
#I will save the output of that this time.

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




run_model<-function(model_filename,model_description,filedir="",informative_priors=FALSE,
                    warmup_iter=450,
                    iter=500,
                    chain_num=chains){
  print(paste0("warmup_iter: ",warmup_iter))
  print(paste0("iter: ",iter))
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
  rmfit<-stan(file=paste0(stanfiledir,filedir,model_filename,".stan"), 
              #fit=fit_rl_lba_multi_subj_proto1,
              data = data_to_pass,
              warmup = warmup_iter, 
              iter = iter,
              chains = chain_num, #run as many chains as we have cores to run them, but no more than 12 necessary.
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
  save(rmfit,data_to_pass,file=file_save_name)
  print(rmfit)
  
  return(rmfit)
}
print("running...")
# 
# 
# print("Running the INFORMATIVE PRIORS model.")
# fit_informative_priors <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_informativepriors_repeat",filedir="incremental/",informative_priors = TRUE)
# # 
# # print("------------------------")
# print("Running the base model")
# fit_weakly_informative_priors <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_weakpriors_repeat",filedir="incremental/",informative_priors = FALSE)
# # 
# # print("------------------------")
# print("Running the 500 sample INFORMATIVE PRIORS model.")
# fit_informative_priors <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_informativepriors_repeat",filedir="incremental/",informative_priors = TRUE, warmup_iter=500,iter=1000)

print("------------------------")
print("Running the 50-sample informative priors model WITH 6 CHAINS.")
fit_informative_priors <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_informativepriors_repeat_6chains",filedir="incremental/",informative_priors = TRUE,
                                    chain_num=6)

print("------------------------")
print("Running the 500-sample informative priors model WITH 6 CHAINS.")
fit_informative_priors <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_informativepriors_repeat_6chains",filedir="incremental/",informative_priors = TRUE,
                                    chain_num=6, warmup_iter=500,iter=1000)