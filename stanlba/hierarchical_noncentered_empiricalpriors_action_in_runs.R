#going to try something here: if all the run-level models work nicely,
#and the group-level doesn't, then why not set very small variance at the top two levels,
#regular variance at the run-level, and that should give us the same performance we get for each run individually.

source("stanlba/lba_rl_setup.R")
source("stanlba/lba_rl_allsingles_get_results_summary.R")
chains<-min(get_my_preferred_cores(),6)
cores_to_use <- chains
options(mc.cores = cores_to_use)
print(paste0("using ", cores_to_use, " cores."))

#Get a minimal amount of data to test a three level model.
multisubj_multirun_moresubs<-rawdata[subid %in% c(105:115) #& Motivation=="reward" 
                                     & reaction_time>0,
                                    .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                      ConsecSubId=as.integer(as.factor(as.character(subid))),
                                      UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]

multisubj_multirun_allsubs<-rawdata[#subid %in% c(105:115) #& Motivation=="reward" & 
                                     reaction_time>0,
                                     .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                       ConsecSubId=as.integer(as.factor(as.character(subid))),
                                       UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]


multisubj_multirun_group1<-rawdata[
  RiskLabel=="Safe No Meth" & 
  subid %in% c(105:175) &
  #subid %in% c(105:115) #& Motivation=="reward" & 
  reaction_time>0,
  .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
    ConsecSubId=as.integer(as.factor(as.character(subid))),
    UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]

#View(multisubj_multirun_group1[ConsecSubId==1])
#hmmm, before we can speedtest, we need to ensure the damn thing actually works.
bseed<-1385752049#712363934#set.seed(as.numeric(Sys.time())); sample.int(.Machine$integer.max-1000, 1)
#multisubj_multirun_group1<-multisubj_multirun_group1[ConsecSubId %in% 1:40]
run_model<-function(model_filename,model_description,filedir="",informative_priors=FALSE,
                    a_delta=0.9,data_to_use=multisubj_multirun_moresubs,warmup_iter=30,iter=50){
  tstart<-Sys.time()
  print(paste0("warmup_iter: ",warmup_iter))
  print(paste0("iter: ",iter))
  
  if(informative_priors){
    data_to_pass<- list(
      NUM_CHOICES=2,
      A=0.01,
      NUM_SUBJECTS=length(unique(data_to_use$subid)),
      NUM_TRIALS=dim(data_to_use)[1],
      NUM_RUNS=length(unique(data_to_use$UniqueRunID)),
      run_subjid=data_to_use[,.(RunID=unique(UniqueRunID)),by=ConsecSubId] %>% .[order(RunID),ConsecSubId],
      trial_runid=as.numeric(data_to_use$UniqueRunID),
      
      response_time=data_to_use$reaction_time,
      response=data_to_use$choice,
      required_choice=data_to_use$cor_res_Counterbalanced,
      cue=data_to_use$cue,
      priors_alpha=alpha_pr_mean,
      priors_alpha_spread=alpha_pr_var/10,
      priors_alpha_sd_gamma=alpha_sd_prior,
      priors_alpha_run_sigma_gamma=alpha_run_sigma_gamma,
      # these priors could probably be set even narrower than this, but let's ease into it.
      
      
      priors_lba_k=k_pr_mean,
      priors_lba_k_spread=k_pr_var/10,
      priors_lba_k_sd_gamma=k_sd_prior,
      priors_lba_k_run_sigma_gamma=k_run_sigma_gamma,
      
      priors_lba_tau=tau_pr_mean,
      priors_lba_tau_spread=tau_pr_var/10,
      priors_lba_tau_sd_gamma=tau_sd_prior,
      priors_lba_tau_run_sigma_gamma=tau_run_sigma_gamma
    )
  }else{
    data_to_pass<- list(
      NUM_CHOICES=2,
      A=0.01,
      NUM_SUBJECTS=length(unique(data_to_use$subid)),
      NUM_TRIALS=dim(data_to_use)[1],
      NUM_RUNS=length(unique(data_to_use$UniqueRunID)),
      run_subjid=data_to_use[,.(RunID=unique(UniqueRunID)),by=ConsecSubId] %>% .[order(RunID),ConsecSubId],
      trial_runid=as.numeric(data_to_use$UniqueRunID),
      
      response_time=data_to_use$reaction_time,
      response=data_to_use$choice,
      required_choice=data_to_use$cor_res_Counterbalanced,
      cue=data_to_use$cue,
      # # 
      # priors_alpha=-3,
      # priors_alpha_spread=3,
      # priors_alpha_sd_gamma=5,
      # priors_alpha_run_sigma_gamma=4,
      # # these priors could probably be set even narrower than this, but let's ease into it.
      # 
      # 
      # priors_lba_k=log(0.5),
      # priors_lba_k_spread=1,
      # priors_lba_k_sd_gamma=3,
      # priors_lba_k_run_sigma_gamma=2,
      # 
      # priors_lba_tau=log(0.5),
      # priors_lba_tau_spread=0.5,
      # priors_lba_tau_sd_gamma=2,
      # priors_lba_tau_run_sigma_gamma=1
      # 
      priors_alpha=-3,
      priors_alpha_spread=3/100,
      priors_alpha_sd_gamma=5/100,
      priors_alpha_run_sigma_gamma=4,
      # these priors could probably be set even narrower than this, but let's ease into it.
      
      
      priors_lba_k=log(0.5),
      priors_lba_k_spread=1/100,
      priors_lba_k_sd_gamma=3/100,
      priors_lba_k_run_sigma_gamma=2,
      
      priors_lba_tau=log(0.5),
      priors_lba_tau_spread=0.5/100,
      priors_lba_tau_sd_gamma=2/100,
      priors_lba_tau_run_sigma_gamma=1
      
      )
  }
  rmfit<-stan(file=paste0(stanfiledir,filedir,model_filename,".stan"), 
       #fit=fit_rl_lba_multi_subj_proto1,
       data = data_to_pass,
       warmup = warmup_iter, 
       iter = iter,
       #chains = chains, #run as many chains as we have cores to run them, but no more than 12 necessary.
       chains=1,
       seed=bseed,
       refresh=5,
       control = list(max_treedepth = 13,adapt_delta=a_delta))
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



# print("Running the informative priors model with a MODIFIED DELTA.")
# fit_modified_delta <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_informativepriors_ad08",filedir="incremental/",informative_priors = TRUE,a_delta=0.8)
# # 
# print("------------------------")
# print("Running the informative priors model with 500 warmup, 1000 real iterations.")
# fit_modified_delta <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_informativepriors_1000iter",filedir="incremental/",informative_priors = TRUE,warmup=500,iter=1000)
# 
# print("------------------------")
# print("Running the weakly informative priors model with GROUP 1.")
# fit_modified_delta <- run_model("lba_rl_multi_subj_8_3level_empiricalpriors_noncentered","10subs_informativepriors_allsubjs",filedir="incremental/",informative_priors = FALSE,data_to_use=multisubj_multirun_group1,a_delta = 0.95)

print("------------------------")
print("Running the informative priors model with ALL SUJBJECTS.")
fit_modified_delta <- run_model("lba_rl_multi_subj_8_3level_empiricalpriors_noncentered","10subs_informativepriors_allsubjs",filedir="incremental/",informative_priors = FALSE,data_to_use=multisubj_multirun_group1,a_delta = 0.95)


# #save(fit_normalsds,fit_widevariablecauchys,fit_base,file=paste0(dd, "Fits/hierarchical_stanforum_suggestion_results.RData"))
# save(fit_informative_priors,fit_weakly_informative_priors,file=paste0(dd, "Fits/informative_priors_test_allsubs_450.RData"))
# 
