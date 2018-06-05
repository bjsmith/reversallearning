#we need to do a scientific test of the time taken to run the basic 3-level algorithm, applying:
#(1) vectorization shortcut
#(2) Phi_approx speedup
#(3) Non-centered parameterization

#this should:

#Run two stan models. OK to run sequentially. Print each model fit and gauge its efficiency. 
#Also time it although timing might not be a reliable indicator.

source("stanlba/lba_rl_setup.R")

chains<-min(get_my_preferred_cores(),3)
cores_to_use <- chains
options(mc.cores = cores_to_use)
print(paste0("using ", cores_to_use, " cores."))
print("running hierarchical_speedtest.R")
#Get a minimal amount of data to test a three level model.
multisubj_multirun_twosubs<-rawdata[subid %in% c(105,106) & Motivation=="reward" & reaction_time>0,
                                    .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                      ConsecSubId=as.integer(as.factor(as.character(subid))),
                                      UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]

#hmmm, before we can speedtest, we need to ensure the damn thing actually works.
bseed<-168344357#set.seed(as.numeric(Sys.time())); sample.int(.Machine$integer.max-1000, 1)
iter<-400
warmup_iter=200

run_model<-function(model_filename,model_description,model_folder=""){
  tstart<-Sys.time()
  rmfit<-stan(file=paste0(stanfiledir,model_folder,model_filename,".stan"), 
       #fit=fit_rl_lba_multi_subj_proto1,
       data = list(
         NUM_CHOICES=2,
         A=0.01,
         NUM_SUBJECTS=length(unique(multisubj_multirun_twosubs$subid)),
         NUM_TRIALS=dim(multisubj_multirun_twosubs)[1],
         NUM_RUNS=length(unique(multisubj_multirun_twosubs$UniqueRunID)),
         run_subjid=multisubj_multirun_twosubs[,.(RunID=unique(UniqueRunID)),by=ConsecSubId] %>% .[order(RunID),ConsecSubId],
         trial_runid=as.numeric(multisubj_multirun_twosubs$UniqueRunID),
         
         response_time=multisubj_multirun_twosubs$reaction_time,
         response=multisubj_multirun_twosubs$choice,
         required_choice=multisubj_multirun_twosubs$cor_res_Counterbalanced,
         cue=multisubj_multirun_twosubs$cue
       ),
       warmup = warmup_iter, 
       iter = iter,
       chains = chains, #run as many chains as we have cores to run them, but no more than 12 necessary.
       seed=bseed,
       refresh=5,
       control = list(max_treedepth = 10,adapt_delta=0.9))
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
print("Running the NON-CENTERED MODEL model WITH lba speed improvements")
fit_rl_lba_multi_subj_4_3level_phiapproxvectorized_noncentered <- run_model("lba_rl_multi_subj_4_3level_phiapproxvectorized_noncentered","2sub","incremental/")
print("....the end.")
print("------------------------")
print("Running the NON-CENTERED MODEL model.")
fit_rl_lba_multi_subj_4_3level_noncentered <- run_model("lba_rl_multi_subj_4_3level_noncentered","2sub","incremental/")

print("------------------------")
print("Running the CENTERED MODEL model WITH lba speed improvements")
fit_rl_lba_multi_subj_4_3level_phiapproxvectorized <- run_model("lba_rl_multi_subj_4_3level_phiapproxvectorized","2sub","incremental/")
print("------------------------")
print("Running the CENTERED MODEL (base model)")
fit_rl_lba_multi_subj_4_3level <- run_model("lba_rl_multi_subj_4_3level","2sub")

common_cols<-intersect(intersect(intersect(rownames(summary(fit_lba_rl_multi_subj_4_3level_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary),
          rownames(summary(fit_lba_rl_multi_subj_4_3level_noncentered_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary)),
          rownames(summary(fit_lba_rl_multi_subj_4_3level_phiapproxvectorized_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary)),
          rownames(summary(fit_lba_rl_multi_subj_4_3level_phiapproxvectorized_noncentered_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary))

fit_comparison_Rhat<-cbind(summary(fit_lba_rl_multi_subj_4_3level_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary[common_cols,"Rhat"],
       summary(fit_lba_rl_multi_subj_4_3level_noncentered_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary[common_cols,"Rhat"],
      summary(fit_lba_rl_multi_subj_4_3level_phiapproxvectorized_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary[common_cols,"Rhat"],
      summary(fit_lba_rl_multi_subj_4_3level_phiapproxvectorized_noncentered_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary[common_cols,"Rhat"])
View(fit_comparison_Rhat)

fit_comparison_efficiency<-cbind(summary(fit_lba_rl_multi_subj_4_3level_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary[common_cols,"n_eff"],
                           summary(fit_lba_rl_multi_subj_4_3level_noncentered_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary[common_cols,"n_eff"],
                           summary(fit_lba_rl_multi_subj_4_3level_phiapproxvectorized_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary[common_cols,"n_eff"],
                           summary(fit_lba_rl_multi_subj_4_3level_phiapproxvectorized_noncentered_2sub_run12_model_distinct_runs_itercount400_wup200_MCMC)$summary[common_cols,"n_eff"])
View(fit_comparison_efficiency)


#It does seem like the third one, i.e., using phi_approx and vectorization, is the best.
#HOWEVER there's no way that Phi_approximation and vectorization should improve efficency. At best, they improve speed in real time.
#Thus the variance we're seeing across these, at least in terms of efficiency and probably Rhat, are probably not meaningful.
#we can ignore them and focus on the method that works.
#I might want positive proof of the non-centered parameterization so let's continue with the centered parameterization for now while we try out other methods for speed improvement.
