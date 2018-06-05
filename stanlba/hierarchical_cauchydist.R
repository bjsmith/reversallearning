#we need to do a scientific test to see whether our cauchy distributions are specified wrong.



source("stanlba/lba_rl_setup.R")

chains<-min(get_my_preferred_cores(),3)
cores_to_use <- chains
options(mc.cores = cores_to_use)
print(paste0("using ", cores_to_use, " cores."))

#Get a minimal amount of data to test a three level model.
multisubj_multirun_twosubs<-rawdata[subid %in% c(105,106) & Motivation=="reward" & reaction_time>0,
                                    .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                      ConsecSubId=as.integer(as.factor(as.character(subid))),
                                      UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]

#hmmm, before we can speedtest, we need to ensure the damn thing actually works.
bseed<-2011348577#set.seed(as.numeric(Sys.time())); sample.int(.Machine$integer.max-1000, 1)
iter<-400
warmup_iter=200

run_model<-function(model_filename,model_description,filedir=""){
  tstart<-Sys.time()
  rmfit<-stan(file=paste0(stanfiledir,filedir,model_filename,".stan"), 
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
       control = list(max_treedepth = 15,adapt_delta=0.9))
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
print("Running the WIDE CAUCHYS model.")
fit_rl_lba_multi_subj_4_3level_noncentered <- run_model("lba_rl_multi_subj_4_3level_widecauchys","2sub",filedir="incremental/")
print("------------------------")
print("Running the base model")
fit_rl_lba_multi_subj_4_3level <- run_model("lba_rl_multi_subj_4_3level","2sub")

