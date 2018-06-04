#this should:
#just do an initial pass.

source("stanlba/lba_rl_setup.R")
options(mc.cores = get_my_preferred_cores())
print(paste0("using ", get_my_preferred_cores(), " cores."))

#Get a minimal amount of data to test a three level model.
multisubj_multirun_twosubs<-rawdata[subid %in% c(105,106) & Motivation=="reward" & reaction_time>0,
                                      .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                        ConsecSubId=as.integer(as.factor(as.character(subid))),
                                        UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]

#hmmm, before we can speedtest, we need to ensure the damn thing actually works.
bseed<-76128931#set.seed(as.numeric(Sys.time())); sample.int(.Machine$integer.max-1000, 1)
iter<-500
warmup_iter=250
print("running...")
fit_rl_lba_multi_subj_4_3level <- stan(file=paste0(stanfiledir,'lba_rl_multi_subj_4_3level.stan'), 
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
                                       chains = min(get_my_preferred_cores(),12), #run as many chains as we have cores to run them, but no more than 12 necessary.
                                       seed=bseed,
                                       refresh=5,
                                       control = list(max_treedepth = 15))

file_save_name<-get_fit_desc(use_model = "hierarchical_3l_initial_test",descr = "3sub",run=c(1,2),
             model_rp_separately=TRUE,model_runs_separately=TRUE,
             use_pain=FALSE,fastDebug=FALSE,fileSuffix="",
             estimation_method=ESTIMATION_METHOD.MCMC,
             bseed=bseed,warmup_iter = warmup_iter,
             iterations=iter)
save(fit_rl_lba_multi_subj_4_3level,file=file_save_name)
print(fit_rl_lba_multi_subj_4_3level)