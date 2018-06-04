source("stanlba/lba_rl_setup.R")

options(mc.cores = 4)

#Get a minimal amount of data to test a three level model.
multisubj_multirun_threesubs<-rawdata[subid %in% c(105,106,108) & Motivation=="reward" & reaction_time>0,
                                      .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                        ConsecSubId=as.integer(as.factor(as.character(subid))),
                                        UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]
table(multisubj_multirun_threesubs$UniqueRunID,multisubj_multirun_threesubs$ConsecSubId)

fit_rl_lba_multi_subj_4_3level <- stan(file=paste0(stanfiledir,'lba_rl_multi_subj_4_3level.stan'), 
                                       #fit=fit_rl_lba_multi_subj_proto1,
                                       data = list(
                                         NUM_CHOICES=2,
                                         A=0.01,
                                         NUM_SUBJECTS=length(unique(multisubj_multirun_threesubs$subid)),
                                         NUM_TRIALS=dim(multisubj_multirun_threesubs)[1],
                                         NUM_RUNS=length(unique(multisubj_multirun_threesubs$UniqueRunID)),
                                         run_subjid=multisubj_multirun_threesubs[,.(RunID=unique(UniqueRunID)),by=ConsecSubId] %>% .[order(RunID),ConsecSubId],
                                         trial_runid=as.numeric(multisubj_multirun_threesubs$UniqueRunID),
                                         
                                         response_time=multisubj_multirun_threesubs$reaction_time,
                                         response=multisubj_multirun_threesubs$choice,
                                         required_choice=multisubj_multirun_threesubs$cor_res_Counterbalanced,
                                         cue=multisubj_multirun_threesubs$cue
                                       ),
                                       warmup = 500, 
                                       iter = 1000,
                                       chains = 4,
                                       control = list(max_treedepth = 15))
print(fit_rl_lba_multi_subj_4_3level)
