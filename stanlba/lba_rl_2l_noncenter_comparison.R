source("stanlba/lba_rl_setup.R")

options(mc.cores = 3)

#Get a minimal amount of data to test a three level model.
multisubj_multirun_threesubs<-rawdata[subid %in% c(105,106,108) & Motivation=="reward" & reaction_time>0,
                                      .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                        RunIdForSubj=runid,
                                        ConsecSubId=as.integer(as.factor(as.character(subid))),
                                        UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]

multisubj_threesubs <- multisubj_multirun_threesubs[RunIdForSubj==1]


options(mc.cores = 3)
fit_rl_lba_rl_multi_subj_3_2level_3subj <- stan(file='stanlba/stanfiles/lba_rl_multi_subj_3_2level.stan', 
                                                #fit=fit_rl_lba_multi_subj_proto1,
                                                data = list(
                                                  NUM_CHOICES=2,
                                                  A=0.01,
                                                  NUM_SUBJECTS=length(unique(multisubj_threesubs$subid)),
                                                  NUM_TRIALS=dim(multisubj_threesubs)[1],
                                                  NUM_RUNS=length(unique(multisubj_threesubs$UniqueRunID)),
                                                  trial_subjid=multisubj_threesubs$subid,
                                                  trial_runid=as.numeric(multisubj_threesubs$UniqueRunID),
                                                  
                                                  response_time=multisubj_threesubs$reaction_time,
                                                  response=multisubj_threesubs$choice,
                                                  required_choice=multisubj_threesubs$cor_res_Counterbalanced,
                                                  cue=multisubj_threesubs$cue
                                                ),
                                                warmup = 100, 
                                                iter = 200,
                                                chains = 1,
                                                control = list(max_treedepth = 15))


fit_rl_lba_rl_multi_subj_3_2level_3subj_noncentered <- stan(file='stanlba/stanfiles/incremental/lba_rl_multi_subj_3_2level_noncentered.stan', 
                                                #fit=fit_rl_lba_multi_subj_proto1,
                                                data = list(
                                                  NUM_CHOICES=2,
                                                  A=0.01,
                                                  NUM_SUBJECTS=length(unique(multisubj_threesubs$subid)),
                                                  NUM_TRIALS=dim(multisubj_threesubs)[1],
                                                  NUM_RUNS=length(unique(multisubj_threesubs$UniqueRunID)),
                                                  trial_subjid=multisubj_threesubs$subid,
                                                  trial_runid=as.numeric(multisubj_threesubs$UniqueRunID),
                                                  
                                                  response_time=multisubj_threesubs$reaction_time,
                                                  response=multisubj_threesubs$choice,
                                                  required_choice=multisubj_threesubs$cor_res_Counterbalanced,
                                                  cue=multisubj_threesubs$cue
                                                ),
                                                warmup = 100, 
                                                iter = 200,
                                                chains = 3,
                                                control = list(max_treedepth = 15))


fit_rl_lba_rl_multi_subj_3_2level_3subj_noncentered_altlba <- stan(file='stanlba/stanfiles/incremental/lba_rl_multi_subj_3_2level_noncentered_altlba.stan', 
                                                            #fit=fit_rl_lba_multi_subj_proto1,
                                                            data = list(
                                                              NUM_CHOICES=2,
                                                              A=0.01,
                                                              NUM_SUBJECTS=length(unique(multisubj_threesubs$subid)),
                                                              NUM_TRIALS=dim(multisubj_threesubs)[1],
                                                              NUM_RUNS=length(unique(multisubj_threesubs$UniqueRunID)),
                                                              trial_subjid=multisubj_threesubs$subid,
                                                              trial_runid=as.numeric(multisubj_threesubs$UniqueRunID),
                                                              
                                                              response_time=multisubj_threesubs$reaction_time,
                                                              response=multisubj_threesubs$choice,
                                                              required_choice=multisubj_threesubs$cor_res_Counterbalanced,
                                                              cue=multisubj_threesubs$cue
                                                            ),
                                                            warmup = 100, 
                                                            iter = 200,
                                                            chains = 3,
                                                            control = list(max_treedepth = 15))



fit_rl_lba_rl_multi_subj_3_2level_3subj_noncentered_altlba_nologtransform <- stan(file='stanlba/stanfiles/incremental/lba_rl_multi_subj_3_2level_noncentered_altlba_nologtransform.stan', 
                                                                   #fit=fit_rl_lba_multi_subj_proto1,
                                                                   data = list(
                                                                     NUM_CHOICES=2,
                                                                     A=0.01,
                                                                     NUM_SUBJECTS=length(unique(multisubj_threesubs$subid)),
                                                                     NUM_TRIALS=dim(multisubj_threesubs)[1],
                                                                     NUM_RUNS=length(unique(multisubj_threesubs$UniqueRunID)),
                                                                     trial_subjid=multisubj_threesubs$subid,
                                                                     trial_runid=as.numeric(multisubj_threesubs$UniqueRunID),
                                                                     
                                                                     response_time=multisubj_threesubs$reaction_time,
                                                                     response=multisubj_threesubs$choice,
                                                                     required_choice=multisubj_threesubs$cor_res_Counterbalanced,
                                                                     cue=multisubj_threesubs$cue
                                                                   ),
                                                                   warmup = 100, 
                                                                   iter = 200,
                                                                   chains = 3,
                                                                   control = list(max_treedepth = 15))



fit_rl_lba_rl_multi_subj_3_2level_3subj_noncentered_altlba_nophiapprox <- stan(file='stanlba/stanfiles/incremental/lba_rl_multi_subj_3_2level_noncentered_altlba_nophiapprox.stan', 
                                                                                  #fit=fit_rl_lba_multi_subj_proto1,
                                                                                  data = list(
                                                                                    NUM_CHOICES=2,
                                                                                    A=0.01,
                                                                                    NUM_SUBJECTS=length(unique(multisubj_threesubs$subid)),
                                                                                    NUM_TRIALS=dim(multisubj_threesubs)[1],
                                                                                    NUM_RUNS=length(unique(multisubj_threesubs$UniqueRunID)),
                                                                                    trial_subjid=multisubj_threesubs$subid,
                                                                                    trial_runid=as.numeric(multisubj_threesubs$UniqueRunID),
                                                                                    
                                                                                    response_time=multisubj_threesubs$reaction_time,
                                                                                    response=multisubj_threesubs$choice,
                                                                                    required_choice=multisubj_threesubs$cor_res_Counterbalanced,
                                                                                    cue=multisubj_threesubs$cue
                                                                                  ),
                                                                                  warmup = 100, 
                                                                                  iter = 200,
                                                                                  chains = 3,
                                                                                  control = list(max_treedepth = 15))

