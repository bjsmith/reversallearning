#estimate a single model across all subjects so that we can get a better estimate of run-level values.
#only one run per subject.

source("stanlba/lba_rl_setup.R")

multisubj_threesubs<-rawdata[subid %in% c(105:125) & Motivation=="reward" & runid==1,
                             .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,UniqueRunID=interaction(subid,runid,Motivation,drop = TRUE))]


#hmmm, before we can speedtest, we need to ensure the damn thing actually works.
bseed<-76128931#set.seed(as.numeric(Sys.time())); sample.int(.Machine$integer.max-1000, 1)
iter<-50
warmup_iter=45
chains<-min(get_my_preferred_cores(),3)
cores_to_use <- chains
options(mc.cores = cores_to_use)
print(paste0("using ", cores_to_use, " cores."))
print("running hierarchical_speedtest.R")
#Get a minimal amount of data to test a three level model.

fit_rl_lba_rl_multi_subj_3_2level_3subj <- stan(file=paste0(stanfiledir,'lba_rl_multi_run_4_2level.stan'), 
                                                #fit=fit_rl_lba_multi_subj_proto1,
                                                data = list(
                                                  NUM_CHOICES=2,
                                                  A=0.01,
                                                  #NUM_SUBJECTS=length(unique(multisubj_threesubs$subid)),
                                                  NUM_TRIALS=dim(multisubj_threesubs)[1],
                                                  NUM_RUNS=length(unique(multisubj_threesubs$UniqueRunID)),
                                                  #trial_subjid=multisubj_threesubs$subid,
                                                  trial_runid=as.numeric(multisubj_threesubs$UniqueRunID),
                                                  
                                                  response_time=multisubj_threesubs$reaction_time,
                                                  response=multisubj_threesubs$choice,
                                                  required_choice=multisubj_threesubs$cor_res_Counterbalanced,
                                                  cue=multisubj_threesubs$cue
                                                ),
                                                warmup = warmup_iter, 
                                                iter = iter,
                                                chains = 3,
                                                control = list(max_treedepth = 15))
print(fit_rl_lba_rl_multi_subj_3_2level_3subj)

#will this offer faster iterations? We want to get those priors as close to empirical values as possible.
fit_rl_lba_rl_multi_subj_3_2level_3subj_v2 <- stan(file=paste0(stanfiledir,'lba_rl_multi_run_4_2level_v2.stan'), 
                                                #fit=fit_rl_lba_multi_subj_proto1,
                                                data = list(
                                                  NUM_CHOICES=2,
                                                  A=0.01,
                                                  #NUM_SUBJECTS=length(unique(multisubj_threesubs$subid)),
                                                  NUM_TRIALS=dim(multisubj_threesubs)[1],
                                                  NUM_RUNS=length(unique(multisubj_threesubs$UniqueRunID)),
                                                  #trial_subjid=multisubj_threesubs$subid,
                                                  trial_runid=as.numeric(multisubj_threesubs$UniqueRunID),
                                                  
                                                  response_time=multisubj_threesubs$reaction_time,
                                                  response=multisubj_threesubs$choice,
                                                  required_choice=multisubj_threesubs$cor_res_Counterbalanced,
                                                  cue=multisubj_threesubs$cue
                                                ),
                                                warmup = warmup_iter, 
                                                iter = iter,
                                                chains = 3,
                                                control = list(max_treedepth = 15))
print(fit_rl_lba_rl_multi_subj_3_2level_3subj_v2)