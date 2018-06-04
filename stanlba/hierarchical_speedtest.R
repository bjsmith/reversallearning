#we need to do a scientific test of the time taken to run the basic 3-level algorithm, applying:
#(1) vectorization shortcut
#(2) Phi_approx speedup
#(3) Non-centered parameterization

#this should:

#Run two stan models. OK to run sequentially. Print each model fit and gauge its efficiency. 
#Also time it although timing might not be a reliable indicator.

source("stanlba/lba_rl_setup.R")

options(mc.cores = 3)

#Get a minimal amount of data to test a three level model.
multisubj_multirun_threesubs<-rawdata[subid %in% c(105,106,108) & Motivation=="reward" & reaction_time>0,
                                      .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                        ConsecSubId=as.integer(as.factor(as.character(subid))),
                                        UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]

#hmmm, before we can speedtest, we need to ensure the damn thing actually works.
