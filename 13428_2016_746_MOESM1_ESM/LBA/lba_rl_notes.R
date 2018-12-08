rm(list=ls())

source('lba-math.r')
library(rstan)
library(parallel)
library(dplyr)

options(mc.cores = 3)


#right, this time, we need to get some real data.
source("../../../util/apply_local_settings.R")
apply_local_settings("../../")
dd<-localsettings$data.dir
library(data.table)


rawdata <- data.table(read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment.txt"), header=T))
colnames(rawdata)

#make simualated data
          #n,   b,A,vs,     s,t0,
out = rlba(300,1,.5,c(3,2),1,0.4)
hist(out$rt,breaks=100)
rt = cbind(out$rt,out$resp)
len = length(rt[,1])
#run the Stan model
fit <- stan(file='lba_single.stan', 
            data = list(RT=rt,LENGTH=len,NUM_CHOICES=2),
            warmup = 500, 
            iter = 1000,
            chains = 3)
print(fit)
#OK. So how about those exponential functions I was using?
fit_exp <- stan(file='lba_single_exp.stan', 
                data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                warmup = 500, 
                iter = 1000,
                chains = 3)
print(fit_exp)
exp(-0.03)
exp(-1.10)


#so this ran fine to those settings.
#however, my own data is more complex; we have our RL algorithm which uses choice data to constrain a learning rate
#it ends up producing--for some reason--very very imbalanced v values.

#model summary
print(fit)

#does this work if we constrain starting evidence A?
fit_constrainedA <- stan(file='lba_single_constrainedA.stan', 
data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.33),
warmup = 500, 
iter = 1000,
chains = 3)

#model summary
print(fit_constrainedA)
#seems to work!
#alright - what if we constrained A to zero?

fit_constrainedA2 <- stan(file='lba_single_constrainedA.stan', 
                         data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.1),
                         warmup = 500, 
                         iter = 1000,
                         chains = 3)

#Nope. Constraining A to zero is a bad idea! We can constrain it; a constraint to 0.1 works... 
print(fit_constrainedA2)

fit_constrainedA_0 <- stan(file='lba_single_constrainedA.stan', 
                          data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0),
                          warmup = 500, 
                          iter = 1000,
                          chains = 3)
print(fit_constrainedA_0)

fit_constrainedA3 <- stan(file='lba_single_constrainedA.stan', 
                           data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                           warmup = 500, 
                           iter = 1000,
                           chains = 3)
print(fit_constrainedA3)
#looks like we can constrain A to be *very close to zero* without any problems; but constraining to zero is a bad idea.
#this is probably due to learning, and to properly examine, I probably need to import single-subject data here.
#however, let's just take a look at what that looks like...
out = rlba(300,1,.5,invlogit(c(5,3)),1,0.4)
library(gtools)
inv.logit(c(5,3))
# so the kind of learning values we're passing in are consistent with the subject responding entirely one way rather than the other;
# almost at perfectly good or perfectly bad performance; yet we know the data isn't like that.
# so something is wrong with that likelihood process, and we need to dig down to find out what's going on with it.
# to do that, we're going to need some empirical data. Let's take a single subject, single run, and see what's going on.

rt = cbind(out$rt,out$resp)
len = length(rt[,1])

#This stan model does NOT use the empirical data; but it contains a modified lba function that takes single values 
#instead of a matrix of responses and RTs
#this will be important to integrate the LBA with reinforcement learning.
fit_rl_lba_proto1 <- stan(file='lba_rl_single_exp_proto1.stan', 
            data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
            warmup = 500, 
            iter = 1000,
            chains = 3)
print(fit_rl_lba_proto1)
#this looks good! I got a problem when I ran this; it may have been related to including an "alpha" parameter without any constraints
#this has now been removed.

#what values of v are sensible?
rlba(300,1,.5,c(10,2),1,0.4)$resp
rlba(300,1,.5,c(2,2),1,0.4)$resp
rlba(300,1,.5,c(2,0.1),1,0.4)$resp
rlba(300,1,.5,c(2,0),1,0.4)$resp
table(rlba(300,1,.5,c(1,0),1,0.4)$resp)
table(rlba(3000,1,.01,c(1,1),1,0.4)$resp)
#it's actually perfectly fine untransformed. 
#it actually has infinite support, in theory, but in practice , you want it within a few SD of zero.
#our values are asymptotic to [-1, 1]; this appears to be O.K.; however we *could* transform it to have infinite support; 
#I'm not sure if this would capture the learning relationship we want or not.
logit(seq(-0.9,0.9,0.1)/2+0.5)
table(rlba(300,1,.5,c(10,-10),1,0.4)$resp)


logit_ev<-logit(ev_rec/2+0.5)
invlogit_ev<-inv.logit(ev_rec/2+0.5)
plot(logit_ev)
plot(ev_rec)
plot(invlogit_ev)

sub108data<-rawdata[subid==108 & Motivation=="reward" & runid==1,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]

table(rawdata$choice,rawdata$response_key)#'choice' and 'response_key' are the same
#what is 'required choice'?
table(rawdata$choice,rawdata$cor_res_Counterbalanced,rawdata$outcome)

# int LENGTH;
# int NUM_CHOICES;
# real<lower=0> A;
# //matrix[LENGTH,2] RT;
# vector[LENGTH] response_time;
# int response[LENGTH];
# int required_choice[LENGTH];//the choice which would be reinforced on each round.
# int cue[LENGTH];

fit_rl_lba_proto2 <- stan(file='lba_rl_single_exp_proto2.stan', 
                          data = list(
                            LENGTH=dim(sub108data)[1],
                            NUM_CHOICES=2,
                            A=0.01,
                            response_time=sub108data$reaction_time,
                            response=sub108data$choice,
                            required_choice=sub108data$cor_res_Counterbalanced,
                            cue=sub108data$cue
                            ),
                          warmup = 500, 
                          iter = 1000,
                          chains = 1,
                          control = list(max_treedepth = 15))

print(fit_rl_lba_proto2)

fit_rl_lba_proto3 <- stan(file='lba_rl_single_exp_proto3.stan', 
                          data = list(
                            LENGTH=dim(sub108data)[1],
                            NUM_CHOICES=2,
                            A=0.01,
                            response_time=sub108data$reaction_time,
                            response=sub108data$choice,
                            required_choice=sub108data$cor_res_Counterbalanced,
                            cue=sub108data$cue
                          ),
                          warmup = 500, 
                          iter = 1000,
                          chains = 1,
                          control = list(max_treedepth = 15))

print(fit_rl_lba_proto3)

sub105data<-rawdata[subid==105 & Motivation=="reward" & runid==1,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]
fit_rl_lba_proto3_sub105 <- stan(file='lba_rl_single_exp_proto3.stan', 
                          fit=fit_rl_lba_proto3,
                          data = list(
                            LENGTH=dim(sub105data)[1],
                            NUM_CHOICES=2,
                            A=0.01,
                            response_time=sub105data$reaction_time,
                            response=sub105data$choice,
                            required_choice=sub105data$cor_res_Counterbalanced,
                            cue=sub105data$cue
                          ),
                          warmup = 500, 
                          iter = 1000,
                          chains = 3,
                          control = list(max_treedepth = 15))
print(fit_rl_lba_proto3_sub105)

#hmm. Can we do a low-learning-rate assumption and still end up with a sensible learning rate?
fit_rl_lba_proto4_sub105 <- stan(file='lba_rl_single_exp_proto4.stan', 
                                 data = list(
                                   LENGTH=dim(sub105data)[1],
                                   NUM_CHOICES=2,
                                   A=0.01,
                                   response_time=sub105data$reaction_time,
                                   response=sub105data$choice,
                                   required_choice=sub105data$cor_res_Counterbalanced,
                                   cue=sub105data$cue
                                 ),
                                 warmup = 500, 
                                 iter = 1000,
                                 chains = 3,
                                 control = list(max_treedepth = 15))
print(fit_rl_lba_proto4_sub105)
#yes! A low alpha prior does not seem to make much difference. After 1000 iterations, we end up with an alpha value near what we were expecting.

#get three subjects' worth of data. all from the same run.
# real<lower=0> A;
# int<lower=2> NUM_CHOICES;
# 
# int<lower=1> NUM_SUBJECTS;
# int<lower=1> NUM_TRIALS;
# int<lower=1> NUM_RUNS;
# int<lower=1> trial_subjid[NUM_TRIALS];
# int<lower=1> trial_runid[NUM_TRIALS];
# 
# vector[NUM_TRIALS] response_time;
# int response[NUM_TRIALS];
# int required_choice[NUM_TRIALS];//the choice which would be reinforced on each round.
# int cue[NUM_TRIALS];

#this model models multiple subjects and multiple runs, but it is not a hierarchical model. 
#All subjects and all runs are completely pooled.
#parameters defining which run each trial belongs to are only used to properly determine the ongoing expected values within each run.
multisubj_threesubs<-rawdata[subid %in% c(105,106,108) & Motivation=="reward" & runid==1,
                             .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,UniqueRunID=interaction(subid,runid,Motivation,drop = TRUE))]
multisubj_threesubs[,.N,subid]
multisubj_threesubs[,.N,UniqueRunID]
table(multisubj_threesubs$UniqueRunID)
as.integer(multisubj_threesubs$UniqueRunID)
stan.input.data<-list(
  A=0.01,
  NUM_CHOICES=2,
  
  NUM_SUBJECTS=length(unique(multisubj_threesubs$subid)),
  NUM_TRIALS=dim(multisubj_threesubs)[1],
  NUM_RUNS=length(unique(multisubj_threesubs$UniqueRunID)),
  trial_subjid=multisubj_threesubs$subid,
  trial_runid=as.integer(multisubj_threesubs$UniqueRunID),
  
  response_time=multisubj_threesubs$reaction_time,
  response=multisubj_threesubs$choice,
  required_choice=multisubj_threesubs$cor_res_Counterbalanced,
  cue=multisubj_threesubs$cue
)
fit_rl_lba_multi_subj_proto1 <- stan(file='lba_rl_multi_subj_proto1.stan', 
                                 data = stan.input.data,
                                 warmup = 500, 
                                 iter = 1000,
                                 chains = 1,
                                 control = list(max_treedepth = 15))
print(fit_rl_lba_multi_subj_proto1)

multisubj_1sub<-rawdata[subid %in% c(105) & Motivation=="reward" & runid==1,
                             .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,UniqueRunID=interaction(subid,runid,Motivation,drop = TRUE))]
multisubj_1sub[,.N,subid]
multisubj_1sub[,.N,UniqueRunID]
table(multisubj_1sub$UniqueRunID)
as.integer(multisubj_1sub$UniqueRunID)
stan.input.data.1s<-list(
  A=0.01,
  NUM_CHOICES=2,
  
  NUM_SUBJECTS=length(unique(multisubj_1sub$subid)),
  NUM_TRIALS=dim(multisubj_1sub)[1],
  NUM_RUNS=length(unique(multisubj_1sub$UniqueRunID)),
  trial_subjid=multisubj_1sub$subid,
  trial_runid=as.integer(multisubj_1sub$UniqueRunID),
  
  response_time=multisubj_1sub$reaction_time,
  response=multisubj_1sub$choice,
  required_choice=multisubj_1sub$cor_res_Counterbalanced,
  cue=multisubj_1sub$cue
)
fit_rl_lba_multi_subj_proto1 <- stan(file='lba_rl_multi_subj_proto1.stan', 
                                     #fit=fit_rl_lba_multi_subj_proto1,
                                     data = stan.input.data.1s,
                                     warmup = 500, 
                                     iter = 1000,
                                     chains = 1,
                                     control = list(max_treedepth = 15))


#OK We'll get this bastard pared down! Will need to commenting out stuff to get it back to the single-subject model until it seems to work,
#then build up piece by piece to work out what is wrong. :-)

#this model models multiple subjects, but it is not a hierarchical model. Each subject is modeled independently.
#It can handle multiple runs, but parameters across runs within subjects will be completely pooled.

fit_rl_lba_multi_subj_proto1_stripped <- stan(file='lba_rl_multi_subj_proto1_stripped.stan', 
                                     #fit=fit_rl_lba_multi_subj_proto1,
                                     data = list(
                                       NUM_CHOICES=2,
                                       A=0.01,
                                       NUM_SUBJECTS=length(unique(multisubj_1sub$subid)),
                                       NUM_TRIALS=dim(multisubj_1sub)[1],
                                       NUM_RUNS=length(unique(multisubj_1sub$UniqueRunID)),
                                       trial_subjid=multisubj_1sub$subid,
                                       trial_runid=as.numeric(multisubj_1sub$UniqueRunID),
                                       
                                       response_time=multisubj_1sub$reaction_time,
                                       response=multisubj_1sub$choice,
                                       required_choice=multisubj_1sub$cor_res_Counterbalanced,
                                       cue=multisubj_1sub$cue
                                     ),
                                     warmup = 500, 
                                     iter = 1000,
                                     chains = 1,
                                     control = list(max_treedepth = 15))
print(fit_rl_lba_multi_subj_proto1_stripped)


fit_rl_lba_multi_subj_proto1_stripped_3subj <- stan(file='lba_rl_multi_subj_proto1_stripped.stan', 
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
                                              warmup = 500, 
                                              iter = 1000,
                                              chains = 1,
                                              control = list(max_treedepth = 15))
print(fit_rl_lba_multi_subj_proto1_stripped_3subj)
#really poor efficiency but this might be because we've got multiple subjects and are complete pooling.
#so we can try the same thing

fit_rl_lba_multi_subj_proto1_3subj <- stan(file='lba_rl_multi_subj_proto1.stan', 
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
                                                    warmup = 500, 
                                                    iter = 1000,
                                                    chains = 1,
                                                    control = list(max_treedepth = 15))
print(fit_rl_lba_multi_subj_proto1_3subj)

#now we'll try use no pooling, i.e., we calculate separate parameters for each run, but do not attempt to calculate a hierarchical group parameter.
fit_rl_lba_multi_subj_proto2_nopooling_3subj <- stan(file='lba_rl_multi_subj_proto2_nopooling.stan', 
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
                                           warmup = 500, 
                                           iter = 1000,
                                           chains = 1,
                                           control = list(max_treedepth = 15))
print(fit_rl_lba_multi_subj_proto2_nopooling_3subj)

options(mc.cores = 3)
fit_rl_lba_rl_multi_subj_3_2level_3subj <- stan(file='lba_rl_multi_subj_3_2level.stan', 
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
                                                     warmup = 500, 
                                                     iter = 1000,
                                                     chains = 3,
                                                     control = list(max_treedepth = 15))
print(fit_rl_lba_rl_multi_subj_3_2level_3subj)
#additional sampling time to add a hierarchical parameter doesn't seem to add *that* much calculation time.
#the hierarchical design seems to have dramatically reduced efficency, although encouragingly, the model still converged.
fit_rl_lba_rl_multi_subj_3_2level_3subj_strongpriors <- stan(file='lba_rl_multi_subj_3_2level_strongpriors.stan', 
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
                                                warmup = 500, 
                                                iter = 1000,
                                                chains = 3,
                                                control = list(max_treedepth = 15))
print(fit_rl_lba_rl_multi_subj_3_2level_3subj_strongpriors)

#some initial evidence that when we change both
#(1) the strength of the prior and 
#(2) the bias of the prior
#do NOT really give us any different alpha values, but substantially lower efficiency. So let's keep priors pessimistic and weakly informative.

#next step is a three level model. Let's create the data we'll use to test it:

multisubj_multirun_threesubs<-rawdata[subid %in% c(105,106,108) & Motivation=="reward",
                             .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                               ConsecSubId=as.integer(as.factor(as.character(subid))),
                               UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]

multisubj_multirun_threesubs$ConsecSubId
table(multisubj_multirun_threesubs$UniqueRunID,multisubj_multirun_threesubs$ConsecSubId)

#here is the three level model.

fit_rl_lba_multi_subj_4_3level <- stan(file='lba_rl_multi_subj_4_3level.stan', 
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
                                                             chains = 3,
                                                             control = list(max_treedepth = 15))
print(fit_rl_lba_multi_subj_4_3level)
