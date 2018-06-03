rm(list=ls())

source('lba-math.r')
library(rstan)
library(parallel)

options(mc.cores = 3)
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
source("../../../util/apply_local_settings.R")
apply_local_settings("../../")
dd<-localsettings$data.dir
library(data.table)
rawdata <- data.table(read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment.txt"), header=T))

sub108data<-rawdata[subid==108 & Motivation=="reward" & runid==1,.(reaction_time,outcome,cue)]

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

fit_rl_lba_proto2 <- stan(file='lba_rl_single_exp_proto1.stan', 
                          data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                          warmup = 500, 
                          iter = 1000,
                          chains = 3,
                          control = list(max_treedepth = 15))


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
alpha<-0.2
ev<-0
iter<-13
ev_rec<-rep(NA,iter)
for (i in 1:iter){
  pe<-1-ev
  ev<-ev+pe*alpha
  ev_rec[i]<-ev
}

logit_ev<-logit(ev_rec/2+0.5)
plot(logit_ev)
plot(ev_rec)
