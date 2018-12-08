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

#OK. So how about those exponential functions I was using?
fit_exp <- stan(file='lba_single_exp.stan', 
                          data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                          warmup = 500, 
                          iter = 1000,
                          chains = 3)
print(fit_exp)
exp(-0.03)
exp(-1.10)
