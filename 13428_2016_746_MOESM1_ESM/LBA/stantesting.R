rm(list=ls())

source('lba-math.r')
library(rstan)
library(parallel)

options(mc.cores = 3)
#make simualated data
          #n,   b,A,vs,     s,t0,
out = rlba(300,1,.5,c(3,2),1,.5)
hist(out$rt)
rt = cbind(out$rt,out$resp)
len = length(rt[,1])
#run the Stan model
fit <- stan(file='stantesting.stan', 
            data = list(RT=rt,LENGTH=len,NUM_CHOICES=3),
            warmup = 5, 
            iter = 10,
            refresh=5,
            chains = 1)

#model summary
print(fit)
#collapse chains
mcmc_chain = as.matrix(fit)
#posterior predictions
pred_rt = mcmc_chain[,'pred[1]']
pred_resp = mcmc_chain[,'pred[2]']
hist(rt[rt[,2]==1,1],probability=T)
lines(density(pred_rt[pred_resp==1]))
hist(rt[rt[,2]==2,1],probability=T)
lines(density(pred_rt[pred_resp==2]))
hist(rt[rt[,2]==3,1],probability=T)
lines(density(pred_rt[pred_resp==3]))
#autocorrelation plots
acf(mcmc_chain[,'k'])
acf(mcmc_chain[,'A'])
acf(mcmc_chain[,'v[1]'])
acf(mcmc_chain[,'v[2]'])
acf(mcmc_chain[,'tau'])
#samples vs. iteration plot
traceplot(fit)
