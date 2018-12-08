rm(list=ls())

source('stanlba/lba-math.r')
library(rstan)
library(parallel)
library(dplyr)

options(mc.cores = 3)


#right, this time, we need to get some real data.
source("../util/apply_local_settings.R")
apply_local_settings("")
dd<-localsettings$data.dir
library(data.table)

#make simualated data
#n,   b,A,vs,     s,t0,
out = rlba(300,1,.5,c(3,2),1,0.4)
hist(out$rt,breaks=100)
rt = cbind(out$rt,out$resp)
len = length(rt[,1])

mymodel<-stan_model(file='stanlba/stanfiles/lba_single_exp.stan')
#OK. So how about those exponential functions I was using?

fit_exp <- sampling(m1, data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                    warmup = 500, 
                    iter = 1000,
                    chains = 3,
                sample_file=paste0(localsettings$data.dir,"MCMCSample"),
                diagnostic_file=paste0(localsettings$data.dir,"MCMCdiagnostic"),
                seed=bseed,
                verbose=TRUE)


print(fit_exp)

# mymodel<-stan_model(model_code = mc, model_name = "external", allow_undefined = TRUE,
#                     includes = paste0('\n#include "', 
#                                       file.path(getwd(), 'fib.hpp'), '"\n'))

