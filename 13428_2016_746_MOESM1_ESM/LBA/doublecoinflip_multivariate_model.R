rm(list=ls())
#setwd("13428_2016_746_MOESM1_ESM/LBA/")
library(rstan)
library(parallel)

options(mc.cores = 3)

#no dataa
fit <- stan(file='naive_double_coinflip_model_multivariate5.stan', 
            data = list(tosses=c(rep(0,150),rep(1,250)),n_tosses=400,tosses_coin2=c(rep(0,190),rep(1,210))),
            warmup = 500, 
            iter = 1000,
            chains = 3)
#some data
fit <- stan(file='naive_double_coinflip_model_multivariate6.stan', 
            data = list(tosses=c(rep(0,150),rep(1,250)),n_tosses=400,tosses_coin2=c(rep(0,190),rep(1,210))),
            warmup = 500, 
            iter = 1000,
            chains = 3)
#strong covariance
fit.strongcovar <- stan(file='naive_double_coinflip_model_multivariate6.stan', 
            data = list(tosses=c(rep(0,150),rep(1,250)),n_tosses=400,tosses_coin2=c(rep(0,151),rep(1,249))),
            warmup = 500, 
            iter = 1000,
            chains = 3)
