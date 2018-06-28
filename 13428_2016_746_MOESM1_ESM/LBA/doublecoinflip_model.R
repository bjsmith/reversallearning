rm(list=ls())
#setwd("13428_2016_746_MOESM1_ESM/LBA/")
library(rstan)
library(parallel)

options(mc.cores = 3)

#run the Stan model
fit <- stan(file='naive_coinflip_model_multivariate.stan', 
            data = list(tosses=c(rep(0,15),rep(1,25)),n_tosses=40,tosses_coin2=c(rep(0,19),rep(1,21))),
            warmup = 500, 
            iter = 1000,
            chains = 3)
