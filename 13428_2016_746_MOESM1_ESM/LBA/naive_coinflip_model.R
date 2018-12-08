rm(list=ls())
setwd("13428_2016_746_MOESM1_ESM/LBA/")
library(rstan)
library(parallel)

options(mc.cores = 3)

#run the Stan model
fit <- stan(file='naive_coinflip_model.stan', 
            data = list(tosses=c(0,0,0,1,1,1),n_tosses=6),
            warmup = 500, 
            iter = 1000,
            chains = 3)

#model summary
print(fit)

fit <- stan(file='naive_coinflip_model.stan', 
            data = list(tosses=c(rep(0,20),rep(1,20)),n_tosses=40),
            warmup = 500, 
            iter = 1000,
            chains = 3)

init_val_1<-function(){
  return(list(pr_heads_norm=1))
}

fit.smallwarmup <- stan(file='naive_coinflip_model.stan', 
            data = list(tosses=c(rep(0,20),rep(1,20)),n_tosses=40),
            warmup = 50, 
            iter = 100,
            init=init_val_1,
            chains = 3)


init_val_3<-function(){
  return(list(pr_heads_norm=3))
}

fit.smallwarmup.iv3 <- stan(file='naive_coinflip_model.stan', 
                        data = list(tosses=c(rep(0,20),rep(1,20)),n_tosses=40),
                        warmup = 50, 
                        iter = 100,
                        init=init_val_3,
                        chains = 3)

fit.vsmallwarmup.iv3 <- stan(file='naive_coinflip_model.stan', 
                            data = list(tosses=c(rep(0,20),rep(1,20)),n_tosses=40),
                            warmup = 10, 
                            iter = 20,
                            init=init_val_3,
                            chains = 3)

#QED we shouldn't need to specify the transformed values.
init_val_irrelevant<-function(){
  return(list(abc=3))
}
fit.vsmallwarmup.ivi <- stan(file='naive_coinflip_model.stan', 
                             data = list(tosses=c(rep(0,20),rep(1,20)),n_tosses=40),
                             warmup = 10, 
                             iter = 20,
                             init=init_val_irrelevant,
                             chains = 3)

fit.printvals <- stan(file='naive_coinflip_model_printvals.stan', 
                             data = list(tosses=c(rep(0,20),rep(1,20)),n_tosses=40,useless_array=rbind(c(1,3,6),c(2,4,8))),
                             warmup = 10, 
                             iter = 20,
                             init=init_val_3,
                             chains = 3)
#fit.printvals@

fit.printvals
