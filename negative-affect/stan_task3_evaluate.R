source("negative-affect/negative_affect_trials_setup_amendment2.R")
unique(rawdata$subid)
length(unique(rawdata$subid))
length(unique(interaction(rawdata$subid,rawdata$runmotiveid)))
#620 runs in total.
source("negative-affect/negative_affect_trials_setup.R")
library(rstanarm)
library(parallel)
library(loo)

load("pain_stan_output_2.Rdata")
unique(rawdata$subid)
length(unique(rawdata$subid))
length(unique(interaction(rawdata$subid,rawdata$runid,rawdata$Motivation)))

length(rownames(summary(m.rp.1.stan))[seq(7,1272,2)])

# rownames(summary(m.rp.0.stan))
summary(m.rp.1.stan)[1:10,]
# #seems like we have a result here.


load(file="pain_stan_output_3.Rdata")
load(file="pain_stan_output_3_no_correctincorrect.Rdata")
load(file="pain_stan_output_3_no_motivation.Rdata")

m.rp.1.nocorrect.stan.loo <-  loo(m.rp.1.nocorrect.stan)
save(m.rp.1.nocorrect.stan.loo,"m.rp.1.nocorrect.stan.loo.RData")
m.rp.1.stan.loo <- loo(m.rp.1.stan)
save(m.rp.1.stan.loo,"m.rp.1.stan.loo.RData")
m.rp.1.nomotivation.stan.loo  <-  loo(m.rp.1.nomotivation.stan)
save(m.rp.1.nomotivation.stan.loo,"m.rp.1.nomotivation.stan.loo.RData")

# loo::compare(loo(m.rp.1.nomotivation.stan,save_psis = TRUE),loo(m.rp.1.stan,save_psis = TRUE))
# 
# save(m.rp.1.nocorrect.stan.loo,m.rp.1.stan.loo,m.rp.1.nomotivation.stan.loo,file="pain_stan_output_loo.RData")