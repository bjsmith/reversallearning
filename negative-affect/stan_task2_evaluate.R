source("negative-affect/negative_affect_trials_setup.R")
library(rstanarm)
library(parallel)

load("pain_stan_output_2.Rdata")

summary(m.rp.1.stan)
# rownames(summary(m.rp.0.stan))
summary(m.rp.1.stan)[1:10,]
# #seems like we have a result here.

