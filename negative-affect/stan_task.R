source("negative-affect/negative_affect_trials_setup.R")
library(rstanarm)
library(parallel)

m.rp.0.stan<-stan_glmer(ValueScaled~
                          (ResponseCorrect==FALSE) + 
                          presentation_n_in_segment + 
                          (1+presentation_n_in_segment | subid/runmotiveid) + 
                          (1 | image),rawdata.ordered.complete,cores=detectCores())
save(m.rp.0.stan,file="pain_stan_output.Rdata")
