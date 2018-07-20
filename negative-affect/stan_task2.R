source("negative-affect/negative_affect_trials_setup.R")
library(rstanarm)
library(parallel)

m.rp.1.stan<-stan_glmer(ValueScaled~
                          (ResponseCorrect==FALSE)*(Motivation=="punishment") + 
                          presentation_n_in_segment + 
                          (1+presentation_n_in_segment | subid/runmotiveid) + 
                          (1 | image),rawdata.ordered.complete,cores=detectCores())

save(m.rp.1.stan,file="pain_stan_output_2.Rdata")
# load("pain_stan_output.Rdata")

# summary(m.rp.0.stan)
# rownames(summary(m.rp.0.stan))
# summary(m.rp.0.stan)[1:10,]
# #seems like we have a result here.

