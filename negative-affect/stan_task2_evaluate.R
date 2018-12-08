source("negative-affect/negative_affect_trials_setup_amendment2.R")
unique(rawdata$subid)
length(unique(rawdata$subid))
length(unique(interaction(rawdata$subid,rawdata$runmotiveid)))
#620 runs in total.
source("negative-affect/negative_affect_trials_setup.R")
library(rstanarm)
library(parallel)

load("pain_stan_output_2.Rdata")
unique(rawdata$subid)
length(unique(rawdata$subid))
length(unique(interaction(rawdata$subid,rawdata$runid,rawdata$Motivation)))

length(rownames(summary(m.rp.1.stan))[seq(7,1272,2)])
nrow()
# rownames(summary(m.rp.0.stan))
summary(m.rp.1.stan)[1:10,]
# #seems like we have a result here.

