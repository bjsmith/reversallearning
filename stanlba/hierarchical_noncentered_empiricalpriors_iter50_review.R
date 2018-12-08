source("stanlba/lba_rl_setup.R")

load(file=paste0(dd, "Fits/informative_priors_test_allsubs_450.RData"))

dif<-cbind(summary(fit_informative_priors)$summary[,"n_eff"],
summary(fit_weakly_informative_priors)$summary[,"n_eff"],
(summary(fit_informative_priors)$summary[,"n_eff"]-
summary(fit_weakly_informative_priors)$summary[,"n_eff"])/(
  abs(summary(fit_informative_priors)$summary[,"n_eff"]-
    summary(fit_weakly_informative_priors)$summary[,"n_eff"])
))
table(dif[,3])

t.test(dif[!is.nan(dif[,3]),3])
#so, very clearly NO DIFFERENCE in the effective sample size here.
summary(fit_informative_priors)$summary
summary(fit_weakly_informative_priors)$summary
