load("/expdata/bensmith/joint-modeling/code/msm/reversallearning/good-fit.RData")
library(rstan)


#june 11
rmfit_controlgroup@date
rmfit_empiricalpriors_500_iter@date
rmfit_testgroup@date
#june 13
rmfit_empiricalpriors_500_iter_lowerdelta@date

#june 14
rmfit_1000iter@date
rmfit@date

#june 25
fit_with_manual_init_vals@date

fit_with_manual_init_vals@stan_args
unlist(lapply(fit_with_manual_init_vals@inits,function(init){init$subj_mu[1]}))
fit_with_manual_init_vals@sim
#8 chains. A strange choice!
#probably the other ones couldn't get off the ground??
#now did these 
fit_with_manual_init_vals@inits[[1]]
mean(summary(fit_with_manual_init_vals)$summary[,"n_eff"])
mean(summary(rmfit)$summary[,"n_eff"])
mean(summary(rmfit)$summary[,"Rhat"])