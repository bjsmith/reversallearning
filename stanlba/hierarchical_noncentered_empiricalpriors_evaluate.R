load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_6_3level_empiricalpriors_noncentered_15sub_run12_model_distinct_runs_itercount500_wup400_MCMC_testgroup.RData")

rmfit_testgroup<-rmfit

load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_6_3level_empiricalpriors_noncentered_15sub_run12_model_distinct_runs_itercount500_wup400_MCMC.RData")

rmfit_controlgroup<-rmfit

load("../../../data/msm/reversallearning/Fits/informative_priors_test_allsubs_450.RData")

t(rbind(summary(rmfit_controlgroup)$summary[,"n_eff"],summary(rmfit_testgroup)$summary[,"n_eff"]))

t(rbind(summary(fit_informative_priors)$summary[,"n_eff"],summary(fit_weakly_informative_priors)$summary[,"n_eff"]))