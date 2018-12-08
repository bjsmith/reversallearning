

####### ANALYSIS OF stanlba/hierarchical_noncentered_empiricalpriors_repeat.R
load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_informativepriors_repeat_run12_model_distinct_runs_itercount500_wup450_MCMC.RData")
fit_informative_p<-rmfit
efficiency_score(rmfit)
efficiency_score_group_params(rmfit)
draw_sampling_progress(rmfit,"subj_mu[1]")
#This works, with just three chains.

load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_weakpriors_repeat_run12_model_distinct_runs_itercount500_wup450_MCMC.RData")
fit_weak_p<-rmfit
efficiency_score(rmfit)
efficiency_score_group_params(rmfit) #also not bad
draw_sampling_progress(rmfit,"subj_mu[1]")
