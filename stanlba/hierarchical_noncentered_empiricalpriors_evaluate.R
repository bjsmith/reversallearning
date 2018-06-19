load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_6_3level_empiricalpriors_noncentered_15sub_run12_model_distinct_runs_itercount500_wup400_MCMC_testgroup.RData")

rmfit_testgroup<-rmfit

load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_6_3level_empiricalpriors_noncentered_15sub_run12_model_distinct_runs_itercount500_wup400_MCMC.RData")

rmfit_controlgroup<-rmfit

load("../../../data/msm/reversallearning/Fits/informative_priors_test_allsubs_450.RData")

t(rbind(summary(rmfit_controlgroup)$summary[,"n_eff"],summary(rmfit_testgroup)$summary[,"n_eff"]))

t(rbind(summary(fit_informative_priors)$summary[,"n_eff"],summary(fit_weakly_informative_priors)$summary[,"n_eff"]))

efficiency_score<-function(myfit){
  param_num<-length(summary(myfit)$summary[,"n_eff"])
  sum(summary(myfit)$summary[,"n_eff"])/(length(myfit@stan_args)*(myfit@stan_args[[1]]$iter-myfit@stan_args[[1]]$warmup))/param_num
}

efficiency_score_group_params<-function(myfit){#myfit<-rmfit
  sum(summary(myfit)$summary[1:9,"n_eff"])/(length(myfit@stan_args)*(myfit@stan_args[[1]]$iter-myfit@stan_args[[1]]$warmup))/9
}


efficiency_score_by_param<-function(myfit){
  param_num<-length(summary(myfit)$summary[,"n_eff"])
  summary(myfit)$summary[,"n_eff"]/(length(myfit@stan_args)*(myfit@stan_args[[1]]$iter-myfit@stan_args[[1]]$warmup))/param_num
}


rmfit_samples<-function(myfit){
  myfit@stan_args[[1]]$iter-myfit@stan_args[[1]]$warmup
}
load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_6_3level_empiricalpriors_noncentered_15sub_run12_model_distinct_runs_itercount500_wup400_MCMC.RData")

rmfit_samples(rmfit)
efficiency_score(rmfit)



load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_informativepriors_run12_model_distinct_runs_itercount500_wup450_MCMC.RData")

rmfit_samples(rmfit)
efficiency_score(rmfit)
rmfit_empiricalpriors_500_iter<-rmfit
#still the same acceptable level of efficiency



load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_informativepriors_ad08_run12_model_distinct_runs_itercount500_wup450_MCMC.RData")
rmfit_samples(rmfit)
efficiency_score(rmfit)
rmfit_empiricalpriors_500_iter_lowerdelta<-rmfit
#big hit to efficiency here. 
#The intention in this version was to cut the delta back to 0.8 and see if that worked.
#It doesn't.

# load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_8_3level_empiricalpriors_noncentered_10subs_informativepriors_allsubjs_run12_model_distinct_runs_itercount500_wup450_MCMC.RData")
# 
# rmfit_samples(rmfit)
# efficiency_score(rmfit)
#further substantial hit to efficiency.



load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_informativepriors_1000iter_run12_model_distinct_runs_itercount1000_wup500_MCMC.RData")
rmfit_samples(rmfit)
efficiency_score(rmfit)
rmfit_1000iter<-rmfit
#it's strange because this has an even poorer level of efficiency, in spite of theoretically being a complete advance on a model run before (same but with more iterations). Did we change something?
t(rbind(summary(rmfit_1000iter)$summary[,"n_eff"],summary(rmfit_empiricalpriors_500_iter)$summary[,"n_eff"]))
all(rmfit_1000iter@model_pars==rmfit_empiricalpriors_500_iter@model_pars)
#params are the same
#we didn't change how we specified initial values....
cbind(rmfit_empiricalpriors_500_iter@stan_args[[1]],
      rmfit_1000iter@stan_args[[1]])
cbind(rmfit_empiricalpriors_500_iter@stan_args[[1]]$control,
      rmfit_1000iter@stan_args[[1]]$control)
#these seem the same!!!
#stan arguemnts are the same.

#did the model itself change?
rmfit_empiricalpriors_500_iter@stanmodel@model_code[[1]]==rmfit_1000iter@stanmodel@model_code[[1]]

#no! So why did it work once and not the other time?
efficiency_score(rmfit_1000iter)
efficiency_score(rmfit_empiricalpriors_500_iter)
#priors???
#how do we access those?
all(rmfit_1000iter@inits[[1]]$subj_mu==rmfit_empiricalpriors_500_iter@inits[[1]]$subj_mu)
all(rmfit_1000iter@inits[[1]]$alpha==rmfit_empiricalpriors_500_iter@inits[[1]]$alpha)
#The initial values are the same. We can't find out the priors passed to htem odel though can we?

draw_sampling_progress<-function(xfit,parameter_name){
  rm_arr<-as.array(xfit)
  rm_ex_subj_mu_alpha<-as.data.table(rm_arr[,,parameter_name]) %>% tidyr::gather(key = "Chain","Value") %>% data.table
  rm_ex_subj_mu_alpha[,iter:=(1:.N),by=Chain]
  ggplot(rm_ex_subj_mu_alpha,aes(x=iter,y=Value,color=Chain,group=Chain))+geom_line()
}
draw_sampling_progress(rmfit_1000iter,"subj_mu[1]")
draw_sampling_progress(rmfit_empiricalpriors_500_iter,"subj_mu[1]")
#take a look at what was actually produced.
draw_sampling_progress(rmfit_empiricalpriors_500_iter,"subj_sigma[1]")
draw_sampling_progress(rmfit_1000iter,"subj_sigma[1]")

draw_sampling_progress(rmfit_empiricalpriors_500_iter,"subj_mu[3]")
draw_sampling_progress(rmfit_1000iter,"subj_mu[3]")



load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_informativepriors_allsubjs_run12_model_distinct_runs_itercount500_wup450_MCMC.RData")
rmfit

#so the last good version appears to be...
#lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_informativepriors_run12_model_distinct_runs_itercount500_wup450_MCMC.RData


####### ANALYSIS OF stanlba/hierarchical_noncentered_empiricalpriors_repeat.R
load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_informativepriors_repeat_run12_model_distinct_runs_itercount500_wup450_MCMC.RData")

efficiency_score(rmfit)
efficiency_score_group_params(rmfit)
draw_sampling_progress(rmfit,"subj_mu[1]")
#This works, with just three chains.

load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_weakpriors_repeat_run12_model_distinct_runs_itercount500_wup450_MCMC.RData")
efficiency_score(rmfit)
efficiency_score_group_params(rmfit) #also not bad
draw_sampling_progress(rmfit,"subj_mu[1]")
