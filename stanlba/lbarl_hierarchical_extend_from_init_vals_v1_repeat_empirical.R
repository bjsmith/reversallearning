
source("stanlba/lbarl_hierarchical_extend_from_init_vals_v2.R")

print("------------------------")
print("Running with EMPIRICAL initial values, 10subs")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_empiricaldistribution_init_v1rep",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="empirical_distribution",warmup_iter=450,iter=500)

#lapply(init_method,function(li){li[[1]][1]})
print("------------------------")
print("Running with BOOTSTRAPPED initial values, 10subs")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_bootstrapdistribution_init_v1rep",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="bootstrapped",warmup_iter=450,iter=500)


load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_auto_init_12c_run12_model_distinct_runs_itercount500_wup450_MCMC.RData")
auto_init_vals_fit<-rmfit
mean(summary(auto_init_vals_fit)$summary[,"n_eff"])
mean(summary(auto_init_vals_fit)$summary[,"Rhat"])

load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_bootstrapped_init_12c_run12_model_distinct_runs_itercount500_wup450_MCMC.RData")
bootstrapped_init_vals_fit<-rmfit


mean(summary(bootstrapped_init_vals_fit)$summary[,"n_eff"])
mean(summary(bootstrapped_init_vals_fit)$summary[,"Rhat"])


#complete failure, so what's different?
load("../../../data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_10subs_empiricaldistribution_init_v1repprior_blowup_1_run12_model_distinct_runs_itercount500_wup450_MCMC.RData")
empirical_priors_fit<-rmfit
library(rstan)

empirical_priors_fit
mean(summary(empirical_priors_fit)$summary[,"n_eff"])
mean(summary(empirical_priors_fit)$summary[,"Rhat"])


lapply(1:12,function(i){#go through params, not through chains.
  bootstrapped_init_vals_fit@inits[[1]][[i]]-empirical_priors_fit@inits[[1]][[i]]
})
bootstrapped_init_vals_fit@inits[[1]][[1]]
empirical_priors_fit@inits[[1]][[1]]# but that's output, that wont' tellus much about what's going on.

lapply(empirical_priors_fit@inits,function(li){li[[1]][1]})
  
lapply(bootstrapped_init_vals_fit@inits,function(li){li[[1]][1]})
#this has MUCH lower priors for alpha. Is it because one is bootstrapped from the local dataset and the other is bootstrapped from all subjects?

#let's see what each method would get us.
#lba_group_sstats<-generate_lbarl_group_summary_stats(results.summary.dt[!(FullRunId %in% improperly.estimated.runs$FullRunId)])
data_to_use=multisubj_multirun_moresubs
run_list_by_run<-data_to_use[,.(StanRunID=unique(UniqueRunID)),by=.(ConsecSubId,subid,Motivation,WithinSubjRunId)] %>% .[order(StanRunID)]
data_to_pass<-get_informative_priors(data_to_use,lba_group_sstats,prior_blowup=1,run_list_by_run)
source("bootstrap_smart_init_vals.R")
smart_init_vals<-bootstrap_smart_init_vals(n_samples = n_chains,
                                           subid_set = sort(unique(data_to_use$subid)),
                                           bootstrap_seed = c(1973449269))

bootstrapped_init_vals<-lapply(smart_init_vals,get_bootstrapped_init_vals,1761614456,data_to_pass)
summary(unlist(lapply(bootstrapped_init_vals,function(li){li[[1]][1]})))
summary(unlist(lapply(bootstrapped_init_vals_fit@inits,function(li){li[[1]][1]})))
#so that's the process that produces these quite low values. 
#How did we get those original higher values? probably from the dataset as a whole???

smart_init_vals<-bootstrap_smart_init_vals(n_samples = n_chains,
                                           subid_set = sort(unique(multisubj_multirun_allsubs$subid)),
                                           bootstrap_seed = c(1973449269))

whole_dataset_bootstrap_vals<-lapply(smart_init_vals,get_bootstrapped_init_vals,1761614456,data_to_pass)
whole_dataset_bootstrap_vals
summary(unlist(lapply(whole_dataset_bootstrap_vals,function(li){li[[1]][1]}))) #no. even this whole-dataset sample is lower. how did it happen?

bootstrapped_init_vals_fit@date
bootstrapped_init_vals_fit@model_name
bootstrapped_init_vals_fit@model_pars
bootstrapped_init_vals_fit@par_dims
names(bootstrapped_init_vals_fit@sim)
bootstrapped_init_vals_fit@inits[[1]]

#could just be the seed!?!?!
#nah we have been using this a lot.
for (argi in 1:length(empirical_priors_fit@stan_args[[1]])){
  tryCatch({
  print(empirical_priors_fit@stan_args[[1]][[argi]]==bootstrapped_init_vals_fit@stan_args[[1]][[argi]])},
  error=function(e){print(e)}
  
  )
}
mean(summary(rmfit)$summary[,"n_eff"])
mean(summary(rmfit)$summary[,"Rhat"])
