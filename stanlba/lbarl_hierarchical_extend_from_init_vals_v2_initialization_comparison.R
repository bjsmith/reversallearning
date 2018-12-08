#This file compares initialization for 10 subjects for a larger number of methods, all using informative priors:
#Distributions: Auto, Random, Bootstrapped, Empirical
#normal vs cauchy run_sigma_gamma prior
#that's 6 altogether; did we want anything else?

source("stanlba/lbarl_hierarchical_extend_from_init_vals_v2.R")

#does the data match the priors?

#let's check the second subject,

#are those the same runIDs accounted for in the summary statistics, and how would we know???
source("bootstrap_smart_init_vals.R")

source("init_vals_generate.R")

for (dataset_name in c("10subs","G1")){#list(multisubj_multirun_moresubs,multisubj_multirun_Group1_ex343_362)){
  for (model in c("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered",
                  "lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_rsg_n")){
    if(dataset_name=="10subs")dataset<-multisubj_multirun_moresubs
    if(dataset_name=="G1")dataset<-multisubj_multirun_Group1_ex343_362
    for (init_val_method in c("auto","randomized","bootstrapped","empirical_distribution")){
      print("------------------------")
      print(paste0("Running ",toupper(model) ," model with ", toupper(init_val_method), " initial values, ",dataset_name,", initialization only"))
      fit_with_manual_init_vals <- run_model(model,paste0(dataset_name,"_",init_val_method,"_init_v2"),
                                             filedir="incremental/",informative_priors = TRUE,data_to_use = dataset,
                                             init_vals=init_val_method,warmup_iter=5,iter=10)
    }
  }
}

library(rstan)
#now summarize.
source("nate_files/fitGroupsV3Onegroup.R")
for (dataset_name in c("10subs")){#,"G1")){#list(multisubj_multirun_moresubs,multisubj_multirun_Group1_ex343_362)){
  for (model in c("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered",
                  "lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_rsg_n")){
    # if(dataset_name=="10subs")dataset<-multisubj_multirun_moresubs
    # if(dataset_name=="G1")dataset<-multisubj_multirun_Group1_ex343_362
    for (init_val_method in c("auto","randomized","bootstrapped","empirical_distribution")){
      file_save_name<-get_fit_desc(use_model = model,descr = paste0(dataset_name,"_",init_val_method,"_init_v2","prior_blowup_",1),run=c(1,2),
                                   model_rp_separately=TRUE,model_runs_separately=TRUE,
                                   use_pain=FALSE,fastDebug=FALSE,fileSuffix="",
                                   estimation_method=ESTIMATION_METHOD.MCMC,
                                   bseed=bseed,warmup_iter = 5,
                                   iterations=10)
      load(file_save_name)
      print(paste0("Rhat for ",sub("/expdata/bensmith/joint-modeling/data/msm/reversallearning/Fits/lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","",file_save_name), "is :",as.character(sum(summary(rmfit)$summary[,"Rhat"]))))
      
    }
  }
}
