#1. Go to analyze a full group (say, Group 1) using both randomized and bootstrapped [EXCLUDED for now; it takes forever!]
#2. Go to analyze 10 subjects at w=500,i=1000 using both randomized and bootstrapped
#3. Go to analyze 10 subjects using adapt_delta=0.8. Our efficiency is still not what I’d like, and I’d like to see how I can improve on it.

#these are run using a slightly modified dataset, with different criteria for:
#- removing runs due to button-mashing
#- now removing trials due to button-pressing prior to possible reaction time.

source("stanlba/lbarl_hierarchical_extend_from_init_vals_v2.R")

#does the data match the priors?

#let's check the second subject,

#are those the same runIDs accounted for in the summary statistics, and how would we know???
source("bootstrap_smart_init_vals.R")

source("init_vals_generate.R")



multisubj_multirun_Group1<-rawdata[SubjectGroup==1
                                   & reaction_time>0,
                                   .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                     ConsecSubId=as.integer(as.factor(as.character(subid))),
                                     WithinSubjRunId=runid,Motivation=Motivation,
                                     UniqueRunID=as.numeric(interaction(runid,Motivation,subid,drop = TRUE)))]

#try excluding 343 and 362
#I couldn't calculate a model for the following:
# 109.1.punishment
# 144.1.punishment
# 214.2.punishment
# 220.2.punishment
# 230.2.reward
# 307.1.reward
# 326.1.reward
# 329.2.punishment
# 343.2.punishment
# 350.1.reward
# 362.2.punishment
multisubj_multirun_Group1_ex343_362<-rawdata[SubjectGroup==1 & !(subid %in% c(343,362))
   & reaction_time>0,
   .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
     ConsecSubId=as.integer(as.factor(as.character(subid))),
     WithinSubjRunId=runid,Motivation=Motivation,
     UniqueRunID=as.numeric(interaction(runid,Motivation,subid,drop = TRUE)))]

fit_with_manual_init_vals <- run_model(
  "lba_rl_multi_subj_7_3level_empiricalpriors_noncentered",
  "G1_ex343_362_empiricaldistribution_init_v2",
  filedir="incremental/",informative_priors = TRUE,
  init_vals="empirical_distribution",data_to_use=multisubj_multirun_Group1_ex343_362,warmup_iter=5,iter=10)

data_to_use=multisubj_multirun_Group1_ex343_362
run_list_by_run<-data_to_use[,.(StanRunID=unique(UniqueRunID)),by=.(ConsecSubId,subid,Motivation,WithinSubjRunId)] %>% .[order(StanRunID)]
init_method=get_empirical_distribution_allchains(n_chains = n_chains,run_list_by_run)


print("------------------------")
print("Running with BOOTSTRAPPED initial values, 10subs, full-group 1")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","G1_ex343_362_empiricaldistribution_init_v2",
  filedir="incremental/",informative_priors = TRUE,
  init_vals="empirical_distribution",data_to_use=multisubj_multirun_Group1_ex343_362,warmup_iter=5,iter=10)

library(rstan)
#this didn't work. 
hist(unlist(init_method[[1]]$run_mu_var))
hist(unlist(init_method[[1]]$run_sigma))
hist(unlist(init_method[[1]]$alpha_pr_var),breaks = 50)
hist(unlist(init_method[[1]]$k_pr_var))
hist(unlist(init_method[[1]]$tau_pr_var))
hist(unlist(init_method[[2]]$alpha_pr_var))
hist(unlist(init_method[[2]]$k_pr_var))
hist(unlist(init_method[[2]]$tau_pr_var))
hist(unlist(init_method[[3]]$alpha_pr_var))
hist(unlist(init_method[[3]]$k_pr_var),breaks=50)
hist(unlist(init_method[[3]]$tau_pr_var),breaks=50)

init_method[[4]]$subj_mu
init_method[[4]]$subj_sigma
init_method[[4]]$run_sigma_gamma
hist(unlist(init_method[[4]]$alpha_pr_var))
hist(unlist(init_method[[4]]$k_pr_var))
hist(unlist(init_method[[4]]$tau_pr_var))
# 
# get_informative_priors(data_to_use,lba_group_sstats,100)
# get_informative_priors(data_to_use,lba_group_sstats,1)

#these distributions look really not normally distributed; they look 
sort(unlist(init_method[[3]]$alpha_pr_var))
sort(unlist(init_method[[3]]$k_pr_var))
sum(dnorm(sort(unlist(init_method[[3]]$alpha_pr_var)),0,1,log=TRUE))
sum(dnorm(sort(unlist(init_method[[3]]$k_pr_var)),0,1,log=TRUE))
#sum(dnorm(unlist(init_method[[3]]$tau_pr_var),0,1,log=TRUE))

sampled_priors<-data.table(sample_prior_from_distribution(run_list_by_run,n_chains))

hist(sampled_priors$alpha_pr)
length(sampled_priors$alpha_pr)
subjs_to_use<-unique(data_to_use$subid)
hist(sampled_priors$alpha_pr)
names(sampled_priors)
by_subj<-sampled_priors[,.(alpha_pr_mean=mean(alpha_pr),
                           k_pr_mean=mean(k_pr),
                           tau_pr_mean=mean(tau_pr),
                           alpha_pr_sd=sd(alpha_pr),
                           k_pr_sd=sd(k_pr),
                           tau_pr_sd=sd(tau_pr)
                           ),.(subid,chain)]
sampled_priors.waggs<-merge(sampled_priors,by_subj,by=c("subid","chain"))

hist((sampled_priors.waggs$alpha_pr-sampled_priors.waggs$alpha_pr_mean)/sampled_priors.waggs$alpha_pr_sd)
hist((sampled_priors.waggs$k_pr-sampled_priors.waggs$k_pr_mean)/sampled_priors.waggs$k_pr_sd)
hist((sampled_priors.waggs$tau_pr-sampled_priors.waggs$tau_pr_mean)/sampled_priors.waggs$tau_pr_sd)

ggplot(sampled_priors.waggs,aes((alpha_pr-alpha_pr_mean)/alpha_pr_sd))+geom_histogram()+facet_wrap(~chain,nrow = 4)
ggplot(sampled_priors.waggs,aes((k_pr-k_pr_mean)/k_pr_sd))+geom_histogram()+facet_wrap(~chain,nrow = 4)
ggplot(sampled_priors.waggs,aes((alpha_pr)))+geom_histogram()+facet_wrap(~chain,nrow = 4)
ggplot(sampled_priors.waggs,aes((alpha_pr-alpha_pr_mean)))+geom_histogram()+facet_wrap(~chain,nrow = 4)
hist(unlist(init_method[[1]]$alpha_pr_var),breaks = 50)
hist(unlist(init_method[[10]]$alpha_pr_var),breaks = 50)


sum(dnorm((sampled_priors.waggs$alpha_pr-sampled_priors.waggs$alpha_pr_mean)/sampled_priors.waggs$alpha_pr_sd,0,1,log=TRUE))
sum(dunif((sampled_priors.waggs$alpha_pr-sampled_priors.waggs$alpha_pr_mean)/sampled_priors.waggs$alpha_pr_sd,-1.5,1.5,log=TRUE))



fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_unif","G1_ex343_362_empiricaldistribution_init_v2",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="empirical_distribution",data_to_use=multisubj_multirun_Group1_ex343_362,warmup_iter=5,iter=10)

n_chains<-1
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_unif","G1_ex343_362_empiricaldistribution_init_v2",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="empirical_distribution",data_to_use=multisubj_multirun_Group1_ex343_362,warmup_iter=5,iter=10,prior_blowup = 5)


fit_with_manual_init_vals <- run_model(
  "lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_hacked",
  "G1_ex343_362_empiricaldistribution_init_v2",
  filedir="incremental/",informative_priors = TRUE,
  init_vals="empirical_distribution",data_to_use=multisubj_multirun_Group1_ex343_362,
  warmup_iter=5,iter=10)
#fit_with_manual_init_vals