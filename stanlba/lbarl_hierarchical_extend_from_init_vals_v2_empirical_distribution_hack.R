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


data_to_use=multisubj_multirun_Group1_ex343_362
run_list_by_run<-data_to_use[,.(StanRunID=unique(UniqueRunID)),by=.(ConsecSubId,subid,Motivation,WithinSubjRunId)] %>% .[order(StanRunID)]
init_method=get_empirical_distribution_allchains(n_chains = n_chains,run_list_by_run)


sampled_priors<-data.table(sample_prior_from_distribution(run_list_by_run,n_chains))

fit_with_manual_init_vals <- run_model(
  "lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_hacked",
  "G1_ex343_362_empiricaldistribution_init_v2",
  filedir="incremental/",informative_priors = TRUE,
  init_vals="empirical_distribution",data_to_use=multisubj_multirun_Group1_ex343_362,
  warmup_iter=5,iter=10)
#fit_with_manual_init_vals