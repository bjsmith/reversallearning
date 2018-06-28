#1. Go to analyze a full group (say, Group 1) using both randomized and bootstrapped [EXCLUDED for now; it takes forever!]
#2. Go to analyze 10 subjects at w=500,i=1000 using both randomized and bootstrapped
#3. Go to analyze 10 subjects using adapt_delta=0.8. Our efficiency is still not what I’d like, and I’d like to see how I can improve on it.

#these are run using a slightly modified dataset, with different criteria for:
#- removing runs due to button-mashing
#- now removing trials due to button-pressing prior to possible reaction time.

source("stanlba/lbarl_hierarchical_extend_from_init_vals_v2.R")
# 
# print("------------------------")
# print("Running with BOOTSTRAPPED initial values, 10subs, full-group 1")
# fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","G1_bootstrapped_init_v2",
#                                        filedir="incremental/",informative_priors = TRUE,
#                                        init_vals="bootstrapped",data_to_use=multisubj_multirun_Group1)



print("------------------------")
print("Running with BOOTSTRAPPED initial values, 10subs, full-length")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_bootstrapped_init_v2",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="bootstrapped",warmup_iter=5,iter=10)

print("------------------------")
print("Running with BOOTSTRAPPED initial values, 10subs, full-length")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_bootstrapped_init_v2",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="bootstrapped",warmup_iter=500,iter=1000)



print("------------------------")
print("Running with BOOTSTRAPPED initial values, 10subs, full-group 1")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_bootstrapped_init_v2_ad08",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="bootstrapped",adapt_delta=0.8)
