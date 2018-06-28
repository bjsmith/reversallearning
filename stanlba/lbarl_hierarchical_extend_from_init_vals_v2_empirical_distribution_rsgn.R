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


fit_with_manual_init_vals <- run_model(
  "lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_rsg_n",
  "10subs_empiricaldistribution_init_v2",
  filedir="incremental/",informative_priors = TRUE,
  init_vals="empirical_distribution",warmup_iter=15,iter=20,prior_blowup=10)

print("------------------------")
print("Running with BOOTSTRAPPED initial values, 10subs, ad08")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_rsg_n","10subs_empiricaldistribution_init_v2_ad08_rsg_n",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="empirical_distribution",a_delta=0.8,warmup_iter=450,iter=500,prior_blowup=1)

print("------------------------")
print("Running with BOOTSTRAPPED initial values, 10subs, full-length")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_rsg_n","10subs_empiricaldistribution_init_v2_rsg_n",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="empirical_distribution",warmup_iter=500,iter=1000,prior_blowup=1)

