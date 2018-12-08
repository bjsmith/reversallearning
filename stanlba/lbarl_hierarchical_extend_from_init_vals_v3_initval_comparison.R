# (1) Trying to replicate the earlier findings that randomized, bootstrapped methods give us reasonable estimates for 10 subjects.
# (2) Showing this also works for empirical distribution sampling, and also looking at whether empirical distribution actually performs better! 

source("stanlba/lbarl_hierarchical_extend_from_init_vals_v3.R")

#does the data match the priors?

#let's check the second subject,

#are those the same runIDs accounted for in the summary statistics, and how would we know???

source("bootstrap_smart_init_vals.R")

source("init_vals_generate.R")

print("------------------------")
print("Running the informative priors model WITH EMPIRICAL DISTRIBUTION INITIAL VALUES SPECIFIED.")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered",
                                       "informativepriors_10subjs_initvals_empirical",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="empirical_distribution")

print("------------------------")
print("Running the informative priors model WITH RANDOM INITIAL VALUES SPECIFIED.")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered",
                                       "informativepriors_10subjs_initvals_randomized",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="randomized")


print("------------------------")
print("Running the informative priors model WITH BOOTSTRAPPED INITIAL VALUES SPECIFIED.")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered",
                                       "informativepriors_10subjs_initvals_bootstrapped",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="bootstrapped")



print("------------------------")
print("Running the informative priors model WITHOUT INITIAL VALUES SPECIFIED.")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered",
                                       "informativepriors_10subjs_stanauto",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="auto")
