
#### POSITIVE LOG PROBABILITIES
hist(rsdt[param_name=="lp__",mean],breaks = 50)
#so let's take a look at the 106
lp_above_zero<-rsdt[param_name=="lp__" & mean>0]
#142 of the models ended up with an LP above zero. this is alarming.
get_srm_fit<-function(s,r,m,modelnameversion){
  load(paste0(
    '/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180709_1/run_package_',
    s,'_',r,'_',m,'_',modelnameversion,'.RData'))
  return(run_package)
}

res<-get_srm_fit(106,r=1,m="punishment",'lba_rl_single_exp_joint_v11f')
summary(res$fit)$summary["lp__",]
#here it is, definitely a log probability above zero. crazy! What' the distribution
library(rstan)
res$extractedfit<-as.matrix(res$fit)
hist(res$extractedfit[,"lp__"])
#This is an absurd log probability. Why would we be getting this? What do the summary stats look like?
summary(res$fit)$summary[c("alpha","k","tau",
                           "alpha_pr","k_pr","tau_pr",
                           "lp__"),]

summary(srm.fit)$summary[c("alpha","k","tau",
                           "alpha_pr","k_pr","tau_pr",
                           "lp__"),]
#not exactly the same, but we got the same result more or less.
#OK. we're going to need to run through.

library(rstan)
source("stanlba/lba_rl_joint_setup.R")
require(R.utils)
options(mc.cores = 6)
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v7_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v10_functions.R")

#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.
lba_rl_version<-"joint_20180709_1"
model.subversion<-"f"
single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
# output_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")
# dir.create(single_run_dir, showWarnings = FALSE)
# dir.create(output_dir, showWarnings = FALSE)
#file_folder<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/lba_rl_single_estimates.RData"
#load(file=file_folder)
Rhat_corevals_limit=1.05 # I don't care about this at the moment. I just want to take a look at what we'regetting!
Rhat_general_limit=1.1


results.list<-list()

model.name<-"lba_rl_single_exp_joint_v11"
lba_rl_single_joint<-stan_model(paste0('stanlba/stanfiles/incremental/',model.name,'.stan'))
regions<-get_dmn_regions()
motivations<-unique(rawdata[subid==sid & runid==r,Motivation])
sid=106;r=1;m="punishment"
srm.data<-select_rawdata_cols_for_run(rawdata,sid,r,m)
baseseed<-53171370#129221139#sample.int(.Machine$integer.max, 1)
roundseed<-sid+r+which(motivations==m)+baseseed
model_attempts=0
warmup=400
iterations=500
n_chains=6
srm.fit <- sampling(lba_rl_single_joint, 
               data = create_standatalist(srm.data = srm.data,theta_count = 2, delta_names = regions),
               warmup = warmup, 
               iter = iterations,
               init= get_starting_values(n_chains,thetaDelta_count=length(regions)+2),
               chains = n_chains,
               seed = roundseed,
               control = list(max_treedepth = 12,adapt_delta=0.85))
               
summary(srm.fit)$summary[c("alpha","k","tau",
                                  "alpha_pr","k_pr","tau_pr",
                                  "lp__"),]



model.name.revert<-"lba_rl_single_exp_joint_v11_lpdebug_revert"
lba_rl_single_joint.revert<-stan_model(paste0('stanlba/stanfiles/incremental/',model.name.revert,'.stan'))

srm.fit.revert <- sampling(lba_rl_single_joint.revert, 
                    data = create_standatalist(srm.data = srm.data,theta_count = 2, delta_names = regions),
                    warmup = warmup, 
                    iter = iterations,
                    init= get_starting_values(n_chains,thetaDelta_count=length(regions)+2),
                    chains = n_chains,
                    seed = roundseed,
                    control = list(max_treedepth = 12,adapt_delta=0.85))

summary(srm.fit.revert)$summary[c("alpha","k","tau",
                           "alpha_pr","k_pr","tau_pr",
                           "lp__"),]


model.name.revert<-"lba_rl_single_exp_joint_v11_lpdebug_revert_2"
lba_rl_single_joint.revert2<-stan_model(paste0('stanlba/stanfiles/incremental/',model.name.revert,'.stan'))

srm.fit.revert2 <- sampling(lba_rl_single_joint.revert2, 
                           data = create_standatalist(srm.data = srm.data,theta_count = 2, delta_names = regions),
                           warmup = warmup, 
                           iter = iterations,
                           init= get_starting_values(n_chains,thetaDelta_count=length(regions)+2),
                           chains = n_chains,
                           seed = roundseed,
                           control = list(max_treedepth = 12,adapt_delta=0.85))

summary(srm.fit.revert2)$summary[c("alpha","k","tau",
                                  "alpha_pr","k_pr","tau_pr",
                                  "lp__"),]

library(loo)

srm.logl.revert2 <- extract_log_lik(srm.fit.revert2, merge_chains = FALSE)
anova(srm.fit.revert2,srm.fit)


#http://discourse.mc-stan.org/t/is-there-any-existing-function-in-rstan-to-compute-dic-aic-or-bic/4182

#https://github.com/stan-dev/loo

