setwd("/expdata/bensmith/joint-modeling/code/msm/reversallearning")

rm(list = ls())
########################################## load the functions you will use
source("../joint_msm_combined/bjs_misc_utils.R")
version="h_m5l" 
version.base<-"h_m5l"
folderappend="_15subs"

source('de_mcmc/functions.R')
source('de_mcmc/main_m1_setup.R')
source('de_mcmc/functions_joint_v2.R')
source(paste0('de_mcmc/functions_',version,'.R'))

########################################## generate data
source(paste0('de_mcmc/main_',version,'_setup.R'))

########################################## initialize
#h_m5l: Applied empirically-determined priors and initial values.
#       Brandon has done this before (initial values, priors less so)
#       but this time I'm using rstan-determined empirical data rather than an optimizer.
#h_m5k: removed the inverse logit command. This was a fatal bug, I think. Let's see how this does without it!
#h_m5j: iterations back to 10,000; updated chain migration function to truly randomize swapped chains.
#h_m5i: increased iterations to 20,000
#h_m5g:
# - With level 2 migration sorted out, this is looking a lot better, but we're still getting chains that seem stuck/persistent.
#   If we can get the gamma numerator up from 0.5 to perhaps 1 or 1.5 (It has been set to 2.38) then perhaps we can it moving more.
#   Actually, my tests showed that a *lower* gamma of just 0.3 is better!
#   so we'll go with that. It still doesn't seem perfect .
#h_m5f: d still couldn't solve the problem. Here, I experiment with several factors:
# - migration - got migration level 2 working properly.
# - chain distribution - haven't changed this.
# - starting values - may have changed this???
# - no burnin; I need to find out what's going on right from the start!
# to try to find a value that works.
#h_m5d contains some misc fixes, notably, ending chain migration at the second level.
#h_m5c omits the optimization step.
#h_m5b sets a gamma numerator of 1 instead of 2.38.
#This allows for a more delicate differential evolution algorithm and hopefully a better estimate.
#with an extra level, 2.38 seems to be too much.
#I'm trying to work out how to get this 3-level model working.
#h_m5 includes a three-level model, with all four runs.

#Safe Meth subjects are excluded.

#the link parameters are now the third level of analysis
#the second level is simply analyzed across the runs for a single subject
#we need to:
# - add priors for the third level
# - specify the second-level priors as functions of the third level
# - re-write the estimator to use a third level. this is going to be the hardest.

#nomenclature
#alpha, thresh, tau are our modelled parameters
#alpha_s_mu, alpha_s_sigma....are the subject-level hyper-parameters dealing with each subject's distribution of run parameters
#alpha_s_mu_g_mu, alpha_s_mu_g_sigma are the group-level hyper parameters describing the distribution of subject means
#alpha_s_sigma_g* are the group-level hyper-parameters describing the distribution of subject-level sigmas.
#this will only be necessary if we actually decide to sample subject sigmas from a prior distribution
#instead of just assuming equal variance for each subject across their runs.

#I want to re-write the nomenclature so we're not talking about hyperparameters
#I hope this isn't premature, but it gets vague when we have hypers and 'hyper-hypers'
#as we do in a three-level model.


########################################## prior values for the hypers
prior.big=NULL
#prior.big$mu=rep(0,n.link.pars)
prior.big$m=1/10
#prior.big$phi=diag(n.link.pars)
prior.big$n0=length(prior.big$mu) + 2

#level 3 priors. 
prior.l3=NULL
#I don't know what good alpha and beta values are here.
prior.l3$alpha=list("mu"=lba_group_sstats$alpha_pr_mean,"sigma"=lba_group_sstats$alpha_pr_var,alpha=4,beta=10)
                    #"alpha"=lba_group_sstats$alpha_pr_var^2/6+2,
                    #"beta"=lba_group_sstats$alpha_pr_var^3/6+2*lba_group_sstats$alpha_pr_var)
prior.l3$thresh=list("mu"=lba_group_sstats$k_pr_mean,"sigma"=lba_group_sstats$k_pr_var,alpha=4,beta=10)
                     #"alpha"=lba_group_sstats$k_pr_var^2/6+2,
                     #"beta"=lba_group_sstats$k_pr_var^3/6+2*lba_group_sstats$k_pr_var)
rt_s_mean<-unlist(lapply(data,FUN=function(s){mean(unlist(lapply(s$runs,function(r){min(r$rt,na.rm=TRUE)})))}))
rt_s_mean_all_mean<-mean(rt_s_mean)
prior.l3$tau=list("mu"=lba_group_sstats$tau_pr_mean,
               "sigma"=lba_group_sstats$tau_pr_var,alpha=4,beta=10)
#               "alpha"=lba_group_sstats$tau_pr_var^2/6+2,
#               "beta"=lba_group_sstats$tau_pr_var^3/6+2*lba_group_sstats$tau_pr_var)

#level 2 priors or initial values????
#for level 2, we have level 3 from which to draw mu, so we don't include that,
#but we still need to estimate the other values.
#We start with priors that are the same for each subject
prior.l2=NULL
#alright.
#a close approximation for this gamma is to
#set alpha and beta to value such that
#2/(alpha+beta)==SD
#beta/alpha==mean
#remember we're trying to set the *distribution* for sigma, so...
#central prior for sigma is around
#which one is *uncertainty* in mu and which one is distribution in subject values of mu????
#sigma is the magnitude of distribution in subject values of mu
#alpha and beta are the priors for estimating sigma
#and...these are really poorly named, but we use alpha_pr_var to set the sigma prior, and 
#alpha_sd_prior to set the alpha and beta values
prior.l2$alpha=list("mu"=lba_group_sstats$alpha_pr_mean,
                    "sigma"=lba_group_sstats$alpha_pr_var,
                    alpha=4,beta=10,
                    #alpha=lba_group_sstats$alpha_sd_prior^2/6+2,
                    beta=lba_group_sstats$alpha_sd_prior^3/6+2*lba_group_sstats$alpha_sd_prior,
                    sigma_gamma=lba_group_sstats$alpha_run_sigma_gamma)

#so if we're targeting a mean of lba_group_sstats$alpha_sd_prior and variance of... 2 that would be


# The mean (for α > 2) is:
#   E(X) = β / (α – 1).
# 
# The variance is:
#   β2 / ((α – 1)2*(α – 2)).


prior.l2$thresh=list("mu"= lba_group_sstats$k_pr_mean,"sigma"=lba_group_sstats$k_pr_var,
                     alpha=4,beta=10,
                     #alpha=lba_group_sstats$k_sd_prior^2/6+2,
                     #beta=lba_group_sstats$k_sd_prior^3/6+2*lba_group_sstats$k_sd_prior,
                     sigma_gamma=lba_group_sstats$k_run_sigma_gamma)
prior.l2$tau=list("mu"= lba_group_sstats$tau_pr_mean,"sigma"=lba_group_sstats$tau_pr_var,
                  alpha=4,beta=10,
                     #alpha=lba_group_sstats$tau_sd_prior^2/6+2,
                     #beta=lba_group_sstats$tau_sd_prior^3/6+2*lba_group_sstats$tau_sd_prior,
                  sigma_gamma=lba_group_sstats$tau_run_sigma_gamma)
# prior.l2$tau=list("mu"=log(.6*(rt_s_mean_all_mean)),
#   "sigma"=sqrt(abs(log(.6*(rt_s_mean_all_mean)))),
#                   alpha=4,beta=10,sigma_gamma=10)

#useful for updating sigma vectors. 
#these are the set parameters for the priors
#now we're doing a three-level model, we'll want to estimate some of this with another level...
#I'm not sure yet on the best solution but for now, let's set up two sets of hyper priors,
#one for the subject level and one for the group level :-)

########################################## run it
cores=min(max(1,detectCores()-1),12)
print(paste0("Starting sfInit to run sfInit with ",cores," cores..."))
sfInit(parallel=TRUE, cpus=cores, type="SOCK")
print("...snowfall initialized; running clustercd setup...")
sfClusterSetupRNG()
print("...cluster setup run.")

source(paste("de_mcmc/de_",version,"_functions.R",sep=""))
###############################################################
source(paste("de_mcmc/de_",version,"_start.R",sep=""))

#sfStop();save.image(file=paste0(mainDataDir,"de_h_m5k_testing.RData"))
#sfStop();load(file="/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/de_h_m5k_testing.RData")

#debugSource(paste("/expdata/bensmith/joint-modeling/code/msm/reversallearning/de_mcmc/functions_h_m5j.R",sep=""))
source(paste("de_mcmc/de_",version,"_run.R",sep=""))


#debugSource(paste("de_mcmc/de_",version,".R",sep=""))

sfStop()

save.image(paste(save.dir,save.name,".RData",sep=""))

########################################## plot
setwd(mainDir)
