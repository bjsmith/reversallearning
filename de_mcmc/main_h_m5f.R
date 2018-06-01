#setwd("/expdata/bensmith/joint-modeling/code/msm/reversallearning")
#rm(list = ls())
########################################## load the functions you will use
source("../joint_msm_combined/bjs_misc_utils.R")
version="h_m5g"
version.base<-"h_m5f"
folderappend=""

source('de_mcmc/functions.R')
source('de_mcmc/main_m1_setup.R')
source('de_mcmc/functions_joint_v2.R')
source(paste0('de_mcmc/functions_',version,'.R'))
########################################## generate data
source(paste0('de_mcmc/main_',version,'_setup.R'))

########################################## initialize
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
prior.l3$alpha=list("mu"=-3,"sigma"=1,alpha=4,beta=10)
prior.l3$thresh=list("mu"=log(2),"sigma"=sqrt(log(2)),alpha=4,beta=10)
rt_s_mean<-unlist(lapply(data,FUN=function(s){mean(unlist(lapply(s$runs,function(r){min(r$rt,na.rm=TRUE)})))}))
rt_s_mean_all_mean<-mean(rt_s_mean)
prior.l3$tau=list("mu"=log(.6*(rt_s_mean_all_mean)),
               "sigma"=sqrt(mean(abs(log(.6*(rt_s_mean_all_mean))))),
               alpha=4,beta=10)

#level 2 priors or initial values????
#for level 2, we have level 3 from which to draw mu, so we don't include that,
#but we still need to estimate the other values.
#We start with priors that are the same for each subject
prior.l2=NULL
prior.l2$alpha=list("mu"=-3,"sigma"=1,alpha=4,beta=10,sigma_sigma=10)
prior.l2$thresh=list("mu"=log(2),"sigma"=sqrt(log(2)),alpha=4,beta=10,sigma_sigma=10)
prior.l2$tau=list("mu"=log(.6*(rt_s_mean_all_mean)),
  "sigma"=sqrt(abs(log(.6*(rt_s_mean_all_mean)))),
                  alpha=4,beta=10,sigma_sigma=10)

#useful for updating sigma vectors. 
#these are the set parameters for the priors
#now we're doing a three-level model, we'll want to estimate some of this with another level...
#I'm not sure yet on the best solution but for now, let's set up two sets of hyper priors,
#one for the subject level and one for the group level :-)

########################################## run it
cores=max(1,detectCores()-1)
print(paste0("Starting sfInit to run sfInit with ",cores," cores..."))
sfInit(parallel=TRUE, cpus=cores, type="SOCK")
print("...snowfall initialized; running clustercd setup...")
sfClusterSetupRNG()
print("...cluster setup run.")

source(paste("de_mcmc/de_",version,"_functions.R",sep=""))
source(paste("de_mcmc/de_",version,"_start.R",sep=""))

#sfStop();save.image(file=paste0(mainDataDir,"de_h_m5f_testing.RData"))
#sfStop();load(file="/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/de_h_m5f_testing.RData")
source(paste("de_mcmc/de_",version,"_run.R",sep=""))


#debugSource(paste("de_mcmc/de_",version,".R",sep=""))

sfStop()

save.image(paste(save.dir,save.name,".RData",sep=""))

########################################## plot
setwd(mainDir)
