#setwd("/expdata/bensmith/joint-modeling/code/msm/reversallearning")
#/expdata/bensmith/joint-modeling/data
########################################## load the functions you will use
source("../joint_msm_combined/bjs_misc_utils.R")
version="h_m5b"#with enhanced diagnostic data-gathering
save.name=paste0("main_", version)
source('de_mcmc/functions.R')
source('de_mcmc/main_m1_setup.R')
source('de_mcmc/functions_joint_v2.R')
source(paste0('de_mcmc/functions_',version,'.R'))
########################################## generate data

source("de_mcmc/raw_data_all_runs.R")
safe_meth_subjs<-unlist(lapply(data,function(d){d$group=="SafeMeth"}))
subjs.without.group<-unlist(lapply(data,function(d){is.na(d$group)}))

data<-data[!safe_meth_subjs & !subjs.without.group]

#get a set of subjects who are in each of the three groups.
g1_subs<-which(unlist(lapply(data,function(d){d$group=="SafeNoMeth"})))
g2_subs<-which(unlist(lapply(data,function(d){d$group=="RiskyNoMeth"})))
g3_subs<-which(unlist(lapply(data,function(d){d$group=="RiskyMeth"})))
subs_to_inc<-c(g1_subs[1:5],g2_subs[1:5],g3_subs[1:5])
subs_to_inc#now what do we do with these subjects?
data<-data[subs_to_inc]

rm(rawdata,rawdata.dt) #save memory.

########################################## initialize
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


param.l1.names=c("alpha",#"beta",
                   "thresh","tau")
param.l1.ids<-as.list(1:3)
param.l1.N<-length(param.l1.names)
names(param.l1.ids)=param.l1.names
par.names=c(param.l1.names)

param.l2.names<-c(paste0(param.l1.names, "_s_mu"),paste0(param.l1.names, "_s_sigma"))
param.l2.ids<-as.list(1:length(param.l2.names))
param.l2.N<-length(param.l2.names)
names(param.l2.ids)<-param.l2.names
param.l2.distributions.N<-param.l2.N/2


#level three parameters.
param.l3.names<-c(paste0(param.l2.names[1:3], "_g_mu"),paste0(param.l2.names[1:3], "_g_sigma"))
#Let's NOT sample subject variance from a random distribution, for now. Instead,
#we'll just assume constant run-level variance across subjects. That will halve the number of third-level parameters we have to estimate.
param.l3.ids<-as.list(1:length(param.l3.names))
param.l3.N<-length(param.l3.names)
names(param.l3.ids)<-param.l3.names
param.l3.distributions.N<-param.l3.N/2
#we'll need these separately for each group...
#not sure how to handle that just yet.

phi.ids<-as.list(1:(param.l3.N+param.l2.N))
names(phi.ids)<-c(param.l3.names,param.l2.names)

#hpar.names=par.names.l2


n.chains=24
param.N=length(par.names) #count of raw parameters, not considering the number of subjects or the number of levels we process it on.

#n.hpars=length(hpar.names)#not sure what we do with this. Maybe best to avoid replacing it until we go through the code and see what we need to do with it.
#n.phi.mu=n.hpars/2

group_by_subject<-unlist(lapply(data,function(d){d$group})) #lists the group membership of each subject.
groups.l3.list<-unique(group_by_subject)
groups.l3.N<-length(groups.l3.list)#formerly n.l2.groups
groups.l3.ids<-as.list(1:length(groups.l3.list)) #formerly ids.l2.groups
names(groups.l3.ids)<-groups.l3.list


sname<-unlist(lapply(data,function(d){d$SubID})) #lists the group membership of each subject.
groups.l2.list<-sname
groups.l2.N<-length(sname)
groups.l2.ids<-as.list(1:length(groups.l2.list)) #formerly ids.l2.groups
names(groups.l2.ids)<-groups.l2.list

#hyper-parameters are the parameters describing the distributions from which the main model parameters are drawn
#so it seems that we aren't calculating sigmas across all subjects; if we were, they'd be hyper-parameters

#link parameters are parameters for which we're creating sigma correlations at the end.
link.pars=c() #at this stage
#link.pars=c(1:n.components, n.components+1, n.components+2, n.components+3)
unlink.pars=c(1:param.l3.distributions.N)
  
n.pars=param.l1.N
  
n.link.pars=length(link.pars)
n.unlink.pars=length(unlink.pars)

n.mu=n.link.pars
n.Sigma=n.link.pars^2

# n.delta.pars=length(delta.pars)
# n.theta.pars=length(theta.pars)
nmc=5000
burnin=4000
thin=1
keep.samples=seq(burnin,nmc,thin)
length(keep.samples)*n.chains

migrate.prob=.1
migrate.duration=round(burnin*.5) + 1
b=.001
gamma_numerator=1

S=length(data)
R_max=max(unlist(lapply(data,function(s){length(s$runs)})))#maximum number of runs for all subjects.
s_runs.N<-unlist(lapply(data,function(s){length(s$runs)}))
s_runs.MotivationType<-lapply(data,function(x){lapply(x[["runs"]],function(y){y[["motivation"]]})})

param.l1.init=array(NA,c(S,R_max, param.l1.N))#renamed x.init to param.l1.init
#initial values for alpha and thresh
for(j in 1:S){#j<-1
  for (r in 1:s_runs.N[j]){#r<-1
    param.l1.init[j,r,param.l1.ids$alpha]=-3
    param.l1.init[j,r,param.l1.ids$thresh]=log(2)
    #we're giving every single run a default tau based on the minimum RT in that run.
    param.l1.init[j,r,param.l1.ids$tau]=log(.6*(min(data[[j]]$runs[[r]]$rt,na.rm=TRUE)))
  }
}


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
prior.l2$alpha=list("mu"=-3,"sigma"=1,alpha=4,beta=10)
prior.l2$thresh=list("mu"=log(2),"sigma"=sqrt(log(2)),alpha=4,beta=10)
prior.l2$tau=list("mu"=log(.6*(rt_s_mean_all_mean)),
  "sigma"=sqrt(abs(log(.6*(rt_s_mean_all_mean)))),
                  alpha=4,beta=10)

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
#load(file=paste0(mainDataDir,"de_h_m5_testing3.RData"))
source(paste("de_mcmc/de_",version,"_run.R",sep=""))
#save.image(file=paste0(mainDataDir,"de_h_m5_testing3.RData"))

#debugSource(paste("de_mcmc/de_",version,".R",sep=""))

sfStop()

save.image(paste(save.dir,save.name,".RData",sep=""))

########################################## plot
setwd(mainDir)