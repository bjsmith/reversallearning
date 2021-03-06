#repeats main_h_m3, but does so for an updated dataset...
# - with poorly-attending subjects removed (using the "amendment2" dataset);
# - repeats the analysis for each run, so that we can be more confident about our results across all the runs.
########################################## load the functions you will use
source("../joint_msm_combined/bjs_misc_utils.R")
version="h_m3"
save.name="main_h_m3"
source('de_mcmc/functions.R')
source('de_mcmc/main_m1_setup.R')
source('de_mcmc/functions_joint_v2.R')
source(paste0('de_mcmc/functions_',version,'.R'))
########################################## generate data

source("de_mcmc/raw_data_all_runs_v2.R")
data_allruns<-data

#create a restricted data list, just a one-run list.
data<-vector("list",length(data_allruns))
runid<-2
mid<-"punishment"

for (i in 1:length(data_allruns)){
  #identify the run which we want to extract
  run_to_extract<-which(unlist(lapply(data_allruns[[i]]$runs,function(r){r$runid==runid & r$motivation==mid})))
  if(length(run_to_extract)>0){
    data[[i]]<-list("group"=data_allruns[[i]]$group,
                    "SubID"=data_allruns[[i]]$SubID,
                    "rt"=data_allruns[[i]]$runs[[run_to_extract]]$rt,
                    "choice"=data_allruns[[i]]$runs[[run_to_extract]]$choice,
                    "cue"=data_allruns[[i]]$runs[[run_to_extract]]$cue,
                    "outcome"=data_allruns[[i]]$runs[[run_to_extract]]$outcome)
  }
}

#remove blank entries
data<-data[!unlist(lapply(data,is.null))]

#remove subjects who've not been identified to a specific group.
data<-data[!is.na(unlist(lapply(data,function(d){d$group})))]

safe_meth_subjs<-unlist(lapply(data,function(d){d$group=="SafeMeth"}))
data<-data[!safe_meth_subjs]

# for (i in 1:length(data)){
#   #print(paste(i,data[[i]]$SubID))
#   print(paste(i,length(data[[i]]$rt)))
# }

########################################## initialize
#this version analyzes reward data from one run for three groups.
#Safe Meth subjects are excluded.


par.names.l1=c("alpha",#"beta",
                   "thresh","tau")
par.ids.l1<-as.list(1:3)
names(par.ids.l1)=par.names.l1

par.names.l2<-c(paste0(par.names.l1, "_mu"),paste0(par.names.l1, "_sigma"))
par.ids.l2<-as.list(1:length(par.names.l2))
names(par.ids.l2)<-par.names.l2
par.names=c(par.names.l1)
#if I understand correctly, these are hyper-parameters because they are describing the distribution of individual subject parameters
#Each subject has a mu (which decomposes to eta, I think???? need to drill down to this),
#a sigma, and a tau; these are all important for the exGaussian estimation
#but those parameters themselves are estimated across subjects on an simple normal distribution;
#hence they each have a mu and a sigma.
hpar.names=par.names.l2


n.chains=24
n.pars=length(par.names)
n.hpars=length(hpar.names)
n.phi.mu=n.hpars/2

group_by_subject<-unlist(lapply(data,function(d){d$group}))
l2.groups.list<-unique(group_by_subject)
n.l2.groups<-length(l2.groups.list)
ids.l2.groups<-as.list(1:length(l2.groups.list))
names(ids.l2.groups)<-l2.groups.list


#hyper-parameters are the parameters describing the distributions from which the main model parameters are drawn
#so it seems that we aren't calculating sigmas across all subjects; if we were, they'd be hyper-parameters

#link parameters are parameters for which we're creating sigma correlations at the end.
link.pars=c() #at this stage
#link.pars=c(1:n.components, n.components+1, n.components+2, n.components+3)
unlink.pars=c(1:n.phi.mu)
  
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

S=length(data)

x.init=matrix(NA,S,n.pars)
for(j in 1:S){
  x.init[j,par.ids.l1$alpha]=-3
  x.init[j,par.ids.l1$thresh]=log(2)
}
x.init[,par.ids.l1$tau]=log(.6*(sapply(data,function(x)min(x$rt,na.rm=TRUE))))


########################################## prior values for the hypers, I think
prior.big=NULL
#prior.big$mu=rep(0,n.link.pars)
prior.big$m=1/10
#prior.big$phi=diag(n.link.pars)
prior.big$n0=length(prior.big$mu) + 2

prior=NULL
prior$alpha=list("mu"=-3,"sigma"=1,alpha=4,beta=10)
prior$thresh=list("mu"=log(2),"sigma"=sqrt(log(2)),alpha=4,beta=10)
prior$tau=list("mu"=mean(log(.6*(sapply(data,function(x)min(x$rt,na.rm=TRUE))))),
               "sigma"=sqrt(mean(abs(log(.6*(sapply(data,function(x)min(x$rt,na.rm=TRUE))))))),
               alpha=4,beta=10)


########################################## run it
cores=8#detectCores()
print(paste0("Starting sfInit to run sfInit with ",cores," cores..."))
sfInit(parallel=TRUE, cpus=cores, type="SOCK")
print("...snowfall initialized; running clustercd setup...")
sfClusterSetupRNG()
print("...cluster setup run.")

source(paste("de_mcmc/de_",version,".R",sep=""))

sfStop()

save.image(paste(save.dir,save.name,".RData",sep=""))

########################################## plot
setwd(mainDir)