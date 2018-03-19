version="m1"
verbose=TRUE
source('de_mcmc/main_m1_setup.R')

##############################################  generate data
source("de_mcmc/raw_data_reward_only.R")
##############################################  initialize

par.names=c("alpha","beta","thresh","tau")
n.pars=length(par.names)

n.chains=24
nmc=5000
burnin=1000
thin=1
keep.samples=seq(burnin,nmc,thin)
print(length(keep.samples)*n.chains)
  
use.optim=TRUE
optim.gamma=TRUE
migrate.prob=.1
migrate.duration=round(burnin*.25)+1
b=.001
  
cores=8
  
x.init=matrix(c(.3,.2,2,NA),S,n.pars,byrow=T)
x.init[,4]=.6*sapply(data,function(x)min(x$rt,na.rm=TRUE))

##############################################  set prior

prior=NULL
# upper and lower boundaries for the concentration parameters
prior$lower=0  
prior$upper=1

########################################## run it
source(paste("de_mcmc/de_",version,"_config.R",sep=""))
source(paste("de_mcmc/de_",version,"_run.R",sep=""))


run_env<-de_mcmc_execute(log.dens.like.m1,log.dens.prior)
attach(run_env)


########################################## estimation
##graphics.  

# plot.lower=TRUE
# plot.weights=TRUE
# start=2
# start.weights=2
# 
# pdf(paste(save.dir,save.name,".pdf",sep=""),10,5)
# par(mfrow=c(1,2),ask=FALSE)
# source("de_mcmc/fig_base.R")
# dev.off()