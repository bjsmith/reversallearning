########################################## load the functions you will use
source("bjs_misc_utils.R")
run.ts<-get.datetimestamp()
#mainDir <- "C:/Users/Brandon/Dropbox/projects/christian"
mainDir=getwd()
save.name="joint_v4_6"
subDir <- paste("output_",save.name,run.ts,sep="")

setwd(mainDir)

source("functions_joint_v2.R")
#custom.lib<-"/expdata/bensmith/joint-modeling/code/Rmanagement/custom-library"
require(parallel)
library(mvtnorm)
library(coda)
library(snow)
library(snowfall)
require(MASS)
library(msm)
library(MCMCpack)
library(gamlss.dist)

#library(coda,lib=custom.lib)
#library(snow,lib=custom.lib)
#library(snowfall,lib=custom.lib)
#require(MASS,lib=custom.lib)
#library(msm,lib=custom.lib)
#library(MCMCpack,lib=custom.lib)
#library(gamlss.dist,lib=custom.lib)


########################################## generate data
#load data that Xiangrui generated
load("data12.RData")

data=data
S=length(data)
n.components=29

#correct some mistakes
data[[90]]$IsGo=data[[90]]$IsGo[-11]
data[[90]]$correct=data[[90]]$correct[-11]
data[[90]]$rt=data[[90]]$rt[-11]
data[[90]]$beta=data[[90]]$beta[-11,]

data[[127]]$IsGo=data[[127]]$IsGo[-52]
data[[127]]$correct=data[[127]]$correct[-52]
data[[127]]$rt=data[[127]]$rt[-52]
data[[127]]$beta=data[[127]]$beta[-52,]

for(j in 1:S){
  data[[j]]$rt <- data[[j]]$rt*100
  data[[j]]$beta <- data[[j]]$beta[1:n.components]
  data[[j]]$cond = rep(1,length(data[[j]]$rt))
}


sigs=numeric(n.components)
for(j in 1:n.components){
  sigs[j]=mean(sapply(data,function(x,j)sd(x$beta[,j]),j=j))
}

temp.data=data
#load data that Xiangrui generated
load("data34.RData")
data<-data[seq(1,160,40)]

S=length(data)
n.components=29

#correct some mistakes
data[[47]]$IsGo=data[[47]]$IsGo[-c(28,29)]
data[[47]]$correct=data[[47]]$correct[-c(28,29)]
data[[47]]$rt=data[[47]]$rt[-c(28,29)]
data[[47]]$beta=data[[47]]$beta[-c(28,29),]

for(j in 1:S){
  temp.data[[j]]$IsGo=c(temp.data[[j]]$IsGo, data[[j]]$IsGo)
  temp.data[[j]]$correct=c(temp.data[[j]]$correct, data[[j]]$correct)
  temp.data[[j]]$rt=c(temp.data[[j]]$rt, data[[j]]$rt*100)
  temp.data[[j]]$beta <- rbind(temp.data[[j]]$beta, data[[j]]$beta[1:n.components])
  temp.data[[j]]$cond = c(temp.data[[j]]$cond, rep(2,length(data[[j]]$rt)))
}

sigs=numeric(n.components)
for(j in 1:n.components){
  sigs[j]=mean(sapply(data,function(x,j)sd(x$beta[,j]),j=j))
}

data=temp.data

setwd(mainDir)

########################################## initialize

# eta1 is stimulus effect for erotic images
# eta2 is stimulus effect for non erotic images
# eta3 is task instructions for go
# eta4 is task instructions for stop
# mu.go1=x["eta1"] + x["eta3"] # this is Erotic stimulus paired with Go instruction
# mu.go2=x["eta2"] + x["eta3"] # this is Non-erotic stimulus paired with Go instruction
# mu.stop1=x["eta2"] + x["eta4"] # this is Non-erotic stimulus paired with NoGo instruction
# mu.stop2=x["eta1"] + x["eta4"] # this is Erotic stimulus paired with NoGo instruction
#? should this reverse between data12 and data34?
#prooooobably not; it should hopefully be built in to data12.Rdata and data34.Rdata.

#par.names=c(paste("beta",1:n.components,sep=""),"eta1","eta2","eta3","eta4","sigma.go","tau.go","sigma.stop","tau.stop")
par.names=c(paste("beta",1:n.components,sep=""),"eta1","eta3","eta4","sigma.go","tau.go","sigma.stop","tau.stop")

#if I understand correctly, these are hyper-parameters because they are describing the distribution of individual subject parameters
#Each subject has a mu (which decomposes to eta, I think???? need to drill down to this),
#a sigma, and a tau; these are all important for the exGaussian estimation
#but those parameters themselves are estimated across subjects on an simple normal distribution;
#hence they each have a mu and a sigma.
hpar.names=c("sigma.go.mu","tau.go.mu","sigma.stop.mu","tau.stop.mu",
             "sigma.go.sigma","tau.go.sigma","sigma.stop.sigma","tau.stop.sigma")


n.chains=24
n.pars=length(par.names)
n.hpars=length(hpar.names)
n.phi.mu=n.hpars/2

#hyper-parameters are the parameters describing the distributions from which the main model parameters are drawn
#so it seems that we aren't calculating sigmas across all subjects; if we were, they'd be hyper-parameters

#link parameters are parameters for which we're creating sigma correlations at the end.
link.pars=c(1:n.components, n.components+1, n.components+2, n.components+3)
unlink.pars=n.components + c(4,5,6,7)
  
n.link.pars=length(link.pars)
n.unlink.pars=length(unlink.pars)

n.mu=n.link.pars
n.Sigma=n.link.pars^2

# n.delta.pars=length(delta.pars)
# n.theta.pars=length(theta.pars)

nmc=60000
burnin=50000
thin=1
keep.samples=seq(burnin,nmc,thin)
length(keep.samples)*n.chains

migrate.prob=.1
migrate.duration=round(burnin*.5) + 1
b=.001


#priors for parameters for each subject
# x.init=matrix(NA,S,n.pars)
# for(j in 1:S){
# x.init[j,]=c(
# apply(data[[j]]$beta,2,mean),
# 200,150,150,100,
#   80,10,40,10
# )
# }
x.init=matrix(NA,S,n.pars)
for(j in 1:S){
  x.init[j,]=c(
    apply(data[[j]]$beta,2,mean), #priors for betas (i.e., the neural data)
    200,150,100, #priors for eta mus
    80,10,40,10  #priors for eta sigmas
  )
}

########################################## prior

prior.big=NULL
prior.big$mu=rep(0,n.link.pars)
prior.big$m=1/10
prior.big$phi=diag(n.link.pars)
prior.big$n0=length(prior.big$mu) + 2

prior=NULL
prior$sigma.go=list(mu=1.5,sigma=0.8,alpha=4,beta=10)
prior$tau.go=list(mu=.75,sigma=0.5,alpha=4,beta=10)
prior$sigma.stop=list(mu=1.5,sigma=0.8,alpha=4,beta=10)
prior$tau.stop=list(mu=.75,sigma=.5,alpha=4,beta=10)

########################################## run it
cores=detectCores()
print(paste0("Starting sfInit to run sfInit with ",cores," cores..."))
sfInit(parallel=TRUE, cpus=cores, type="SOCK")
print("...snowfall initialized; running cluster setup...")
sfClusterSetupRNG()
print("...cluster setup run.")

debugSource(paste("de_",save.name,".R",sep=""))

sfStop()

save.image(paste(save.name,run.ts,".RData",sep=""))

########################################## plot
#I want to avoid plotting anything in this run. We can plot after the data is all generated.
# 
# plot.phi=TRUE
# plot.lower=FALSE
# plot.weights=FALSE
# plot.priors=FALSE
# 
# plot.mu=TRUE #plot mu, central tendency values for each parameter
# 
# plot.sigma=FALSE
# plot.rho=TRUE
# start=2
# start.weights=10
# 
# source("fig_base2.r")
# 



