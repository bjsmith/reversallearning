
source("de_mcmc/functions.R")
#install.packages("snowfall")
library("snowfall")
library("MASS")
library("msm")#install.packages("msm")
library("MCMCpack")#install.packages("MCMCpack")
  
mainDir <- getwd()
setwd(mainDir)
  
version="m1"
save.name <- paste("output_",version,sep="")
  
##############################################  generate data
  
source("raw_data_wrapper_for_brandon.R")

#for now just focus on reward run 1
rawdata<-rawdata[rawdata$Motivation=="reward" & rawdata$runid==1,]
subs=sort(unique(rawdata[,"subid"]))
S=length(subs)

rawdata.dt<-data.table(rawdata)
rawdata.dt[,runmotive:=paste0(Motivation,runid)]

data=NULL
# i'm pretty sure some of the run information needs to be in here. 
for(j in 1:S){#j=1
  s<-subs[j]
  for (r in unique(rawdata.dt[subid==s,runid])){#r=1
    for (m in unique(rawdata.dt[subid==s&runid==r,Motivation])){#m="reward"
      #for now just focus on reward run 1
      if(r==1 & m=="reward"){
        temp.data=rawdata.dt[subid==s & runid==r & Motivation==m,]
        rt=temp.data[,reaction_time]
        rt[rt==0] <- NA
        data[[j]]=list("cue"=as.vector(temp.data[,cue]),"choice"=as.vector(temp.data[,choice]),"rt"=rt,"outcome"=as.vector(temp.data[,outcome]))
      }
    }
  }
}

##############################################  initialize

par.names=c("alpha","beta","thresh","theta")
n.pars=length(par.names)
  
n.chains=24
nmc=100
burnin=50
thin=1
keep.samples=seq(burnin,nmc,thin)
print(length(keep.samples)*n.chains)
  
use.optim=TRUE
optim.gamma=TRUE
migrate.prob=.1
migrate.duration=round(burnin*.25)+1
b=.001
  
cores=8
  
x.init=matrix(c(.3,2,NA,0.2),S,n.pars,byrow=T)
x.init[,3]=.6*sapply(data,function(x)min(x$rt,na.rm=TRUE))

##############################################  set prior

prior=NULL
# upper and lower boundaries for the concentration parameters
prior$lower=0  
prior$upper=1

########################################## run it
  
subDir=save.name

sfInit(parallel=TRUE, cpus=cores, type="SOCK")
sfClusterSetupRNG()
  
ptm=proc.time()[3]
debugSource(paste("de_mcmc/de_",version,".R",sep=""))
proc.time()[3]-ptm
  
sfStop()
  
save.image(paste(save.name,".RData",sep=""))
  
########################################## estimation
  
plot.lower=TRUE
plot.weights=TRUE
start=2
start.weights=2

#ps1,    2,    3,    4,   5,   6,    7,    8
# tt, n1n1, n2n2, n3n3, n1t, tn1, n1n2, n2n1
pdf(paste(save.name,".pdf",sep=""),10,5)
par(mfrow=c(1,2),ask=FALSE)
source("fig_base.r")
dev.off()
  
  