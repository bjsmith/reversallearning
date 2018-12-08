plot.phi.s=TRUE
plot.phi.g=TRUE
plot.lower=TRUE
plot.sigma=FALSE
plot.rho=FALSE
plot.mu=FALSE
plot.weights=FALSE
plot.priors=FALSE
#load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m5g20180522T175350gn0_3_full.RData")
#output_h_m5f20180520T183320.RData
#load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m5j20180528T120720_15subs.RData")
load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m5j20180528T121720.RData")
#load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m5j20180528T184405_15subs_iterations20000.RData")
#load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m5k20180605T122611_15subs.RData")

temp=t(as.matrix(read.table(paste("chain",1,"_hyper_phi_s.txt",sep=""),header=F)))
start=2 
start.weights=2


tnmc=length(keep.samples)
#range<-16000:tnmc
range<-1:tnmc
lnmc=length(range)

setwd(file.path(mainDir))
source("de_mcmc/hierarchical_3l_summarize.R")


pdf(paste(save.dir,save.name,".pdf",sep=""),10,6)
par(mfrow=c(1,2),ask=FALSE)


#bjs 2016-10-24
#updated to work as a function.
# fig_base <- function(env,plot.lower=FALSE,plot.phi=FALSE,plot.weights=FALSE,plot.sigma=FALSE,plot.rho=FALSE,plot.mu=FALSE,plot.priors=FALSE,
#                      ask=FALSE){
#attach(env)
setwd(file.path(mainDataDir, subDir))


if(plot.phi.s==TRUE)phi.s=array(NA,c(n.chains,param.l2.N*groups.l2.N,lnmc),
                                dimnames=list(paste0("Chain",1:n.chains),
                                              paste0("S",rep(1:groups.l2.N,each=param.l2.N),"_",rep(param.l2.names,groups.l2.N))))
if(plot.phi.g==TRUE)phi.g=array(NA,c(n.chains,param.l3.N*groups.l3.N,lnmc),
                                dimnames=list(paste0("Chain",1:n.chains),
                                              paste0(rep(groups.l3.list,each=param.l3.N),"_",rep(param.l3.names,groups.l3.N))))

#we're formatting this one a bit differently.
if(plot.lower==TRUE)theta=array(NA,c(n.chains,n.pars,S,R_max,lnmc),
                                dimnames=list(NULL,
                                              param.l1.names,
                                              paste0("S",1:groups.l2.N),
                                              NULL,
                                              NULL))

if(plot.weights==TRUE)weights=array(NA,c(n.chains,S,lnmc))
if(plot.sigma==TRUE | plot.rho==TRUE)Sigma=array(NA,c(n.chains,n.Sigma,lnmc))
if(plot.mu==TRUE)mu=array(NA,c(n.chains,n.mu,lnmc))

#iterating through chains I guess?
for(q in 1:n.chains){#q<-1
  if(plot.lower==TRUE){
    for(s in 1:S){
      #need a temporary bug fix in here for a particular run of h_m5a, 
      #which mistakenly put all the runs into each file for the individual runs.
      for (r in 1:s_runs.N[s]){
        temp=t(as.matrix(read.table(paste("chain",q,"_sub",s,"_run",r,".txt",sep=""),header=F)))
        theta[q,,s,r,]=temp[,range]
      }
    }
  }
  if(plot.sigma==TRUE | plot.rho==TRUE){
    temp=t(as.matrix(read.table(paste("chain",q,"_Sigma.txt",sep=""),header=F)));Sigma[q,,]=temp[,-1];
  }
  if(plot.mu==TRUE){
    temp=t(as.matrix(read.table(paste("chain",q,"_mu.txt",sep=""),header=F)));mu[q,,]=temp[,-1];
  }
  if(plot.phi.s==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_hyper_phi_s.txt",sep=""),header=F)));phi.s[q,,]=temp[,-1][,range]}
  #let's get it straight how to interpret this. dim1 represents params*groups.
  
  if(plot.phi.g==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_hyper_phi_g.txt",sep=""),header=F)));phi.g[q,,]=temp[,-1][,range]}
  
  if(plot.weights==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_weights.txt",sep=""),header=F)));weights[q,,]=temp[,-1]}
  #if(plot.weights.delta==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_weights_delta.txt",sep=""),header=F)));weight.delta[q,,]=temp[,-1]}
  print(round(q/n.chains*100))
}
#######################################################################################

#efficiency measure: how many accepted changes are there in total?
dim(theta)
efficiency_theta=sum(theta[,,,,2:(dim(theta)[5])]!=theta[,,,,1:(dim(theta)[5]-1)],na.rm=TRUE)/((prod(dim(theta)[1:4])*(dim(theta)[5]-1))-sum(is.na(theta)))
efficiency_theta=sum(theta[,,,,2:(dim(theta)[5])]!=theta[,,,,1:(dim(theta)[5]-1)],na.rm=TRUE)/((prod(dim(theta)[1:4])*(dim(theta)[5]-1))-sum(is.na(theta)))
efficiency_phi_s_dt<-sum(phi.s[,,2:dim(phi.s)[3]]!=phi.s[,,1:(dim(phi.s)[3]-1)])/(prod(dim(phi.s)[1:2])*dim(phi.s)[3])

#######################################################################################

#hierarchical_3l_summarize(tnmc=tnmc)

#######################################################################################
#if(!exists("n.l2.groups"))n.l2.groups<-1

setwd(file.path(mainDir))
pdf(paste(save.dir,save.name,".pdf",sep=""),10,6)
par(mfrow=c(1,2),ask=FALSE)

breaks=50
ask=FALSE
count=1
phi_vals<-list()

if (plot.phi.s==TRUE){phi_vals[["phi.s"]]<-phi.s}
if (plot.phi.g==TRUE){phi_vals[["phi.g"]]<-phi.g}
for (phi.x in phi_vals){
  #phi.x<-phi_vals[[1]]
  par(mfrow=c(3,4),ask=ask)
  
  for(k in rep((0:(dim(phi.x)[2]/6-1))*6,each=3)+rep(1:3,times=dim(phi.x)[2]/6)){#loop through each of the params, and at each iteration, do both the central dtendency and deviancy param.
    #k<-1
    if(plot.priors==TRUE){
      if(k==(n.pars+1))count=1
      xs=seq(min(phi.x[,k,]),max(phi.x[,k,]),length=200)
      if(k<=n.pars)ys=dnorm(xs,prior[[count]]$mu,prior[[count]]$sigma)
      if(k>n.pars)ys=dinvgamma(xs,prior[[count]]$alpha,prior[[count]]$beta)
    }
    for (hyper_type in c(0,3)){
      #do it this way so we cycle through each mu and sigma pair at the same time.
      k_t<-k+hyper_type
      matplot(t(phi.x[,k_t,]),type="l",lty=1,main="",ylab=dimnames(phi.x)[[2]][k_t])
      hist(phi.x[,k_t,],prob=T,breaks=breaks,main="",xlab=dimnames(phi.x)[[2]][k_t])
    }
    if(plot.priors==TRUE){
      lines(xs,ys,lty=2)
      count=count+1
    }
  }
}

#let's graph the thetas

#a pseudorandom sample - only prime numbered subjects

for(t_s in 1:dim(theta)[3]){
  if(is.prime(t_s)){
    par(mfrow=c(s_runs.N[t_s],6),ask=FALSE)
    for (r in 1:s_runs.N[t_s]){
      
      for(k in 1:(dim(theta)[2])){#parameters
        #t_s<-1;r<-1;k<-1
        # if(plot.priors==TRUE){
        #   if(k==(n.pars+1))count=1
        #   xs=seq(min(phi.x[,k,start:tnmc]),max(phi.x[,k,start:tnmc]),length=200)
        #   if(k<=n.pars)ys=dnorm(xs,prior[[count]]$mu,prior[[count]]$sigma)
        #   if(k>n.pars)ys=dinvgamma(xs,prior[[count]]$alpha,prior[[count]]$beta)
        # }
        theta_val <- paste0("S",t_s, " R",r)
        matplot(t(theta[,k,t_s,r, ]),type="l",lty=1, main=theta_val, ylab=dimnames(theta)[[2]][k])
        hist(theta[,k,t_s,r,],prob=T,breaks=breaks, main=theta_val, xlab=dimnames(theta)[[2]][k])
        if(plot.priors==TRUE){
          lines(xs,ys,lty=2)
          count=count+1
        }
      }
    }
  }
}



dev.off()

setwd(mainDir)

