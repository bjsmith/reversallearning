#bjs 2016-10-24
#updated to work as a function.
fig_base <- function(env,plot.lower=FALSE,plot.phi=FALSE,plot.weights=FALSE,plot.sigma=FALSE,plot.rho=FALSE,plot.mu=FALSE,plot.priors=FALSE,
                     ask=FALSE){
  attach(env)
  setwd(file.path(mainDataDir, subDir))
  tnmc=length(keep.samples)
  
  if(plot.phi==TRUE)phi=array(NA,c(n.chains,n.hpars,tnmc))
  if(plot.lower==TRUE)theta=array(NA,c(n.chains,n.pars,S,tnmc))
  if(plot.weights==TRUE)weights=array(NA,c(n.chains,S,tnmc))
  if(plot.sigma==TRUE | plot.rho==TRUE)Sigma=array(NA,c(n.chains,n.Sigma,tnmc))
  if(plot.mu==TRUE)mu=array(NA,c(n.chains,n.mu,tnmc))
  
  #iterating through chains I guess?
  for(q in 1:n.chains){
    if(plot.lower==TRUE){
      for(j in 1:S){
        temp=t(as.matrix(read.table(paste("chain",q,"_sub",j,"_lower.txt",sep=""),header=F)))
        theta[q,,j,]=temp[,-1]
      }}
    if(plot.sigma==TRUE | plot.rho==TRUE){
      temp=t(as.matrix(read.table(paste("chain",q,"_Sigma.txt",sep=""),header=F)));Sigma[q,,]=temp[,-1];
    }
    if(plot.mu==TRUE){
      temp=t(as.matrix(read.table(paste("chain",q,"_mu.txt",sep=""),header=F)));mu[q,,]=temp[,-1];
    }
    if(plot.phi==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_hyper.txt",sep=""),header=F)));phi[q,]=temp[,-1]}
    if(plot.weights==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_weights.txt",sep=""),header=F)));weights[q,,]=temp[,-1]}
    #if(plot.weights.delta==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_weights_delta.txt",sep=""),header=F)));weight.delta[q,,]=temp[,-1]}
    print(round(q/n.chains*100))
  }
  
  #######################################################################################
  #if(!exists("n.l2.groups"))n.l2.groups<-1
    
  breaks=50
  
  count=1
  if(plot.phi==TRUE){
    par(mfrow=c(2,2),ask=ask)
    
    for(k in 1:n.hpars){
      if(plot.priors==TRUE){
        if(k==(n.pars+1))count=1
        xs=seq(min(phi[,k,start:tnmc]),max(phi[,k,start:tnmc]),length=200)
        if(k<=n.pars)ys=dnorm(xs,prior[[count]]$mu,prior[[count]]$sigma)
        if(k>n.pars)ys=dinvgamma(xs,prior[[count]]$alpha,prior[[count]]$beta)
      }
      matplot(t(phi[,k,start:tnmc]),type="l",lty=1,main="",ylab=hpar.names[k])
      hist(phi[,k,start:tnmc],prob=T,breaks=breaks,main="",xlab=hpar.names[k])
      if(plot.priors==TRUE){
        lines(xs,ys,lty=2)
        count=count+1
      }}}
  
  if(plot.lower==TRUE){
    par(mfrow=c(2,2),ask=ask)
    for(j in 1:S){
      for(k in 1:n.pars){
        matplot(t(theta[,k,j,start:tnmc]),type="l",lty=1,main=paste("Subject ",j),ylab=par.names[k])
        #if(any(k==c(1,2,3,6)))abline(h=log(true$vec[k]),lwd=3)
        #if(any(k==c(4,5)))abline(h=logit(true$vec[k]),lwd=3)
        hist(theta[,k,j,start:tnmc],prob=T,breaks=breaks,main=paste("Subject ",j),xlab=par.names[k])
        #if(any(k==c(1,2,3,6)))abline(v=log(true$vec[k]),lwd=3,col="red")
        #if(any(k==c(4,5)))abline(v=logit(true$vec[k]),lwd=3,col="red")
      }}}
  

  
  if(plot.sigma==TRUE){
    par(mfrow=c(2,2),ask=ask)
    for(k in 1:n.Sigma){
      matplot(t(Sigma[,k,start:tnmc]),type="l",lty=1,main=paste("Sigma1",k))
      hist(Sigma[,k,start:tnmc],prob=T,breaks=breaks)
    }
  }
  
  if(plot.mu==TRUE){
    par(mfrow=c(2,2),ask=ask)
    for(k in 1:n.mu){
      matplot(t(mu[,k,start:tnmc]),type="l",lty=1,main=paste("mu1",k))
      hist(mu[,k,start:tnmc],prob=T,breaks=breaks)
    }
  }
  
  if(plot.rho==TRUE){
    par(mfrow=c(2,2),ask=ask)
    shift=n.mu
    for(k in 3:shift){
      temp=Sigma[,k,start:tnmc]/(sqrt(Sigma[,shift*(k-1)+k,start:tnmc])*sqrt(Sigma[,1,start:tnmc]))
      matplot(t(temp),type="l",lty=1,main=paste("Sigma1",k))
      hist(temp,prob=T,breaks=breaks)
      
      temp=Sigma[,k+shift,start:tnmc]/(sqrt(Sigma[,shift*(k-1)+k,start:tnmc])*sqrt(Sigma[,shift+2,start:tnmc]))
      matplot(t(temp),type="l",lty=1,main=paste("Sigma2",k))
      hist(temp,prob=T,breaks=breaks)
    }
  }
  
  if(plot.weights==TRUE){
    par(mfrow=c(2,2),ask=ask)
    for(j in 1:S){
      matplot(t(weights[,j,start.weights:tnmc]),type="l",lty=1,main=paste("Subject ",j),ylab="log likelihood")
    }}
  
  setwd(file.path(mainDir))
  
  
}
