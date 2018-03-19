
setwd(file.path(mainDataDir, subDir))
tnmc=length(keep.samples)

#if(plot.phi==TRUE)phi=array(NA,c(n.chains,n.hpars,tnmc))
if(plot.lower==TRUE)theta=array(NA,c(n.chains,n.pars,S,tnmc))
#if(plot.weights==TRUE)weights=array(NA,c(n.chains,S,tnmc))
# if(plot.sigma==TRUE){
#   Sigma1=array(NA,c(n.chains,n.Sigma1,tnmc))
#   Sigma2=array(NA,c(n.chains,n.Sigma2,tnmc))
# }
# if(plot.mu==TRUE){
#   mu1=array(NA,c(n.chains,n.mu1,tnmc))
#   mu2=array(NA,c(n.chains,n.mu2,tnmc))
# }

for(q in 1:n.chains){
  if(plot.lower==TRUE){
    for(j in 1:S){
      temp=t(as.matrix(read.table(paste("chain",q,"_sub",j,"_lower.txt",sep=""),header=F)))
      theta[q,,j,]=temp[,-1]
    }}
  if(plot.sigma==TRUE){
    temp=t(as.matrix(read.table(paste("chain",q,"_Sigma1.txt",sep=""),header=F)));Sigma1[q,,]=temp[,-1];
    temp=t(as.matrix(read.table(paste("chain",q,"_Sigma2.txt",sep=""),header=F)));Sigma2[q,,]=temp[,-1];
  }
  if(plot.mu==TRUE){
    temp=t(as.matrix(read.table(paste("chain",q,"_mu1.txt",sep=""),header=F)));mu1[q,,]=temp[,-1];
    temp=t(as.matrix(read.table(paste("chain",q,"_mu2.txt",sep=""),header=F)));mu2[q,,]=temp[,-1];
  }
  if(plot.phi==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_hyper.txt",sep=""),header=F)));phi[q,,]=temp[,-1]}
  if(plot.weights==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_weights.txt",sep=""),header=F)));weights[q,,]=temp[,-1]}
  #if(plot.weights.delta==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_weights_delta.txt",sep=""),header=F)));weight.delta[q,,]=temp[,-1]}
  print(round(q/n.chains*100))
}

#######################################################################################

breaks=50

if(plot.lower==TRUE){
  par(mfrow=c(2,2),ask=F)
  for(j in 1:10){
    for(k in 1:n.pars){
      matplot(t(theta[,k,j,starti:tnmc]),type="l",lty=1,main=paste0(par.names[k],"; Subject ",j),ylab=par.names[k])
      #if(any(k==c(1,2,3,6)))abline(h=log(true$vec[k]),lwd=3)
      #if(any(k==c(4,5)))abline(h=logit(true$vec[k]),lwd=3)
      hist(theta[,k,j,starti:tnmc],prob=T,breaks=breaks,main=paste0(par.names[k],"; Subject ",j),xlab=par.names[k])
      #if(any(k==c(1,2,3,6)))abline(v=log(true$vec[k]),lwd=3,col="red")
      #if(any(k==c(4,5)))abline(v=logit(true$vec[k]),lwd=3,col="red")
    }
  }
  
  par(mfrow=c(2,3),ask=F)
  for(j in 11:S){
    for(k in 1:n.pars){
      if (k==2) {
        next
      }
      #matplot(t(theta[,k,j,starti:tnmc]),type="l",lty=1,main=paste0(par.names[k],"; Subject ",j),ylab=par.names[k])
      #if(any(k==c(1,2,3,6)))abline(h=log(true$vec[k]),lwd=3)
      #if(any(k==c(4,5)))abline(h=logit(true$vec[k]),lwd=3)
      hist(theta[,k,j,starti:tnmc],prob=T,breaks=breaks,main=paste0(par.names[k],"; Subject ",j),xlab=par.names[k])
      #if(any(k==c(1,2,3,6)))abline(v=log(true$vec[k]),lwd=3,col="red")
      #if(any(k==c(4,5)))abline(v=logit(true$vec[k]),lwd=3,col="red")
    }
  }
}

count=1
if(plot.phi==TRUE){
par(mfrow=c(2,2),ask=T)
for(k in 1:n.hpars){
if(plot.priors==TRUE){
if(k==(n.pars+1))count=1
xs=seq(min(phi[,k,starti:tnmc]),max(phi[,k,starti:tnmc]),length=200)
if(k<=n.pars)ys=dnorm(xs,prior[[count]]$mu,prior[[count]]$sigma)
if(k>n.pars)ys=dinvgamma(xs,prior[[count]]$alpha,prior[[count]]$beta)
}
matplot(t(phi[,k,starti:tnmc]),type="l",lty=1,main="",ylab=hpar.names[k])
hist(phi[,k,starti:tnmc],prob=T,breaks=breaks,main="",xlab=hpar.names[k])
if(plot.priors==TRUE){
lines(xs,ys,lty=2)
count=count+1
}}}

if(plot.sigma==TRUE){
  par(mfrow=c(2,2),ask=T)
  for(k in 1:n.Sigma1){
    matplot(t(Sigma1[,k,starti:tnmc]),type="l",lty=1,main=paste("Sigma1",k))
    hist(Sigma1[,k,starti:tnmc],prob=T,breaks=breaks)
  }
  for(k in 1:n.Sigma2){
    matplot(t(Sigma2[,k,starti:tnmc]),type="l",lty=1,main=paste("Sigma2",k))
    hist(Sigma2[,k,starti:tnmc],prob=T,breaks=breaks)
  }
}

if(plot.mu==TRUE){
  par(mfrow=c(2,2),ask=T)
  for(k in 1:n.mu1){
    matplot(t(mu1[,k,starti:tnmc]),type="l",lty=1,main=paste("mu1",k))
    hist(mu1[,k,starti:tnmc],prob=T,breaks=breaks)
  }
  for(k in 1:n.mu2){
    matplot(t(mu2[,k,starti:tnmc]),type="l",lty=1,main=paste("mu2",k))
    hist(mu2[,k,starti:tnmc],prob=T,breaks=breaks)
  }
}

if(plot.rho==TRUE){
  par(mfrow=c(2,2),ask=T)
  shift=n.mu1
  for(k in 2:shift){
    temp=Sigma1[,k,starti:tnmc]/(sqrt(Sigma1[,shift*(k-1)+k,starti:tnmc])*sqrt(Sigma1[,1,starti:tnmc]))
    matplot(t(temp),type="l",lty=1,main=paste("Sigma1",k))
    hist(temp,prob=T,breaks=breaks)
  
    temp=Sigma2[,k,starti:tnmc]/(sqrt(Sigma2[,shift*(k-1)+k,starti:tnmc])*sqrt(Sigma2[,1,starti:tnmc]))
    matplot(t(temp),type="l",lty=1,main=paste("Sigma2",k))
    hist(temp,prob=T,breaks=breaks)
  }
}

if(plot.weights==TRUE){
par(mfrow=c(2,2),ask=T)
for(j in 1:S){
matplot(t(weights[,j,starti.weights:tnmc]),type="l",lty=1,main=paste("Subject ",j),ylab="log likelihood")
}}

setwd(file.path(mainDir))

