
log.dens.prior=function(x,use.mu,use.Sigma,use.phi){
require(mvtnorm)
names(x) <- par.names
names(use.phi) <- hpar.names
sigma=matrix(use.Sigma,sqrt(n.Sigma),sqrt(n.Sigma))
sum(log(dmvnorm(x[link.pars],use.mu,sigma))) +
sum(log(dnorm(x["sigma.go"],use.phi["sigma.go.mu"],use.phi["sigma.go.sigma"]))) + 
sum(log(dnorm(x["tau.go"],use.phi["tau.go.mu"],use.phi["tau.go.sigma"]))) + 
sum(log(dnorm(x["sigma.stop"],use.phi["sigma.stop.mu"],use.phi["sigma.stop.sigma"]))) + 
sum(log(dnorm(x["tau.stop"],use.phi["tau.stop.mu"],use.phi["tau.stop.sigma"]))) 
}

log.dens.like=function(x,use.data,method="full"){
names(x) <- par.names  
x[c("sigma.go","tau.go","sigma.stop","tau.stop")] <- exp(x[c("sigma.go","tau.go","sigma.stop","tau.stop")])
#x[c("eta1","eta2","eta3","eta4")] <- exp(x[c("eta1","eta2","eta3","eta4")])
x[c("eta1","eta3","eta4")] <- exp(x[c("eta1","eta3","eta4")])
# eta1 is stimulus effect for erotic images
# eta2 is stimulus effect for non erotic images
# eta3 is task instructions for go
# eta4 is task instructions for stop
eta2=0
mu.go1=x["eta1"] + x["eta3"]
#mu.go2=x["eta2"] + x["eta3"]
mu.go2=eta2 + x["eta3"]
#mu.stop1=-x["eta2"] + x["eta4"]
mu.stop1=-eta2 + x["eta4"]
mu.stop2=-x["eta1"] + x["eta4"]
if(all(x<Inf) & all(!is.na(x)) & 
       mu.go1<1000 & mu.stop1<600 & 
       mu.go2<1000 & mu.stop2<600 & 
        all(x[c("eta1","eta3","eta4")]>=0)&
       x["sigma.go"]<300 & x["sigma.stop"]<250 & 
       x["tau.go"]<300 & x["tau.stop"]<250 & 
       x["tau.go"]>1 & x["tau.stop"]>1 & 
       x["sigma.go"]>1 & x["sigma.stop"]>1 & 
       x["tau.go"]<min(use.data$rt,na.rm=TRUE) & x["tau.stop"]<min(use.data$rt,na.rm=TRUE)){
  require(gamlss.dist)
  SSD=0
  org.data=use.data
  use.data=list("IsGo"=org.data$IsGo[org.data$cond==1],"correct"=org.data$correct[org.data$cond==1],"rt"=org.data$rt[org.data$cond==1],"beta"=org.data$beta[org.data$cond==1,])
  a1=dexGAUS(use.data$rt[use.data$IsGo==1 & use.data$correct==1],mu.go1,x["sigma.go"],x["tau.go"])
  tr=use.data$rt[use.data$IsGo==0 & use.data$correct==0]
  ti=length(use.data$rt[use.data$IsGo==0 & use.data$correct==1])
  b1=sum(log(dexGAUS(tr,mu.go1,x["sigma.go"],x["tau.go"])) + log(1-pexGAUS(tr-SSD,mu.stop1,x["sigma.stop"],x["tau.stop"])))
  integ=integrate(fun,0,1000,mu.go=mu.go1,sigma.go=x["sigma.go"],tau.go=x["tau.go"],mu.stop=mu.stop1,sigma.stop=x["sigma.stop"],tau.stop=x["tau.stop"],stop.on.error=FALSE)$value
  c1=log(integ)*ti
  
  use.data=list("IsGo"=org.data$IsGo[org.data$cond==2],"correct"=org.data$correct[org.data$cond==2],"rt"=org.data$rt[org.data$cond==2],"beta"=org.data$beta[org.data$cond==2,])
  a2=dexGAUS(use.data$rt[use.data$IsGo==1 & use.data$correct==1],mu.go2,x["sigma.go"],x["tau.go"])
  tr=use.data$rt[use.data$IsGo==0 & use.data$correct==0]
  ti=length(use.data$rt[use.data$IsGo==0 & use.data$correct==1])
  b2=sum(log(dexGAUS(tr,mu.go2,x["sigma.go"],x["tau.go"])) + log(1-pexGAUS(tr-SSD,mu.stop2,x["sigma.stop"],x["tau.stop"])))
  integ=integrate(fun,0,1000,mu.go=mu.go2,sigma.go=x["sigma.go"],tau.go=x["tau.go"],mu.stop=mu.stop2,sigma.stop=x["sigma.stop"],tau.stop=x["tau.stop"],stop.on.error=FALSE)$value
  if(integ==Inf){
    integ=0
    warning("The integral was non-finite, and was set to zero to avoid generating an error, but there may be problems in the analysis. Possibly relevant values have been printed to the output stream.")
    print(paste0(
      "integrate(",fun,",0,1000,mu.go=",mu.go2,",sigma.go=",x["sigma.go"],",tau.go=",
      x["tau.go"],",mu.stop=",mu.stop2,",sigma.stop=",x["sigma.stop"],",tau.stop=",
      x["tau.stop"],",stop.on.error=FALSE)$value"
    ))
  }
  c2=log(integ)*ti

  fmri=apply(use.data$beta,2,dnorm,mean=x[paste("beta",1:n.components,sep="")],sd=sigs,log=TRUE)
  out=sum(log(a1)) + b1 + c1 + sum(log(a2)) + b2 + c2 + sum(fmri)
if(is.na(out))out=-Inf
} else {
out=-Inf
}
out
}

########################################### Optimize and Initialize

theta=array(NA,c(n.chains,n.pars,S))
weight=array(-Inf,c(n.chains,S))
mu=array(NA,c(n.chains,n.mu))
Sigma=array(NA,c(n.chains,n.Sigma))
phi=array(NA,c(n.chains,n.hpars))

colnames(theta) <- par.names
colnames(phi) <- hpar.names

sfExportAll(except=list("theta","weight","mu","Sigma","phi"))

init.pars=matrix(NA,S,n.pars)
for(j in 1:S){
  x=x.init[j,]
  names(x) <- par.names
  x[c("sigma.go","tau.go","sigma.stop","tau.stop")] <- log(x[c("sigma.go","tau.go","sigma.stop","tau.stop")])
  #x[c("eta1","eta2","eta3","eta4")] <- log(x[c("eta1","eta2","eta3","eta4")])
  x[c("eta1","eta3","eta4")] <- log(x[c("eta1","eta3","eta4")])
  temp.weight=log.dens.like(x,data[[j]])
  new.x=x
  while(temp.weight==-Inf){
    new.x=rnorm(n.pars,x,5)
    temp.weight=log.dens.like(new.x,data[[j]])
  }
  init.pars[j,]=optim(new.x,function(x,data)-log.dens.like(x,data),data=data[[j]])$par
  print(paste("Optimization ",round(j/S*100),"% Complete.",sep=""))
}

write(t(init.pars),file=paste("MLEs_",save.name,".txt",sep=""),ncolumns=n.pars)

for(i in 1:n.chains){
  for(j in 1:S){
    temp.weight=-Inf
    while(weight[i,j]==-Inf){
      theta[i,,j]=rnorm(n.pars,init.pars[j,],.25)
      weight[i,j]=log.dens.like(theta[i,,j],data[[j]])
      if(is.na(weight[i,j]))weight[i,j]=-Inf
    }
  }
  #don't need these
  # mu[i,]=apply(theta[i,link.pars,],1,mean)
  # Sigma[i,]=update.Sigma(i,use.theta=theta[,link.pars,],prior=prior.big)
  #
  
  #do need these below.
  for(k in 1:n.phi.mu)phi[i,k]=update.mu.vector(i,use.core=theta[,unlink.pars[k],],use.sigma=apply(theta[,unlink.pars[k],],1,sd,na.rm=TRUE),prior=prior[[k]])
  for(k in (n.phi.mu+1):(2*n.phi.mu))phi[i,k]=update.sigma.vector(i,use.core=theta[,unlink.pars[k-n.phi.mu],],use.mu=apply(theta[,unlink.pars[k-n.phi.mu],],1,mean,na.rm=TRUE),prior=prior[[k-n.phi.mu]])
  print(paste("Initialization ",round(i/n.chains*100),"% Complete",sep=""))
}

junk=sfLapply(1:n.chains,write.files,use.theta=theta,use.mu=mu,use.Sigma=Sigma,use.phi=phi,use.weight=weight,append=FALSE)

###########################################

grid=expand.grid(1:S,1:n.chains)
n.grid=nrow(grid)

for(i in 2:nmc){
  
temp=sfLapply(1:n.grid,wrap.crossover,idx=grid,pars=1:n.pars,use.theta=array(theta,c(n.chains,n.pars,S)),use.like=array(weight,c(n.chains,S)),use.mu=array(mu,c(n.chains,n.mu)),use.Sigma=array(Sigma,c(n.chains,n.Sigma)),use.phi=array(phi,c(n.chains,n.hpars)),use.data=data)
for(j in 1:n.grid){
weight[grid[j,2],grid[j,1]]=temp[[j]][1]
theta[grid[j,2],,grid[j,1]]=temp[[j]][2:(n.pars+1)]
}

if(i<migrate.duration){
  if(runif(1)<migrate.prob){
    out=sfLapply(1:S,wrapper,use.theta=array(theta,c(n.chains,n.pars,S)),use.like=weight,log.dens=log.dens.like,use.data=data,method="block")
    for(j in 1:S){
    weight[,j]=out[[j]]$weight
    theta[,,j]=out[[j]]$theta
    }
  }
}
# 
#don't need this
# mu=matrix(unlist(sfLapply(1:n.chains,update.mu,use.theta=array(theta[,link.pars,],c(n.chains,n.link.pars,S)),use.mu=array(mu,c(n.chains,n.mu)),use.Sigma=array(Sigma,c(n.chains,n.Sigma)),prior=prior.big )),n.chains,n.mu,byrow=T)
# 
# Sigma=matrix(unlist(sfLapply(1:n.chains,update.Sigma,use.theta=array(theta[,link.pars,],c(n.chains,n.link.pars,S)),prior=prior.big )),n.chains,n.Sigma,byrow=T)

#need these below.
for(k in 1:n.phi.mu){
phi[,k]=unlist(sfLapply(1:n.chains,update.mu.vector,use.core=theta[,unlink.pars[k],],use.sigma=phi[,k+n.phi.mu],prior=prior[[k]]))
}
for(k in (n.phi.mu+1):(2*n.phi.mu)){
phi[,k]=unlist(sfLapply(1:n.chains,update.sigma.vector,use.core=theta[,unlink.pars[k-n.phi.mu],],use.mu=phi[,k-n.phi.mu],prior=prior[[k-n.phi.mu]]))
}

if(any(i==keep.samples))temp=sfLapply(1:n.chains,write.files,use.theta=theta,use.mu=mu,use.Sigma=Sigma,use.phi=phi,use.weight=weight,append=T)
if(i%%10==0)print(i)
}

