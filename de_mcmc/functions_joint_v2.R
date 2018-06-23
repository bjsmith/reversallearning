
crossover_hyper=function(i,pars,use.theta,use.phi,data){
use.weight=log.dens.hyper(t(use.theta[i,,]),use.phi[i,],data)

gamma = 2.38/sqrt(2*length(pars))
index=sample(c(1:n.chains)[-i],2,replace=F)
phi=use.phi[i,]
phi[pars]=use.phi[i,pars] + gamma*(use.phi[index[1],pars]-use.phi[index[2],pars]) + runif(1,-b,b)
phi=matrix(phi,1,length(phi))
weight=log.dens.hyper(t(use.theta[i,,]),phi[1,],data)
if(is.na(weight))weight=-Inf
if(runif(1) < exp(weight-use.weight)) {							
use.phi[i,]=phi
}
use.phi[i,]
}

gamma_numerator=2.38#the default; specified here so it can be overridden.

#main function to be called, via its wrapper function 
crossover=function(i,pars,use.theta,use.like,use.data,use.mu,use.Sigma,
                   use.phi,log.dens.prior.f=log.dens.prior,log.dens.like.f=log.dens.like){
  #print(i)
  #print(sum(sum(is.na(use.theta))))
  #this is a package for multi-state and hidden markov models in continuous time.
  require(msm)
  #get the weight to use calculated as the sum of the current likelihood
  #and the calculated density likelihood of parameters given current parameter values and hyperparameters
  #If this here was giving two good fit an estimate then it sets the benchmark unfairly high
  #these are effectively *multiplied* since they're on log scale.
  use.weight=use.like[i] + log.dens.prior.f(
    x=use.theta[i,],
    use.mu=use.mu[i,],
    use.Sigma=use.Sigma[i,],
    use.phi=use.phi[i,])
  #from Turner et al. (2013, on de-MCMC, Equation 3), this is the tuning parameter for DE-MCMC, controlling the magnitude of the jumping distribution
  #gamma = 2.38/sqrt(2*length(pars))
  gamma = gamma_numerator/sqrt(2*length(pars))
  #gamma = 4/sqrt(2*length(pars))
  
  #get two randomly sampled without replacement chains
  index=sample(c(1:n.chains)[-i],2,replace=F)
  
  #recalculate theta values for each parameter
  theta=use.theta[i,]    				
  
  #equation 3 in Turner et al. (2014). Ingenious - by using difference between two randomly chosen values
  #we can kind of bootstrap a variance in our estimation of theta
  #the tiny random noise at the end seems hacky, but what do I know?
  theta[pars]=use.theta[i,pars] + gamma*(use.theta[index[1],pars]-use.theta[index[2],pars]) + runif(1,-b,b)
  theta=matrix(theta,1,length(theta)) #what is this?
  
  #Turner et al. (2014) notes "the stationary distribution of this chain will be the target distribution"
  #which is great - we can sample across chains to get the target distribution at the end.
  
  #define the log density likelihood based on the theta values and data, and the 
  #if this was somehow returning really low likelihoods we would also lose efficiency
  #but it's difficult to see how this could be because it's simply the theta distributions and the data.
  like=log.dens.like.f(x=theta,use.data=use.data)
  
  #add the log density likelihood calculated for the model with the log density likelihood calculated for the link parameters and the hyperparameters
  #so this is quite an imporant pivot point because it combines the likelihood of the parameters based on DATA (log.dens.like) with likelihood of parameters based on HYPERS (log.dens.prior)
  #if this is doing a BAD job then we might lose efficiency
  prior_weight<-log.dens.prior.f(x=theta,use.mu=use.mu[i,],use.Sigma=use.Sigma[i,],use.phi=use.phi[i,])
  weight=like + prior_weight
  
  
  
  if(is.na(weight))weight=-Inf
  
  write.crossover.record<-function(filename){
    use.data.names<-paste0(c(unlist(dimnames(use.data)),unlist(names(use.data))),collapse="")
    write(c(i,like,prior_weight,use.weight,use.data.names),file=paste0(mainDataDir, subDir, filename),ncolumns=5,append=TRUE)
  }
  
  if(is.infinite(weight)){
    
    if(is.infinite(like)){
      write.crossover.record(filename="ln_density_likelihood.txt")

    }
    if(is.infinite(prior_weight)){
      write.crossover.record("ln_density_prior.txt")
    }
  }
  #so here, we're comparing the weight of the current likelihoods ('use.like') and the density from the priors
  #with the weight from the priors and the newly calculated log density likelihood
  #and if the new weight exceeds the existing weight by more than a specified amount, we use the new one!
  if(is.infinite(weight) | is.infinite(use.weight)){
    #print(paste0("weight or use.weight are infinite; ",weight,",",use.weight))
    if(is.infinite(weight) & is.infinite(use.weight)){
      print("At crossover(), both weight and use weight are infinite. This might be a problem. Dumping info:")
      for (param in c(i,pars,use.theta,use.data,use.mu,use.Sigma,
      use.phi)){
        print(param)
      }
      print("----")
    }
  }
  if(!is.infinite(weight) | !is.infinite(use.weight)){
    if(runif(1) < exp(weight-use.weight)) {							
      use.theta[i,]=theta
      use.like[i]=like
      write.crossover.record("updated_weight.txt")
    }else{
      write.crossover.record("no_update_weight.txt")
    }
    
  }
  c(use.like[i],use.theta[i,])
}

crossover_delta=function(i,pars,use.theta,use.like,data=data,use.mu,use.Sigma,use.phi){
require(msm)
use.weight=use.like[i] + log.dens.prior.delta(x=use.theta[i,],use.mu=use.mu[i,],use.Sigma=use.Sigma[i,],use.phi=use.phi[i,])
#gamma = 2.38/sqrt(2*length(pars))
gamma=runif(1,.5,1)
index=sample(c(1:n.chains)[-i],2,replace=F)
theta=use.theta[i,]    				
theta[pars]=use.theta[i,pars] + gamma*(use.theta[index[1],pars]-use.theta[index[2],pars]) + runif(1,-b,b)
theta=matrix(theta,1,length(theta))
like=log.dens.like.delta(x=theta,use.data=data)
weight=like + log.dens.prior.delta(x=theta,use.mu=use.mu[i,],use.Sigma=use.Sigma[i,],use.phi=use.phi[i,])
if(is.na(weight))weight=-Inf
if(runif(1) < exp(weight-use.weight)) {							
use.theta[i,]=theta
use.like[i]=like
}
c(use.like[i],use.theta[i,])
}

crossover_theta=function(i,pars,use.theta,use.like,data=data,use.mu,use.Sigma,use.phi){
  use.weight=use.like[i] + log.dens.prior.theta(x=use.theta[i,],use.mu=use.mu[i,],use.Sigma=use.Sigma[i,],use.phi=use.phi[i,])
  #gamma = 2.38/sqrt(2*length(pars))
  gamma=runif(1,.5,1)
  index=sample(c(1:n.chains)[-i],2,replace=F)
  theta=use.theta[i,]      			
  theta[pars]=use.theta[i,pars] + gamma*(use.theta[index[1],pars]-use.theta[index[2],pars]) + runif(1,-b,b)
  theta=matrix(theta,1,length(theta))
  like=log.dens.like.theta(x=theta,use.data=data)
  weight=like + log.dens.prior.theta(x=theta,use.mu=use.mu[i,],use.Sigma=use.Sigma[i,],use.phi=use.phi[i,])
  if(is.na(weight))weight=-Inf
  if(runif(1) < exp(weight-use.weight)) {							
    use.theta[i,]=theta
    use.like[i]=like
  }
  c(use.like[i],use.theta[i,])
}

#this is the main function run by the the de_joint iterator.
#it wraps the main "crossover" function
#extracts "sub" from idx[x,1], chain from idx[x,2]
#where idx is a subj*chain list where there is 1 row for every subject*chain combination
#selects the theta values and data relevant to the subject
#and mu, sigma, phi values across all.
wrap.crossover=function(x,idx,pars,use.theta,use.like,use.mu,use.Sigma,use.phi,use.data){
  sub=idx[x,1]
  chain=idx[x,2]
  #print(sum(sum(is.na(use.theta))))
  crossover(chain,pars,
            use.theta=array(use.theta[,,sub],c(n.chains,n.pars)),
            use.like=use.like[,sub],
            use.mu=array(use.mu,c(n.chains,n.mu)),
            use.Sigma=array(use.Sigma,c(n.chains,n.Sigma)),
            use.phi=array(use.phi,c(n.chains,n.hpars)),
            use.data=use.data[[sub]])
}

#called second in the main iterative function in de_*
#does some kind of swapping 
migrate=function(use.theta,use.like,log.dens,...){
  pars=dim(use.theta)[2]
  lnum1=sample(c(1:n.chains),1)										# determine how many groups to work with
  lnum2=sort(sample(c(1:n.chains),lnum1,replace=F))							# which groups we will work with
  thetaset=matrix(NA,lnum1,pars)									# initialize
  currentset=propset=propw=currw=numeric(lnum1)
  index=numeric(lnum1)
  #for each of the selected chains, in a randomly selected order
  for(i in 1:lnum1){
    index[i]=sample(1:n.chains,1,replace=F)	
    thetaset[i,]=use.theta[lnum2[i],] + runif(1,-b,b)				# create a set of these particles to swap
    #the log density likelihood function
    propset[i]=log.dens(thetaset[i,],...)
    currentset[i]=use.like[lnum2[i]]
    propw[i]=propset[i]
    currw[i]=currentset[i]
  }
  if(runif(1) < exp(propw[lnum1] - currw[1])){
    use.theta[lnum2[1],]=thetaset[lnum1,]							# swap the first with the last (creating a circle)
    use.like[lnum2[1]]=propset[lnum1]
  }
  if(lnum1!=1){											# make sure we are not done yet
    for(i in 1:(lnum1-1)){		
      #compare the new proposal value with the recent value, and if a condition is met, replace the last used likelihood with the current value.
      if(runif(1) < exp(propw[i] - currw[i+1])){
        use.theta[lnum2[i+1],]=thetaset[i,]							# swap the first with the last (creating a circle)
        use.like[lnum2[i+1]]=propset[i]
  }}}
  list(weight=use.like,theta=use.theta)
}

#wraps the migrate function
wrapper=function(x,use.theta,use.like,log.dens,use.data,method)migrate(use.theta=use.theta[,,x],use.like=use.like[,x],log.dens=log.dens,use.data=data[[x]],method=method)

#sample a new value for mu from the random multivariate normal distibution, using the mean value and a sigma
update.mu=function(i,use.theta,use.mu,use.Sigma,prior){
  require(mvtnorm)
  n.pars=dim(use.theta)[2]
  N=dim(use.theta)[3]
  n.Sigma=length(use.Sigma[i,])
  Sigma=matrix(use.Sigma[i,],sqrt(n.Sigma),sqrt(n.Sigma))
  X=cbind(t(use.theta[i,1:n.pars,]))
  mu0=prior$mu
  mean=(N*apply(X,2,mean) + prior$m*mu0)/(N+prior$m)
  rmvnorm(1,mean,Sigma/(N+prior$m))
}

#sample a sigma distribution
update.Sigma=function(i,use.theta,prior){
  require(MCMCpack)
  n.pars=dim(use.theta)[2]
  X=cbind(t(use.theta[i,1:n.pars,]))
  N=nrow(X)
  xbar=cbind(apply(X,2,mean))
  #get S
  S=cov(X)
  xmat=(xbar-cbind(prior$mu))%*%t(xbar-cbind(prior$mu))
  mat=prior$phi+N*S + N*prior$m/(N+prior$m)*xmat
  temp=riwish(N+prior$n0,mat)
  as.numeric(temp)
}

#this is used when updating the hyper mu values
update.mu.vector=function(x,use.core,use.sigma,prior){
  #x=1;use.core=as.matrix(theta[,unlink.pars[k],j,run_range],nrow=n.chains,ncol=run_range),
  #use.sigma=shape.to.chains.by.runs(apply(shape.to.chains.by.runs(theta[,unlink.pars[k],j,run_range]),1,sd,na.rm=TRUE)),
  #prior=prior.l2[[k]]
  X=use.core[x,]
  n=length(X)
  mu0=prior$mu
  sigma.sq0=prior$sigma^2
  sigma.sq=use.sigma[x]^2
  num=mu0/sigma.sq0 + sum(X)/sigma.sq
  den=1/sigma.sq0 + n/sigma.sq
  mean=num/den
  sigma=den^(-1/2)
  
  #this seems a bit oversimplistic. Do we really simply calculate a hyper as the mean and SD of the density
  res<-rnorm(1,mean,sigma)
  
  if(is.na(res)){
    print(paste0("mean:",mean,"; sigma:",sigma,"; use.core[x,]:",X,"; use.sigma[x]:",use.sigma[x],"; prior:", prior))
  }
  
  return(res)
}

update.sigma.vector=function(x,use.core,use.mu,prior){
  require(MCMCpack)
  X=use.core[x,]
  N=length(X)
  alpha=prior$alpha
  beta=prior$beta
  mu=use.mu[x]
  a=alpha+N/2
  b=beta+sum((X-mu)**2)/2
  sqrt(rinvgamma(1,a,b))
}


write.files = function(q,use.theta,use.mu,use.Sigma,use.phi,use.weight,use.weight.delta,append=TRUE){
  #print(dim(use.theta))
  #print(class(use.phi))
  if (file.exists(paste0(mainDataDir,subDir))){
  setwd(file.path(mainDataDir, subDir))
  } else {
  dir.create(file.path(mainDataDir, subDir))
  setwd(file.path(mainDataDir, subDir))
  }
  for(j in 1:S)write(round(use.theta[q,,j],6),paste("chain",q,"_sub",j,"_lower.txt",sep=""),ncolumns=n.pars,append=append)
  write(round(use.mu[q,],6),paste("chain",q,"_mu.txt",sep=""),ncolumns=n.mu,append=append)
  write(round(use.Sigma[q,],6),paste("chain",q,"_Sigma.txt",sep=""),ncolumns=n.Sigma,append=append)
  #print(use.phi)
  if(class(use.phi)=="list"){
    for (phi_name in names(use.phi)){
      write(round(use.phi[[phi_name]][q,],6),paste("chain",q,"_hyper_",phi_name,".txt",sep=""),ncolumns=dim(use.phi[[phi_name]])[2],append=append)
    }
  }else write(round(use.phi[q,],6),paste("chain",q,"_hyper.txt",sep=""),ncolumns=dim(use.phi)[2],append=append)
  write(round(use.weight[q,],8),paste("chain",q,"_weights.txt",sep=""),ncolumns=S,append=append)
  write(round(use.weight.delta[q,],8),paste("chain",q,"_weights_delta.txt",sep=""),ncolumns=S,append=append)
  setwd(mainDir)
  
}

write.files=function(q,use.theta,use.mu1,use.Sigma1,use.mu2,use.Sigma2,use.phi,use.weight,append=TRUE){
if (file.exists(mainDataDir, subDir)){
setwd(file.path(mainDataDir, subDir))
} else {
dir.create(file.path(mainDataDir, subDir))
setwd(file.path(mainDataDir, subDir))
}
for(j in 1:S)write(round(use.theta[q,,j],6),paste("chain",q,"_sub",j,"_lower.txt",sep=""),ncolumns=n.pars,append=append)
write(round(use.mu1[q,],6),paste("chain",q,"_mu1.txt",sep=""),ncolumns=n.mu1,append=append)
write(round(use.Sigma1[q,],6),paste("chain",q,"_Sigma1.txt",sep=""),ncolumns=n.Sigma1,append=append)
write(round(use.mu2[q,],6),paste("chain",q,"_mu2.txt",sep=""),ncolumns=n.mu2,append=append)
write(round(use.Sigma2[q,],6),paste("chain",q,"_Sigma2.txt",sep=""),ncolumns=n.Sigma2,append=append)
write(round(use.phi[q,],6),paste("chain",q,"_hyper.txt",sep=""),ncolumns=dim(use.phi)[2],append=append)
write(round(use.weight[q,],8),paste("chain",q,"_weights.txt",sep=""),ncolumns=S,append=append)
setwd(mainDir)
}




write.files=function(q,use.theta,use.mu,use.Sigma,use.phi,use.weight,append=TRUE){
  if (file.exists(paste0(mainDataDir,subDir))){
    setwd(file.path(mainDataDir, subDir))
  } else {
    dir.create(file.path(mainDataDir, subDir))
    setwd(file.path(mainDataDir, subDir))
  }
  for(j in 1:S)write(round(use.theta[q,,j],6),paste("chain",q,"_sub",j,"_lower.txt",sep=""),ncolumns=dim(use.theta)[2],append=append)
  if(n.mu>0) write(round(use.mu[q,],6),paste("chain",q,"_mu.txt",sep=""),ncolumns=n.mu,append=append)
  if(n.Sigma>0) write(round(use.Sigma[q,],6),paste("chain",q,"_Sigma.txt",sep=""),ncolumns=n.Sigma,append=append)
  if(class(use.phi)=="list"){
    for (phi_name in names(use.phi)){
      write(round(use.phi[[phi_name]][q,],6),paste("chain",q,"_hyper_",phi_name,".txt",sep=""),ncolumns=dim(use.phi[[phi_name]])[2],append=append)
    }
  }else write(round(use.phi[q,],6),paste("chain",q,"_hyper.txt",sep=""),ncolumns=dim(use.phi)[2],append=append)
  write(round(matrix(use.weight,nrow=dim(use.weight)[1],ncol=prod(dim(use.weight)[2:length(dim(use.weight))]))[q,],8),paste("chain",q,"_weights.txt",sep=""),ncolumns=dim(use.weight)[2],append=append)
  
  setwd(mainDir)
}




# write.files.3l=function(q,use.theta,use.mu,use.Sigma,use.phi,use.weight,append=TRUE){
#   if (file.exists(paste0(mainDataDir,subDir))){
#     setwd(file.path(mainDataDir, subDir))
#   } else {
#     dir.create(file.path(mainDataDir, subDir))
#     setwd(file.path(mainDataDir, subDir))
#   }
#   for(s in 1:S){
#     for (r in 1:s_runs.N[s]){
#       write(round(use.theta[q,,s],6),paste("chain",q,"_sub",s,"_run",r,".txt",sep=""),ncolumns=n.pars,append=append)
#     }
#   }
#   if(n.mu>0) write(round(use.mu[q,],6),paste("chain",q,"_mu.txt",sep=""),ncolumns=n.mu,append=append)
#   if(n.Sigma>0) write(round(use.Sigma[q,],6),paste("chain",q,"_Sigma.txt",sep=""),ncolumns=n.Sigma,append=append)
#   if(class(use.phi)=="list"){
#     for (phi_name in names(use.phi)){
#       write(round(use.phi[[phi_name]][q,],6),paste("chain",q,"_hyper_",phi_name,".txt",sep=""),ncolumns=dim(use.phi[[phi_name]])[2],append=append)
#     }
#   }else write(round(use.phi[q,],6),paste("chain",q,"_hyper.txt",sep=""),ncolumns=dim(use.phi)[2],append=append)
#   write(round(matrix(use.weight,nrow=dim(use.weight)[1],ncol=prod(dim(use.weight)[2:length(dim(use.weight))]))[q,],8),paste("chain",q,"_weights.txt",sep=""),ncolumns=dim(use.weight)[2],append=append)
#   
#   setwd(mainDir)
# }

write.param.key = function(param_key){
  if (file.exists(paste0(mainDataDir,subDir))){
    setwd(file.path(mainDataDir, subDir))
  } else {
    dir.create(file.path(mainDataDir, subDir))
    setwd(file.path(mainDataDir, subDir))
  }
  write.csv(param_key,paste0(mainDataDir,subDir,"param_key.csv"))
}
###########################################################
logit=function(x){
log(x/(1-x))
}

invlogit=function(x){
1/(1+exp(-x))
}

#point density for a particular hypothetical or actual observation 't'
#given an exGaussian distribution defined by mu, sigma, and tau
dexGauss=function(t, mu, sigma, tau){
  1/tau * exp((mu-t)/tau + sigma^2/(2*tau^2)) * pnorm((t-mu)/sigma - sigma/tau)
}

#cumulative probability for a particular hypothetical or actual observation 't'
#given an exGaussian distribution defined by mu, sigma, and tau
pexGauss=function(t, mu, sigma, tau){
  pnorm((t-mu)/sigma) - exp(sigma^2/(2*tau^2)-(t-mu)/tau) * pnorm((t-mu)/sigma - sigma/tau)
}

#these functions calculate the multiplicative of 
#(1- the CUMULATIVE PROBABILITY) for on a distribution modeling the GO parameter, and
#the POINT DENSITY for ti on a distribution modeling the STOP parameter.
#I don't know why they do that. If it helps, these functions are used to find the integral.
fun=function(ti,mu.go,sigma.go,tau.go,mu.stop,sigma.stop,tau.stop){
  #uses the gaussian distributions as defined above
  #get 1- cumulative probability for ti on the go distribution
  a=1-pexGauss(ti,mu.go,sigma.go,tau.go)
  #and point density for ti on the stop distribution
  b=dexGauss(ti,mu.stop,sigma.stop,tau.stop)
  #and find the multiple of those two
  a*b
}
fun=function(ti,mu.go,sigma.go,tau.go,mu.stop,sigma.stop,tau.stop){
  #uses the gaussian distributions in the gamlss.dist package
  a=1-pexGAUS(ti,mu.go,sigma.go,tau.go)
  b=dexGAUS(ti,mu.stop,sigma.stop,tau.stop)
  a*b
}
