
logit=function(x){
  log(x/(1-x))
}

invlogit=function(x){
  1/(1+exp(-x))
}

wald.pdf=function(t,alpha,v,theta){
  # alpha is threshold
  # v is drift
  # theta is nondecision time
  # t is the response time
  idx=t>theta
  tmp=numeric(length(idx))
  tmp[idx]=alpha/sqrt(2*pi*(t[idx]-theta)^3) * exp(-(alpha-v*(t[idx]-theta))^2/(2*(t[idx]-theta)))  
  tmp[!idx] <- 0
  tmp
}

wald.cdf=function(t,alpha,v,theta){
  # alpha is threshold
  # v is drift
  # theta is nondecision time
  # t is the response time
  lambda=alpha^2
  mu=alpha/v
  idx=t>theta
  tmp=numeric(length(idx))
  tmp[idx]=pnorm(sqrt(lambda/(t[idx]-theta))*((t[idx]-theta)/mu-1)) + exp(2*lambda/mu) * pnorm(-1*sqrt(lambda/(t[idx]-theta))*((t[idx]-theta)/mu+1))
  tmp[!idx] <- 0
  tmp
}

rddm=function(i,alpha,v,theta){
  tol=.01
  xs=seq(0,10,tol)
  lxs=length(xs)
  pdf1=get.dens.2choice.simple(t=xs,choice=rep(1,lxs),alpha=alpha,v=v[i,],theta=theta)
  pdf2=get.dens.2choice.simple(t=xs,choice=rep(2,lxs),alpha=alpha,v=v[i,],theta=theta)
  cdf1=sapply(1:length(xs),function(x,pdf)sum(pdf[1:x]),pdf=pdf1)*tol
  cdf2=sapply(1:length(xs),function(x,pdf)sum(pdf[1:x]),pdf=pdf2)*tol
  upp=cdf1[length(xs)]
  sel=runif(1)
  if(sel<upp){
    resp=1
    temp=runif(1,0,max(cdf1))
    out=sort(abs(cdf1-temp),index.return=T,decreasing=F)
    rt=xs[out$ix[1]]
  } else {
    resp=2
    temp=runif(1,0,max(cdf2))
    out=sort(abs(cdf2-temp),index.return=T,decreasing=F)
    rt=xs[out$ix[1]]
  }
  c(resp,rt)
}

get.dens.2choice=function(t,choice,alpha,v,theta){
  # t is a vector of times
  # choice is a vector of responses, same length as t
  # alpha and v are vectors of length 2, one for each accumulator
  idx1=(choice==1)
  tmp=numeric(length(idx1))
  tmp[idx1]=wald.pdf(t[idx1],alpha[1],v[idx1,1],theta[1])*(1-wald.cdf(t[idx1],alpha[2],v[idx1,2],theta[2]))
  idx2=(choice==2)
  tmp[idx2]=wald.pdf(t[idx2],alpha[2],v[idx2,2],theta[2])*(1-wald.cdf(t[idx2],alpha[1],v[idx2,1],theta[1]))
  tmp
}

get.dens.2choice.simple=function(t,choice,alpha,v,theta){
  # t is a vector of times
  # choice is a vector of responses, same length as t
  # alpha and v are vectors of length 2, one for each accumulator
  idx1=(choice==1)
  tmp=numeric(length(idx1))
  tmp[idx1]=wald.pdf(t[idx1],alpha[1],v[1],theta[1])*(1-wald.cdf(t[idx1],alpha[2],v[2],theta[2]))
  tmp[!idx1]=wald.pdf(t[!idx1],alpha[2],v[2],theta[2])*(1-wald.cdf(t[!idx1],alpha[1],v[1],theta[1]))
  tmp
}
############################################################

write.files=function(q,use.theta,use.weight,append=TRUE){
  if (file.exists(subDir)){
    setwd(file.path(mainDir, subDir))
  } else {
    dir.create(file.path(mainDir, subDir))
    setwd(file.path(mainDir, subDir))
  }
  for(j in 1:S)write(round(use.theta[q,,j],6),paste("chain",q,"_sub",j,"_lower.txt",sep=""),ncolumns=n.pars,append=append)
  write(round(use.weight[q,],8),paste("chain",q,"_weights.txt",sep=""),ncolumns=S,append=append)
  setwd(mainDir)
}

crossover=function(i,pars,use.theta,use.like,prior,...){
  require(msm)
  use.weight=use.like[i] + log.dens.prior(x=use.theta[i,],prior=prior)
  gamma = runif(1,.5,1)
  index=sample(c(1:n.chains)[-i],2,replace=F)
  theta=use.theta[i,]						
  theta[pars]=use.theta[i,pars] + gamma*(use.theta[index[1],pars]-use.theta[index[2],pars]) + runif(1,-b,b)
  theta=matrix(theta,1,length(theta))
  like=log.dens.like(x=theta,...)
  weight=like + log.dens.prior(x=theta,prior=prior)
  if(is.na(weight))weight=-Inf
  if(runif(1) < exp(weight-use.weight)) {							
    use.theta[i,]=theta
    use.like[i]=like
  }
  c(use.like[i],use.theta[i,])
}

wrapper=function(x,use.theta,use.like,log.dens,use.data,prior,...)migrate(use.theta=array(use.theta[,,x],c(n.chains,n.pars)),use.like=use.like[,x],log.dens=log.dens,use.data=use.data[[x]],prior=prior,...)

migrate=function(use.theta,use.like,log.dens,...){
  pars=dim(use.theta)[2]
  lnum1=sample(c(1:n.chains),1)										# determine how many groups to work with
  lnum2=sort(sample(c(1:n.chains),lnum1,replace=F))							# which groups we will work with
  thetaset=matrix(NA,lnum1,pars)									# initialize
  currentset=propset=propw=currw=numeric(lnum1)
  index=numeric(lnum1)
  for(i in 1:lnum1){
    index[i]=sample(1:n.chains,1,replace=F)	
    thetaset[i,]=use.theta[lnum2[i],] + runif(1,-b,b)				# create a set of these particles to swap
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
      if(runif(1) < exp(propw[i] - currw[i+1])){
        use.theta[lnum2[i+1],]=thetaset[i,]							# swap the first with the last (creating a circle)
        use.like[lnum2[i+1]]=propset[i]
      }}}
  list(weight=use.like,theta=use.theta)
}
