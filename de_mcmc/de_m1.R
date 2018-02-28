
log.dens.prior=function(x,prior){
  require(MCMCpack)
  names(x) <- par.names
  #priors
  dens=sum(log(dunif(x["alpha"],0,1))) + 
    sum(log(dunif(x["beta"],0,1))) + 
    sum(log(dunif(x["thresh"],0,1000))) +
    sum(log(dunif(x["tau"],0,10))) 
  if(is.na(dens))dens=-Inf
  dens
}

log.dens.like.m1<-function(x,use.data){
  names(x) <- par.names
  #model
  if(TRUE){
    #iterate through all trials.
    nt=length(use.data$choice)
    dens=numeric(nt)
    #100 is the number of slots we have to store cues (images), not individual trials.
    #since we don't record a new EV value each time.
    ev=matrix(0,100,2)
    #record the values at each time point.
    v_t=matrix(0,nt,2)
    for(t in 1:nt){
      if (use.data$choice[t]!=0) {
        # prediction error
        PE   =  use.data$outcome[t] - ev[use.data$cue[t],use.data$choice[t]]
        PEnc = -use.data$outcome[t] - ev[use.data$cue[[t]],3-use.data$choice[[t]]]
        
        # value updating (learning)
        ev[use.data$cue[t],3-use.data$choice[t]] = ev[use.data$cue[t],3-use.data$choice[t]] + as.numeric(x["alpha"]) * PEnc;
        ev[use.data$cue[t],use.data$choice[t]] = ev[use.data$cue[t],use.data$choice[t]] + as.numeric(x["alpha"]) * PE;
        # there is some transformation based on ev and beta needed before a drift rate can be obtained
        v_t[t,]=invlogit(ev[use.data$cue[t],])
      }
    }
    # now pass the matrix of v into the density wrapper, where RT and choice are vectors.
    dens=get.dens.2choice(use.data$rt[use.data$choice!=0],
                          use.data$choice[use.data$choice!=0],
                          c(x["thresh"],x["thresh"]),#is this right?-BJS we need a vector of length 2 for thresh and theta.
                          v_t,
                          c(x["theta"],x["theta"]))#is this right?-BJS
    out=sum(log(dens))
    if(is.na(out))out=-Inf
  } else {
    out=-Inf
  }
  out
}

log.dens.like.m2<-function(x,use.data){
  names(x) <- par.names
  #model
  if(TRUE){
    #iterate through all trials.
    nt=length(use.data$choice)
    dens=numeric(nt)
    #100 is the number of slots we have to store cues (images), not individual trials.
    #since we don't record a new EV value each time.
    ev=matrix(0,100,2)
    #record the values at each time point.
    v_t=matrix(0,nt,2)
    for(t in 1:nt){
      if (use.data$choice[t]!=0) {
        # prediction error
        PE   =  use.data$outcome[t] - ev[use.data$cue[t],use.data$choice[t]]
        PEnc = -use.data$outcome[t] - ev[use.data$cue[[t]],3-use.data$choice[[t]]]
        
        # value updating (learning)
        ev[use.data$cue[t],3-use.data$choice[t]] = ev[use.data$cue[t],3-use.data$choice[t]] + as.numeric(x["alpha"]) * PEnc;
        ev[use.data$cue[t],use.data$choice[t]] = ev[use.data$cue[t],use.data$choice[t]] + as.numeric(x["alpha"]) * PE;
        # there is some transformation based on ev and beta needed before a drift rate can be obtained
        v_t[t,]=invlogit(ev[use.data$cue[t],])
      }
    }
    # now pass the matrix of v into the density wrapper, where RT and choice are vectors.
    dens=get.dens.2choice(use.data$rt[use.data$choice!=0],
                          use.data$choice[use.data$choice!=0],
                          c(x["thresh"],x["thresh"]),#is this right?-BJS we need a vector of length 2 for thresh and theta.
                          v_t,
                          c(x["theta"],x["theta"]))#is this right?-BJS
    out=sum(log(dens))
    if(is.na(out))out=-Inf
  } else {
    out=-Inf
  }
  out
}

log.dens.like <- log.dens.like.m1

log.dens.post=function(x,use.data,prior)log.dens.prior(x,prior) + log.dens.like(x,use.data)

########################################### initialize the chains

theta=array(NA,c(n.chains,n.pars,S))
weight=array(-Inf,c(n.chains,S))

colnames(theta) <- par.names

sfExportAll(except=list("theta","weight"))

init.pars=matrix(1,S,n.pars)
for(j in 1:S){
  x=x.init[j,]
  temp.weight=log.dens.like(x,use.data=data[[j]])
  new.x=x
  while(temp.weight==-Inf){
    new.x=rtnorm(n.pars,x,.1,0,c(1,1,Inf,Inf))
    temp.weight=log.dens.like(new.x,use.data=data[[j]])
  }
  if(use.optim==TRUE)init.pars[j,]=optim(new.x,function(x,...)-log.dens.like(x,...),use.data=data[[j]],control=list("maxit"=1000))$par
  if(use.optim==FALSE)init.pars[j,]=new.x
  print(paste("Optimization ",round(j/S*100),"% Complete.",sep=""))
}

for(i in 1:n.chains){
  for(j in 1:S){
    while(weight[i,j]==-Inf){
      theta[i,,j]=rtnorm(n.pars,init.pars[j,],c(10,10,rep(.1,n.pterms)),0,c(Inf,Inf,rep(1,n.pterms)))
      weight[i,j]=log.dens.like(theta[i,,j],use.data=data[[j]])
      if(is.na(weight[i,j]))weight[i,j]=-Inf
    }}
  print(paste("Initialization ",round(i/n.chains*100),"% Complete",sep=""))
}

junk=sfLapply(1:n.chains,write.files,use.theta=theta,use.weight=weight,append=FALSE)

########################################### run the sampler!

for(i in 1:nmc){
  
  for(j in 1:S){
    temp=matrix(unlist(sfLapply(1:n.chains,crossover,pars=1:n.pars,use.theta=array(theta[,,j],c(n.chains,n.pars)),use.like=weight[,j],use.data=data[[j]],prior=prior)),n.chains,n.pars+1,byrow=T)
    weight[,j]=temp[,1]
    theta[,,j]=temp[,2:(n.pars+1)]
  }
  
  if(i<migrate.duration){
    if(runif(1)<migrate.prob){
      if(S==1){
        out=wrapper(1,use.theta=array(theta,c(n.chains,n.pars,S)),use.like=array(weight,c(n.chains,S)),log.dens=log.dens.post,use.data=data,prior=prior)
        weight[,j]=out$weight
        theta[,,j]=out$theta
      } else {
        out=sfLapply(1:S,wrapper,use.theta=theta,use.like=weight,log.dens=log.dens.post,use.data=data,prior=prior)
        for(j in 1:S){
          weight[,j]=out[[j]]$weight
          theta[,,j]=out[[j]]$theta
        }}}}
  
  if(any(i==keep.samples))temp=sfLapply(1:n.chains,write.files,use.theta=theta,use.weight=weight,append=T)
  if(i%%10==0)print(i)
}

