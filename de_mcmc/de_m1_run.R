de_m1_run <- function(log.dens.like.f,log.dens.prior.f){
  log.dens.post=function(x,use.data,prior)log.dens.prior.f(x,prior) + log.dens.like.f(x,use.data)
  
  ########################################### initialize the chains
  printv("intializing chains...")
  theta<<-array(NA,c(n.chains,n.pars,S))
  weight<<-array(-Inf,c(n.chains,S))
  
  colnames(theta) <- par.names
  printv("exporting...")
  sfExportAll(except=list("theta","weight"))
  
  print("Optimizing...")
  init.pars=matrix(1,S,n.pars)
  for(j in 1:S){
    start_time <- Sys.time()
    x=x.init[j,]
    print(x)
    temp.weight=log.dens.like.f(x,use.data=data[[j]])
    new.x=x
    while(temp.weight==-Inf){
      new.x=rtnorm(n.pars,x,.1,0,c(1,1,Inf,Inf))
      temp.weight=log.dens.like.f(new.x,use.data=data[[j]])
    }
    if(use.optim==TRUE){
      optim_res<-optim(new.x,function(x,...)-log.dens.like.f(x,...),use.data=data[[j]],control=list("maxit"=1000))
      print(optim_res$counts)
      init.pars[j,]=optim_res$par
    }
    end_time <- Sys.time()
    
    print(end_time - start_time)
    if(use.optim==FALSE)init.pars[j,]=new.x
    print(paste("Optimization ",round(j/S*100),"% Complete.",sep=""))
  }
  
  for(i in 1:n.chains){
    for(j in 1:S){
      while(weight[i,j]==-Inf){
        theta[i,,j]=rtnorm(n.pars,init.pars[j,],c(.05,.05,.5,.05),0,c(1,1,Inf,Inf))
        weight[i,j]=log.dens.like.f(theta[i,,j],use.data=data[[j]])
        if(is.na(weight[i,j]))weight[i,j]=-Inf
      }}
    print(paste("Initialization ",round(i/n.chains*100),"% Complete",sep=""))
  }
  
  junk=sfLapply(1:n.chains,write.files,use.theta=theta,use.weight=weight,append=FALSE)
  
  ########################################### run the sampler!
  
  for(i in 1:nmc){
    
    for(j in 1:S){
      temp=matrix(unlist(sfLapply(1:n.chains,crossover,pars=1:n.pars,use.theta=array(theta[,,j],c(n.chains,n.pars)),use.like=weight[,j],use.data=data[[j]],prior=prior,log.dens.like=log.dens.like.f)),n.chains,n.pars+1,byrow=T)
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
  
  
}
