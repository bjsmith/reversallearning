de_m1_run <- function(log.dens.like.f,log.dens.prior.f){#log.dens.like.f<-log.dens.like.h.m1;log.dens.prior.f<-log.dens.prior.h.m1
  
  log.dens.post=function(x,use.data,prior)log.dens.prior.f(x,prior) + log.dens.like.f(x,use.data)
  #we'll need this for the crossover function
  log.dens.prior<<-log.dens.prior.f
  ########################################### initialize the chains
  printv("intializing chains...")
  param<<-array(NA,c(n.chains,n.parinstances))
  weight<<-array(-Inf,c(n.chains))
  
  
  printv("exporting...")
  sfExportAll(except=list("param","weight"))
  
  print("Optimizing...(this may take some time)")
  init.pars=matrix(1,n.parinstances)
  #I don't look through becasue we no longer have a single matrix of everything
  #we have a list, some of which are across-subject values, some of which are not.
  x=unlist(x.init)
  colnames(param) <- names(x)
  temp.weight=log.dens.like.f(x,use.data=data)#x=x;use.data=data
  new.x=x
  while(temp.weight==-Inf){
    stop("I think this hasn't been set up properly. Not only are the values probably wrong but the function takes far too long to run!")
    #is this appropriate for all the variables? I'm not sure.
    new.x=rtnorm(n.parinstances,.1,0,init.inf.vals()) #BJS to BT: why does this use a truncated normal distribution?
    #how do we get a 0 value for SD?
    warning("We use this truncated normal distribution to estimate new x values, but I'm not sure that this is appropriate for all variables.")
    
    #shouldn't we be getting the priors here?
    temp.weight=log.dens.like.f(new.x,use.data=data)
  }
  if(use.optim==TRUE)init.pars=optim(new.x,function(x,...)-log.dens.like.f(x,...),use.data=data,control=list("maxit"=1000))$par
  if(use.optim==FALSE)init.pars=new.x
  print(paste("Optimization Complete."))
  #}
  
  for(i in 1:n.chains){#i=1
    while(weight[i]==-Inf){
      param[i,]=rtnorm(n.parinstances,init.pars,param.sd,0,inf.vals)#we gonna have to change this one.
      #Init_pars for sigma values makes no sense; how did we get a sigma value below zero?
      weight[i]=log.dens.like.f(param[i,],use.data=data)
      if(is.na(weight[i]))weight[i]=-Inf
    }
    print(paste("Initialization ",round(i/n.chains*100),"% Complete",sep=""))
  }
  
  junk=sfLapply(1:n.chains,write.files.h2,use.param=param,
                use.weight=weight,append=FALSE,colnames(param))
  
  ########################################### run the sampler!
  
  for(i in 1:nmc){#i<-1
    #log.dens.like.f<-log.dens.like.h.m1
    #for(j in 1:S){
    temp=matrix(
      unlist(
        lapply(1:n.chains,crossover,par_ind=1:n.parinstances,use.param=array(param,c(n.chains,n.parinstances)),use.like=weight,use.data=data,prior=prior,log.dens.like=log.dens.like.f,log.dens.prior=log.dens.prior.f)
        )
      ,n.chains,n.pars+1,byrow=T)
    
    weight=temp[,1]
    param=temp[,2:(n.pars+1)]
    #}
    
    if(i<migrate.duration){
      if(runif(1)<migrate.prob){
        #how to do this without applying separately for each subject? Difficult, because I don't know what it's actually doing.
        #wrapper is a wrapper for the migrate command.
        #it's unclear whether wrapper can handle our parameter format, but I guess we'll see.
        out=wrapper(param=array(param,c(n.chains,n.pars,S)),
                    use.like=array(weight,c(n.chains,S)),
                    log.dens=log.dens.post,
                    use.data=data,
                    prior=prior)
        weight=out$weight
        param=out$param
        
          
        }}
    
    if(any(i==keep.samples))temp=sfLapply(1:n.chains,write.files,use.param=param,use.weight=weight,append=T)
    if(i%%10==0)print(i)
  }
  
  
}
