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
  get_init_x_vals<-function(x){
    new.x=x
    #is this appropriate for all the variables? I'm not sure.
    #new.x=rtnorm(n.parinstances,1,0,init.inf.vals()) #BJS to BT: why does this use a truncated normal distribution?
    #randomly produced priors in case we can't get a decent starting point.
    for (s in 1:S){
      new.x[paste0("alpha_s",s)]<-rnorm(1,-3,1)#we start alpha low, as a conservative prior.
      new.x[paste0("thresh_s",s)]<-rnorm(1,x[paste0("thresh_s",s)],x["thresh_s_sigma"])
      new.x[paste0("tau_s",s)]<-rnorm(1,x[paste0("tau_s",s)],x["tau_s_sigma"])
    }
    new.x["alpha_s_mu"]<-rnorm(1,-3,1)
    new.x["thresh_s_mu"]<-rnorm(1,x[paste0("thresh_s_mu")],x[paste0("thresh_s_sigma")])
    new.x["tau_s_mu"]<-rnorm(1,x[paste0("tau_s",s)],x["tau_s_sigma"])
    new.x["alpha_s_sigma"]<-rhalfcauchy(1,1)
    new.x["thresh_s_sigma"]<-rhalfcauchy(1,1)
    new.x["tau_s_sigma"]<-rhalfcauchy(1,1)
    new.x
  }
  while(temp.weight==-Inf){
    new.x=get_init_x_vals(x)
    
    #how do we get a 0 value for SD?
    #warning("We use this truncated normal distribution to estimate new x values, but I'm not sure that this is appropriate for all variables.")
    
    #shouldn't we be getting the priors here?
    temp.weight=log.dens.like.f(new.x,use.data=data)
    temp.weight
  }
  init.pars=NULL
  #while(is.null(init.pars)){
    #tryCatch(try={
      if(use.optim==TRUE)init.pars=optim(new.x,function(x,...)-log.dens.like.f(x,...),use.data=data,control=list("maxit"=1000))$par
  #can specify an option within "control" to specify, e.g., only positive values.
    #},catch={print("optim didn't work the first time around. Trying again...")})
  #}
  
  
  if(use.optim==FALSE)init.pars=new.x
  print(paste("Optimization Complete."))
  #}
  
  for(i in 1:n.chains){#i=1
    while(weight[i]==-Inf){
      param[i,]=new.x=get_init_x_vals(init.pars)
      weight[i]=log.dens.like.f(param[i,],use.data=data)
      if(is.na(weight[i]))weight[i]=-Inf
    }
    print(paste("Initialization ",round(i/n.chains*100),"% Complete",sep=""))
  }
  
  junk=sfLapply(1:n.chains,write.files.h2,use.param=param,
                use.weight=weight,append=FALSE,colnames(param))
  
  ########################################### run the sampler!
  
  for(i in 1:nmc){#i<-1
    #log.dens.like.f<-log.dens.like.h.m1;log.dens.prior.f<-log.dens.prior.h.m1
    #for(j in 1:S){
    temp=matrix(
      unlist(
        sfLapply(1:n.chains,crossover,par_ind=1:n.parinstances,use.param=array(param,c(n.chains,n.parinstances)),use.like=weight,use.data=data,prior=prior,log.dens.like=log.dens.like.f,log.dens.prior=log.dens.prior.f)
        )
      ,n.chains,n.parinstances+1,byrow=T)
    
    weight=temp[,1]
    param=temp[,2:(n.parinstances+1)]
    #}
    
    if(i<migrate.duration){
      if(runif(1)<migrate.prob){
        #how to do this without applying separately for each subject? Difficult, because I don't know what it's actually doing.
        #wrapper is a wrapper for the migrate command.
        #it's unclear whether wrapper can handle our parameter format, but I guess we'll see.
        out=#function(x,use.paramlist,use.like,log.dens,use.data,prior,...)
          migrate(use.paramlist=array(param,c(n.chains,n.pars)),use.like=array(weight,c(n.chains)),log.dens=log.dens.post,use.data=data,prior=prior)

        weight=out$weight
        param=out$param
        
          
        }}
    
    if(any(i==keep.samples))temp=sfLapply(1:n.chains,write.files,use.param=param,use.weight=weight,append=T)
    if(i%%10==0)print(i)
  }
  
  
}
