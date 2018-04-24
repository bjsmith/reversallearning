
########################################### Optimize and Initialize
theta=array(NA,c(n.chains,n.pars,S,R_max))
weight=array(-Inf,c(n.chains,S,R_max))
mu=array(NA,c(n.chains,n.mu))#array(NA,c(n.chains,n.mu))
Sigma=array(NA,c(n.chains))#array(NA,c(n.chains,n.Sigma))
warning("no definitions set for mu or sigma. Do we need definitions for these before we start?")
#phi=array(NA,c(n.chains,n.hpars,n.l2.groups))
phi_s<-array(NA,c(n.chains,param.l2.N,S)) #subject-level phis
dimnames(phi_s)[[2]]<-param.l2.names
dimnames(phi_s)[[3]]<-groups.l2.list

phi_g<-array(NA,c(n.chains,param.l3.N,groups.l3.N))  #group-level phis
dimnames(phi_g)[[2]]<-param.l3.names
dimnames(phi_g)[[3]]<-groups.l3.list


colnames(theta) <- param.l1.names
colnames(phi_s) <- param.l2.names#coincidentally, these are the same, I think???
colnames(phi_g) <- param.l3.names

sfExportAll(except=list("theta","weight"#,"mu","Sigma"
                        ,"phi_s","phi_g"))

init.pars=array(NA,c(S,R_max,n.pars))

warned.dist1<-FALSE
for(j in 1:S){
  for (r in 1:s_runs.N[j]){#j=1;r=1
    x_s=param.l1.init[j,r,]
    names(x_s) <- par.names
    #what is this below? we're going to need to change this.
    temp.weight=log.dens.like(x_s,data[[j]]$runs[[r]])
    new.x_s=x_s
    while(temp.weight==-Inf){
      print(new.x_s)
      new.x_s["alpha"]<-rnorm(1,-3,1)#we start alpha low, as a conservative prior.
      new.x_s["thresh"]<-rnorm(1,x_s["thresh"],sqrt(abs(x_s["thresh"])))
      new.x_s["tau"]<-rnorm(1,x_s["tau"],sqrt(abs(x_s["tau"])))
      if(!warned.dist1){
        warning("I think ideally we should have distributions based on the initial values but currently I don't have those.")
        warned.dist1=TRUE
      }
      
      temp.weight=log.dens.like(new.x_s,use.data=data[[j]]$runs[[r]])
    }
    
    init.pars[j,r,]=optim(new.x_s,function(x_s,data)-log.dens.like(x_s,data),data=data[[j]]$runs[[r]])$par
  }
  print(paste("Optimization ",round(j/S*100),"% Complete.",sep=""))
}#so no hypers here.

write(matrix(aperm(init.pars,c(2,1,3)),nrow=prod(dim(init.pars)[1:2])),
        file=paste("MLEs_",save.name,".txt",sep=""),ncolumns=n.pars)

#now we initialize.
warned.dist2<-FALSE
warning("Not sure how we oughtta be setting priors for level 2. Should they be based on level 3 current values?")
for(i in 1:n.chains){#i<-1
  for(j in 1:S){#j<-1
    for (r in 1:s_runs.N[j]){
      run_range<-1:s_runs.N[j]
      temp.weight=-Inf
      while(weight[i,j,r]==-Inf){
        theta[i,par.ids.l1$alpha,j,r]=rnorm(1,init.pars[j,r,par.ids.l1$alpha],.25)
        theta[i,par.ids.l1$thresh,j,r]=rnorm(1,init.pars[j,r,par.ids.l1$thresh],.25)
        theta[i,par.ids.l1$tau,j,r]=rnorm(1,init.pars[j,r,par.ids.l1$tau],.25)
        
        if(!warned.dist2){
          warning("I think ideally we should have distributions based on the initial values but currently I don't have those.")
          warned.dist2=TRUE}
        
        weight[i,j,r]=
          log.dens.like(theta[i,,j,r],data[[j]]$runs[[r]])
        if(is.na(weight[i,j,r]))weight[i,j,r]=-Inf
      }
    }
    
    #level 2 initialization
    
    for(k in 1:n.phi.mu) {
      phi_s[i,k,j]=
        update.mu.vector(i,
                         use.core=theta[,unlink.pars[k],j,run_range],
                         use.sigma=apply(theta[,unlink.pars[k],j,run_range],1,sd,na.rm=TRUE),
                         prior=prior.l2[[k]])
    }
    
    
    for(k in (n.phi.mu+1):(2*n.phi.mu)) {
      phi_s[i,k,j]=
        update.sigma.vector(i,
                            use.core=theta[,unlink.pars[k-n.phi.mu],j,run_range],
                            use.mu=apply(theta[,unlink.pars[k-n.phi.mu],j,run_range],1,mean,na.rm=TRUE),
                            prior=prior.l2[[k-n.phi.mu]])
    }
    
  }
  #mu[i,]=apply(theta[i,link.pars,],1,mean)
  #Sigma[i,]=update.Sigma(i,use.theta=theta[,link.pars,],prior=prior.big)

  
  #we'll need these update commands for both the level 2 and level three variables
  #I don't know whether we'll have to re-write an update mu function or not.
  #may be able to use the same one.
  #for the third-level, we're not passing in theta and sigma.
  #we're passing on the next level of theta and sigma!
  for(g in 1:groups.l3.N){
    subjs_in_group_g<-group_by_subject==groups.l3.list[g]
    
    for(k in 1:n.phi.mu) phi_g[i,k,g]=
        update.mu.vector(i,
                         use.core=phi_s[,unlink.pars[k],subjs_in_group_g],
                         use.sigma=apply(phi_s[,unlink.pars[k],subjs_in_group_g],1,sd,na.rm=TRUE),
                         prior=prior.l3[[k]])
      
    
    for(k in (n.phi.mu+1):(2*n.phi.mu)) phi_g[i,k,g]=
        update.sigma.vector(i,
                            use.core=phi_s[,unlink.pars[k-n.phi.mu],subjs_in_group_g],
                            use.mu=apply(phi_s[,unlink.pars[k-n.phi.mu],subjs_in_group_g],1,mean,na.rm=TRUE),
                            prior=prior.l3[[k-n.phi.mu]])
  }
  
  
  
  print(paste("Initialization ",round(i/n.chains*100),"% Complete",sep=""))
}

#WHERE WE'RE AT: TRYING TO GET THE DIMENSIONS RIGHT FOR WRITE.FILES.
#USING testing.R TO DEBUG, SINCE WRITE.FILES WON'T DEBUG PROPERLY, BECAUSE IT'S OVERLOADED.
junk=#sfLapply
  lapply(
    1:n.chains,write.files,
              use.theta=array(aperm(theta,c(1,2,4,3)),c(dim(theta)[1],prod(dim(theta)[c(2,4)]),dim(theta)[3])),
              use.mu=mu,
              use.Sigma=Sigma,
              use.phi=list("phi_g"=matrix(phi_g,nrow=dim(phi_g)[1],ncol=prod(dim(phi_g)[2:3])),
                           "phi_s"=matrix(phi_s,nrow=dim(phi_g)[1],ncol=prod(dim(phi_s)[2:3]))),
              use.weight=weight,append=FALSE)

###########################################


# 4*S*24
# data.frame(#rep(rep(1:S,s_runs.N),n.chains), #do a series of 1:S, each repeated s_runs.N times, then repeat that n.chains times.
#            #then do a series for each s_runs.N, from 1 to s_runs.N, then repeat that n.chains times.
#            rep(unlist(sapply(s_runs.N,function(x)1:x)),n.chains),
#            #then do a series of 1:n.chains, each repeated s_runs.N times for each item in s_runs.N
#            #unlist(sapply(s_runs.N,function(s)rep(1:n.chains,each=s_runs.N[s]*S)))
#            #unlist(sapply(s_runs.N,function(s)rep(1:n,each=s_runs.N*S)))
#            
# )

    

#ignore the chains here, we'll repeat later.
#index with one item for each run, subject, etc.
grid.eachchain <- data.frame(Var1=rep(1:S,s_runs.N), Var2=unlist(sapply(s_runs.N,function(x)1:x)))
grid<-grid.eachchain[rep(1:nrow(grid.eachchain),times=n.chains),]
grid$Var3<-rep(1:24,each=sum(s_runs.N))
n.grid=nrow(grid)

param.key<-rbind(data.frame("level"=3,"varname"=paste0(rep(dimnames(phi_g)[[2]],times=length(dimnames(phi_g)[[3]])),"_",rep(dimnames(phi_g)[[3]],each=length(dimnames(phi_g)[[2]])))),
                 data.frame("level"=2,"varname"=param.l2.names),
                 data.frame("level"=1,"varname"=param.l1.names))

write.param.key(param.key)


de_time_start<-Sys.time()
print("Starting...")
last_time_print_progress<-de_time_start
for(i in 2:nmc){#loop through iterations#i<-2
    warning("using lapply instead of sfLapply for debugging")
  temp=#sfLapply(
    lapply(
    1:n.grid,
    wrap.crossover,idx=grid,
    pars=1:n.pars,
    use.theta=theta,#array(theta,c(n.chains,n.pars,S)),
    use.like=weight,#array(weight,c(n.chains,S)),
    use.mu=mu,#array(mu,c(n.chains,n.mu)),
    use.Sigma=Sigma,#array(Sigma,c(n.chains,n.Sigma)),
    use.phi_s=phi_s,#array(phi,c(n.chains,n.hpars)),
    use.phi_g=phi_g,#array(phi,c(n.chains,n.hpars)),
    use.data=data)
  
  for(j in 1:n.grid){#loop through bottom-level items (all runs in the dataset)
    weight[grid[j,3],grid[j,1],grid[j,2]]=temp[[j]][1]
    theta[grid[j,3],grid[j,1],grid[j,2]]=temp[[j]][2:(n.pars+1)]
  }
  
  #decide whether to replace existing estimate with new one?
  if(i<migrate.duration){
    if(runif(1)<migrate.prob){
      out=sfLapply(1:S,wrapper,use.theta=array(theta,c(n.chains,n.pars,S,R_max)),use.like=weight,log.dens=log.dens.like,use.data=data,method="block")
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
  #we do need l2vars.
  
  #l2
  for(s in 1:S){
    for(k in 1:n.phi.mu){
      phi_s[,k,s]=unlist(sfLapply(1:n.chains,
                                update.mu.vector,
                                use.core=theta[,unlink.pars[k],s,],
                                use.sigma=phi_s[,k+n.phi.mu,s],prior=prior.l2[[k]]))
    }
    for(k in (param.l2.distributions.N+1):(2*param.l2.distributions.N)){
      phi_s[,k,g]=unlist(sfLapply(1:n.chains,
                                update.sigma.vector,
                                use.core=theta[,unlink.pars[k-n.phi.mu],s,],
                                use.mu=phi_s[,k-param.l2.distributions.N,g],prior=prior.l2[[k-param.l2.distributions.N]]))
    }
  }
  
  #l3
  for(g in 1:n.l2.groups){
    for(k in 1:n.phi.mu){
      phi_g[,k,g]=unlist(sfLapply(1:n.chains,
                                update.mu.vector,
                                use.core=phi_s[,unlink.pars[k],group_by_subject==l2.groups.list[g]],
                                use.sigma=phi_g[,k+param.l3.distributions.N,g],prior=prior[[k]]))
    }
    for(k in (param.l3.distributions.N+1):(2*param.l3.distributions.N)){
      phi_g[,k,g]=unlist(sfLapply(1:n.chains,
                                update.sigma.vector,
                                use.core=phi_s[,unlink.pars[k-n.phi.mu],group_by_subject==l2.groups.list[g]],
                                use.mu=phi_g[,k-param.l3.distributions.N,g],prior=prior[[k-n.phi.mu]]))
    }
  }
  
  if(any(i==keep.samples))temp=sfLapply(1:n.chains,write.files,use.theta=theta,use.mu=mu,use.Sigma=Sigma,
                                        use.phi=matrix(phi,nrow=dim(phi)[1],ncol=prod(dim(phi)[2:3])),
                                        use.weight=weight,append=T)
  last_time<-Sys.time()
  if(last_time-10>last_time_print_progress){
    #if(i%%10==0)print(i)
    print(i)
    last_time_print_progress<-last_time
  }
  
}

