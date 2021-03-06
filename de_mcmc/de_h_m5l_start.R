
########################################### Optimize and Initialize
theta=array(NA,c(n.chains,param.N,S,R_max))
weight=array(-Inf,c(n.chains,S,R_max))
weight.l2<-array(-Inf,c(n.chains,S))
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

init.pars=array(NA,c(S,R_max,param.N))

warned.dist1<-FALSE
#print("Using uninformed optimization")
for(j in 1:S){#j<-1
  subid<-as.integer(gsub("SUB","",data[[j]]$SubID))
  
  for (r in 1:s_runs.N[j]){#j=1;r=1
    runid<-data[[j]]$runs[[r]]$runid
    mid<-data[[j]]$runs[[r]]$motivation
    #print(paste0(j,",",r))
    x_s=param.l1.init[j,r,]
    names(x_s) <- par.names
    #what is this below? we're going to need to change this.
    temp.weight=log.dens.like(x_s,data[[j]]$runs[[r]])
    new.x_s=x_s
    a<- -1.0 #gotta try 100 iterations of random generation before we try mixing in an optimized value.
    attemptcount=0
    cur_run_empirical_priors<-results.summary.dt[sid==subid & rid==runid & motivation==mid]
    while(temp.weight==-Inf){
      new.x_s["alpha"]<-rnorm(1,cur_run_empirical_priors[param_name=="alpha_pr",mean],cur_run_empirical_priors[param_name=="alpha_pr",sd])
      new.x_s["thresh"]<-rnorm(1,cur_run_empirical_priors[param_name=="k_pr",mean],cur_run_empirical_priors[param_name=="k_pr",sd])
      new.x_s["tau"]<-rnorm(1,cur_run_empirical_priors[param_name=="tau_pr",mean],cur_run_empirical_priors[param_name=="tau_pr",sd])

      temp.weight=log.dens.like(new.x_s,use.data=data[[j]]$runs[[r]])
      
      #print(paste(j,r))
      a<-min(a+0.01,1)
      attemptcount=attemptcount+1
      if(attemptcount>200){
        warning('difficulty finding rnorm for this subj')
      }
    }
    print(a)
    print(attemptcount)
    
    #init.pars[j,r,]=new.x_s#colMeans(rbind(new.x_s,
                    #     optim(new.x_s,function(x_s,data)-log.dens.like(x_s,data),data=data[[j]]$runs[[r]])$par))#compromise
    init.pars[j,r,]=(1-max(a,0))*new.x_s+(max(a,0))*optim(new.x_s,function(x_s,data)-log.dens.like(x_s,data),
                                                          data=data[[j]]$runs[[r]])$par
    
    if (attemptcount>500){
      stop(paste0("Couldn't optimize for subject ",j,", run ", r))
    }
    #print(init.pars[j,r,])
    #compromise based on trying a value based on random generation but move up toward optimization if the default random generation is unsuitable.
    #only apply optimization if we tried really hard to randomly settle on something and it didn't work.

  }
  print(paste("Optimization ",round(j/S*100),"% (Subject ",j," of ",S,") Complete.",sep=""))
}#so no hypers here.

write(matrix(aperm(init.pars,c(2,1,3)),nrow=prod(dim(init.pars)[1:2])),
        file=paste("MLEs_",save.name,".txt",sep=""),ncolumns=param.N)

#now we initialize.
warned.dist2<-FALSE

shape.to.chains.by.runs<-function(mat){as.matrix(mat,nrow=n.chains,ncol=run_range)}
#controls distribution across chains.
for(i in 1:n.chains){#i<-1
  for(j in 1:S){#j<-1
    for (r in 1:s_runs.N[j]){#r<-1
      run_range<-1:s_runs.N[j]
      print(run_range)
      temp.weight=-Inf
      while(weight[i,j,r]==-Inf){
        theta[i,param.l1.ids$alpha,j,r]=rnorm(1,init.pars[j,r,param.l1.ids$alpha],sqrt(2))
        theta[i,param.l1.ids$thresh,j,r]=rnorm(1,init.pars[j,r,param.l1.ids$thresh],sqrt(2))
        theta[i,param.l1.ids$tau,j,r]=rnorm(1,init.pars[j,r,param.l1.ids$tau],sqrt(2))
        
        if(!warned.dist2){
          warning("I think ideally we should have distributions based on the initial values but currently I don't have those.")
          warned.dist2=TRUE}
        
        weight[i,j,r]=
          log.dens.like(theta[i,,j,r],data[[j]]$runs[[r]])
        if(is.na(weight[i,j,r])) weight[i,j,r]=-Inf
      }
    }
  
    #level 2 initialization
    for(k in 1:param.l2.distributions.N) {#k<-1
      phi_s[i,k,j]=
        update.mu.vector(i,
                         use.core=as.matrix(theta[,unlink.pars[k],j,run_range],nrow=n.chains,ncol=run_range),
                         use.sigma=shape.to.chains.by.runs(apply(shape.to.chains.by.runs(theta[,unlink.pars[k],j,run_range]),1,sd,na.rm=TRUE)),
                         prior=prior.l2[[k]])
    }
    
    for(k in (param.l2.distributions.N+1):(2*param.l2.distributions.N)) {
      warning("Need to modify this. I think that because there are so few runs per subject, this is going to give us widely-varying initial mean values which is likely to be problematic.")
      phi_s[i,k,j]=
        update.sigma.vector(i,
                            use.core=shape.to.chains.by.runs(theta[,unlink.pars[k-param.l2.distributions.N],j,run_range]),
                            use.mu=shape.to.chains.by.runs(apply(shape.to.chains.by.runs(theta[,unlink.pars[k-param.l2.distributions.N],j,run_range]),1,mean,na.rm=TRUE)),
                            prior=prior.l2[[k-param.l2.distributions.N]])
    }
    #likelihood
    weight.l2[i,j]=
      log.dens.like.l2(phi_s[i,,j],theta[i,,j,])
    #cat(weight.l2[i,j])
    
  }
  #mu[i,]=apply(theta[i,link.pars,],1,mean)
  #Sigma[i,]=update.Sigma(i,use.theta=theta[,link.pars,],prior=prior.big)

  
  #we'll need these update commands for both the level 2 and level three variables
  #I don't know whether we'll have to re-write an update mu function or not.
  #may be able to use the same one.
  #for the third-level, we're not passing in theta and sigma.
  #we're passing on the next level of theta and sigma!
  for(g in 1:groups.l3.N){#g<-1
    subjs_in_group_g<-group_by_subject==groups.l3.list[g]
    
    for(k in 1:param.l2.distributions.N) phi_g[i,k,g]=
        update.mu.vector(i,
                         use.core=phi_s[,unlink.pars[k],subjs_in_group_g],
                         use.sigma=apply(phi_s[,unlink.pars[k],subjs_in_group_g],1,sd,na.rm=TRUE),
                         prior=prior.l3[[k]])
    
    for(k in (param.l2.distributions.N+1):(2*param.l2.distributions.N)) phi_g[i,k,g]=
        update.sigma.vector(i,
                            use.core=phi_s[,unlink.pars[k-param.l2.distributions.N],subjs_in_group_g],
                            use.mu=apply(phi_s[,unlink.pars[k-param.l2.distributions.N],subjs_in_group_g],1,mean,na.rm=TRUE),
                            prior=prior.l3[[k-param.l2.distributions.N]])
  }
  
  print(paste("Initialization ",round(i/n.chains*100),"% Complete",sep=""))
}

#WHERE WE'RE AT: TRYING TO GET THE DIMENSIONS RIGHT FOR WRITE.FILES.
#USING testing.R TO DEBUG, SINCE WRITE.FILES WON'T DEBUG PROPERLY, BECAUSE IT'S OVERLOADED.
#n.pars<-param.N*
junk=sfLapply(
  #lapply(
    1:n.chains,
    write.files,
    use.theta=array(aperm(theta,c(1,2,4,3)),c(dim(theta)[1],prod(dim(theta)[c(2,4)]),dim(theta)[3])),
    use.mu=mu,
    use.Sigma=Sigma,
    use.phi=list("phi_g"=matrix(phi_g,nrow=dim(phi_g)[1],ncol=prod(dim(phi_g)[2:3])),
                 "phi_s"=matrix(phi_s,nrow=dim(phi_s)[1],ncol=prod(dim(phi_s)[2:3]))),
    use.weight=weight,append=FALSE
    )

#source("visualize_initial_values.R")
