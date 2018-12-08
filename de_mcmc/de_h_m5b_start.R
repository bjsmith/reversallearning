
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
for(j in 1:S){
  for (r in 1:s_runs.N[j]){#j=1;r=1
    #print(paste0(j,",",r))
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
  print(paste("Optimization ",round(j/S*100),"% (Subject ",j," of ",S,") Complete.",sep=""))
}#so no hypers here.

write(matrix(aperm(init.pars,c(2,1,3)),nrow=prod(dim(init.pars)[1:2])),
        file=paste("MLEs_",save.name,".txt",sep=""),ncolumns=param.N)

#now we initialize.
warned.dist2<-FALSE
warning("Not sure how we oughtta be setting priors for level 2. Should they be based on level 3 current values?")
for(i in 1:n.chains){#i<-1
  for(j in 1:S){#j<-1
    for (r in 1:s_runs.N[j]){#r<-1
      run_range<-1:s_runs.N[j]
      temp.weight=-Inf
      while(weight[i,j,r]==-Inf){
        theta[i,param.l1.ids$alpha,j,r]=rnorm(1,init.pars[j,r,param.l1.ids$alpha],.25)
        theta[i,param.l1.ids$thresh,j,r]=rnorm(1,init.pars[j,r,param.l1.ids$thresh],.25)
        theta[i,param.l1.ids$tau,j,r]=rnorm(1,init.pars[j,r,param.l1.ids$tau],.25)
        
        if(!warned.dist2){
          warning("I think ideally we should have distributions based on the initial values but currently I don't have those.")
          warned.dist2=TRUE}
        
        weight[i,j,r]=
          log.dens.like(theta[i,,j,r],data[[j]]$runs[[r]])
        if(is.na(weight[i,j,r]))weight[i,j,r]=-Inf
      }
    }
    
    #level 2 initialization
    for(k in 1:param.l2.distributions.N) {#k<-1
      phi_s[i,k,j]=
        update.mu.vector(i,
                         use.core=theta[,unlink.pars[k],j,run_range],
                         use.sigma=apply(theta[,unlink.pars[k],j,run_range],1,sd,na.rm=TRUE),
                         prior=prior.l2[[k]])
    }
    
    for(k in (param.l2.distributions.N+1):(2*param.l2.distributions.N)) {
      phi_s[i,k,j]=
        update.sigma.vector(i,
                            use.core=theta[,unlink.pars[k-param.l2.distributions.N],j,run_range],
                            use.mu=apply(theta[,unlink.pars[k-param.l2.distributions.N],j,run_range],1,mean,na.rm=TRUE),
                            prior=prior.l2[[k-param.l2.distributions.N]])
    }
    
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
                 "phi_s"=matrix(phi_s,nrow=dim(phi_g)[1],ncol=prod(dim(phi_s)[2:3]))),
    use.weight=weight,append=FALSE
    )
