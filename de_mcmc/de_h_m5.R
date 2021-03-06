
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
  for (r in 1:s_runs.N[j]){#j=1;r=2
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
    for(k in 1:param.l2.distributions.N) {
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

grid.l2<-data.table(grid)[,.N,by=.(Var1,Var3)] %>% .[,.(Var1,Var3)]
n.grid.l2<-nrow(grid.l2)

grid.migrate<-data.table(grid)[,.N,by=.(Var1,Var2)] %>% .[,.(Var1,Var2)]

param.key<-rbind(data.frame("level"=3,"varname"=paste0(rep(dimnames(phi_g)[[2]],times=length(dimnames(phi_g)[[3]])),"_",rep(dimnames(phi_g)[[3]],each=length(dimnames(phi_g)[[2]])))),
                 data.frame("level"=2,"varname"=param.l2.names),
                 data.frame("level"=1,"varname"=paste0(rep(param.l1.names,times=4),"_r",rep(1:4,each=length(param.l1.names)))))

write.param.key(param.key)

de_time_start<-Sys.time()
print("Starting...")
last_time_print_progress<-de_time_start


#save.image(file=paste0(mainDataDir,"de_h_m5_testing.RData"))

#source("../joint_msm_combined/bjs_misc_utils.R")
#setwd("/expdata/bensmith/joint-modeling/code/msm/reversallearning/")
#load(file=paste0("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/de_h_m5_testing.RData"))


#
# debugSource('~/expdata/bensmith/joint-modeling/code/msm/reversallearning/de_mcmc/functions_joint_v2.R')
# debugSource('~/expdata/bensmith/joint-modeling/code/msm/reversallearning/de_mcmc/functions_h_m5.R')
# debugSource('~/expdata/bensmith/joint-modeling/code/msm/reversallearning/de_mcmc/de_h_m5_functions.R')

for(i in 2:nmc){#loop through iterations#i<-12
  temp=lapply(#sfLapply(
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
    weight[grid[j,3], grid[j,1], grid[j,2]] = temp[[j]][1]
    theta[grid[j,3],, grid[j,1], grid[j,2]] = temp[[j]][2:(n.pars+1)]
  }
  #sfLapply(
  temp.l2=lapply(
    1:n.grid.l2,
    wrap.crossover.l2,idx=grid.l2,
    pars=1:param.l2.N,
    use.theta=theta,#array(theta,c(n.chains,n.pars,S)),
    use.like=weight.l2,#array(weight,c(n.chains,S)),
    use.mu=mu,#array(mu,c(n.chains,n.mu)),
    use.Sigma=Sigma,#array(Sigma,c(n.chains,n.Sigma)),
    use.phi_s=phi_s,#array(phi,c(n.chains,n.hpars)),
    use.phi_g=phi_g,#array(phi,c(n.chains,n.hpars)),
    use.data=data)
  
  for(j in 1:n.grid.l2){#loop through second-level items (all runs in the dataset)#j<-1
    weight.l2[grid.l2[[j,2]], grid.l2[[j,1]]] = temp.l2[[j]][1]
    phi_s[grid.l2[[j,2]], , grid.l2[[j,1]]] = temp.l2[[j]][2:(param.l2.N+1)]
  }
  
  #migrate
  if(i<migrate.duration){
    if(runif(1)<migrate.prob){
      out=sfLapply(
        #lapply(
        1:nrow(grid.migrate),
        wrapper,idx=grid.migrate,
        use.theta=array(theta,c(n.chains,n.pars,S,R_max)),
        use.like=weight,
        log.dens=log.dens.like,
        use.data=data,
        method="block")
      
      for(j in 1:nrow(grid.migrate)){#loop through bottom-level items (all runs in the dataset)
        weight[,grid.migrate[[j,1]], grid.migrate[[j,2]]] = out[[j]]$weight
        theta[,,grid.migrate[[j,1]], grid.migrate[[j,2]]] = out[[j]]$theta
      }
      
      #level 2
      
      out2=#lapply(
        sfLapply(
          1:S,wrapper.l2,use.theta=array(theta,c(n.chains,n.pars,S,R_max)),use.phi_s=phi_s,use.like=weight,log.dens=log.dens.like.l2,method="block")
      # if(is.list(out2[[1]])){
      #   print("out2 is list")
      # }else{
      #   print("out2 is not list")
      # }
      for(s in 1:S){#s<-1]
        weight[,s,]<-out2[[s]]$weight
        theta[,,s,]<-out2[[s]]$theta
      }
      # tryCatch(
      #   print("huzzah"),error=function(e){
      #     print(e)
      #     print(dim(weight))
      #     print(weight)
      #     print(out[[s]]$weight)
      #     stop(e)
      #   })
    }
  }
  
  #l3
  for(g in 1:groups.l3.N){#g<-1
    for(k in 1:param.l3.distributions.N){
      phi_g[,k,g]=unlist(sfLapply(1:n.chains,
                                  update.mu.vector,
                                  use.core=phi_s[,unlink.pars[k],group_by_subject==groups.l3.list[g]],
                                  use.sigma=phi_g[,k+param.l3.distributions.N,g],
                                  prior=prior.l3[[k]]))
    }
    for(k in (param.l3.distributions.N+1):(2*param.l3.distributions.N)){
      phi_g[,k,g]=unlist(sfLapply(1:n.chains,
                                  update.sigma.vector,
                                  use.core=phi_s[,unlink.pars[k-param.l3.distributions.N],group_by_subject==groups.l3.list[g]],
                                  use.mu=phi_g[,k-param.l3.distributions.N,g],
                                  prior=prior.l3[[k-param.l3.distributions.N]]))
    }
  }
  
  if(any(i==keep.samples))temp=sfLapply(1:n.chains,
                                        write.files.3l,
                                        use.theta=array(aperm(theta,c(1,4,2,3)),c(dim(theta)[1],prod(dim(theta)[c(2,4)]),dim(theta)[3])),
                                        use.mu=mu,
                                        use.Sigma=Sigma,
                                        use.phi=list("phi_g"=matrix(phi_g,nrow=dim(phi_g)[1],ncol=prod(dim(phi_g)[2:3])),
                                                     "phi_s"=matrix(phi_s,nrow=dim(phi_g)[1],ncol=prod(dim(phi_s)[2:3]))),
                                        use.weight=weight,append=FALSE
  )
  last_time<-Sys.time()
  if(last_time-10>last_time_print_progress){
    print(i)
    last_time_print_progress<-last_time
  }
  
}
