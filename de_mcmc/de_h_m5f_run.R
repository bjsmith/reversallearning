
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

grid.l2<-data.table(grid)[,.N,by=.(Var1,Var3)] %>% .[,.(Var1,Var3)]#subjects*chains.
n.grid.l2<-nrow(grid.l2)

grid.migrate<-data.table(grid)[,.N,by=.(Var1,Var2)] %>% .[,.(Var1,Var2)] #subject*runs

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
# debugSource('~/expdata/bensmith/joint-modeling/code/msm/reversallearning/de_mcmc/de_h_m5b_functions.R')
vis_1<-100
for(i in 2:nmc){#loop through iterations#i<-2
  
  #Not *really* struggling at this level.
  temp=sfLapply(
    #lapply( 
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
  
  #and we are also struggling HERE, in calculating phis at the subject level
  #sfLapply(
  temp.l2=sfLapply(
    #lapply(
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
      #print(paste0("migrating at i=",i))
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
      
      #level 2 migration was causing a strange error where Runs {2, 4} and {1, 3} become perfectly correlated
      #I've taken it out for now.
      #Brandon says it is probably unnecessary.
      #level 2
      out2=#lapply(
        sfLapply(
          1:S,wrapper.l2,use.theta=array(theta,c(n.chains,n.pars,S,R_max)),use.phi_s=phi_s,use.like=weight.l2,log.dens=log.dens.like.l2,method="block")
      
      for(s in 1:S){#s<-5]
        weight.l2[,s]<-out2[[s]]$weight
        phi_s[,,s]<-out2[[s]]$theta
      }
      #lapply(1:12,function(s){phi_s[,,s]==out2[[s]]$theta})
    }
  }
  
  #we are not necessarily struggling HERE, when calculating group-level phis.
  #l3
  for(g in 1:groups.l3.N){#g<-1
    for(k in 1:param.l3.distributions.N){#k<-1
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
  
  if(any(i==keep.samples))temp=#sfLapply(
                                lapply(
                                  1:n.chains,
                                        write.files.3l,
                                        use.theta=theta,
                                         #array(aperm(theta,c(1,4,2,3)),c(dim(theta)[1],prod(dim(theta)[c(2,4)]),dim(theta)[3])),
                                        use.mu=mu,
                                        use.Sigma=Sigma,
                                        use.phi=list("phi_g"=matrix(phi_g,nrow=dim(phi_g)[1],ncol=prod(dim(phi_g)[2:3])),
                                                     "phi_s"=matrix(phi_s,nrow=dim(phi_s)[1],ncol=prod(dim(phi_s)[2:3]))),
                                        use.weight=weight,append=TRUE)
  
  last_time<-Sys.time()
  
  if(last_time-20>last_time_print_progress  || (i %%100)==0){
    print(colMeans(as.data.frame(diagnostic_record)))
    #let's do a visualization every 100 timesteps.
    if(i>vis_1){
      
      pdf(paste(save.dir,save.name,"/status_t",vis_1,".pdf",sep=""),16,9)
      source(paste0(mainDir,"/visualize_initial_values.R"))
      dev.off()
      vis_1<-vis_1+100
    }
    print(i)
    last_time_print_progress<-last_time
  }
  
}
