
log.dens.prior=function(x_s,use.mu,use.Sigma,
                        use.phi
                        ){#x_s<-use.theta[i,];use.mu<-use.mu[i,];use.phi=use.phi[i,]
  require(MCMCpack)
  #names(x) <- par.names
  #priors
  #what scale are these on; how do they relate to the absolute values here?
  #SOMEHOW the code we are using here should pass in values that are related to each individual subject.
  #add phi values here.
  dens=sum(dnorm(x_s[par.ids.l1[["alpha"]]],use.phi[par.ids.l2[["alpha_mu"]]],use.phi[par.ids.l2[["alpha_sigma"]]],log=TRUE)) + 
    sum(dnorm(x_s[par.ids.l1[["thresh"]]],use.phi[par.ids.l2[["thresh_mu"]]],use.phi[par.ids.l2[["thresh_sigma"]]],log=TRUE)) +
    sum(dnorm(x_s[par.ids.l1[["tau"]]],use.phi[par.ids.l2[["tau_mu"]]],use.phi[par.ids.l2[["tau_sigma"]]],log=TRUE)) 
  
  if(is.na(dens))dens=-Inf
  
  dens
}

check.pars=function(x_s,use.data_s){
  return(TRUE)
}
log.dens.like=function(x_s,use.data_s,method="full"){
  cat("*")
  #transformation functions
  f_alpha_s_tr<-function(alpha){invlogit(alpha)}
  f_thresh_s_tr<-function(thresh){exp(thresh)}
  f_tau_s_tr<-function(tau){exp(tau)}
  #model
  dens=0
  if(check.pars(x=x,use.data_s=use.data_s)){
    #iterate through all trials.
    #one approach is to write a separate function
      nt=length(use.data_s$choice)
      #print(paste0("processing sub ",s))
      #100 is the number of slots we have to store cues (images), not individual trials.
      #since we don't record a new EV value each time.
      ev=matrix(0,100,2)
      #record the values at each time point.
      v_t=matrix(0,nt,2)
      alpha_tr<-f_alpha_s_tr(as.numeric(x_s[which(par.names=="alpha")]))
      for(tr in 1:nt){#tr=1;tr=tr+1
        #print (tr)
        #start_time <- Sys.time()
        if (use.data_s$choice[tr]!=0) {
          
          #this must come first - this represents the choice being made.
          # there is some transformation based on ev and beta needed before a drift rate can be obtained
          v_t[tr,]=invlogit(ev[use.data_s$cue[tr],])
          
          # prediction error
          PE   =  use.data_s$outcome[tr] - ev[use.data_s$cue[tr],use.data_s$choice[tr]]
          PEnc = -use.data_s$outcome[tr] - ev[use.data_s$cue[tr],3-use.data_s$choice[tr]]
          
          # value updating (learning)
          ev[use.data_s$cue[tr],3-use.data_s$choice[tr]] = 
            ev[use.data_s$cue[tr],3-use.data_s$choice[tr]] + alpha_tr * PEnc;
          ev[use.data_s$cue[tr],use.data_s$choice[tr]] = 
            ev[use.data_s$cue[tr],use.data_s$choice[tr]] + alpha_tr * PE;
          
        }
      }
      #end_time <- Sys.time()
      #log.dens.like.h.m1.timer.sectionA<<-log.dens.like.h.m1.timer.sectionA+(end_time-start_time)
      #start_time <- Sys.time()
      
      #save(list(list(use.data_s),list(x),list(v_t)), file=paste0(dd, "test_de_h_m1_config.Rdata"))
      #stop("stopping here so I can use profvis well.")
      #print("hello")
      #print(x)
      thresh_s_tr<-f_thresh_s_tr(x_s[which(par.names=="thresh")])
      tau_s_tr<-f_tau_s_tr(x_s[which(par.names=="tau")] )
      dens.s<-sum(log(get.dens.2choice(t=use.data_s$rt[use.data_s$choice!=0],
                                       choice=use.data_s$choice[use.data_s$choice!=0],
                                       alpha=c(thresh_s_tr,thresh_s_tr),
                                       v=v_t[use.data_s$choice!=0,],
                                       theta=c(tau_s_tr,tau_s_tr)
      )))
      dens=dens+dens.s#is this right?-BJS
      if(is.nan(dens.s)){
        print("NaN density produced. check out why.")
      }
      #end_time <- Sys.time()
      #log.dens.like.h.m1.timer.sectionB<<-log.dens.like.h.m1.timer.sectionB+(end_time-start_time)
      #printv("running getting density")
      # now pass the matrix of v into the density wrapper, where RT and choice are vectors.
      #we use this to calculate the density associated with the model
      #
    #what about the density for the group-level variables?
    #I think we can just do density for the subject-level variables; and since they are being fit from the group-level,
    #that in itself tells us how close we're getting.
    #we have no group-level 'data' to fit so it wouldn't make sense to get any density for hte group level.

    out=dens
    if(is.na(out))out=-Inf
  } else {
    out=-Inf
  }
  out
}

########################################### Optimize and Initialize
theta=array(NA,c(n.chains,n.pars,S))
weight=array(-Inf,c(n.chains,S))
mu=array(NA,c(n.chains,n.mu))
Sigma=array(NA,c(n.chains,n.Sigma))
warning("no definitions set for mu or sigma. Do we need definitions for these before we start?")
phi=array(NA,c(n.chains,n.hpars))

colnames(theta) <- par.names
colnames(phi) <- hpar.names

sfExportAll(except=list("theta","weight"#,"mu","Sigma"
                        ,"phi"))

init.pars=matrix(NA,S,n.pars)

warned.dist1<-FALSE
for(j in 1:S){
  x_s=x.init[j,]
  names(x_s) <- par.names
  #what is this below? we're going to need to change this.
  temp.weight=log.dens.like(x_s,data[[j]])
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
    
    temp.weight=log.dens.like(new.x_s,use.data=data[[j]])
  }
  
  init.pars[j,]=optim(new.x_s,function(x_s,data)-log.dens.like(x_s,data),data=data[[j]])$par
  print(paste("Optimization ",round(j/S*100),"% Complete.",sep=""))
}#so no hypers here.

write(t(init.pars),file=paste("MLEs_",save.name,".txt",sep=""),ncolumns=n.pars)

#now we initialize.
warned.dist2<-FALSE
for(i in 1:n.chains){#i<-1
  for(j in 1:S){#j<-1
    temp.weight=-Inf
    while(weight[i,j]==-Inf){
      theta[i,par.ids.l1$alpha,j]=rnorm(1,init.pars[j,par.ids.l1$alpha],.25)
      theta[i,par.ids.l1$thresh,j]=rnorm(1,init.pars[j,par.ids.l1$thresh],.25)
      theta[i,par.ids.l1$tau,j]=rnorm(1,init.pars[j,par.ids.l1$tau],.25)
      
      if(!warned.dist2){
        warning("I think ideally we should have distributions based on the initial values but currently I don't have those.")
        warned.dist2=TRUE}
      weight[i,j]=
        log.dens.like(theta[i,,j],data[[j]])
      if(is.na(weight[i,j]))weight[i,j]=-Inf
    }
  }
  #mu[i,]=apply(theta[i,link.pars,],1,mean)
  #Sigma[i,]=update.Sigma(i,use.theta=theta[,link.pars,],prior=prior.big)

  #do need these below.
  #but then we need the unlink parameters - what are those?
  for(k in 1:n.phi.mu)phi[i,k]=update.mu.vector(i,use.core=theta[,unlink.pars[k],],use.sigma=apply(theta[,unlink.pars[k],],1,sd,na.rm=TRUE),prior=prior[[k]])
  for(k in (n.phi.mu+1):(2*n.phi.mu))phi[i,k]=update.sigma.vector(i,use.core=theta[,unlink.pars[k-n.phi.mu],],use.mu=apply(theta[,unlink.pars[k-n.phi.mu],],1,mean,na.rm=TRUE),prior=prior[[k-n.phi.mu]])
  print(paste("Initialization ",round(i/n.chains*100),"% Complete",sep=""))
}

junk=sfLapply(1:n.chains,write.files,use.theta=theta,use.mu=mu,use.Sigma=Sigma,use.phi=phi,use.weight=weight,append=FALSE)

###########################################

grid=expand.grid(1:S,1:n.chains)
n.grid=nrow(grid)

for(i in 2:nmc){#i<-2
    
  temp=sfLapply(
    #lapply(
    1:n.grid,
    wrap.crossover,idx=grid,pars=1:n.pars,
    use.theta=theta,#array(theta,c(n.chains,n.pars,S)),
    use.like=weight,#array(weight,c(n.chains,S)),
    use.mu=mu,#array(mu,c(n.chains,n.mu)),
    use.Sigma=Sigma,#array(Sigma,c(n.chains,n.Sigma)),
    use.phi=phi,#array(phi,c(n.chains,n.hpars)),
    use.data=data)
  
  for(j in 1:n.grid){
  weight[grid[j,2],grid[j,1]]=temp[[j]][1]
  theta[grid[j,2],,grid[j,1]]=temp[[j]][2:(n.pars+1)]
  }
  
  if(i<migrate.duration){
    if(runif(1)<migrate.prob){
      out=sfLapply(1:S,wrapper,use.theta=array(theta,c(n.chains,n.pars,S)),use.like=weight,log.dens=log.dens.like,use.data=data,method="block")
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
  
  
  #need these below.
  for(k in 1:n.phi.mu){
    phi[,k]=unlist(sfLapply(1:n.chains,update.mu.vector,use.core=theta[,unlink.pars[k],],use.sigma=phi[,k+n.phi.mu],prior=prior[[k]]))
  }
  
  for(k in (n.phi.mu+1):(2*n.phi.mu)){
  phi[,k]=unlist(sfLapply(1:n.chains,update.sigma.vector,use.core=theta[,unlink.pars[k-n.phi.mu],],use.mu=phi[,k-n.phi.mu],prior=prior[[k-n.phi.mu]]))
  }
  
  if(any(i==keep.samples))temp=sfLapply(1:n.chains,write.files,use.theta=theta,use.mu=mu,use.Sigma=Sigma,use.phi=phi,use.weight=weight,append=T)
  if(i%%10==0)print(i)
}

