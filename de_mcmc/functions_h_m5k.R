#this is the main function run by the the de_joint iterator.
#it wraps the main "crossover" function
#extracts "sub" from idx[x,1], chain from idx[x,2]
#where idx is a subj*chain list where there is 1 row for every subject*chain combination
#selects the theta values and data relevant to the subject
#and mu, sigma, phi values across all.
#for the three-level model, we should make idx a subj*run*chain list.
wrap.crossover=function(x,idx,pars,use.theta,use.like,use.mu,use.Sigma,use.phi_s,use.phi_g,use.data){
  sub=idx[x,1]
  run=idx[x,2]
  chain=idx[x,3]
  #to modify this for our three-level model we're to somehow run this for subject and run
  #I believe it still makes sense to run this separately for each subject and run.
  new.lt.pair <- crossover(chain,pars,
            use.theta=array(use.theta[,,sub,run],c(n.chains,n.pars)),
            use.like=use.like[,sub,run],
            use.mu=array(use.mu,c(n.chains,n.mu)),
            use.Sigma=array(use.Sigma,c(n.chains,n.Sigma)),
            #we're concatenating the phi vars at different levels. I *think* this is OK...
            #use.phi=array(cbind(use.phi_g[,,use.data[[idx[x,1]]]$group],use.phi_s[,,use.data[[idx[x,1]]]$SubID]),
            #              c(n.chains,param.l3.N+param.l2.N)),
            #just going to pass one level
            use.phi=array(use.phi_s[,,use.data[[idx[x,1]]]$SubID],
                          c(n.chains,param.l2.N)),
            use.data=use.data[[sub]]$runs[[run]]
            )
  #paste0("datasize",paste0(dim(use.data),collapse="_"))
  if(all(use.like[chain,sub,run]==new.lt.pair[[1]])){
    diagnostic_record$crossover_l1_noupdate[sub]<<-diagnostic_record$crossover_l1_noupdate[sub]+1
  }else{
    diagnostic_record$crossover_l1_update[sub]<<-diagnostic_record$crossover_l1_update[sub]+1
  }
  
  return(new.lt.pair)
  
}

wrap.crossover.l2=function(x,idx,pars,use.theta,use.like,use.mu,use.Sigma,use.phi_s,use.phi_g,use.data){
  sub=idx[[x,1]]
  chain=idx[[x,2]]
  #to modify this for our three-level model we're to somehow run this for subject and run
  #I believe it still makes sense to run this separately for each subject and run.
  # tryCatch(
  #####ONE PROBLEM HERE IS THAT THE CROSSOVER ALGORITHM SUBTRACTS ONE CHAIN'S THETA FROM ANOTHER'S
  #####WHEN WE PASS USE_PHI_S INTO THE THETA VALUE WE CREATE A POTENTIAL FOR THE SIGMAS TO BE NEGATIVE, WHICH IS NONSENSICAL
  #####BRANDON SAYS WE CAN ALSO JUST REJECT ANY DISTRIBUTION WHERE A SIGMA ENDS UP NEGATIVE.
  ####IS USE.SUB WRITTEN CORRECTLY HERE?
  new.lt.pair <- crossover(chain,pars,
            use.theta=array(use.phi_s[,,sub],c(n.chains,param.l2.N)),
            use.like=use.like[,sub],
            use.mu=array(use.mu,c(n.chains,n.mu)),
            use.Sigma=array(use.Sigma,c(n.chains,n.Sigma)),
            #we're concatenating the phi vars at different levels. I *think* this is OK...
            use.phi=array(cbind(use.phi_g[,,use.data[[sub]]$group]),
                          c(n.chains,param.l3.N)),
            use.data=use.theta[chain,,sub,],#gotta set the chain here because it's not selected for in the crossover function.
            log.dens.prior.f=log.dens.prior.l2,
            log.dens.like.f=log.dens.like.l2
   )
  
  if(all(use.like[chain,sub]==new.lt.pair[[1]])){
    diagnostic_record$crossover_l2_noupdate[sub]<<-diagnostic_record$crossover_l2_noupdate[sub]+1
  }else{
    diagnostic_record$crossover_l2_update[sub]<<-diagnostic_record$crossover_l2_update[sub]+1
  }
  return(new.lt.pair)
}

wrapper=function(x,idx,use.theta,use.like,log.dens,use.data,method){
  sub=idx[[x,1]]
  run=idx[[x,2]]
  res<-migrate(use.theta=use.theta[,,sub,run],use.like=use.like[,sub,run],log.dens=log.dens,use.data=use.data[[sub]]$run[[run]],method=method)
  return(res)
}

wrapper.l2=function(x,use.phi_s,use.like,log.dens,use.theta,method){
  #Difference between this and crossover is that we don't call specific chain external to migrate.
  #If we're migrating between CHAINS not SUBJECTS...which IS what is going on...then
  #why was migration responsible for that weird effect of syncing up runs {1,3}, {2,4}????????
  return(migrate(
    use.theta=use.phi_s[,,x],#use.theta[,,x],
    use.like=use.like[,x],
    log.dens=log.dens,
    use.data=use.theta[,,x,],#data[[x]],
    method=method))
}

sample.m<-function(s,...){
  #handle a special sampling case where we may pass a vector of length 1 in; if we do that, we want value out, not a sampling from 1 to the value of the vector's element.
  if(length(s)==1)return(s)
  return(sample(s,...))
}

#really watned to avoid creating a custom migrate function
#however this one is not fit for purpose migrating using the array structure I created...
#called second in the main iterative function in de_*
#does some kind of swapping 
migrate=function(use.theta,use.like,log.dens,use.data,...){
  
  pars=dim(use.theta)[2]
  n.chains.to.swap=sample(c(1:n.chains),1)
  source.chain.indices=sort(sample(c(1:n.chains),n.chains.to.swap,replace=F))							# which chains we will work with
  repeat{
    #cat(".")
    target.chain.indices=sample(c(1:n.chains),n.chains.to.swap,replace=F)
    if(all(source.chain.indices!=target.chain.indices)){
      #cat("\n")
      break #break out if we successfully found a pair of entirely non-matches
    }
  }
  
  
  thetaset=matrix(NA,n.chains.to.swap,pars)									# initialize
  currentset=propset=propw=currw=numeric(n.chains.to.swap)
  #for each of the selected chains, in a randomly selected order
  for(i in 1:n.chains.to.swap){ #iterate n.chains.to.swap number of times (ultimately iterating through source.chain.indices, the 'current' chain we're proposing to replace)

    thetaset[i,]=use.theta[source.chain.indices[i],] + runif(1,-b,b)				# thetas for a particular run*chain.
    #we pick a thetaset, calculate the log density likelihood for it, and hten compare it to the CORRESPONDING likelihood
    #both the likelihood and the thetas we picked correspond to source.chain.indices.
    
    if(!is.array(use.data)){
      #added this in because we have to cope with different levels
      ud<-use.data
    } else if(length(dim(use.data))==3){
      #ud<-use.data[source.chain.indices[i],,] #Select the 'data' relating to this specific chain for testing.
      ud<-use.data[target.chain.indices[i],,] #Select the 'data' relating to the chain that this chain would replace.
    }else{
      stop("Unknown data format.")
    }

    #the log density likelihood function
    propset[i]=log.dens(thetaset[i,], use.data=ud, ...) #fitting hte randomly sampled theta
    currentset[i]=use.like[target.chain.indices[i]] #and the current likelihood, specified by source.chain.indices
    propw[i]=propset[i]
    currw[i]=currentset[i]
  }
  for(i in 1:(n.chains.to.swap)){		
    #compare the new proposal value with the recent value, and if a condition is met, replace the last used likelihood with the current value.
    if(runif(1) < exp(propw[i] - currw[i])){
      #this is where the cycle happens!
      use.theta[target.chain.indices[i],]=thetaset[i,]							# swap the first with the last (creating a circle)
      use.like[target.chain.indices[i]]=propset[i]
    }}
  return(list(weight=use.like,theta=use.theta))
}

write.files.3l=function(q,use.theta,use.mu,use.Sigma,use.phi,use.weight,append=TRUE){
  if (file.exists(paste0(mainDataDir,subDir))){
    setwd(file.path(mainDataDir, subDir))
  } else {
    dir.create(file.path(mainDataDir, subDir))
    setwd(file.path(mainDataDir, subDir))
  }
  
  for(s in 1:S){
    for (r in 1:s_runs.N[s]){
      #run-level
      write(round(use.theta[q,,s,r],6),paste("chain",q,"_sub",s,"_run",r,".txt",sep=""),ncolumns=n.pars,append=append)
    }
    #subject-level?
    #write(round(use.phi$phi_s[q,,s],6),paste("chain",q,"_sub",s,".txt",sep=""),ncolumns=n.pars,append=append)
  }
  #group level
  if(n.mu>0) write(round(use.mu[q,],6),paste("chain",q,"_mu.txt",sep=""),ncolumns=n.mu,append=append)
  if(n.Sigma>0) write(round(use.Sigma[q,],6),paste("chain",q,"_Sigma.txt",sep=""),ncolumns=n.Sigma,append=append)
  if(class(use.phi)=="list"){
    #this includes subject-level and group-level
    for (phi_name in names(use.phi)){
      write(round(use.phi[[phi_name]][q,],6),paste("chain",q,"_hyper_",phi_name,".txt",sep=""),ncolumns=dim(use.phi[[phi_name]])[2],append=append)
    }
  }else write(round(use.phi[q,],6),paste("chain",q,"_hyper.txt",sep=""),ncolumns=dim(use.phi)[2],append=append)
  write(round(matrix(use.weight,nrow=dim(use.weight)[1],ncol=prod(dim(use.weight)[2:length(dim(use.weight))]))[q,],8),paste("chain",q,"_weights.txt",sep=""),ncolumns=dim(use.weight)[2],append=append)

  setwd(mainDir)
}


#transformation functions to transform variables from our estimation space into the space we need them for calculations.
f_alpha_s_tr<-function(alpha){invlogit(alpha)}
f_thresh_s_tr<-function(thresh){exp(thresh)}
f_tau_s_tr<-function(tau){exp(tau)}

diagnostic_record<-list()


