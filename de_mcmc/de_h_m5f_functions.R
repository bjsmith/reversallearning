require(MCMCpack)
require(dplyr)

diagnostic_record$NaN_densities_l1<-0
diagnostic_record$NaN_densities_l2<-0
diagnostic_record$NaN_density_likelihood<-0
diagnostic_record$base_rate_density_likelihood<-0
diagnostic_record$crossover_l2_noupdate<-rep(0,S)
diagnostic_record$crossover_l2_update<-rep(0,S)
diagnostic_record$crossover_l1_noupdate<-rep(0,S)
diagnostic_record$crossover_l1_update<-rep(0,S)

#this may be problematic
log.dens.prior=function(x_s,
                        use.mu,
                        use.Sigma,
                        use.phi
){#phi includes level 2 and level 3 phi values.
  #add phi values here.
  #we have to add param.l3.N because use.phi is a concatenated vector of group-level and subject-level parameters.
  dens.prior.code<-function(){
    sum(dnorm(x_s[param.l1.ids[["alpha"]]], use.phi[param.l2.ids[["alpha_s_mu"]]], abs(use.phi[param.l2.ids[["alpha_s_sigma"]]]),log=TRUE)) + 
    sum(dnorm(x_s[param.l1.ids[["thresh"]]],use.phi[param.l2.ids[["thresh_s_mu"]]],abs(use.phi[param.l2.ids[["thresh_s_sigma"]]]),log=TRUE)) +
    sum(dnorm(x_s[param.l1.ids[["tau"]]],   use.phi[param.l2.ids[["tau_s_mu"]]],   abs(use.phi[param.l2.ids[["tau_s_sigma"]]]),log=TRUE))
  }
  #this density hack will be OK, I think. It will ensure that the phi variables can be swappeed on the crossover 
  
  dens<-tryCatch(
    dens.prior.code()
    ,warning=function(w){
      print("bad density calculation when using the following parameters:")
      print(use.phi)
      return(dens.prior.code())
    })
  #so this is only calculating density for run-level vars. 
  #is that somehow a viable solution?
  if(is.na(dens)){
    dens=-Inf
    diagnostic_record$NaN_densities_l1<<-diagnostic_record$NaN_densities_l1+1
  }
  dens
}

#this may also be problematic
log.dens.prior.l2=function(x_s,        #  actually these are phi, just a lower level phi
                           use.mu,
                           use.Sigma,
                           use.phi      #  higher level phi
                           ){
  # print("running log.dens.prior.l2")
  # print(x_s)
  # print(use.phi)
  # print("----")
  #calculate the prior probability density of the second-level parameters based on third-level distributions
  dens=sum(dnorm(x_s[param.l2.ids[["alpha_s_mu"]]],use.phi[param.l3.ids[["alpha_s_mu_g_mu"]]],use.phi[param.l3.ids[["alpha_s_mu_g_sigma"]]],log=TRUE)) + 
    sum(dnorm(x_s[param.l2.ids[["thresh_s_mu"]]],use.phi[param.l3.ids[["thresh_s_mu_g_mu"]]],use.phi[param.l3.ids[["thresh_s_mu_g_sigma"]]],log=TRUE)) +
    sum(dnorm(x_s[param.l2.ids[["tau_s_mu"]]],use.phi[param.l3.ids[["tau_s_mu_g_mu"]]],use.phi[param.l3.ids[["tau_s_mu_g_sigma"]]],log=TRUE))+
    #we also need the prior probability densities for the sigmas
    #but what is the distribution we're using for the sigmas? Why dhalfcauchy(0,5)?
    sum(2*dcauchy(x_s[param.l2.ids[["alpha_s_sigma"]]],0,prior.l2$alpha$sigma_sigma,log=TRUE)) + 
    sum(2*dcauchy(x_s[param.l2.ids[["thresh_s_sigma"]]],0,prior.l2$thresh$sigma_sigma,log=TRUE)) +
    sum(2*dcauchy(x_s[param.l2.ids[["tau_s_sigma"]]],0,prior.l2$thresh$sigma_sigma,log=TRUE))
      #weakly informative priors for the sigmas
      #because these are mostly not informative, sigmas will be mostly influenced by their ability to produce a good posterior estimate of the data.
  
  if(is.na(dens)){
    dens=-Inf
    diagnostic_record$NaN_densities_l2<<-diagnostic_record$NaN_densities_l2+1
  }
  
  dens
}

check.pars=function(x_s,use.data_s){
  return(TRUE)
}

log.dens.like=function(x_s,use.data_s,method="full"){
  #cat("*")
  #transformation functions
  
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
    #alpha_tr<-f_alpha_s_tr(as.numeric(x_s[which(par.names=="alpha")]))
    alpha_tr<-f_alpha_s_tr(x_s[param.l1.ids$alpha])
    for(tr in 1:nt){#tr=1;tr=tr+1
      #print (tr)
      #start_time <- Sys.time()
      if (use.data_s$choice[tr]!=0) {
        
        #this must come first - this represents the choice being made.
        # there is some transformation based on ev and beta needed before a drift rate can be obtained
        v_t[tr,]=ev[use.data_s$cue[tr],]
        
        
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
    
    #thresh_s_tr<-f_thresh_s_tr(x_s[which(par.names=="thresh")])
    #tau_s_tr<-f_tau_s_tr(x_s[which(par.names=="tau")] )
    thresh_s_tr<-f_thresh_s_tr(x_s[param.l1.ids$thresh])
    tau_s_tr<-f_tau_s_tr(x_s[param.l1.ids$tau] )
    
    dens.s<-sum(log(get.dens.2choice(t=use.data_s$rt[use.data_s$choice!=0],
                                     choice=use.data_s$choice[use.data_s$choice!=0],
                                     alpha=c(thresh_s_tr,thresh_s_tr),
                                     v=v_t[use.data_s$choice!=0,],
                                     theta=c(tau_s_tr,tau_s_tr)
    )))
    dens=dens+dens.s#is this right?-BJS
    if(is.nan(dens.s)){
      #print("NaN density produced. check out why.")
      #print(x_s)
      #print("first 3 RTs for subject*run are:")
      diagnostic_record$NaN_density_likelihood<<-diagnostic_record$NaN_density_likelihood+1
      #print(use.data_s$rt[1:3])
    }
    diagnostic_record$base_rate_density_likelihood<<-diagnostic_record$base_rate_density_likelihood+1
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

#I think that's all this boils down to.
log.dens.like.l2=function(x_s,use.data,method="full"){
  #if use.data contains four indices, knock it down to 3 if necessary
  #by using lnum2 which is a chain index and i which specifies which chain.

  sum(apply(use.data[,!apply(is.na(use.data),2,all)],2,function(r){log.dens.prior(r,NA,NA,x_s)}))
}
