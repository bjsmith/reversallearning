
log.dens.prior=function(x_s,
                        use.mu,
                        use.Sigma,
                        use.phi
){#x_s<-use.theta[i,];use.mu<-use.mu[i,];use.phi=use.phi[i,]
  #phi includes level 2 and level 3 phi values.
  require(MCMCpack)
  #names(x) <- par.names
  #priors
  
  #SOMEHOW the code we are using here should pass in values that are related to each individual subject.
  #add phi values here.
  dens=sum(dnorm(x_s[param.l1.ids[["alpha"]]],use.phi[param.l2.ids[["alpha_mu_s"]]],use.phi[param.l2.ids[["alpha_sigma_s"]]],log=TRUE)) + 
    sum(dnorm(x_s[param.l1.ids[["thresh"]]],use.phi[param.l2.ids[["thresh_mu_s"]]],use.phi[param.l2.ids[["thresh_sigma_s"]]],log=TRUE)) +
    sum(dnorm(x_s[param.l1.ids[["tau"]]],use.phi[param.l2.ids[["tau_mu_s"]]],use.phi[param.l2.ids[["tau_sigma_s"]]],log=TRUE)) 
  #so this is only calculating density for run-level vars. 
  #how do we add the density for subject-level? Considering that this function is being run separately for each run.
  #I think I want to see how it is run. How much data is in each of these; if we re-program to take all subjects' data,
  #is that somehow a viable solution?
  stop("We haven't programmed log.dens.prior to work with level 3 yet. gotta figure this out.")
  #this would usually just calculate density for a single subject.
  #it doesn't make a lot of sense to c
  
  #density for a three-level model must contain:
  #level 1 alpha, thresh, taus
  #level 2 {alpha, thresh, tau}_{mu, sigma}
  #level 3 {alpha, thresh, tau}_{mu, sigma}
  #does x_s usually contain one subject or...
  dens = NA
  warning("haven't created the density function for the three level model yet.")
  
  
  if(is.na(dens))dens=-Inf
  
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
    
    #save(list(list(use.data_s),list(x),list(v_t)), file=paste0(dd, "test_de_h_m1_config.Rdata"))
    #stop("stopping here so I can use profvis well.")
    
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
