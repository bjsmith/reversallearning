
#log.dens.prior=function(x,prior,phi){
log.dens.prior=function(x,prior){
  require(MCMCpack)
  names(x) <- par.names
  #priors
  #what scale are these on; how do they relate to the absolute values here?
  #add phi values here.
  dens=sum(log(dunif(x["alpha"],0,1))) + 
    sum(log(dunif(x["beta"],0,1))) + 
    sum(log(dunif(x["thresh"],0,1000))) +
    sum(log(dunif(x["tau"],0,10))) 
  if(is.na(dens))dens=-Inf
  dens
}

check.pars=function(x,use.data){
  names(x) = par.names
  x["tau"]<min(use.data$rt[use.data$choice!=0]) &
    x["alpha"]<1 &
    all(x>=0)
}

log.dens.like.m1<-function(x,use.data){
  
  names(x) <- par.names
  #model
  if(check.pars(x=x,use.data=use.data)){
    #iterate through all trials.
    nt=length(use.data$choice)
    dens=numeric(nt)
    #100 is the number of slots we have to store cues (images), not individual trials.
    #since we don't record a new EV value each time.
    ev=matrix(0,100,2)
    #record the values at each time point.
    v_t=matrix(0,nt,2)
    for(t in 1:nt){
      if (use.data$choice[t]!=0) {
        #this must come first - this represents the choice being made.
        # there is some transformation based on ev and beta needed before a drift rate can be obtained
        v_t[t,]=invlogit(ev[use.data$cue[t],])
        
        # prediction error
        PE   =  use.data$outcome[t] - ev[use.data$cue[t],use.data$choice[t]]
        PEnc = -use.data$outcome[t] - ev[use.data$cue[[t]],3-use.data$choice[[t]]]
        
        # value updating (learning)
        ev[use.data$cue[t],3-use.data$choice[t]] = ev[use.data$cue[t],3-use.data$choice[t]] + as.numeric(x["alpha"]) * PEnc;
        ev[use.data$cue[t],use.data$choice[t]] = ev[use.data$cue[t],use.data$choice[t]] + as.numeric(x["alpha"]) * PE;
        
      }
    }
    #printv("running getting density")
    # now pass the matrix of v into the density wrapper, where RT and choice are vectors.
    dens=get.dens.2choice(t=use.data$rt[use.data$choice!=0],
                          choice=use.data$choice[use.data$choice!=0],
                          alpha=c(x["thresh"],x["thresh"]),
                          v=v_t[use.data$choice!=0,],
                          theta=c(x["tau"],x["tau"]))#is this right?-BJS
    
    
    
    #printv("...got density.")
    out=sum(log(dens))
    if(is.na(out))out=-Inf
  } else {
    out=-Inf
  }
  out
}
