#this is adapted from de_m1_config
#this version is hierarchical, unlike de_m1
#for this hierarchical model, we'll only add room for a single run per subject.

log.dens.prior.h.m1=function(x,prior){
  #x=use.paramlist[i,];prior=prior
  #specifies distributions
  require(MCMCpack)
  names(x) <- parinstancenames
  #priors
  l2<-list()
  l2[level2.par.names[c(1,3)]]<-dnorm(x[level2.par.names[c(1,3)]],0,1,log=TRUE)
  l2[level2.par.names[2]]<-dcauchy(x[level2.par.names[2]],0,10,log=TRUE)
  l2[level2.par.names[4:6]]<-dcauchy(x[level2.par.names[4:6]],0,1,log=TRUE)
  #that's cool. Now...
  #Each of the level 1 vars are distributed using the level 2 parameters
  #iterate through the three variables (alpha,thresh,tau)
  if(is.null(log.dens.like.h.m1.parnames.alpha)){
    update.parnames.ids(S)
  }
  #s<-1
  print(log.dens.like.h.m1.parnames.alpha)
  print(x[log.dens.like.h.m1.parnames.alpha])
  print(level2.par.names)
  print(x[level2.par.names])
  dens=sum(unlist(lapply(1:S,function(s){
    dnorm(x[log.dens.like.h.m1.parnames.alpha[[s]]],
          x[level2.par.names[[1]]],
          x[level2.par.names[[4]]],
          log=TRUE)+
      dnorm(x[log.dens.like.h.m1.parnames.alpha[[s]]],
            x[level2.par.names[[2]]],
            x[level2.par.names[[5]]],
            log=TRUE)+
      dnorm(x[log.dens.like.h.m1.parnames.alpha[[s]]],
            x[level2.par.names[[3]]],
            x[level2.par.names[[6]]],
            log=TRUE)})))+
    sum(unlist(l2))
      
  if(is.na(dens)){dens=-Inf
    print(parinstancenames)
    print(x)
    stop("asdf")
    
  }
  dens
}

check.pars.nonfunction.warned<<-FALSE
check.pars=function(x,use.data){
  names(x) = parinstancenames
  if (!check.pars.nonfunction.warned){
    warning("check.pars is currently disabled. I need to revisit this and fix up how it looks.")
    check.pars.nonfunction.warned<<-TRUE
  }
  
  # x["tau"]<min(use.data$rt[use.data$choice!=0]) &
  #   x["alpha"]<1 &
  #   all(x>=0)
  return(TRUE)
}

log.dens.like.h.m1.timer.sectionA<-0
log.dens.like.h.m1.timer.sectionB<-0
log.dens.like.h.m1.parnames.alpha<-NULL
log.dens.like.h.m1.parnames.thresh<-NULL
log.dens.like.h.m1.parnames.tau<-NULL
log.dens.like.h.m1.parid.alpha<-NULL
log.dens.like.h.m1.parid.thresh<-NULL
log.dens.like.h.m1.parid.tau<-NULL

log.dens.like.h.m1<-function(x,use.data){
  cat("*")
  
  if(is.null(log.dens.like.h.m1.parnames.alpha)){
    update.parnames.ids(length(use.data))
  }
  #
  #model
  dens=0
  if(check.pars(x=x,use.data=use.data)){
    #iterate through all trials.
    #one approach is to write a separate function
    for (s in 1:length(use.data)){#s<-1#first subject
      nt=length(use.data[[s]]$choice)
      #print(paste0("processing sub ",s))
      #100 is the number of slots we have to store cues (images), not individual trials.
      #since we don't record a new EV value each time.
      ev=matrix(0,100,2)
      #record the values at each time point.
      v_t=matrix(0,nt,2)
      alpha_tr<-invlogit(as.numeric(x[[log.dens.like.h.m1.parid.alpha[[s]] ]]))
      for(tr in 1:nt){#tr=1;tr=tr+1
        #print (tr)
        #start_time <- Sys.time()
        if (use.data[[s]]$choice[tr]!=0) {
          
          #this must come first - this represents the choice being made.
          # there is some transformation based on ev and beta needed before a drift rate can be obtained
          v_t[tr,]=invlogit(ev[use.data[[s]]$cue[tr],])

          # prediction error
          PE   =  use.data[[s]]$outcome[tr] - ev[use.data[[s]]$cue[tr],use.data[[s]]$choice[tr]]
          PEnc = -use.data[[s]]$outcome[tr] - ev[use.data[[s]]$cue[tr],3-use.data[[s]]$choice[tr]]

          # value updating (learning)
          ev[use.data[[s]]$cue[tr],3-use.data[[s]]$choice[tr]] = 
            ev[use.data[[s]]$cue[tr],3-use.data[[s]]$choice[tr]] + alpha_tr * PEnc;
          ev[use.data[[s]]$cue[tr],use.data[[s]]$choice[tr]] = 
            ev[use.data[[s]]$cue[tr],use.data[[s]]$choice[tr]] + alpha_tr * PE;
          
        }
      }
        #end_time <- Sys.time()
        #log.dens.like.h.m1.timer.sectionA<<-log.dens.like.h.m1.timer.sectionA+(end_time-start_time)
        #start_time <- Sys.time()

        #save(list(list(use.data),list(x),list(v_t)), file=paste0(dd, "test_de_h_m1_config.Rdata"))
        #stop("stopping here so I can use profvis well.")
      print("hello")
      #print(x)
      if(x[[ log.dens.like.h.m1.parid.thresh[[s]] ]]<0){
        print("alpha below zero; here is x:")
        print(parinstancenames)
        print(x)
      }
      thresh_s<-x[[ log.dens.like.h.m1.parid.thresh[[s]] ]]
      dens.s<-sum(log(get.dens.2choice(t=use.data[[s]]$rt[use.data[[s]]$choice!=0],
                                       choice=use.data[[s]]$choice[use.data[[s]]$choice!=0],
                                       alpha=c(thresh_s,thresh_s),
                                       v=v_t[use.data[[s]]$choice!=0,],
                                       theta=c(x[[ log.dens.like.h.m1.parid.tau[[s]] ]],x[[  log.dens.like.h.m1.parid.tau[[s]] ]])
      )))
      dens=dens+dens.s#is this right?-BJS
      # if(is.nan(dens.s)){
      #    print("NaN density produced. check out why.")
      #   
      #   print(use.data[[s]]$rt[use.data[[s]]$choice!=0])
      #   print(use.data[[s]]$choice[use.data[[s]]$choice!=0])
      #   print(c(x[[ log.dens.like.h.m1.parid.thresh[[s]] ]],x[[ log.dens.like.h.m1.parid.thresh[[s]] ]]))
      #   print(v_t[use.data[[s]]$choice!=0,])
      #   print(c(x[[ log.dens.like.h.m1.parid.tau[[s]] ]],x[[  log.dens.like.h.m1.parid.tau[[s]] ]]))
      # }
        #end_time <- Sys.time()
        #log.dens.like.h.m1.timer.sectionB<<-log.dens.like.h.m1.timer.sectionB+(end_time-start_time)
      #printv("running getting density")
      # now pass the matrix of v into the density wrapper, where RT and choice are vectors.
      #we use this to calculate the density associated with the model
      #
    }
    #what about the density for the group-level variables?
    #I think we can just do density for the subject-level variables; and since they are being fit from the group-level,
    #that in itself tells us how close we're getting.
    #we have no group-level 'data' to fit so it wouldn't make sense to get any density for hte group level.

    #printv("...got density.")
    #print(paste0("A:",log.dens.like.h.m1.timer.sectionA,"; B:",log.dens.like.h.m1.timer.sectionB))
    

    out=dens
    if(is.na(out))out=-Inf
  } else {
    out=-Inf
  }
  out
}

update.parnames.ids <- function(S){
  #do this here to speed up processing. Should only be one ONCE.
  log.dens.like.h.m1.parnames.alpha<<-unlist(lapply(1:S,function(s){paste0("alpha_s",s)}))
  log.dens.like.h.m1.parnames.thresh<<-unlist(lapply(1:S,function(s){paste0("thresh_s",s)}))
  log.dens.like.h.m1.parnames.tau<<-unlist(lapply(1:S,function(s){paste0("tau_s",s)}))
  #NB: The below code relies on the first time I run this following this naming convention; subsequently it does not need to.
  #This might be OK for now.
  log.dens.like.h.m1.parid.alpha<<-unlist(lapply(1:S,function(s){which(names(x)==paste0("alpha_s",s))}))
  log.dens.like.h.m1.parid.thresh<<-unlist(lapply(1:S,function(s){which(names(x)==paste0("thresh_s",s))}))
  log.dens.like.h.m1.parid.tau<<-unlist(lapply(1:S,function(s){which(names(x)==paste0("tau_s",s))}))
  
}

#creating a special function because this should probably be given consideration,
#but right now I'm just going to set everything to 0.
init.param.sds <- function(){
  return(rep(1,n.parinstances))
}

#we're fine with everything being unconstrained right now.
init.inf.vals <- function(){
  return(rep(Inf,n.parinstances))
}