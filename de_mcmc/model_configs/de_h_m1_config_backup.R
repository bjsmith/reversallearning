#this is adapted from de_m1_config
#this version is hierarchical, unlike de_m1
#for this hierarchical model, we'll only add room for a single run per subject.

log.dens.prior.h.m1=function(x,prior){
  #specifies distributions
  require(MCMCpack)
  names(x) <- par.names
  #priors
  #what scale are these on; how do they relate to the absolute values here?
  #x is simply the prior values; here they are for specific subjects but 
  #we should be able to generalize to do this for a group of subjects all at once.
  #interesting thing here is that the distribution of each of these level 1 values is going to depend on 
  #the distribution of level 2 values. How do we do that?
  #we'd have to save the generated level2 values...
  #1-4 are normal; 5-8 are....cauchy or something...
  l2<-list()
  l2[level2.par.names[1:3]]<-dnorm(x[level2.par.names[1:3]],0,1,log=TRUE)
  l2[level2.par.names[4:6]]<-dcauchy(x[level2.par.names[4:6]],0,1,log=TRUE)
  #that's cool. Now...
  #Each of the level 1 vars are distributed using the level 2 parameters
  
  
  
  dens=sum(lapply(1:3,function(i)dnorm(x[level1.par.names],x[level2.par.names[i]],x[level2.par.names[i]]+3,log=TRUE)))+
    sum(l2)
  if(is.na(dens))dens=-Inf
  dens
}

check.pars=function(x,use.data){
  names(x) = par.names
  warning("check.pars is currently disabled. I need to revisit this and fix up how it looks.")
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
    #do this here to speed up processing. Should only be one ONCE.
    log.dens.like.h.m1.parnames.alpha<<-unlist(lapply(1:length(use.data),function(s){paste0("alpha_s",s)}))
    log.dens.like.h.m1.parnames.thresh<<-unlist(lapply(1:length(use.data),function(s){paste0("thresh_s",s)}))
    log.dens.like.h.m1.parnames.tau<<-unlist(lapply(1:length(use.data),function(s){paste0("tau_s",s)}))
    #NB: The below code relies on the first time I run this following this naming convention; subsequently it does not need to.
    #This might be OK for now.
    log.dens.like.h.m1.parid.alpha<<-unlist(lapply(1:length(use.data),function(s){which(names(x)==paste0("alpha_s",s))}))
    log.dens.like.h.m1.parid.thresh<<-unlist(lapply(1:length(use.data),function(s){which(names(x)==paste0("thresh_s",s))}))
    log.dens.like.h.m1.parid.tau<<-unlist(lapply(1:length(use.data),function(s){which(names(x)==paste0("tau_s",s))}))
  }
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
            ev[use.data[[s]]$cue[tr],3-use.data[[s]]$choice[tr]] + as.numeric(x[[log.dens.like.h.m1.parid.alpha[[s]] ]]) * PEnc;
          ev[use.data[[s]]$cue[tr],use.data[[s]]$choice[tr]] = 
            ev[use.data[[s]]$cue[tr],use.data[[s]]$choice[tr]] + as.numeric(x[[log.dens.like.h.m1.parid.alpha[[s]] ]]) * PE;
          
        }
      }
        #end_time <- Sys.time()
        #log.dens.like.h.m1.timer.sectionA<<-log.dens.like.h.m1.timer.sectionA+(end_time-start_time)
        
        #warning("Does it make sense to get the log of the likelihood for each single trial then sum that together?")
        #warning("Does it make sense to never get likelihood data based on the double update model, and only the racing diffusion model?")
        #start_time <- Sys.time()

        #save(list(list(use.data),list(x),list(v_t)), file=paste0(dd, "test_de_h_m1_config.Rdata"))
        #stop("stopping here so I can use profvis well.")
      dens=dens+sum(log(get.dens.2choice(t=use.data[[s]]$rt[use.data[[s]]$choice!=0],
                              choice=use.data[[s]]$choice[use.data[[s]]$choice!=0],
                              alpha=c(x[[ log.dens.like.h.m1.parid.thresh[[s]] ]],x[[ log.dens.like.h.m1.parid.thresh[[s]] ]]),
                              v=v_t[use.data[[s]]$choice!=0,],
                              theta=c(x[[ log.dens.like.h.m1.parid.tau[[s]] ]],x[[  log.dens.like.h.m1.parid.tau[[s]] ]])
                                )))#is this right?-BJS
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

# 
# log.dens.like.h.m1<-function(x,use.data){
#   #printv("running log.dens.like.m1")
#   print("hi")
#   names(x) <- par.names
#   #model
#   dens=0
#   if(check.pars(x=x,use.data=use.data)){
#     #iterate through all trials.
# 
# 
# 
#     #one approach is to write a separate function
#     for (s in 1:length(use.data)){#s<-1#first subject
#       nt=length(use.data[[s]]$choice)
#       print(paste0("processing sub ",s))
#       #100 is the number of slots we have to store cues (images), not individual trials.
#       #since we don't record a new EV value each time.
#       ev=matrix(0,100,2)
#       #record the values at each time point.
#       v_t=matrix(0,nt,2)
#       for(tr in 1:nt){#tr=1;tr=tr+1
#         #print (tr)
#         if (use.data[[s]]$choice[tr]!=0) {
#           #this must come first - this represents the choice being made.
#           # there is some transformation based on ev and beta needed before a drift rate can be obtained
#           v_t[tr,]=invlogit(ev[use.data[[s]]$cue[tr],])
# 
#           # prediction error
#           PE   =  use.data[[s]]$outcome[tr] - ev[use.data[[s]]$cue[tr],use.data[[s]]$choice[tr]]
#           PEnc = -use.data[[s]]$outcome[tr] - ev[use.data[[s]]$cue[tr],3-use.data[[s]]$choice[tr]]
# 
#           # value updating (learning)
#           ev[use.data[[s]]$cue[tr],3-use.data[[s]]$choice[tr]] = ev[use.data[[s]]$cue[tr],3-use.data[[s]]$choice[tr]] + as.numeric(x[["alpha_s"]][s]) * PEnc;
#           ev[use.data[[s]]$cue[tr],use.data[[s]]$choice[tr]] = ev[use.data[[s]]$cue[tr],use.data[[s]]$choice[tr]] + as.numeric(x[["alpha_s"]][s]) * PE;
# 
#         }
#         warning("Does it make sense to get the log of the likelihood for each single trial then sum that together?")
#         warning("Does it make sense to never get likelihood data based on the double update model, and only the racing diffusion model?")
#         dens=dens+sum(log(get.dens.2choice(t=use.data[[s]]$rt[use.data[[s]]$choice!=0],
#                                            choice=use.data[[s]]$choice[use.data[[s]]$choice!=0],
#                                            alpha=c(x[["thresh_s"]][s],x[["thresh_s"]][s]),
#                                            v=v_t[use.data[[s]]$choice!=0,],
#                                            theta=c(x[["tau_s"]][s],x[["tau_s"]][s])
#         )))#is this right?-BJS
#       }
#       #printv("running getting density")
#       # now pass the matrix of v into the density wrapper, where RT and choice are vectors.
#       #we use this to calculate the density associated with the model
#       #
#     }
#     #what about the density for the group-level variables?
#     #I think we can just do density for the subject-level variables; and since they are being fit from the group-level,
#     #that in itself tells us how close we're getting.
#     #we have no group-level 'data' to fit so it wouldn't make sense to get any density for hte group level.
# 
#     #printv("...got density.")
# 
#     out=dens
#     if(is.na(out))out=-Inf
#   } else {
#     out=-Inf
#   }
#   out
# }
