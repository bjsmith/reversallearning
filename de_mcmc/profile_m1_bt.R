version="m1"
verbose=TRUE
source('de_mcmc/main_m1_setup.R')

##############################################  generate data
source("de_mcmc/raw_data_reward_only.R")
##############################################  initialize

par.names=c("alpha","beta","thresh","tau")
n.pars=length(par.names)

n.chains=24
nmc=5000
burnin=1000
thin=1
keep.samples=seq(burnin,nmc,thin)
print(length(keep.samples)*n.chains)

use.optim=TRUE
optim.gamma=TRUE
migrate.prob=.1
migrate.duration=round(burnin*.25)+1
b=.001

cores=8
data<-data[seq(1,161,10)]
S=length(data)


x.init=matrix(c(.3,.2,2,NA),S,n.pars,byrow=T)
x.init[,4]=.6*sapply(data,function(x)min(x$rt,na.rm=TRUE))

##############################################  set prior

prior=NULL
# upper and lower boundaries for the concentration parameters
prior$lower=0  
prior$upper=1

########################################## run it
source(paste("de_mcmc/de_",version,"_config.R",sep=""))
source(paste("de_mcmc/de_",version,"_run.R",sep=""))


#run_env<-de_mcmc_execute(log.dens.like.m1,log.dens.prior)
##############################################################################################################################
#log.dens.like.f<-log.dens.like.h.m1;log.dens.prior.f<-log.dens.prior.h.m1
mainDir<-getwd()
subDir=""

sfInit(parallel=TRUE, cpus=cores, type="SOCK")
#printv("setting up cluster...")
sfClusterSetupRNG()

ptm=proc.time()[3]
# #printv ("running the model...")
# de_m1_run(log.dens.like.f=log.dens.like.f,
#           log.dens.prior.f=log.dens.prior.f)


##############################################################################################################################
log.dens.like.f<-log.dens.like.m1;log.dens.prior.f<-log.dens.prior
log.dens.post=function(x,use.data,prior)log.dens.prior.f(x,prior) + log.dens.like.f(x,use.data)

########################################### initialize the chains
printv("intializing chains...")
theta<<-array(NA,c(n.chains,n.pars,S))
weight<<-array(-Inf,c(n.chains,S))

colnames(theta) <- par.names
printv("exporting...")
sfExportAll(except=list("theta","weight"))

print("Optimizing...")
init.pars=matrix(1,S,n.pars)
profvis({
for(j in 1:S){#j<-1
  start_time <- Sys.time()
  x=x.init[j,]
  print(x)
  temp.weight=log.dens.like.f(x,use.data=data[[j]])
  new.x=x
  while(temp.weight==-Inf){
    new.x=rtnorm(n.pars,x,.1,0,c(1,1,Inf,Inf))
    
    temp.weight=log.dens.like.f(new.x,use.data=data[[j]])
  }
  
}
})



#temp.weight=log.dens.like.f(x,use.data=data)
new.x=x
#while(temp.weight==-Inf){
  #stop("I think this hasn't been set up properly. Not only are the values probably wrong but the function takes far too long to run!")
  #is this appropriate for all the variables? I'm not sure.
 # new.x=rtnorm(n.parinstances,.1,0,inf.vals) #BJS to BT: why does this use a truncated normal distribution?
  #warning("We use this truncated normal distribution to estimate new x values, but I'm not sure that this is appropriate for all variables.")
  
  #shouldn't we be getting the priors here?
  #temp.weight=log.dens.like.f(new.x,use.data=data)
  x=new.x;use.data=data[[1]]
  
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
      #######################################################################################################################################
      # dens=dens+sum(log(get.dens.2choice(t=use.data[[s]]$rt[use.data[[s]]$choice!=0],
      #                                    choice=use.data[[s]]$choice[use.data[[s]]$choice!=0],
      #                                    alpha=c(x[[paste0("thresh_s",s)]],x[[paste0("thresh_s",s)]]),
      #                                    v=v_t[use.data[[s]]$choice!=0,],
      #                                    theta=c(x[[paste0("tau_s",s)]],x[[paste0("tau_s",s)]])
      t=use.data$rt[use.data$choice!=0]
      choice=use.data$choice[use.data$choice!=0]
      alpha=c(x["thresh"],x["thresh"])
      v=v_t[use.data$choice!=0,]
      theta=c(x["tau"],x["tau"])
      # )))
      #######################################################################################################################################
      
      idx1=(choice==1)
      tmp=numeric(length(idx1))
      #tstart<-Sys.time()
      
      #x_w=t[idx]-theta
      #tmp[idx1]=wald.pdf.raw(t[idx1],alpha[1],v[idx1,1],theta[1])*(1-wald.cdf.raw(t[idx1],alpha[2],v[idx1,2],theta[2]))
      tmp[idx1]=wald.pdf.raw(t[idx1],alpha[1],v[idx1,1],theta[1])*(1-wald.cdf.raw(t[idx1],alpha[2],v[idx1,2],theta[2]))
      
      idx2=(choice==2)
      
      #tmp[idx2]=wald.pdf.raw(t[idx2],alpha[2],v[idx2,2],theta[2])*(1-wald.cdf.raw(t[idx2],alpha[1],v[idx2,1],theta[1]))
      tmp[idx2]=wald.pdf.raw(t[idx2],alpha[2],v[idx2,2],theta[2])*(1-wald.cdf.raw(t[idx2],alpha[1],v[idx2,1],theta[1]))
      get.dens.2choice_count<<-get.dens.2choice_count+1
      #tend<-Sys.time()
      #time.wald<<-time.wald+(tend-tstart)
      #if((get.dens.2choice_count%%1000)==0) print(paste0(get.dens.2choice_count, "twalds:",time.wald))
      tmp
      
      #printv("...got density.")
      out=sum(log(dens))
      if(is.na(out))out=-Inf
    } else {
      out=-Inf
    }

#profvis({if(use.optim==TRUE)init.pars=optim(new.x,function(x,...)-log.dens.like.f(x,...),use.data=data,control=list("maxit"=1000))$par})
if(use.optim==FALSE)init.pars=new.x
print(paste("Optimization Complete."))
#}

