plot.phi=TRUE
plot.lower=TRUE
plot.sigma=FALSE
plot.rho=FALSE
plot.mu=FALSE
plot.weights=FALSE
plot.priors=FALSE
load(paste("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m420180419T165827",".RData",sep=""))

start=2 
start.weights=2
setwd(file.path(mainDir))

pdf(paste(save.dir,save.name,".pdf",sep=""),10,5)
par(mfrow=c(1,2),ask=FALSE)

tnmc<-1001
hpar.names<-paste0(dimnames(phi)[[2]],"_",rep(dimnames(phi)[[3]],times=1,each=6))
if(plot.phi==TRUE) phi<-array(NA,c(n.chains,n.hpars*g,tnmc),
                              dimnames=list(NULL,
                                            paste0(rep(l2.groups.list,each=n.hpars),"_",rep(par.names.l2,g)))
                              )
# phi=array(NA,c(n.chains,param.l2.N*groups.l2.N,tnmc),
#           dimnames=list(paste0("Chain",1:n.chains),
#                         paste0("S",rep(1:groups.l2.N,each=param.l2.N),"_",rep(param.l2.names,groups.l2.N))))

n.hpars<-18


setwd(file.path(mainDataDir, subDir))
tnmc=length(keep.samples)

#need to get thetas....


#we're formatting this one a bit differently.
if(plot.lower==TRUE)theta=array(NA,c(n.chains,n.pars,S,tnmc),
                                 dimnames=list(NULL,
                                               par.names.l1,
                                               paste0("S",1:S),
                                               NULL))

# if(plot.weights==TRUE)weights=array(NA,c(n.chains,S,tnmc))
# if(plot.sigma==TRUE | plot.rho==TRUE)Sigma=array(NA,c(n.chains,n.Sigma,tnmc))
# if(plot.mu==TRUE)mu=array(NA,c(n.chains,n.mu,tnmc))

#iterating through chains I guess?
for(q in 1:n.chains){#q<-1
  if(plot.lower==TRUE){
    for(s in 1:S){#q<-1;s<-1
      temp=t(as.matrix(read.table(paste(mainDataDir, save.name, "/chain",q,"_sub",s,"_lower.txt",sep=""),header=F)))
      theta[q,,s,]=temp[,-1]
      # theta[1,,1,1:10]
      # temp[,1:10]
      }
    }
  if(plot.sigma==TRUE | plot.rho==TRUE){
    temp=t(as.matrix(read.table(paste("chain",q,"_Sigma.txt",sep=""),header=F)));Sigma[q,,]=temp[,-1];
  }
  if(plot.mu==TRUE){
    temp=t(as.matrix(read.table(paste("chain",q,"_mu.txt",sep=""),header=F)));mu[q,,]=temp[,-1];
  }
  if(plot.phi==TRUE){
    temp=t(as.matrix(read.table(paste(mainDataDir, save.name, "/chain",q,"_hyper.txt",sep=""),header=F)))
    phi[q,,]=temp[,-1]
  }
  #let's get it straight how to interpret this. dim1 represents params*groups.
  
  if(plot.weights==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_weights.txt",sep=""),header=F)));weights[q,,]=temp[,-1]}
  #if(plot.weights.delta==TRUE){temp=t(as.matrix(read.table(paste("chain",q,"_weights_delta.txt",sep=""),header=F)));weight.delta[q,,]=temp[,-1]}
  print(round(q/n.chains*100))
}
warning("still need to check that phi matrix correctly labs groups*params")

#######################################################################################
#if(!exists("n.l2.groups"))n.l2.groups<-1

breaks=50
ask=FALSE
count=1
phi_vals<-list()



#our phi has 18 params.
#but let's do one page for each of the three groups.

#we have named them already...
for(k in 1:g){#loop through groups
  #loop through the priors in a 2x3 pattern.
  par(mfrow=c(3,4),ask=ask)
  for (param in 1:length(par.ids.l1)){ #each set of parameters
    #and then the hypers for those.
    for (hyper_type in c(0,3)){
      #do it this way so we cycle through each mu and sigma pair at the same time.
      k_t<-(k-1)*g+param+(hyper_type)
      matplot(t(phi[,k_t,start:tnmc]),type="l",lty=1,main="",ylab=dimnames(phi)[[2]][k_t])
      hist(phi[,k_t,start:tnmc],prob=T,breaks=breaks,main="",xlab=dimnames(phi)[[2]][k_t])
    }
  }
}

#let's graph the thetas

#a pseudorandom sample - only prime numbered subjects
is.prime <- function(n) n == 2L || all(n %% 2L:max(2,floor(sqrt(n))) != 0)

for(t_s in 1:dim(theta)[3]){
  if(is.prime(t_s)){
    par(mfrow=c(3,2),ask=FALSE)
      for(k in 1:(dim(theta)[2])){#parameters
        theta_val <- paste0("S",t_s)
        matplot(t(theta[,k,t_s,start:tnmc]),type="l",lty=1, main=theta_val, ylab=dimnames(theta)[[2]][k])
        hist(theta[,k,t_s,start:tnmc],prob=T,breaks=breaks, main=theta_val, xlab=dimnames(theta)[[2]][k])
        if(plot.priors==TRUE){
          lines(xs,ys,lty=2)
          count=count+1
      }
    }
  }
}




dev.off()

setwd(mainDir)
