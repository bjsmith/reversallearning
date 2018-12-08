library(ggplot2)

source("../joint_msm_combined/bjs_misc_utils.R")
version="h_m3"

source('de_mcmc/functions.R')
source('de_mcmc/main_m1_setup.R')
source('de_mcmc/functions_joint_v2.R')
source(paste0('de_mcmc/functions_',version,'.R'))

source("de_mcmc/functions_h_m3.R")
source("visualization/geom_hdi.R")

#load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m320180716T135813.RData");length(data)
#this is punishment, round 1
#load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m320180716T140213.RData");length(data)
#this is punishment, round 2
#so why are we yielding identical results for them?
for (run_version in c(
  'output_h_m320180716T135813',
  'output_h_m320180716T140213'
  #'output_h_m320180716T134722'
  )){
  load(paste(save.dir,run_version,".RData",sep=""))
  #load.image(paste(save.name,"run.ts",".RData",sep=""))
  setwd(mainDir)
  
  start=2
  start.weights=2
  save.name=paste0("main_", version,run_version)
  pdf(paste(save.dir,save.name,".pdf",sep=""),10,5)
  par(mfrow=c(1,2),ask=FALSE)
  setwd(mainDir)
  source("de_mcmc/fig_base5.R")
  hpar.names<-paste0(dimnames(phi)[[2]],"_",rep(dimnames(phi)[[3]],times=1,each=6))
  phi<-matrix(phi,nrow = dim(phi)[1],ncol=prod(dim(phi)[2:3]))
  n.hpars<-18
  #fig_base(env = environment(),plot.phi=TRUE,plot.lower = TRUE,ask=FALSE)
  fig_base( env = environment(),plot.phi=TRUE,plot.lower = FALSE,ask=FALSE,
            par.functions=rep(c(f_alpha_s_tr,f_thresh_s_tr,f_tau_s_tr),6))#list one function for each param. the second set are for the sigmas.
  dev.off()
  
  #now let's do some more plots.
  additional_output<-paste(save.dir,save.name,"_extended",sep="")
  dir.create(additional_output)
  
  
  #cool
  
  source("de_mcmc/hierarchical_summarize.R")
  print(hierarchical_summarize(gnames=list(SafeNoMeth="SafeNoMeth",RiskyNoMeth="RiskyNoMeth",RiskyMeth="RiskyMeth")))
  
}

