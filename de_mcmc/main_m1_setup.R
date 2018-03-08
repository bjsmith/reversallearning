source("../util/apply_local_settings.R")
apply_local_settings()
source("../util/bjs_misc_utils.R")
run.ts<-get.datetimestamp()

source("de_mcmc/functions.R")
library("data.table")
library("snowfall")#install.packages("snowfall")
library("MASS")
library("msm")      #install.packages("msm")
library("MCMCpack") #install.packages("MCMCpack")

mainDir <- getwd()

setwd(mainDir)




save.dir <- paste0(localsettings$data_dir, "/de_mcmc/")
mainDataDir <- save.dir

save.name <- paste("output_",version,run.ts,sep="")
subDir <- paste("output_",save.name,sep="")

dir.create(paste0(mainDataDir,subDir))
de_mcmc_execute <- function (log.dens.like.f,log.dens.prior.f){
  
  mainDir<-getwd()
  subDir=""
  
  sfInit(parallel=TRUE, cpus=cores, type="SOCK")
  #printv("setting up cluster...")
  sfClusterSetupRNG()
  
  ptm=proc.time()[3]
  #printv ("running the model...")
  de_m1_run(log.dens.like.f=log.dens.like.f,
            log.dens.prior.f=log.dens.prior.f
  )
  proc.time()[3]-ptm
  
  sfStop()
  
  save.image(paste(save.dir,save.name,".RData",sep=""))
  
  return(environment())
}