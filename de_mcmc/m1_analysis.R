
)
source("util/apply_local_settings.R")
apply_local_settings()

source("de_mcmc/functions.R")
library("data.table")

mainDir <- getwd()
setwd(mainDir)

########################################## estimation

version="m1"
save.name <- paste("demcmc/output_",version,sep="")
save.dir <- localsettings$data_dir
subDir=save.name

#the directory didn't get saved!!! Can we somehow retrieve from the chains?
load(paste(save.dir,save.name,".RData",sep=""))

plot.lower=TRUE
plot.weights=TRUE
start=2
start.weights=2

pdf(paste(save.dir,"/",save.name,".pdf",sep=""),10,5)
par(mfrow=c(1,2),ask=FALSE)
source("de_mcmc/fig_base3.r")
fig_base(env = environment())
dev.off()
