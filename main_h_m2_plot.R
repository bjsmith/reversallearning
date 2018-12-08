

start=2
start.weights=2

pdf(paste(save.dir,save.name,".pdf",sep=""),10,5)
par(mfrow=c(1,2),ask=FALSE)
source("de_mcmc/fig_base4.R")
fig_base(env = environment(),plot.phi=TRUE,plot.lower = TRUE,ask=FALSE)

dev.off()

setwd(mainDir)
