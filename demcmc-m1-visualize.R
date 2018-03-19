load(paste0(localsettings$data.dir,"de_mcmc/output_m120180307T224156.RData"))

#this is a list of subjects.
#length(data)

#data[[1]]
plot.phi=FALSE
plot.lower=TRUE
plot.weights=FALSE
plot.sigma=FALSE
plot.mu=FALSE
plot.rho=FALSE
starti=1000
#contains cue, choice, rt, and outcome. This isn't what we need; this is input, not output.
setwd(file.path(mainDir))
pdf(paste(save.dir,save.name,".pdf",sep=""),10,5)
par(mfrow=c(1,2),ask=FALSE)
# source("de_mcmc/fig_base.R")
source("de_mcmc/fig_base_demcmc_m1.r")
 dev.off()

