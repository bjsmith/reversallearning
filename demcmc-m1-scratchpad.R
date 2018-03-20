load(paste0(localsettings$data.dir,"de_mcmc/output_m120180307T224156.RData"))

#this is a list of subjects.
length(data)

data[[1]]
plot.phi=FALSE
plot.lower=TRUE
plot.weights=FALSE
plot.sigma=FALSE
plot.mu=FALSE
plot.rho=FALSE
starti=1000
#contains cue, choice, rt, and outcome. This isn't what we need; this is input, not output.
setwd(file.path(mainDir))
debugSource("de_mcmc/fig_base_demcmc_m1.r")
