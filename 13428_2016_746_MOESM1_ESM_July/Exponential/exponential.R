rm(list=ls())
library(rstan)
#make simualated data
dat = rexp(500,1)
len = length(dat)
#run the Stan model
fit <- stan(file   = "exponential.stan", 
            data   = list(Y=dat,LENGTH=len),          
            warmup = 750,                 
            iter   = 1500,                
            chains = 3)                   
#model summary
print(fit)
#posterior predictions
mcmc_chain = as.matrix(fit)
lambda = mean(mcmc_chain[,'lambda'])
pred = mcmc_chain[,'pred']
hist(dat,probability=T)
lines(density(pred))
#autocorrelation plot
acf(mcmc_chain[,'lambda'])
#samples vs. iteration plot
traceplot(fit)
#plot lambda
library(coda)
hdi = HPDinterval(as.mcmc(mcmc_chain[,'lambda']),.95)
plot(density(mcmc_chain[,'lambda']),main='',xlab=expression(lambda),las=1)
segments(hdi[1],.1,hdi[2],.1,lwd = 3)
text(x=hdi[1],y=.4,paste(format(hdi[1],digits=2)))
text(x=hdi[2],y=.4,paste(format(hdi[2],digits=3)))

