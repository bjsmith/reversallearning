library(boot)

hist(inv.logit(rnorm(100000,0,1)))
hist(inv.logit(rnorm(100000,0,1.6)),breaks=100)
#wow I think we have to go for 1. 
#because at any other values we're actually building a prior away from the center.


#what about for the exponential transform?
#k
hist(exp(rnorm(100000,log(0.5),2)),xlim = c(0,10),breaks=100000)

#tau
hist(exp(rnorm(100000,log(0.5),3)),xlim = c(0,10),breaks=1000000)
