myseq<-seq(0,1,0.01)
plot(myseq,dbeta(myseq,1.2,1.2))
plot(myseq,dbeta(myseq,1,1))
plot(myseq,dbeta(myseq,2,1))
plot(myseq,dbeta(myseq,3,1))
plot(myseq,dbeta(myseq,4,1))
plot(myseq,dbeta(myseq,5,1.1))
plot(myseq,dbeta(myseq,5,1))
plot(myseq,dbeta(myseq,0.4,0.6))
plot(myseq,dbeta(myseq,0.4+1,0.6+1))
plot(myseq,dbeta(myseq,0.4+1,0.6+1))
plot(myseq,dbeta(myseq,4,6))
plot(myseq,dbeta(myseq,10,90))
#if we use the kappa to SCALE BOTH VARIABLES
plot(myseq,dbeta(myseq,.10,.90))
#and if we keep the whole thing OVER 1
#then mean+1=alpha_unscaled
#where beta_unsaled=1-alpha


plot(myseq,dbeta(myseq,1+.30*5,1+.70*5))
library(dplyr)
library(magrittr)
qbeta(.3,1+.30*5,1+.70*5)

#we want a distribution in which
#values fall between zero and one
#one parameter sets the mean
#another parameter sets the deviation from the mean.
#and ranges from uniform to SD=0.
plot(myseq,dlogis(myseq,0.5,1000))
plot(myseq,dbeta(myseq,1+.10*10,1+.90*10))
plot(myseq,dbeta(myseq,1+.10,1+.90))
plot(myseq,dbeta(myseq,1+.5*(wha),1+.5*1000))

#bernoulli
plot(myseq,dbern(myseq,1))
plot(myseq,dbern(myseq,0.9))
plot(myseq,dbern(myseq,0.8))

hypertheta<-0.4
hyperkappa<-0

plot(dbeta(myseq,(hypertheta)*(exp(hyperkappa))+1,(1-hypertheta)*(exp(hyperkappa))+1))
plot(dbeta(myseq,(hypertheta)*(exp(hyperkappa)-1)+1,(1-hypertheta)*(exp(hyperkappa)-1)+1))
plot(dbeta(myseq,(hypertheta)*(hyperkappa^2)+1,(1-hypertheta)*(hyperkappa^2)+1))

plot(dbeta(myseq,(hypertheta)*(hyperkappa+1)+1,(1-hypertheta)*(hyperkappa+1)+1))
plot(dbeta(myseq,(hypertheta)*(hyperkappa^2+1)+1,(1-hypertheta)*(hyperkappa^2+1)+1))

hyperspread<-.001
plot(dbeta(myseq,(hypertheta)*(1/(hyperspread)-1)+1,(1-hypertheta)*(1/(hyperspread)-1)+1))


plot(myseq,dbeta(myseq,3,3))

myseq.30<-seq(0,30,0.01)
plot(myseq.30,dcauchy(myseq.30,0,7))
