library(boot)

set.seed(4683971)
# hist(inv.logit(rnorm(100000,0,1)))
# hist(inv.logit(rnorm(100000,0,1.6)),breaks=100)
#wow I think we have to go for 1. 
#because at any other values we're actually building a prior away from the center.
#alpha
plot(density(inv.logit(rnorm(1000000,0,1.5)),from = 0,to=1))
#1 seems too high! doesn't really allow for zero learning.
#what about for the exponential transform?

library(ggplot2)
library(data.table)
mygraphs<-list()
#k (relative threshold)
limits=c(0,5)
d_sd1<-density(exp(rnorm(100000,log(0.5),2)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="exp N tr","m"=log(0.5),"sd"=2)))
# d_sd1<-density(exp(rnorm(100000,0.5,2)),from = limits[1],to=limits[2])
# mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="exp N tr","m"=0.5,"sd"=2)))
# d_sd1<-density(exp(rnorm(100000,log(0.5),3)),from = limits[1],to=limits[2])
# mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="exp N tr","m"=log(0.5),"sd"=3)))
# d_sd1<-density(exp(rnorm(100000,0.5,3)),from = limits[1],to=limits[2])
# mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="exp N tr","m"=0.5,"sd"=3)))
# d_sd1<-density(exp(rnorm(100000,0,3)),from = limits[1],to=limits[2])
# mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="exp N tr","m"=0,"sd"=3)))
mygraphs<-list()
d_sd1<-density(exp(rnorm(100000,0,2)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="exp N tr","m"=0,"sd"=2)))
d_sd1<-density(exp(rnorm(100000,0.3,1)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="exp N tr","m"=0.3,"sd"=1)))
d_sd1<-density(exp(rnorm(100000,0.4,1)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="exp N tr","m"=0.4,"sd"=1)))

d_sd1<-density(rnorm(100000,0.5,1),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="trunc N","m"=0.5,"sd"=2)))


ggplot(do.call(rbind,mygraphs),
       aes(x=x,y=y,group=interaction(as.factor(m),as.factor(sd),dist),
           color=interaction(as.factor(m),as.factor(sd),dist)))+geom_line()+labs(color="groups")
  

#now what did Annis do?
plot(density(exp(rnorm(100000,log(0.5),2)),from = 0,to=1))
plot(density(exp(rnorm(100000,log(0.5),5)),from = 0,to=10))
#so we can compare this to the Annis paper distributions, which was my intent!

#tau (non-decision time). Really needs to be mostly under 1
hist(exp(rnorm(100000,log(0.5),2)),xlim = c(0,2),breaks=100000)



#function that a point on the distribution and return the probability density at that point.
#that's dnorm.
#if we wanted to get the probability of a transformed distribution we'd have to 
#condense the distribution that we're passing in by the transformation...


#we need to do a prior-only sample from our model to work out if the priors are well-specified!
#this is the only way to reliably get it, given the joint distribution.
#how will we do that with the LBA distribution though?

# 
# 
# I've fixed starting evidence A as a constant, and drift rate is set to a function of the expected value of each alternative.
# 
# Just thinking over this again as I'm making sure I am getting the priors right for the values I am modeling.
# 
# k is modeled as 
# k~norm(log(0.5),2)
# but transformed with exp to an exponential distribution:
#   k_tr~exp(k)
# Annis paper uses
# 
# 
# tau is modeled similarly:
#   tau~norm(log(0.5),2)
# tau_tr~exp(tau)
# 
# The Annis paper 

#alpha graphs: different values of logit.
mygraphs<-list()
#k (relative threshold)
limits=c(0,1)
d_sd1<-density(inv.logit(rnorm(1000000,0,1.8138)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="inv.logit N","m"=0,"sd"=1.8138)))
d_sd1<-density(inv.logit(rnorm(100000,0,1)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="inv.logit N","m"=0,"sd"=1)))
d_sd1<-density(inv.logit(rnorm(100000,0,1.5)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="inv.logit N","m"=0,"sd"=1.5)))

d_sd1<-density(inv.logit(rnorm(100000,0,1.6)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="inv.logit N","m"=0,"sd"=1.6)))

d_sd1<-density(inv.logit(rnorm(100000,logit(0.5),1)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="inv.logit N","m"=logit(0.25),"sd"=1.5)))


ggplot(do.call(rbind,mygraphs),
       aes(x=x,y=y,group=interaction(as.factor(m),as.factor(sd),dist),
           color=interaction(as.factor(m),as.factor(sd),dist)))+geom_line()+labs(color="groups")


#beta graphs: truncated normal vs exponential vs. cauchy
limits=c(0,10)
mygraphs<-list()
d_sd1<-density(abs(rnorm(100000,0,3)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="NormTrunc","m"=0,"sd"=2)))
d_sd1<-density(abs(rcauchy(100000,0,10)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="Half Cauchy","m"=0,"sd"=1)))
d_sd1<-density(exp(rnorm(100000,0,1)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="Exp Norm","m"=0,"sd"=1)))
d_sd1<-density(exp(rnorm(100000,0,3)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="Exp Norm","m"=0,"sd"=3)))
d_sd1<-density(exp(rnorm(100000,0,2)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="Exp Norm","m"=0,"sd"=2)))
ggplot(do.call(rbind,mygraphs),
       aes(x=x,y=y,group=interaction(as.factor(m),as.factor(sd),dist),
           color=interaction(as.factor(m),as.factor(sd),dist)))+geom_line()+labs(color="groups")

library(rstan)
posterior_distribution_categorical_logit <- stan(file='categorical_logit_sampling2.stan', 
                        data = list(),
                        warmup = 500, 
                        iter = 10000,
                        chains = 6)

distribution<-as.matrix(posterior_distribution_categorical_logit)
distribution_df<-as.data.frame(distribution)
colnames(distribution)
#OK, let's graph this shit. how does 
printdist<-function(dr){
  print(paste0("ev:[",round(dr[["expected_value_vector[1]"]],2),", ",round(dr[["expected_value_vector[2]"]],2),"]"))
  print(paste0("beta:",round(dr[["beta"]],2)))
  print(paste0("P(outcome):[",round(exp(dr[["categorical_outcome1"]]),2),", ",round(exp(dr[["categorical_outcome2"]]),2),"]"))
}
printdist(distribution[1,])
printdist(distribution[2,])
printdist(distribution[3,])
printdist(distribution[4,])
printdist(distribution[5,])
library(ggplot2)

# ggplot(distribution_df, aes(x=beta,y=(abs(probability_difference))))+geom_point()
#with low beta values, it's very hard to get an absolute probability difference. With higher beta values, you're more likely to get a large difference in probability.
#now probability difference is a function of both beta and expected_value difference.

ggplot(distribution_df,aes(x=beta,y=expected_value,colour=exp(categorical_outcome1)))+
  geom_point()+
  scale_colour_gradientn(colors=c("red","black","blue"))+
  coord_cartesian(xlim=c(0,5))+labs(y="EV(Option 1)",color="Pr(Outcome 1)", 
                                    caption="Probability of Option 1 selection for different expected values and beta values, \n in a model of a participant selecting Option 1 given two Options in a 1-shot task. \n Note that stan did not explore the space with a very low probability of selection.\n Iterations in that space were generally rejected because the probability of an Option1 selection was very low." )
  
  

#the higher the categorical logit input, the more likely choice corresponds to expected value.

#k


#tau



##ThetaDeltaSDs
limits=c(0,10)
mygraphs<-list()
d_sd1<-density(abs(rcauchy(100000,0,2.5)),from = limits[1],to=limits[2])
mygraphs<-c(mygraphs,list(data.frame("x"=d_sd1$x,"y"=d_sd1$y,"dist"="HCauchy","m"=0,"sd"=2.5)))
ggplot(do.call(rbind,mygraphs),
       aes(x=x,y=y,group=interaction(as.factor(m),as.factor(sd),dist),
           color=interaction(as.factor(m),as.factor(sd),dist)))+geom_line()+labs(color="groups")



tp<-function(fam,d_prime){
  fam<-sapply(sapply(fam,max,0.001),min,0.999)
  print("discriminability of each of the distractors from the target, 0 to Inf:")
  distract_discriminability<- d_prime+logit(0.5+fam[1]/4*.999)+logit(0.5+fam/4*.999)[2:3]
  print(distract_discriminability)
  
  target<-sum(distract_discriminability)
  odds<-c(target,-distract_discriminability)
  #print("Odds of selecting the target vs the distractor:")
  #print(inv.logit(odds))
  return((0.5+inv.logit(odds))/sum(0.5+inv.logit(odds)))
}
tp(c(0,0.1,0.1),0)
tp(c(0,0.1,0.1),1)
tp(c(0,0.1,0.1),2)
tp(c(0,0.1,0.1),-1)
tp(c(0,0.2,0.2))
tp(c(0,0.2,0.7))
tp(c(0.3,0.2,0.7))
tp(c(0.6,0.2,0.7))
tp(c(0.9,0.2,0.7))
tp(c(0.9,0.4,0.7))
