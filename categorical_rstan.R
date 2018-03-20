library(rstan)

N=1000
x<-rnorm(N,2.5,1)
y<-rbinom(length(x),1,prob=pnorm(x))+1
dataList<-list(x=x,y=y, N=N)
# fit.nodata <- stan("categorical_rstan.stan")
#m1<- stan_model("categorical_rstan.stan")
#m1<- stan_model("categorical_rstan2.stan")
#m1<- stan_model("categorical_rstan3.stan")#works.
m1<- stan_model("categorical_rstan4.stan")

set.seed(as.numeric(Sys.time())); 
#fit <- sampling(m1, data = dataList,seed=sample.int(.Machine$integer.max-1000, 1))
vbfit<- vb(m1, data = dataList, adapt_engaged = F, eta = 1,iter=2000,output_samples=1000)
fit<-vbfit
ef<-rstan::extract(fit)
hist(ef$norm_mu)
#in this instance, can we say something interpretable about the odds of the outcomes?
#we can talk about the 