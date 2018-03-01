library(rstan)

N=1000000
#so we make y our target distribution, model x on a normal distribution, then try to find the best way to predict y from the target

#x<-rnorm(N,1,0)
x<-rnorm(N,0,1)
y<-exp(x)/exp(1)
hist(y)
mean(x)
mean(y)
dataList<-list(x=x,y=y, N=N)
# fit.nodata <- stan("categorical_rstan.stan")
# m1<- stan_model("categorical_rstan.stan")

set.seed(as.numeric(Sys.time())); 
fit <- sampling(m1, data = dataList,seed=sample.int(.Machine$integer.max-1000, 1))
ef<-rstan::extract(fit)
hist(ef$norm_mu)
#in this instance, can we say something interpretable about the odds of the outcomes?
#we can talk about the 