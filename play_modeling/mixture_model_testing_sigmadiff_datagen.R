mu=c(1,1)
sigma=c(1,6)
lambda=0.4

#simulated data
set.seed(689934)

N <- 1000
G <- 100 #100 groups, each with 10 samples.
group_z <- rbinom(G, 1, lambda) + 1;
#group map
y_group <-rep(seq(1,100,1),each=10)
y_z <- group_z[y_group]
y <- rnorm(N, mu[y_z], sigma);

group_size_max<-max(table(y_group))

library(rstan)
rstan_options(auto_write = TRUE)

stan_rdump(c("N", "G","y","y_group","group_size_max"), file="mix.data.sigmadiff.R")
