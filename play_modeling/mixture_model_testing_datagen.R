mu=c(-2.75,2.75)
sigma=1
lambda=0.4

#simulated data
set.seed(689934)

N <- 1000
z <- rbinom(N, 1, lambda) + 1;
y <- rnorm(N, mu[z], sigma);

library(rstan)
rstan_options(auto_write = TRUE)

stan_rdump(c("N", "y"), file="mix.data.R")
