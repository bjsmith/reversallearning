#simplest case. Two distributions, and we model whether data is more likely to have come from one distribution or the other one.
mu=c(-2.75,2.75)
sigma=1
lambda=0.4

#simulated data
set.seed(689934)

N <- 1000
z <- rbinom(N, 1, lambda) + 1;
y <- rnorm(N, mu[z], sigma);

#right, so each of these individual samples will come from a distribution centered on either -2.75 or 2.75

#start stan

library(rstan)
rstan_options(auto_write = TRUE)

stan_rdump(c("N", "y"), file="mix.data.R")


input_data <- read_rdump("mix.data.R")


degenerate_fit <- stan(file='play_modeling/gauss_mix.stan', data=input_data,
                       chains=4, seed=483892929, refresh=2000)

degenerate_fit

#alright, but what if we move something which is in analogy slightly closer to what we have in our study,
#which is grouped data; each dataset comes from either one distribution or another.
#this is a mixture model to determine which subjects are using which strategy (random vs. actual learning)

input_data <- read_rdump("mix.data.grouped.R")
input_data$y_group
group_test_fit <- stan(file='play_modeling/gauss_mix_grouped.stan', data=input_data,
                       chains=4, seed=483892929, refresh=2000)

group_test_fit

group_identified_test_fit <- stan(file='play_modeling/gauss_mix_grouped_2.stan', data=input_data,
                       chains=4, seed=483892929, refresh=2000)
group_identified_test_fit


group_identified_test_fit <- stan(file='play_modeling/gauss_mix_grouped_ordered.stan', data=input_data,
                                  chains=4, seed=483892929, refresh=2000)
group_identified_test_fit


#nice. now it works. What about another scenario in which it's the sigmas rather than the mus that differ?
input_data <- read_rdump("mix.data.sigmadiff.R")
sigmadiff_fit <- stan(file='play_modeling/sigmadiff.stan', data=input_data,
                                  chains=4, seed=483892929, refresh=2000)
sigmadiff_fit

#interesting. it underestimates the difference in the sigmas here because 
#it is not strongly enough biased to separate the two groups.
#I'm not sure what can be done to fix this problem; it's likely to come up in my own work.


input_data <- read_rdump("mix.data.sigmadiff.R")
sigmadiff_fit <- stan(file='play_modeling/sigmadiff_scenarios.stan', data=input_data,
                      chains=4, seed=483892929, refresh=2000)
sigmadiff_fit

#if we were going to apply this to the discriminability model, we'd need a mixtture between#
#more than two models

input_data <- read_rdump("mix.data.3components.R")
sigmadiff_fit <- stan(file='play_modeling/mixture_3scenarios.stan', data=input_data,
                      chains=4, seed=483892929, refresh=2000)
sigmadiff_fit
