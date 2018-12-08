
source("stanlba/lba_rl_setup.R")


#make simualated data
#n,   b,A,vs,     s,t0,
out = rlba(300,1,.5,c(3,2),1,0.4)
hist(out$rt,breaks=100)
rt = cbind(out$rt,out$resp)
len = length(rt[,1])
#run the Stan model

#control
fit_exp <- stan(file='stanlba/stanfiles/lba_speedtesting/lba_single_exp.stan', 
                data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                warmup = 500, 
                iter = 1000,
                chains = 3)
print(fit_exp)

# 
# Elapsed Time: 15.9508 seconds (Warm-up)
# 17.6518 seconds (Sampling)
# 33.6026 seconds (Total)
# 
# Iteration: 1000 / 1000 [100%]  (Sampling)
# 
# Elapsed Time: 16.4291 seconds (Warm-up)
# 17.0343 seconds (Sampling)
# 33.4634 seconds (Total)
# 
# Iteration: 1000 / 1000 [100%]  (Sampling)
# 
# Elapsed Time: 15.7239 seconds (Warm-up)
# 17.2711 seconds (Sampling)
# 32.995 seconds (Total)

fit_exp_logtrick <- stan(file='stanlba/stanfiles/lba_speedtesting/lba_single_exp_logprob.stan', 
                          data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                          warmup = 500, 
                          iter = 1000,
                          chains = 3)
print(fit_exp_logtrick)
#Seems to degrade performance - substantially less efficeint than control.


fit_exp_vectorization <- stan(file='stanlba/stanfiles/lba_speedtesting/lba_single_exp_vectorization.stan', 
                         data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                         warmup = 500, 
                         iter = 1000,
                         chains = 3)
print(fit_exp_vectorization)


# Elapsed Time: 15.2025 seconds (Warm-up)
# 14.7373 seconds (Sampling)
# 29.9399 seconds (Total)

# Elapsed Time: 15.6407 seconds (Warm-up)
# 15.5638 seconds (Sampling)
# 31.2045 seconds (Total)


# Elapsed Time: 16.7462 seconds (Warm-up)
# 13.8711 seconds (Sampling)
# 30.6173 seconds (Total)

fit_exp_Phiapprox <- stan(file='stanlba/stanfiles/lba_speedtesting/lba_single_exp_Phiapprox.stan', 
                         data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                         warmup = 500, 
                         iter = 1000,
                         chains = 3)
print(fit_exp_Phiapprox)

# 
# Elapsed Time: 12.3278 seconds (Warm-up)
# 13.2886 seconds (Sampling)
# 25.6165 seconds (Total)
# 
# Iteration: 800 / 1000 [ 80%]  (Sampling)
# Iteration: 1000 / 1000 [100%]  (Sampling)
# 
# Elapsed Time: 12.5507 seconds (Warm-up)
# 13.7712 seconds (Sampling)
# 26.3219 seconds (Total)
# 
# Iteration: 900 / 1000 [ 90%]  (Sampling)
# Iteration: 1000 / 1000 [100%]  (Sampling)
# 
# Elapsed Time: 14.5874 seconds (Warm-up)
# 12.1555 seconds (Sampling)
# 26.7429 seconds (Total)

#saving about 5 seconds...this might be a slight improvement, but it's nothing dramatic.

fit_exp_allimprovements <- stan(file='stanlba/stanfiles/lba_speedtesting/lba_single_exp_logprob_vectorization_Phiapprox.stan', 
                          data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                          warmup = 500, 
                          iter = 1000,
                          chains = 3,
                          control = list(adapt_delta=0.99))
print(fit_exp_allimprovements)
#putting them together, something seems to go wrong. I don't know why this is!

fit_exp_allimprovements2 <- stan(file='stanlba/stanfiles/lba_speedtesting/lba_single_exp_logprob_vectorization_Phiapprox_lite.stan', 
                                data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                                warmup = 500, 
                                iter = 1000,
                                chains = 3)
print(fit_exp_allimprovements2)
#putting them together, something seems to go wrong. I don't know why this is!

fit_exp_vectorization_Phiapprox <- stan(file='stanlba/stanfiles/lba_speedtesting/lba_single_exp_vectorization_Phiapprox.stan', 
                                data = list(RT=rt,LENGTH=len,NUM_CHOICES=2,A=0.01),
                                warmup = 500, 
                                iter = 1000,
                                chains = 3)
print(fit_exp_vectorization_Phiapprox)
#this seems to indicate we can go ahead with vectorization and Phi_approx although it's not CLEAR that it makes any difference.