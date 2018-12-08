rm(list=ls())
#setwd("13428_2016_746_MOESM1_ESM/LBA/")
library(rstan)
library(parallel)

options(mc.cores = 3)

nsubs<-1000
standard.seed<-9769847
covariance_dist<-rnorm(nsubs,103,15)
population_iq<-rnorm(nsubs,105,14)
population_eq<-rnorm(nsubs,101,19)
# qnorm(0.6,0,1)
# pnorm(100,100,15)

independent_distribution<-list(N_SUB=nsubs,iq_score=population_iq,eq_score=population_eq)
covarying_distribution<-list(N_SUB=nsubs,
                             iq_score=rowMeans(cbind(population_iq,covariance_dist)),
                             eq_score=rowMeans(cbind(population_eq,covariance_dist)))


#multivar10 takes an arbitrary number of variables.
covarying_distribution_2<-list(y=cbind(rowMeans(cbind(population_iq,covariance_dist)),
                                       rowMeans(cbind(population_eq,covariance_dist))))
covarying_distribution_2$N_SUB<-dim(covarying_distribution_2$y)[1]
covarying_distribution_2$K_VAR<-dim(covarying_distribution_2$y)[2]


#let's through in an additional distribution. call it "height". we will make it normally distributed for simplicity.
#https://en.wikipedia.org/wiki/List_of_average_human_height_worldwide
population_height<-rnorm(nsubs,mean(c(175,162)),sd=10)
#let's imagine that height is orthogonal to the other two variables.
covarying_distribution_3<-list(y=cbind(rowMeans(cbind(population_iq,covariance_dist)),
                                       rowMeans(cbind(population_eq,covariance_dist)),
                                       population_height
))
covarying_distribution_3$N_SUB<-dim(covarying_distribution_3$y)[1]
covarying_distribution_3$K_VAR<-dim(covarying_distribution_3$y)[2]
covarying_distribution_3$y_mu_prior<-c(100,100,165)
covarying_distribution_3$y_sd_prior<-c(50,50,50)



covarying_distribution_4<-covarying_distribution_3
covarying_distribution_4$y<-t(covarying_distribution_3$y)

sex<-rbinom(nsubs,1,0.5)
covarying_distribution_5<-list(y=cbind(rowMeans(cbind(population_iq,covariance_dist)),
                                       rowMeans(cbind(population_eq,covariance_dist)),
                                       population_height,
                                       sex
))
covarying_distribution_5$N_SUB<-dim(covarying_distribution_5$y)[1]
covarying_distribution_5$K_VAR<-dim(covarying_distribution_5$y)[2]
covarying_distribution_5$y_mu_prior<-c(100,100,165,0.1)
covarying_distribution_5$y_sd_prior<-c(50,50,50,1)

#no dataa
fit.independent <- stan(file='iq_eq_model.stan', 
            data = independent_distribution,
            warmup = 500, 
            iter = 1000,
            init = list(list(mu_iq=100,mu_eq=15,var_iq=100,var_eq=15)),
            chains = 1)
plot(covarying_distribution$iq_score,covarying_distribution$eq_score)
fit.dependent <- stan(file='iq_eq_model.stan', 
                        data = covarying_distribution,
                        warmup = 500, 
                        iter = 1000,
                        init = list(list(mu_iq=100,mu_eq=15,var_iq=100,var_eq=15)),
                        chains = 1)
#first multivariate model.

fit.independent <- stan(file='iq_eq_model_multivar1.stan', 
                        data = independent_distribution,
                        warmup = 100, 
                        iter = 200,
                        init = list(list(var_iqeq=diag(1,2,2))),
                        chains = 1)
#try again with hopefully a faster design?
fit.independent <- stan(file='iq_eq_model_multivar2.stan', 
                        data = independent_distribution,
                        warmup = 100, 
                        iter = 200,
                        init = list(list(var_iqeq=diag(1,2,2))),
                        chains = 1)
#nope, don't think this is faster at all.
fit.dependent <- stan(file='iq_eq_model_multivar2.stan', 
                        data = covarying_distribution,
                        warmup = 100, 
                        iter = 200,
                        init = list(list(var_iqeq=diag(1,2,2))),
                        chains = 1)
#Although this might be faster when the distribution contains covariance, compared to when it doesn't!
#it's still very slow and not particularly efficient. Because it's such an easy problem, we do get there, but the result is not very satisfying.
#demo below: the model correctly gets all this correctly.
#we don't get posterior estimates of the means, which is a problem.
sqrt(var(covarying_distribution$iq_score,covarying_distribution$iq_score))==sd(covarying_distribution$iq_score)
sqrt(var(covarying_distribution$eq_score,covarying_distribution$eq_score))==sd(covarying_distribution$eq_score)
var(covarying_distribution$iq_score,covarying_distribution$iq_score)
var(covarying_distribution$iq_score,covarying_distribution$eq_score)
var(covarying_distribution$eq_score,covarying_distribution$eq_score)
mean(covarying_distribution$iq_score)
mean(covarying_distribution$eq_score)

#this model estimates a distribution for mu as well:
fit.dependent <- stan(file='iq_eq_model_multivar3.stan', 
                      data = covarying_distribution,
                      warmup = 100, 
                      iter = 200,
                      init = list(list(var_iqeq=diag(1,2,2))),
                      chains = 1)
#i'm having problems getting this to accurately estimate means, 
#however, thisi s probably the wrong way to do it anyway. Let's take al ook at the stan manual on reprarameterziation.


fit.dependent <- stan(file='iq_eq_model_multivar4.stan', 
                      data = covarying_distribution,
                      warmup = 100, 
                      iter = 200,
                      init = list(list(var_iqeq=diag(1,2,2))),
                      chains = 1)


fit.dependent <- stan(file='iq_eq_model_multivar5.stan', 
                      data = covarying_distribution,
                      warmup = 100, 
                      iter = 200,
                      init = list(list(var_iqeq=diag(1,2,2))),
                      chains = 1)



fit.dependent <- stan(file='iq_eq_model_multivar6.stan', 
                      data = covarying_distribution,
                      warmup = 100, 
                      iter = 200,
                      init = list(list(var_iqeq=diag(1,2,2))),
                      chains = 1)

#now try to pull out explicitly the covariance matrix

fit.dependent <- stan(file='iq_eq_model_multivar7.stan', 
                      data = covarying_distribution,
                      warmup = 100, 
                      iter = 200,
                      init = list(list(var_iqeq=diag(1,2,2))),
                      chains = 1)
sqrt(var(covarying_distribution$iq_score,covarying_distribution$iq_score))==sd(covarying_distribution$iq_score)
sqrt(var(covarying_distribution$eq_score,covarying_distribution$eq_score))==sd(covarying_distribution$eq_score)
var(covarying_distribution$iq_score,covarying_distribution$iq_score)
var(covarying_distribution$iq_score,covarying_distribution$eq_score)
var(covarying_distribution$eq_score,covarying_distribution$eq_score)

#OK! var7 now explicitly pulls out a covariance matrix.

#multivar8 removes the predictor variables used in the example, because I don't think I'll need them.
#they're for multivariate regression, which we're not doing here.
#it's possible they will become useful, in which case, I can add them back in at some point
fit.dependent.mv8 <- stan(file='iq_eq_model_multivar8.stan', 
                      data = covarying_distribution,
                      warmup = 100, 
                      iter = 200,
                      init = list(list(var_iqeq=diag(1,2,2))),
                      chains = 1)
fit.dependent.mv8



#multivar9 attempts to estimate means
fit.dependent.mv9 <- stan(file='iq_eq_model_multivar9.stan', 
                      data = covarying_distribution,
                      warmup = 100, 
                      iter = 200,
                      init = list(list(var_iqeq=diag(1,2,2))),
                      chains = 1)
fit.dependent.mv9

#multivar9 attempts to estimate means
fit.dependent.mv9a <- stan(file='iq_eq_model_multivar9a.stan', 
                          data = covarying_distribution,
                          warmup = 200, 
                          iter = 400,
                          init = list(list(y_mu=c(100,100))),
                          chains = 1)
fit.dependent.mv9a

fit.dependent.mv9a <- stan(file='iq_eq_model_multivar9a.stan', 
                           data = covarying_distribution,
                           warmup = 100, 
                           iter = 200,
                           init = list(list(y_mu=c(100,100))),
                           chains = 1)
fit.dependent.mv9a




fit.dependent.m10d9a <- stan(file='iq_eq_model_multivar10d9a.stan', 
                           data = covarying_distribution_2,
                           warmup = 100, 
                           iter = 200,
                           init = list(list(y_mu=c(100,100))),
                           chains = 1)
fit.dependent.m10d9a




fit.dependent.m10d9a <- stan(file='iq_eq_model_multivar10d9a.stan', 
                             data = covarying_distribution_3,
                             warmup = 200, 
                             iter = 300,seed = standard.seed,
                             init = list(list(y_mu=covarying_distribution_3$y_mu_prior)),
                             chains = 1)
fit.dependent.m10d9a
#cat(fit.dependent.m10d9a@stanmodel@model_code)

#this one standardizes the linked parameters separately from the covariance estimate.
fit.dependent.m11d9a <- stan(file='iq_eq_model_multivar11d9a.stan', 
                             data = covarying_distribution_3,
                             warmup = 200, 
                             iter = 300,seed = standard.seed,
                             init = list(list(y_mu=covarying_distribution_3$y_mu_prior)),
                             chains = 1)
fit.dependent.m11d9a
#cat(fit.dependent.m11d9a@stanmodel@model_code)
#this doesn't seem to work, so we'll proceed without doing the standardization, I think.




fit.dependent.m11ad9a <- stan(file='iq_eq_model_multivar11ad9a.stan', 
                             data = covarying_distribution_4,
                             warmup = 200, 
                             iter = 300,seed = standard.seed,
                             init = list(list(y_mu=covarying_distribution_4$y_mu_prior)),
                             chains = 1)
fit.dependent.m11ad9a
#also dooesn't work!

fit.dependent.m11bd9a <- stan(file='iq_eq_model_multivar11bd9a.stan', 
                              data = covarying_distribution_3,
                              warmup = 200, 
                              iter = 300,seed = standard.seed,
                              init = list(list(y_mu=covarying_distribution_3$y_mu_prior)),
                              chains = 1)
fit.dependent.m11bd9a
#it works nicely. Awesome. Standardized covariance matrix!

#now can we compare performance with a version that has a chol() value of 1?
#i.e., an uninformative covariance matrix
fit.dependent.m12_d11b_d9a <- stan(file='iq_eq_model_multivar12_d11b_d9a.stan', 
                              data = covarying_distribution_3,
                              warmup = 200, 
                              iter = 300,seed = standard.seed,
                              init = list(list(y_mu=covarying_distribution_3$y_mu_prior)),
                              chains = 1)
fit.dependent.m12_d11b_d9a

fit.dependent.m12a_d11b_d9a <- stan(file='iq_eq_model_multivar12a_d11b_d9a.stan', 
                                   data = covarying_distribution_3,
                                   warmup = 200, 
                                   iter = 300,seed = standard.seed,
                                   init = list(list(y_mu=covarying_distribution_3$y_mu_prior)),
                                   chains = 1)
fit.dependent.m12a_d11b_d9a
#some hints that the diagnostic measures might be a bit better
#if we are estimating a standardiszed covaraicne matrix then we should start with
#standardized SDs.
#So let's proceed on the

# mat_var = sqrt(diag_matrix(Sigma));
# y_corr = inv(mat_var) * Sigma * inv(mat_var);

#correlations and covariances
#xample covar matrix
covar<-matrix(c(9,4,4,6),nrow = 2)
cov2cor(covar)
D=sqrt(diag(covar))
#now do that by hand.
inv(D) .* covar * inv(D)

S = matrix(c(
  1.0,  1.0,  8.1,
  1.0, 16.0, 18.0,
  8.1, 18.0, 81.0),nrow=3)
D=sqrt(diag(S))
DInv=1/D
DInv*S*DInv
cov2cor(S)
rep(DInv,3)*S*rep(DInv,3)
DInv*S*DInv
V=S
p <- (d <- dim(V))[1L]
if (!is.numeric(V) || length(d) != 2L || p != d[2L]) 
  stop("'V' is not a square numeric matrix")
Is <- sqrt(1/diag(V))
if (any(!is.finite(Is))) 
  warning("diag(.) had 0 or NA entries; non-finite result is doubtful")
r <- V
r[] <- matrix(rep(Is, times = p),nrow=3) * V * matrix(rep(Is, each = p),nrow=3)
matrix(rep(Is, each = p),nrow=3) * V * matrix(rep(Is, times = p),nrow=3)
r[cbind(1L:p, 1L:p)] <- 1
r

Sigma=C

inv_mat_var = 1/sqrt(diag(S));
y_corr = matrix(rep(inv_mat_var,each=3),nrow=3) * S * t(matrix(rep(inv_mat_var,each=3),nrow=3))





############################

fit.dependent.m13_d12a_d11b_d9a <- stan(file='iq_eq_model_multivar13_d12a_d11b_d9a.stan', 
                                    data = covarying_distribution_5,
                                    warmup = 200, 
                                    iter = 300,seed = standard.seed,
                                    init = list(list(y_mu=covarying_distribution_5$y_mu_prior)),
                                    chains = 1)
fit.dependent.m13_d12a_d11b_d9a


covarying_distribution_5$cauchy_gamma=1
covarying_distribution_5$cholesky_param=1

fit.dependent.m13a_d12a_d11b_d9a <- stan(file='iq_eq_model_multivar13_d12a_d11b_d9a.stan', 
                                        data = covarying_distribution_5,
                                        warmup = 200, 
                                        iter = 300,seed = standard.seed,
                                        init = list(list(y_mu=covarying_distribution_5$y_mu_prior)),
                                        chains = 1)
fit.dependent.m13a_d12a_d11b_d9a

covarying_distribution_5$cauchy_gamma=4

fit.dependent.m13a_d12a_d11b_d9a_cg4 <- stan(file='iq_eq_model_multivar13_d12a_d11b_d9a.stan', 
                                         data = covarying_distribution_5,
                                         warmup = 200, 
                                         iter = 300,seed = standard.seed,
                                         init = list(list(y_mu=covarying_distribution_5$y_mu_prior)),
                                         chains = 1)
fit.dependent.m13a_d12a_d11b_d9a_cg4

covarying_distribution_5$cauchy_gamma=1
covarying_distribution_5$cholesky_param=3

fit.dependent.m13a_d12a_d11b_d9a_cg1ch3 <- stan(file='iq_eq_model_multivar13_d12a_d11b_d9a.stan', 
                                             data = covarying_distribution_5,
                                             warmup = 200, 
                                             iter = 300,seed = standard.seed,
                                             init = list(list(y_mu=covarying_distribution_5$y_mu_prior)),
                                             chains = 1)
fit.dependent.m13a_d12a_d11b_d9a_cg1ch3

covarying_distribution_5$cauchy_gamma=1
covarying_distribution_5$cholesky_param=0.4

fit.dependent.m13a_d12a_d11b_d9a_cg1ch04 <- stan(file='iq_eq_model_multivar13_d12a_d11b_d9a.stan', 
                                                data = covarying_distribution_5,
                                                warmup = 200, 
                                                iter = 300,seed = standard.seed,
                                                init = list(list(y_mu=covarying_distribution_5$y_mu_prior)),
                                                chains = 1)
fit.dependent.m13a_d12a_d11b_d9a_cg1ch04

