rm(list=ls())
#setwd("13428_2016_746_MOESM1_ESM/LBA/")
library(rstan)
library(parallel)

options(mc.cores = 3)

nsubs<-1000
standard.seed<-9769847
covar_size<-5
covar_matrix = matrix(NA,covar_size,covar_size)
diag(covar_matrix)<-1
library(boot)
for(i in 1:dim(covar_matrix)[1]){
  for (j in 1:dim(covar_matrix)[2]){
    if(i!=j){
      covar_matrix[i,j]<-inv.logit(rnorm(1,0,1))*1.5-0.75
      covar_matrix[j,i]<-covar_matrix[i,j]
    }
  }
}
#now generate some data based on that matrix.
data_size=400
modeled_data<-MASS::mvrnorm(n=data_size,mu = rep(0,covar_size),Sigma = covar_matrix)
cov(modeled_data)
#no dataa
fit.independent <- stan(file='model_test.stan', 
            data = list(covar_size=covar_size,covar=covar_matrix,modeled_data=modeled_data,N=data_size),
            warmup = 200, 
            iter = 300,
            chains = 6)
summary(fit.independent)$summary[1:2,]


covar_matrix2 = matrix(NA,covar_size,covar_size)
diag(covar_matrix2)<-1
library(boot)
for(i in 1:dim(covar_matrix2)[1]){
  for (j in 1:dim(covar_matrix2)[2]){
    if(i!=j){
      covar_matrix2[i,j]<-inv.logit(rnorm(1,0,1)/2-0.5)*sample(c(-1,1),1,replace=TRUE)
      covar_matrix2[j,i]<-covar_matrix2[i,j]
    }
  }
}
modeled_data2<-MASS::mvrnorm(n=data_size,mu = rep(0,covar_size),Sigma = covar_matrix)
cov(modeled_data2)
fit.independent <- stan(file='model_test_2.stan', 
                        data = list(covar_size=covar_size,modeled_data=modeled_data,modeled_data2=modeled_data2,N=data_size),
                        warmup = 200, 
                        iter = 300,
                        chains = 6)
summary(fit.independent)$summary[1:2,]


fit.independent_allowneg <- stan(file='model_test4.stan', 
                                 data = list(covar_size=covar_size,modeled_data=modeled_data,N=data_size),
                                 warmup = 400, 
                                 iter = 500,
                                 chains = 6)
summary(fit.independent_allowneg)$summary[1:2,]

fit.independent_c0 <- stan(file='model_test5.stan', 
                                 data = list(covar_size=covar_size,modeled_data=modeled_data,N=data_size),
                                 warmup = 400, 
                                 iter = 500,
                                 chains = 6)
summary(fit.independent_c0)$summary[1:2,]
