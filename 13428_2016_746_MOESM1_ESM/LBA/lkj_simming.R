library(rstan)
library(reshape2)
library(ggplot2)
set.seed(123)
sim_data <- list(x = rpois(30, 5), N = 30, R = 4) 

mymat<-matrix(c(1,0.8,0.3,0.8,1,0.4,0.3,0.4,1),ncol=3)
sqrt(mymat * t(mymat))

fit.independent <- stan(file='lkj1.stan', 
                        data = sim_data,
                        warmup = 500, 
                        iter = 1000,
                        chains = 1)

to_mat<-function(fit,val){#fit<-fit_SigmaOmega
  #val<-"Sigma"
  fitsum<-summary(fit)$summary
  print(val)
  matvals<-fitsum[grepl(paste0("^",val,"\\["),rownames(fitsum)),"mean"]
  matorder<-sqrt(length(matvals))
  return(matrix(matvals,nrow=matorder,ncol=matorder))
  
}

to_array_hist<-function(fit,val,breaks=100,lowerOnly=FALSE){#fit<-fit_ljk1
  #val<-"Omega1"
  fit_dist<-extract(fit)
  par(mfrow=c(dim(fit_dist[[val]])[2],dim(fit_dist[[val]])[3]))
  for (r in 1:dim(fit_dist[[val]])[2]){
    for (c in 1:dim(fit_dist[[val]])[3]){
      if(lowerOnly==FALSE || r>=c){
        hist(fit_dist[[val]][,r,c],
             xlim = c(-2,2),
             breaks=breaks
        )
      }else{
        hist(fit_dist[[val]][,c,r],
             xlim = c(-2,2),
             breaks=breaks
        )
      }
      
    }
  }
}

to_mat(fit.independent,"Omega01")
to_mat(fit.independent,"Omega1")
to_mat(fit.independent,"Omega2")
to_mat(fit.independent,"Omega5")
to_mat(fit.independent,"Omega10")

fit_ljk1 <- stan(file='lkj1.stan', 
                           data = sim_data,
                           warmup = 500, 
                           iter = 1000,
                           chains = 10)
to_array_hist(fit_ljk1,"Omega1",breaks=1000)

fitOmegaSigmasigma <- stan(file='lkj2.stan', 
                        data = sim_data,
                        warmup = 500, 
                        iter = 1000,
                        chains = 10)
to_array_hist(fitOmegaSigmasigma,"Omega",breaks=1000)
to_array_hist(fitOmegaSigmasigma,"Sigma",breaks=1000)

fitOmegaSigmasigma_chol <- stan(file='lkj3.stan', 
                           data = sim_data,
                           warmup = 500, 
                           iter = 1000,
                           chains = 1)


fit_wishart <- stan(file='wishart.stan', 
                                data = sim_data,
                                warmup = 500, 
                                iter = 1000,
                                chains = 10)
to_array_hist(fit_wishart,"inv_wishart4")
to_array_hist(fit_wishart,"inv_wishart5")
fit_wishart_extracted<-extract(fit_wishart)
cor.test(fit_wishart_extracted$inv_wishart5[,1,2],
         fit_wishart_extracted$inv_wishart5[,2,1])
cor.test(fit_wishart_extracted$inv_wishart5[,1,2],
         fit_wishart_extracted$inv_wishart5[,1,3])

cor.test(fit_wishart_extracted$inv_wishart5[,1,2],
         fit_wishart_extracted$inv_wishart5[,1,3])

fit_wishart2 <- stan(file='wishart2.stan', 
                    data = sim_data,
                    warmup = 500, 
                    iter = 1000,
                    chains = 1)

to_mat(fit_wishart,"inv_wishart5")

fitOmegaSigmasigma_chol <- stan(file='multimethod.stan', 
                                data = sim_data,
                                warmup = 500, 
                                iter = 1000,
                                chains = 1)




fit_SigmaOmega <- stan(file='lkj4.stan', 
          data = sim_data,
          warmup = 500, 
          iter = 1000,
          chains = 10)

to_mat(fit_SigmaOmega,"Sigma")
to_array_hist(fit_SigmaOmega,"Sigma",breaks=100000)
to_array_hist(fit_SigmaOmega,"L_Sigma",breaks=100000)
to_array_hist(fit_SigmaOmega,"L_Omega",breaks=10000)
#some samples from the cholesky variance
fit_SigmaOmega_dist<-extract(fit_SigmaOmega)
dim(fit_SigmaOmega_dist$cholesky_variance)
empirical_covmat<-apply(fit_SigmaOmega_dist$cholesky_variance,1,cov)
dim(empirical_covmat)
empirical_covmat_median<-apply(empirical_covmat,1,median)
empirical_covmat_sd<-apply(empirical_covmat,1,sd)
matrix(empirical_covmat_sd,nrow=4)
cov(fit_SigmaOmega_dist$cholesky_variance[1000,,])
cov(fit_SigmaOmega_dist$cholesky_variance[1001,,])
cov(fit_SigmaOmega_dist$cholesky_variance[1002,,])
cov(fit_SigmaOmega_dist$cholesky_variance[1003,,])
cov(fit_SigmaOmega_dist$cholesky_variance[1004,,])
cov(fit_SigmaOmega_dist$cholesky_variance[1005,,])
cov(fit_SigmaOmega_dist$cholesky_variance[1006,,])
cov(fit_SigmaOmega_dist$cholesky_variance[1007,,])
cov(fit_SigmaOmega_dist$cholesky_variance[1008,,])
cov(fit_SigmaOmega_dist$cholesky_variance[1009,,])
cov(fit_SigmaOmega_dist$cholesky_variance[1010,,])



fit_SigmaOmega <- stan(file='lkj7.stan', 
                       data = sim_data,
                       warmup = 500, 
                       iter = 1000,
                       chains = 10)

to_array_hist(fit_SigmaOmega,"L_Omega",breaks=100)




fit_SigmaOmega <- stan(file='lkj8.stan', 
                       data = sim_data,
                       warmup = 500, 
                       iter = 1000,
                       chains = 10)

to_array_hist(fit_SigmaOmega,"L_Omega",breaks=100)
to_array_hist(fit_SigmaOmega,"Sigma2",breaks=100)
to_array_hist(fit_SigmaOmega,"Sigma",breaks=100)

fit_SigmaOmega <- stan(file='lkj9.stan', 
                       data = sim_data,
                       warmup = 500, 
                       iter = 1000,
                       chains = 10)

to_array_hist(fit_SigmaOmega,"L_Omega",breaks=100)
to_array_hist(fit_SigmaOmega,"Sigma",breaks=100)

fit_SigmaOmega@stanmodel

fit_lkj10 <- stan(file='lkj10.stan', 
                       data = sim_data,
                       warmup = 500, 
                       iter = 1500,
                       chains = 10)

to_array_hist(fit_lkj10,"L_Sigma",breaks=10000,lowerOnly = TRUE)
fit_dist<-extract(fit_lkj10)
length(fit_dist)
library(data.table)
library(ggplot2)
fit_dist_rbindlist<-vector("list",prod((dim(fit_dist[["L_Sigma"]])[2:3])))
for (y in 1:dim(fit_dist[["L_Sigma"]])[2]){
  for(z in 1:dim(fit_dist[["L_Sigma"]])[3]){
    if(y>=z){
      blankval<-min(which(unlist(lapply(fit_dist_rbindlist,is.null))))
      fit_dist_rbindlist[[blankval]]<- data.table(r=y,c=z, Val=fit_dist[["L_Sigma"]][,y,z])
    }
  }
}
fit_dist_dt<-rbindlist(fit_dist_rbindlist)


ggplot(fit_dist_dt,aes(Val))+
  geom_histogram(bins=100)+
  xlim(-2,2)+coord_cartesian(ylim=c(0,450))+
  facet_grid(r~c)+labs(title="Priors for ")



to_array_hist(fit_SigmaOmega,"Sigma",breaks=100)
