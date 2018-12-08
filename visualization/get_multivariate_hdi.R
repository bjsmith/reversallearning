get_multivariate_hdi(vars){
  res<-cov.mve(var12,quantile.used=nrow(var12)*0.95)
}
#from rand's book - finding a 95% ellipse.
var12<-model.summary.all.g3g2compare.bypar[EstimationMethod=="MCMC" & 
                                             Statistic %in% c("pun_mu"),.(alpha,beta)]
var12<-data.frame(mvrnorm(n = 12000,mu = c(0.5,0.7),Sigma = matrix(c(2,1,1,2),nrow=2)))
colnames(var12)<-c("alpha","beta")
# S=cov(var12)
# X_mean = apply(var12,2,mean)
# sum((var12[,1]-X_mean[1])*(var12[,2]-X_mean[2]))/(nrow(var12)-1)
# sum((var12[,2]-X_mean[2])*(var12[,2]-X_mean[2]))/(nrow(var12)-1)
# 
# D_i<-sqrt((var12-t(X_mean))*solve(S)*t((var12-t(X_mean))))

library(MASS)

length(res$best)
# res$center+
var12$included<-1:nrow(var12) %in% res$best
library(ggplot2)

ggplot(var12,aes(alpha,beta))+stat_ellipse(type="norm")+geom_point(aes(color=included))
