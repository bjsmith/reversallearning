source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")
#this evaluates a cholesky matrix without data - in other words, we're sampling from the prior here.
load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180713_1/run_package_106_1_punishment_lba_rl_single_exp_joint_v11_priorsamplejointh.RData")

myfit<-summary(run_package$fit)
myfit$summary[myfit$summary[,"n_eff"]<300,]

#L_Sigma values didn't converge - this is concerning if we want to work an unbiased sample of the posterior
#though maybe; L_sigma samples from cauch(0,1); this may just be intractable to do properly and arrive at anything stable.
#given the properties of a cauchy distribution.
summary(myfit$summary[,"n_eff"])

#what about Rhat values?
rownames(myfit$summary[!is.na(myfit$summary[,"Rhat"]) & myfit$summary[,"Rhat"]>1.1,])

#OK. What does the sigma heatmap look like?
#is it even worth checking? This goddamn thing won't converge.
#maybe try again with a smaller matrix?



load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180713_1/run_package_106_1_punishment_lba_rl_single_exp_joint_v11_priorsamplejointi.RData")

myfit<-summary(run_package$fit)
myfit$summary[myfit$summary[,"n_eff"]<300,]
myfit$summary[myfit$summary[,"Rhat"]<300,]

#what do the sigmas look like?
heatmap(get_sigma_array(myfit,DeltaThetaLabels = paste0("TD",1:7)))

myfit.summary.dt<-data.table(myfit$summary)
heatmap(matrix(myfit$summary[paste0("Sigma[",rep(1:7,times=7),",",rep(1:7,each=7),"]"),"mean"],nrow=7),
        order=FALSE)
