# create_simulated_dataset<-function(){
# }

#generate group mean and sigma alpha and beta values.
#start with values similar to what we actually estimated
#look up the values. I think we need to apply that phi approximation; should be fine to do the actual function rather than just he approximation

# A fast approximation to the cumulative unit normal distribution function Φ is im- plemented in Stan as the function Phi_approx. The approximate probit regression model may be coded with the following.
# y[n] ~ bernoulli(Phi_approx(alpha + beta * x[n]));
pnorm(1.5)
#library(boot)
#Phi_approx<-function(x){inv.logit(0.07056*x^3+1.5976*x)}
#so yes, pnorm is what we need to transform our normals into 

#what values do we model from?
#let's try these summary stats I pulled from the du_model_rev5a:
get_param_col_means<-function(x){
  if(length(dim(x))==1){
    return(mean(x))
  }else if(length(dim(x))==2){
    return(colMeans(x))
  }else{
    
  }
}
# > cbind(lapply(rstan::extract(model.stanfits[[1]]),mean),lapply(rstan::extract(model.stanfits[[2]]),mean),lapply(rstan::extract(model.stanfits[[3]]),mean))
#                       [,1]        [,2]       [,3]        
# group_pr_mu           -1.11294    -1.02921   -1.207097   
# group_pr_sigma        0.5042885   0.577777   0.4992102   
# group_pr_rpdiff_mu    -0.05776922 0.02649249 -0.01221717 
# group_pr_rpdiff_sigma 0.2571479   0.1956869  0.306303    
# alpha_s_pr_mu         -0.4971525  -0.4356513 -0.7548656  
# beta_s_pr_mu          -1.736311   -1.626549  -1.672656   
# alpha_s_pr_rpdiff_mu  -0.1587205  0.02275932 -0.01621103 
# beta_s_pr_rpdiff_mu   0.04292631  0.03070179 -0.007735852
# alpha_s_pr_sigma      0.3948113   0.2856412  0.3453466   
# beta_s_pr_sigma       0.1700316   0.1318389  0.1428143   
# alpha_pr              -0.4975857  -0.4356989 -0.754887   
# beta_pr               -1.736607   -1.626652  -1.672801   
# alpha                 0.3655653   0.3890362  0.2891645   
# beta                  0.6460625   0.794882   0.7260629   
# group_mu_alpha        0.3130653   0.333738   0.231109    
# group_mu_beta         0.5822459   0.7308061  0.6672794   
# group_sigma_alpha     0.7940012   0.829988   0.7916435   
# group_sigma_beta      8.01848     8.091897   8.004766    
# group_rew_mu_alpha    0.286203    0.3378969  0.2294421   
# group_rew_mu_beta     0.6098083   0.7542341  0.6631916   
# group_pun_mu_alpha    0.3416397   0.3298749  0.2340018   
# group_pun_mu_beta     0.5566619   0.7084657  0.6732218   
# lp__                  -23179.24   -27737.82  -17305.79  

# MODEL 1 (GROUP 1)
# $group_pr_mu	[1] -0.4909911 -1.7348893
# $group_pr_sigma	[1] 0.8252644 0.1833125
# $group_pr_rpdiff_mu	[1] -0.15812869  0.04259026
# $group_pr_rpdiff_sigma	[1] 0.42305243 0.09124341
# $alpha_s_pr_sigma	[1] 0.3948113
# $beta_s_pr_sigma	[1] 0.1700316
# $group_mu_alpha	[1] 0.3130653
# $group_mu_beta	[1] 0.5822459
# $group_sigma_alpha	[1] 0.7940012
# $group_sigma_beta	[1] 8.01848
# $group_rew_mu_alpha	[1] 0.286203
# $group_rew_mu_beta	[1] 0.6098083
# $group_pun_mu_alpha	[1] 0.3416397
# $group_pun_mu_beta	[1] 0.5566619
# $lp__	[1] -23179.24
# MODEL 2 (GROUP 2)
# 
# $group_pr_mu	[1] -0.4325632 -1.6258576
# $group_pr_sigma	[1] 0.9589362 0.1966178
# $group_pr_rpdiff_mu	[1] 0.02228016 0.03070482
# $group_pr_rpdiff_sigma	[1] 0.32770447 0.06366926
# $alpha_s_pr_sigma	[1] 0.2856412
# $beta_s_pr_sigma	[1] 0.1318389
# $group_mu_alpha	[1] 0.333738
# $group_mu_beta	[1] 0.7308061
# $group_sigma_alpha	[1] 0.829988
# $group_sigma_beta	[1] 8.091897
# $group_rew_mu_alpha	[1] 0.3378969
# $group_rew_mu_beta	[1] 0.7542341
# $group_pun_mu_alpha	[1] 0.3298749
# $group_pun_mu_beta	[1] 0.7084657
# $lp__	[1] -27737.82
# 
# MODEL 3 (GROUP 3)
# 
# $group_pr_mu	[1] -0.7431853 -1.6710092
# $group_pr_sigma	[1] 0.8175503 0.1808701
# $group_pr_rpdiff_mu	[1] -0.016748893 -0.007685442
# $group_pr_rpdiff_sigma	[1] 0.4867189 0.1258871
# $alpha_s_pr_sigma	[1] 0.3453466
# $beta_s_pr_sigma	[1] 0.1428143
# 
# $group_mu_alpha	[1] 0.231109
# $group_mu_beta	[1] 0.6672794
# $group_sigma_alpha	[1] 0.7916435
# $group_sigma_beta	[1] 8.004766
# $group_rew_mu_alpha	[1] 0.2294421
# $group_rew_mu_beta	[1] 0.6631916
# $group_pun_mu_alpha	[1] 0.2340018
# $group_pun_mu_beta	[1] 0.6732218
# $lp__	[1] -17305.79

#let's generate everything based on the normally distributed values, since it was generated this way originally...
#group-level mu and sigma, rew-pun diff values
#alpha
sim_group_pr_mu_1<- -0.4325632
sim_group_pr_sigma_1 <- 0.9589362
sim_group_pr_rpdiff_mu_1 <- 0.02228016
sim_group_pr_rpdiff_sigma_1 <- 0.32770447

#beta
sim_group_pr_mu_2<- -1.6258576
sim_group_pr_sigma_2 <- 0.1966178
sim_group_pr_rpdiff_mu_2 <- 0.03070482
sim_group_pr_rpdiff_sigma_2 <- 0.06366926

s_n<-20

sim_alpha_s_pr_sigma<- 0.2856412 #representation of within-subject variance of alpha run averages
sim_beta_s_pr_sigma<-0.1318389 #representation of within-subject variance of beta run averages

#generate subject-level distributions for runs
sim_alpha_s_pr_mu<-rnorm(s_n,sim_group_pr_mu_1,sim_group_pr_sigma_1)
sim_beta_s_pr_mu<-rnorm(s_n,sim_group_pr_mu_2,sim_group_pr_sigma_2)
#generate subject-level rew-pun diffs
sim_alpha_s_pr_rpdiff_mu<-rnorm(s_n,sim_group_pr_rpdiff_mu_1,sim_group_pr_rpdiff_sigma_1)
sim_beta_s_pr_rpdiff_mu<-rnorm(s_n,sim_group_pr_rpdiff_mu_2,sim_group_pr_rpdiff_sigma_2)



#still need some sigmas here, but I'll come back to that...
#each subject can have precisely two reward runs followed by two punishment runs
rew_pun_vector<-c(0.5,0.5,-0.5,-0.5)
runs_per_subj<-length(rew_pun_vector)
sim_alpha_pr <-matrix(NA,s_n,runs_per_subj)
sim_beta_pr<-  matrix(NA,s_n,runs_per_subj)
#generate run mean and sigma
for (s in 1:s_n){
  #s=2
  #for each subject, generate the run averages....
  sim_alpha_pr[s,] <- rnorm(runs_per_subj,sim_alpha_s_pr_mu[s]+rew_pun_vector*sim_alpha_s_pr_rpdiff_mu[s],sim_alpha_s_pr_sigma)
  sim_beta_pr[s,] <- rnorm(runs_per_subj,sim_beta_s_pr_mu[s]+rew_pun_vector*sim_beta_s_pr_rpdiff_mu[s],sim_beta_s_pr_sigma)
}

#TRANSFORMS
#Group level transforms.
sim_group_mu_1<- pnorm(sim_group_pr_mu_1)
sim_group_sigma_1<- pnorm(sim_group_pr_sigma_1)*14
sim_group_rpdiff_mu_1<- pnorm(sim_group_pr_rpdiff_mu_1)
#these do come out to what we expect based on the mean posterior values....


#beta
sim_group_mu_2<-pnorm(sim_group_pr_mu_2)
sim_group_sigma_2<-pnorm(sim_group_pr_sigma_2)*14
sim_group_rpdiff_mu_2<-pnorm(sim_group_pr_rpdiff_mu_2)

#reward and punishment-specific group-level transforms.
#alpha
sim_group_mu_1_rew<-pnorm(sim_group_pr_mu_1+0.5*sim_group_pr_rpdiff_mu_1)
sim_group_mu_1_pun<-pnorm(sim_group_pr_mu_1-0.5*sim_group_pr_rpdiff_mu_1)
#and these also look like what the Bayesian model averages were.

#beta
sim_group_mu_2_rew<-pnorm(sim_group_pr_mu_2+0.5*sim_group_pr_rpdiff_mu_2)*14
sim_group_mu_2_pun<-pnorm(sim_group_pr_mu_2-0.5*sim_group_pr_rpdiff_mu_2)*14
#these check out.

#subject level transforms
sim_alpha<-pnorm(sim_alpha_pr)
sim_beta<-pnorm(sim_beta_pr)*14

#we gotta load the actual data.