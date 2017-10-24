#rm(list = ls())

library(rstan)
library(loo)

# Read in raw data
rawdata.all <- read.table("../data/run1_subjs_datacomplete_reward.txt", header = T)
# Filter out some subjects
rawdata.group2 <- subset(rawdata.all, subid<155 & choice != 0 & cue != 0 & RiskCat==2)
length(unique(rawdata.group2$subid))
rawdata.group4 <- subset(rawdata.all, subid<316 & choice != 0 & cue != 0 & RiskCat==3)
length(unique(rawdata.group4$subid))

rawdata<-rbind(rawdata.group2,rawdata.group4)

rawdata.rewardonly<-subset(rawdata.all, subid<211 & choice != 0 & cue != 0 & RiskCat %in% c(2,3) & Motivation=="reward"
                           &presentation_n<6 & (subid==107 | subid==115))
length(unique(rawdata.rewardonly$subid))

data.to.use<-rawdata.rewardonly
# Individual Subjects
subidgroup<-unique(data.to.use[,c("subid","RiskCat")])
subjList <- subidgroup$subid  # list of subjects x blocks
subjGroupList<-subidgroup$RiskCat #list of subject group
numSubjs <- length(subjList)  # number of subjects
Gr_N <- length(unique(subjGroupList))#number of groups
Tsubj    <- as.vector( rep( 0, numSubjs ) ) # number of trials for each subject
N_cues   <- as.vector( rep( 0, numSubjs ) ) # number of trials for each subject

for ( i in 1:numSubjs )  {
  #i<-1
  curSubj   <- subjList[ i ]
  Tsubj[i]  <- sum( data.to.use$subid == curSubj )  # Tsubj[N]
  N_cues[i] <- length(unique(data.to.use$cue))
}

# Setting maxTrials
maxTrials <- max(Tsubj)

choice  <- array(0, c(numSubjs, maxTrials) )
outcome <- array(0, c(numSubjs, maxTrials) )
cue     <- array(0, c(numSubjs, maxTrials) )
#cue     <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  #i<-numSubjs
  curSubj      <- subjList[i]
  useTrials    <- Tsubj[i]
  tmp          <- subset(data.to.use, data.to.use$subid == curSubj)
  choice[i, 1:useTrials]  <- tmp$choice
  correct_response[i, 1:useTrials]  <- tmp$cor_res
  outcome[i, 1:useTrials] <- tmp$outcome
  cue[i, 1:useTrials]     <- as.numeric(as.factor(tmp$cue))
}

dataList <- list(
  N        = numSubjs,
  T        = maxTrials,
  Tsubj    = Tsubj,
  subjGr  = subjGroupList,
  N_cues   = N_cues - 1,
  choice   = choice, #choice the subject made (in terms of a 0 or 1)
  #correct_choice = correct_response, #choice the subject should have made
  outcome  = outcome, 
  cue      = cue,
  numPars  = 2
)
#is this dataList structured properly?
dim(dataList$outcome)
dim(dataList$choice)
dim(dataList$cue)

# m1 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben.stan")
# m2 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v2.stan")
# m3 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_decay.stan")
# m4 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v3.stan")
base.model<- stan_model("stan/prl_ben_v3_orig.stan")
fit.base <- vb(base.model, data = dataList, 
           pars = c("mu_alpha", "mu_beta", 
                    "sigma",
                    "alpha", "beta", 
                    "log_lik", "y_hat"), 
           adapt_engaged = F, eta = 1)



traceplot(fit.base)
stan_plot(fit.base, "alpha", show_density = T)
loo(rstan::extract(fit.base)$log_lik)

parVals <- rstan::extract(fit.base)

pred <- reshape2::melt(apply(parVals$y_hat, c(2,3), mean))
names(pred) <- c("subjID", "trial", "pred")

new_pred <- pred[pred$pred!=0,]

source("visualization/visualize_stan_model.R")
visualize_stan_model(fit.base,rawdata.rewardonly)
View(rawdata.rewardonly)
#test that the rawdata file matches up with what we passed the estimator

cbind(dataList$cue[1,1:Tsubj[i]],
      dataList$outcome[1,1:Tsubj[i]],
      dataList$choice[1,1:Tsubj[i]],
      rawdata.rewardonly[rawdata.rewardonly$subid=="106",c("cue","outcome","choice")])
#group.model<- stan_model("stan/prl_ben_v3_group.stan")
# fit.group <- vb(group.model, data = dataList, 
#            pars = c("mu_alpha", "mu_beta", 
#                     "sigma",
#                     "alpha", "beta", 
#                     "log_lik", "y_hat"), 
#            adapt_engaged = F, eta = 1)
# 
# group.rp.model<- stan_model("stan/prl_ben_v3_group_rp.stan")
# fit.group.rp <- vb(group.rp.model, data = dataList, 
#            pars = c("mu_alpha", "mu_beta", 
#                     "sigma",
#                     "alpha", "beta", 
#                     "log_lik", "y_hat"), 
#            adapt_engaged = F, eta = 1)


# all_data <- cbind(rawdata, new_pred)
# all_data$round_pred <- round(all_data$pred)
# 
