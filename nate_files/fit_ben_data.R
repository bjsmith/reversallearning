rm(list = ls())

library(rstan)
library(loo)

# Read in raw data
rawdata <- read.table("~/Downloads/all_subjs_datacomplete_reward.txt", header = T)

# Filter out some subjects
rawdata <- subset(rawdata, subjID<150 & choice != 0 & cue != 0)
# subtract 1 for bernoulli dist
rawdata$choice <- rawdata$choice

# Individual Subjects
subjList <- unique(rawdata[,"subjID"])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects
Tsubj    <- as.vector( rep( 0, numSubjs ) ) # number of trials for each subject
N_cues   <- as.vector( rep( 0, numSubjs ) ) # number of trials for each subject

for ( i in 1:numSubjs )  {
  curSubj   <- subjList[ i ]
  Tsubj[i]  <- sum( rawdata$subjID == curSubj )  # Tsubj[N]
  N_cues[i] <- length(unique(rawdata$cue))
}

# Setting maxTrials
maxTrials <- max(Tsubj)

choice  <- array(0, c(numSubjs, maxTrials) )
outcome <- array(0, c(numSubjs, maxTrials) )
cue     <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  useTrials    <- Tsubj[i]
  tmp          <- subset(rawdata, rawdata$subjID == curSubj)
  choice[i, 1:useTrials]  <- tmp$choice
  outcome[i, 1:useTrials] <- tmp$outcome
  cue[i, 1:useTrials]     <- as.numeric(as.factor(tmp$cue))
}

dataList <- list(
  N        = numSubjs,
  T        = maxTrials,
  Tsubj    = Tsubj,
  N_cues   = N_cues - 1,
  choice   = choice, 
  outcome  = outcome,
  cue      = cue,
  numPars  = 2
)

m1 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben.stan")
m2 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v2.stan")
m3 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_decay.stan")
m4 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v3.stan")

fit1 <- vb(m1, data = dataList, 
           pars = c("mu_alpha", "mu_beta", 
                    "sigma",
                    "alpha", "beta", 
                    "log_lik"), 
           adapt_engaged = F, eta = 1)
fit2 <- vb(m2, data = dataList, 
           pars = c("mu_alpha", "mu_alpha_fic", "mu_beta", 
                    "sigma",
                    "alpha", "alpha_fic", "beta", 
                    "log_lik"), 
           adapt_engaged = F, eta = 1)
fit3 <- vb(m3, data = dataList, 
           pars = c("mu_A", "mu_beta", 
                    "sigma",
                    "A", "beta", 
                    "log_lik"), 
           adapt_engaged = F, eta = 1)
fit4 <- vb(m4, data = dataList, 
           pars = c("mu_alpha", "mu_beta", 
                    "sigma",
                    "alpha", "beta", 
                    "log_lik", "y_hat"), 
           adapt_engaged = F, eta = 1)

traceplot(fit4)
stan_plot(fit4, "alpha", show_density = T)
loo(extract(fit4)$log_lik)

parVals <- extract(fit4)

pred <- reshape2::melt(apply(parVals$y_hat, c(2,3), mean))
names(pred) <- c("subjID", "trial", "pred")

new_pred <- pred[pred$pred!=0,]

all_data <- cbind(rawdata, new_pred)
all_data$round_pred <- round(all_data$pred)

