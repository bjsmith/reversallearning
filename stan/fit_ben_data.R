rm(list = ls())

library(rstan)
library(loo)
library(dplyr)
library(pROC)
library(ggplot2)

# Read in raw data
rawdata <- read.table("~/Downloads/all_subjs_datacomplete_reward.txt", header = T)

# Filter out some subjects
rawdata <- subset(rawdata, subjID<1000 & choice != 0 & cue != 0)
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
  outcome[i, 1:useTrials] <- ifelse(tmp$outcome==-1, 0, tmp$outcome)
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
m5 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v4.stan")
m6 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v5.stan")

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
fit5 <- vb(m5, data = dataList, 
           pars = c("mu_alpha", "mu_beta", 
                    "sigma",
                    "alpha", "beta", 
                    "log_lik", "y_hat"), 
           adapt_engaged = F, eta = 1)
fit6 <- vb(m6, data = dataList, 
           pars = c("mu_alpha", "mu_alpha_fic", "mu_beta", 
                    "sigma",
                    "alpha", "alpha_fic", "beta", 
                    "log_lik", "y_hat"), 
           adapt_engaged = F, eta = 1)

fit4_mc <- sampling(m4, data = dataList, 
                    pars = c("mu_alpha", "mu_beta", 
                             "sigma",
                             "alpha", "beta", 
                             "log_lik", "y_hat"), 
                    iter = 500, warmup = 200, 
                    chains = 4, cores = 4)

traceplot(fit6)
stan_plot(fit6, "alpha", show_density = T)
loo(rstan::extract(fit4_mc)$log_lik)

# Compute AUC
parVals <- rstan::extract(fit6)
pred <- reshape2::melt(apply(parVals$y_hat, c(2,3), mean))
names(pred) <- c("subjID", "trial", "pred")
new_pred <- pred[pred$pred!=0,]
all_data <- cbind(rawdata, new_pred[,-1])
all_data$round_pred <- round(all_data$pred)
auc_dat <- all_data %>% group_by(subjID) %>% summarize(auc_score = as.numeric(roc(choice,round_pred)[["auc"]]))
t.test(auc_dat$auc_score-.5)

# Function to replace cue index with it's cumulative frequency
freq_replace <- function(x,by_var=NULL) {
  if (is.null(by_var)) {
    new_x <- vector(length=length(x))
    un_x <- unique(x)
    for (i in un_x) {
      idx <- which(x %in% un_x[i])
      new_x[idx] <- seq_along(idx)
    }
  } else {
    new_x <- NULL
    for (n in unique(by_var)) {
      tmp_x     <- x[by_var==n]
      tmp_new_x <- vector(length=length(tmp_x))
      tmp_un_x  <- unique(tmp_x)
      for (i in tmp_un_x) {
        idx <- which(tmp_x %in% i)
        tmp_new_x[idx] <- seq_along(idx)
      }
      new_x <- c(new_x, tmp_new_x) 
    }
  }
  return(new_x)
}
# Create cumulative frequency 
all_data$cue_freq <- freq_replace(all_data$cue, by_var = all_data$subjID)

# correct % of subject and model
all_data$actual_correct <- ifelse(all_data$outcome==1, 1, 0)
all_data$pred_correct <- ifelse((all_data$round_pred==all_data$choice & all_data$outcome==1) | (all_data$round_pred!=all_data$choice & all_data$outcome!=1), 1, 0)

plot_data <- all_data %>% 
  group_by(subjID,cue_freq) %>% 
  summarize(actual_correct = mean(actual_correct),
            pred_correct = mean(pred_correct))

ggplot(plot_data, aes(x = cue_freq, y = actual_correct, group = subjID)) + 
  geom_line() + 
  geom_line(aes(x = cue_freq, y = pred_correct, group = subjID), color = I("red"))# + 
  #facet_wrap(c("subjID"))

