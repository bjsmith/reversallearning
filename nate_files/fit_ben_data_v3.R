rm(list = ls())

library(rstan)
library(loo)
library(dplyr)
library(pROC)
library(ggplot2)

# Function to replace cue index with it's cumulative frequency
freq_replace <- function(x,by_var=NULL) {
  source("util/get_cue_index_from_cum_freq.R")
  return(get_cue_index_from_cum_freq(x,by_var))
}

# Read in raw data
rawdata <- read.table("~/Downloads/all_subjs_datacomplete_reward_fixed.txt", header = T)
names(rawdata)[1] <- c("subjID")
rawdata <- rawdata %>% select(-trial)

# Filter out some subjects
rawdata <- subset(rawdata, subjID<200 & choice != 0 & cue != 0)
# subtract 1 for bernoulli dist
rawdata$choice <- rawdata$choice

# Create cue frequency
rawdata$cue_freq <- freq_replace(rawdata$cue, by_var = rawdata$subjID)

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

choice   <- array(0, c(numSubjs, maxTrials) )
outcome  <- array(0, c(numSubjs, maxTrials) )
cue      <- array(0, c(numSubjs, maxTrials) )
cue_freq <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  useTrials    <- Tsubj[i]
  tmp          <- subset(rawdata, rawdata$subjID == curSubj)
  choice[i, 1:useTrials]  <- tmp$choice
  outcome[i, 1:useTrials] <- ifelse(tmp$outcome==-1, 0, tmp$outcome)
  cue[i, 1:useTrials]     <- as.numeric(as.factor(tmp$cue))
  cue_freq[i, 1:useTrials] <- tmp$cue_freq
}

dataList <- list(
  N        = numSubjs,
  T        = maxTrials,
  Tsubj    = Tsubj,
  N_cues   = N_cues - 1,
  choice   = choice, 
  outcome  = outcome,
  cue      = cue,
  cue_freq = cue_freq,
  numPars  = 2
)

m1 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben.stan")
m2 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v2.stan")
m3 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_decay.stan")
m4 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v3.stan")
m5 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v4.stan")
m6 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v5.stan")
m7 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v6.stan")
m8 <- stan_model("~/Box Sync/MIND_2017/Hackathon/Ben/prl_ben_v7.stan")

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
fit7 <- vb(m7, data = dataList, 
           pars = c("mu_alpha", "mu_beta", "mu_bias", 
                    "sigma",
                    "alpha", "beta", "bias",
                    "log_lik", "y_hat"), 
           adapt_engaged = F, eta = 1)
fit8 <- vb(m8, data = dataList, 
           pars = c("alpha", "beta", "bias",
                    "log_lik", "y_hat"), 
           adapt_engaged = F, eta = 1)

fit4_mc <- sampling(m4, data = dataList, 
                    pars = c("mu_alpha", "mu_beta", 
                             "sigma",
                             "alpha", "beta", 
                             "log_lik", "y_hat"), 
                    iter = 500, warmup = 200, 
                    chains = 4, cores = 4)
fit7_mc <- sampling(m7, data = dataList, 
                    pars = c("mu_alpha", "mu_beta", "mu_delta", 
                             "sigma",
                             "alpha", "beta", "delta",
                             "log_lik", "y_hat"),  
                    iter = 200, warmup = 50, 
                    chains = 4, cores = 4)

traceplot(fit3)
stan_plot(fit7, "alpha", show_density = T)
loo(extract(fit3)$log_lik)

source("visualization/visualize_stan_model.R")