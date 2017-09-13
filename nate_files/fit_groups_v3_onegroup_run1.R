rm(list = ls())

library(rstan)
library(loo)
library(dplyr)
library(pROC)
library(ggplot2)

#setwd("~/Box Sync/MIND_2017/Hackathon/Ben/reversallearning/nate_files")
setwd("nate_files")
source("Misc/freq_replace.R")
source("Misc/plot_model.R")

# Use pain as outcome?
pain_outcome <- F

# Which dataset to fit? 
meth <- T  # Meth T/F
risk <- T  # Risky sex T/F

# Which model to use?
models <- c("simple_delta", "simple_delta_bias", "switch_lr", "simple_decay", 
            "switch_model", "double_update", "switch_decay", "switch_lr_double_update")
use_model <- "simple_decay_pain"

# Which run? 
run <- 1

# Read in raw data
rawdata <- read.table("Data/all_subjs_datacomplete_punishment_rm153.txt", header = T)
names(rawdata)[1] <- c("subjID")

# Separate groups (Risky Meth == 3, Risky No Meth == 2, Safe Meth == 4, Safe No Meth == 1)
if (meth & risk) {
  group <- 3
  descr <- "Risky_Meth"
} else if (!meth & risk) {
  group <- 2
  descr <- "Risky_NoMeth"
} else if (!meth & !risk) {
  group <- 1
  descr <- "Safe_NoMeth"
} else {
  group <- 4
  descr <- "Safe_Meth"
}
rawdata <- subset(rawdata, cue != 0 & RiskCat == group & runid == run)

# Create cue frequency
rawdata$cue_freq <- freq_replace(rawdata$cue, by_var = rawdata$subjID)

# Individual Subjects
subjList  <- unique(rawdata[,"subjID"])  # list of subjects x blocks
numSubjs  <- length(subjList)  # number of subjects
Tsubj     <- as.vector( rep( 0, numSubjs ) ) # number of trials for each subject
N_cues    <- as.vector( rep( 0, numSubjs ) ) # number of trials for each subject
pain_diff <- as.vector( rep( NA, numSubjs ) ) # number of trials for each subject

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
reversal <- array(0, c(numSubjs, maxTrials) )
trial <- array(0, c(numSubjs, maxTrials) )
subjid <- array(0, c(numSubjs, maxTrials) )
cue_pos <- array(0, c(numSubjs, maxTrials) )
cor_resp <- array(0, c(numSubjs, maxTrials) )
pain <- array(0, c(numSubjs, maxTrials) )

subj_ids_have <- as.numeric(gsub(x = list.files("Data/Pain_Betas"), pattern = "_.*_*.csv", replacement = ""))

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  useTrials    <- Tsubj[i]
  if (curSubj %in% subj_ids_have) {
    tmp_brain <- read.csv(paste0("Data/Pain_Betas/", curSubj, "_punishment_r1.csv"))
  }
  tmp <- subset(rawdata, rawdata$subjID == curSubj)
  choice[i, 1:useTrials]  <- tmp$choice
  if (pain_outcome) {
    outcome[i, 1:useTrials] <- scale(as.numeric(tmp_brain[,!grepl(pattern = "placeholder", x = names(tmp_brain))])[1:useTrials])
  } else {
    outcome[i, 1:useTrials] <- tmp$outcome#ifelse(tmp$outcome==1, 0, tmp$outcome)
  }
  cue[i, 1:useTrials]     <- as.numeric(as.factor(tmp$cue))
  cue_freq[i, 1:useTrials] <- tmp$cue_freq
  reversal[i, 1:useTrials] <- as.numeric(tmp$reversal_trial)
  cue_pos[i, 1:useTrials] <- tmp$presentation_n_after_reversal
  trial[i, 1:useTrials] <- as.numeric(as.factor(tmp$onset_time_actual))
  subjid[i, 1:useTrials] <- tmp$subjID
  cor_resp[i, 1:useTrials] <- tmp$cor_res
  if (curSubj %in% subj_ids_have) {
    tmp_noPlace <- tmp_brain[,!grepl(pattern = "placeholder", x = names(tmp_brain))][1:useTrials]
    pain[i, 1:useTrials] <- scale(as.numeric(tmp_brain[,!grepl(pattern = "placeholder", x = names(tmp_brain))])[1:useTrials])
    pain_diff[i] <- mean(as.numeric(tmp_noPlace[,grepl(pattern = "error", x = names(tmp_noPlace))])[1:useTrials], na.rm = T) - mean(as.numeric(tmp_noPlace[,grepl(pattern = "correct", x = names(tmp_noPlace))])[1:useTrials], na.rm = T)
  } else {
    pain_diff[i] <- NA
  }
}

trial[which(is.na(trial))] <- 0

dataList <- list(
  N        = numSubjs,
  T        = maxTrials,
  Tsubj    = Tsubj,
  N_cues   = N_cues - 1,
  choice   = choice, 
  outcome  = outcome,
  cue      = cue,
  cue_freq = cue_freq,
  reversal = reversal,
  trial = trial,
  cue_pos = cue_pos,
  cor_resp = cor_resp,
  subjid = subjid,
  pain = pain,
  numPars  = 2
)

m1 <- stan_model(paste0("Final_Models/", use_model,".stan"))

fit <- vb(m1, data = dataList, adapt_engaged = F, eta = 1)

# Compute AUC
parVals <- rstan::extract(fit)

#shape the data into a format suitable for plotting.
for_plot <- NULL
for_plot$subjID <- reshape2::melt(apply(parVals$p_subjID, c(2,3), mean, na.rm=T))[,3]
for_plot$cor_res <- reshape2::melt(apply(parVals$p_cor_res, c(2,3), mean, na.rm=T))[,3]
for_plot$outcome <- reshape2::melt(apply(parVals$p_outcome, c(2,3), mean, na.rm=T))[,3]
for_plot$cue_pos <- reshape2::melt(apply(parVals$p_cue_pos, c(2,3), mean, na.rm=T))[,3]
for_plot$cue_freq <- reshape2::melt(apply(parVals$p_cue_freq, c(2,3), mean, na.rm=T))[,3]
for_plot$y_hat <- round(reshape2::melt(apply(parVals$y_hat, c(2,3), mean, na.rm=T))[,3]); for_plot$y_hat[for_plot$y_hat==0] <- NA
for_plot$choice <- round(reshape2::melt(apply(parVals$p_choice, c(2,3), mean, na.rm=T))[,3]); for_plot$choice[for_plot$choice==0] <- NA
for_plot <- as.data.frame(for_plot)

all_pred <- array(NA, dim = c(500, length(for_plot$y_hat)))
for (i in 1:500) {
  tmp_y_hat <- reshape2::melt(parVals$y_hat[i,,])[,3]; tmp_y_hat[tmp_y_hat==0] <- NA
  all_pred[i,] <- ifelse((tmp_y_hat==for_plot$choice & for_plot$outcome==1) | (tmp_y_hat!=for_plot$choice & for_plot$outcome!=1), 1, 0)
}
for_plot$all_pred_correct <- apply(all_pred, 2, mean, na.rm=T)

# correct % of subject and model
for_plot$actual_correct <- ifelse(for_plot$outcome==1, 1, 0)
for_plot$pred_correct <- ifelse((for_plot$y_hat==for_plot$choice & for_plot$outcome==1) | (for_plot$y_hat!=for_plot$choice & for_plot$outcome!=1), 1, 0)

fit_data <- list(fit = fit, plot_object = for_plot)
plot_model(fit_data)
save(fit_data, file = paste0("Fits/", use_model, "_", descr, "_rew1_pun-1.RData"))
