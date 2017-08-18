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

logsumexp <- function (x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

softmax <- function (x) {
  exp(x - logsumexp(x))
}

plot_both <- function(x,y) {
  miny <- min(x,y) 
  maxy <- max(x,y) 
  plot.ts(x, ylim = c(miny, maxy), col = "red") 
  par(new=T) 
  plot.ts(y, ylim = c(miny, maxy), col = "blue")
}
library(dplyr)
library(LaplacesDemon)
# Read in raw data
rawdata <- read.table("../data/run1_subjs_datacomplete_reward.txt", header = T)
names(rawdata)[1] <- c("subjID")

rawdata <- rawdata %>% select(-image)

# Filter out some subjects
rawdata <- subset(rawdata, subjID<200 & choice != 0 & cue != 0)
# subtract 1 for bernoulli dist
rawdata$choice <- rawdata$choice

# Create cue frequency
rawdata$cue_freq <- freq_replace(rawdata$cue, by_var = rawdata$subjID)
#sim_datasub120 <- subset(rawdata, subjID==120)
rawdata[rawdata$subjID==106,"outcome"]
rawdata[rawdata$subjID==107,"outcome"]
rawdata[rawdata$subjID==108,"outcome"]==rawdata[rawdata$subjID==107,"outcome"]
# Simulate from here
for (sid in unique(rawdata$subjID)){
  print(paste0("using subject data from",sid))
  sim_data <- rawdata[rawdata$subjID==sid,]#subset(rawdata, subjID=="subjID")
  choice   <- sim_data$choice
  cue      <- as.numeric(as.factor(sim_data$cue))
  cue_freq <- sim_data$cue_freq
  presentation_n_after_reversal <- sim_data$presentation_n_after_reversal
  outcome <- ifelse(sim_data$outcome==-1,0,sim_data$outcome)
  
  # initialize model variables
  #matrix of response by image
  ev <- matrix(0,nrow = 18, ncol = 2)
  y_hat <- rep(NA,dim(sim_data)[1])
  
  # Simulation parameters
  learning <- TRUE
  
  alpha = 1 # learning rate
  A = .4 #decay model parameter
  beta = 3 #softmax
  
  for (t in 1:length(sim_data$subjID)) {#iterating through trials.
    #print(paste0("Trial ",t,"; cue ",cue[t],":"))
    #print(paste0("Current EV is ",paste0(ev[cue[t],],collapse = ",")))
    y_hat[t] = LaplacesDemon::rcat(1, softmax( ev[cue[t],] * beta))
    #y_hat[t] = which.max(ev[cue[t],])#LaplacesDemon::rcat(1, softmax( ev[cue[t],] * beta))
    if (learning) { # value updating (learning)
      # PE   =  outcome[t] - ev[cue[t],choice[t]]
      # ev[cue[t],choice[t]] = ev[cue[t],choice[t]] + alpha * PE;
      PE   =  outcome[t] - ev[cue[t],y_hat[t]]
      ev[cue[t],choice[t]] = ev[cue[t],y_hat[t]] + alpha * PE;
    } else { # value updating (memory decay)
      ev = ev * A
      ev[cue[t],choice[t]] = outcome[t]
      ev[cue[t],3-choice[t]] = -outcome[t];
    }
  }
  
  # correct % of subject and model
  actual_correct <- outcome
  pred_correct <- ifelse((y_hat==choice & outcome==1) | (y_hat!=choice & outcome!=1), 1, 0)
  
  sum_dat <- data.frame(cue_freq = cue_freq,
                        actual_correct = actual_correct,
                        pred_correct = pred_correct,
                        presentation_n_after_reversal=presentation_n_after_reversal)
  
  library(ggplot2)
  plot_data <- sum_dat %>% 
    group_by(cue_freq) %>% 
    summarize(actual_cor = mean(actual_correct),
              pred_cor = mean(pred_correct),
              se_cor = 2 * sqrt((pred_cor*(1-pred_cor))/length(pred_correct)))
  
  ggplot(plot_data, aes(x = presentation_n_after_reversal, y = actual_cor)) + 
    geom_line() + 
    geom_ribbon(aes(ymin=actual_cor - se_cor, ymax=actual_cor + se_cor), alpha = 0.2) + 
    geom_line(aes(y = pred_cor, color = I("red")))+labs(title=as.character(sid))
  
  ggsave(paste0('sub',sid,'.png'))
  
}
