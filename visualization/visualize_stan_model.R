source("util/get_cue_index_from_cum_freq.R")
library(dplyr)
#install.packages("pROC")
library(pROC)
visualize_stan_model <- function(modelfit,ds){
  # Compute AUC
  parVals <- rstan::extract(modelfit)
  pred <- reshape2::melt(apply(parVals$y_hat, c(2,3), mean))
  names(pred) <- c("subid", "trial", "pred")
  new_pred <- pred[pred$pred!=0,]
  all_data <- cbind(ds, new_pred[,-1])
  all_data$round_pred <- round(all_data$pred)
  auc_dat <- all_data %>% group_by(subid) %>% summarize(auc_score = as.numeric(roc(choice,round_pred)[["auc"]]))
  t.test(auc_dat$auc_score-.5)
  
  
  # Create cumulative frequency 
  all_data$cue_freq <- get_cue_index_from_cum_freq(all_data$cue, by_var = all_data$subid)
  
  # correct % of subject and model
  all_data$actual_correct <- ifelse(all_data$outcome==1, 1, 0)
  all_data$pred_correct <- ifelse(
    (all_data$round_pred==all_data$choice & all_data$outcome==1) 
    | (all_data$round_pred!=all_data$choice & all_data$outcome!=1)
    #if the rounded prediction is equal to 
    ,1, 0)
  
  
  plot_data <- all_data %>% 
    group_by(subid,cue_freq) %>% 
    summarize(actual_correct = mean(actual_correct),
              pred_correct = mean(pred_correct))
  
  ggplot(plot_data, aes(x = cue_freq, y = actual_correct, group = subid)) + 
    geom_line() + 
    geom_line(aes(y = pred_correct, color = I("red"))) + 
    facet_wrap(c("subid"))
  
  
}
