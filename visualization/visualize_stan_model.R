visualize_stan_model <- function(fit,ds){
  # Compute AUC
  parVals <- extract(fit)
  pred <- reshape2::melt(apply(parVals$y_hat, c(2,3), mean))
  names(pred) <- c("subjID", "trial", "pred")
  new_pred <- pred[pred$pred!=0,]
  all_data <- cbind(ds, new_pred[,-1])
  all_data$round_pred <- round(all_data$pred)
  auc_dat <- all_data %>% group_by(subjID) %>% summarize(auc_score = as.numeric(roc(choice,round_pred)[["auc"]]))
  t.test(auc_dat$auc_score-.5)
  
  
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
    geom_line(aes(y = pred_correct, color = I("red"))) + 
    facet_wrap(c("subjID"))
  
  
}
