library(magrittr)
plot_model <- function(x, single = T, compute_auc = T) {
  parVals <- rstan::extract(x$fit)
  
  # for_plot <- NULL
  # for_plot$subjID <- reshape2::melt(apply(parVals$p_subjID, c(2,3), mean, na.rm=T))[,3]
  # for_plot$cor_res <- reshape2::melt(apply(parVals$p_cor_res, c(2,3), mean, na.rm=T))[,3]
  # for_plot$outcome <- reshape2::melt(apply(parVals$p_outcome, c(2,3), mean, na.rm=T))[,3]
  # for_plot$cue_pos <- reshape2::melt(apply(parVals$p_cue_pos, c(2,3), mean, na.rm=T))[,3]
  # for_plot$cue_freq <- reshape2::melt(apply(parVals$p_cue_freq, c(2,3), mean, na.rm=T))[,3]
  # for_plot$y_hat <- round(reshape2::melt(apply(parVals$y_hat, c(2,3), mean, na.rm=T))[,3]); for_plot$y_hat[for_plot$y_hat==0] <- NA
  # for_plot$choice <- round(reshape2::melt(apply(parVals$p_choice, c(2,3), mean, na.rm=T))[,3]); for_plot$choice[for_plot$choice==0] <- NA
  # for_plot <- as.data.frame(for_plot)
  # 
  # all_pred <- array(NA, dim = c(500, length(for_plot$y_hat)))
  # for (i in 1:500) {
  #   tmp_y_hat <- reshape2::melt(parVals$y_hat[i,,])[,3]; tmp_y_hat[tmp_y_hat==0] <- NA
  #   all_pred[i,] <- ifelse((tmp_y_hat==for_plot$choice & for_plot$outcome==1) | (tmp_y_hat!=for_plot$choice & for_plot$outcome!=1), 1, 0)
  # }
  # for_plot$all_pred_correct <- apply(all_pred, 2, mean, na.rm=T)
  # 
  # # correct % of subject and model
  # for_plot$actual_correct <- ifelse(for_plot$outcome==1, 1, 0)
  # for_plot$pred_correct <- ifelse((for_plot$y_hat==for_plot$choice & for_plot$outcome==1) | (for_plot$y_hat!=for_plot$choice & for_plot$outcome!=1), 1, 0)
  for_plot<- x$plot_object
  
  plot_data <- for_plot %>%
    group_by(subjID,cue_pos) %>%
    summarize(actual_cor = mean(actual_correct, na.rm=T),
              pred_cor = mean(all_pred_correct, na.rm=T),
              # pred_low_q = quantile(all_pred_correct, probs = 0.025, na.rm=T),
              # pred_high_q = quantile(all_pred_correct, probs = 0.975, na.rm=T),
              se_cor = as.vector(Hmisc::binconf(x=sum(actual_correct,na.rm=T),n = length(actual_correct)))[1] - as.vector(Hmisc::binconf(x=sum(actual_correct,na.rm=T),n = length(actual_correct)))[2])
  if (single) {
    p <- ggplot(plot_data, aes(x = cue_pos, y = actual_cor, group = subjID)) + 
      geom_line() + 
      geom_ribbon(aes(ymin=actual_cor - se_cor, ymax=actual_cor + se_cor), alpha = 0.05) + 
      # geom_ribbon(aes(ymin=pred_low_q, ymax=pred_high_q), alpha = 0.25, fill = I("red")) + 
      geom_line(aes(x = cue_pos, y = pred_cor, color = I("red"))) + 
      coord_cartesian(ylim= c(0,1))
  } else {
    p <- ggplot(plot_data, aes(x = cue_pos, y = actual_cor, group = subjID)) + 
      geom_line() + 
      geom_ribbon(aes(ymin=actual_cor - se_cor, ymax=actual_cor + se_cor), alpha = 0.25) + 
      # geom_ribbon(aes(ymin=pred_low_q, ymax=pred_high_q), alpha = 0.25, fill = I("red")) + 
      geom_line(aes(x = cue_pos, y = pred_cor, color = I("red"))) + 
      coord_cartesian(ylim= c(0,1)) + 
      facet_wrap(c("subjID"))
  }
  
  if (compute_auc) {
    tryCatch({
      auc_dat <- x$plot_object[x$plot_object$subjID!=0,] %>% group_by(subjID) %>% 
        summarize(auc_score = as.numeric(roc(choice,y_hat,levels=c(1,2))[["auc"]]))
      print("Estimated area under the curve:")
      print(t.test(auc_dat$auc_score, mu = 0.5))
    },error=function(e){
      print("Couldn't compute area under the curve by subject. Here is a computation that avoids a by-subject measure:")
      View(x$plot_object[is.na(x$plot_object$choice),])
      auc_dat <- x$plot_object %>% group_by(subjID) %>% 
        summarize(auc_score = as.numeric(roc(choice,y_hat,levels=c(1,2))[["auc"]]))
      print(auc_dat)
    })
    
  }
  return(p)
}