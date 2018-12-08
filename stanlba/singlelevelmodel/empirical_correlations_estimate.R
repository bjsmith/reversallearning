
merge_with_neural_data<-function(results_summary_dt){
  results_summary_dt<-results_summary_dt
  #Alright, so now, let's try grabbing the neural data and running a correlation ourselves.
  
  model_trial_data<-results_summary_dt[union(grep("run_pred_err_c2",param_name),
                                             grep("trial_expected_val",param_name)),]
  model_trial_data$param_name<-as.character(model_trial_data$param_name)
  #now we gotta parse the param_name column to get a TrialID
  
  model_trial_data$TrialID<-as.numeric(substr(model_trial_data$param_name,regexpr("\\[",model_trial_data$param_name)+1,regexpr("\\]",model_trial_data$param_name)-1))
  model_trial_data$param_general_name<-substr(model_trial_data$param_name,1,regexpr("\\[",model_trial_data$param_name)-1)
  
  model_trial_data_wide<-model_trial_data[,.(sid,rid,motivation,FullRunId,param_general_name,TrialID,mean)] %>% tidyr::spread(param_general_name,mean)
  
  #before we can do that we have to somehow assign values. We have the 
  
  #we will have to add the ID in, and sort it by onset_time_actual
  rawdata[order(onset_time_actual),TrialID:=1:.N,.(subid,runid,Motivation)]
  
  neural_data_sample<-rawdata[,.(subid,runid,Motivation,TrialID,presentation_n,presentation_n_in_segment,
                                 con_ROI_Left.Accumbens.area,con_ROI_Right.Accumbens.area,con_ROI_ctx_lh_S_suborbital,con_ROI_ctx_rh_S_suborbital,
                                 con_fsl_roi_accumbens_l,con_fsl_roi_accumbens_r,ROI_ctx_lh_S_circular_insula_ant,
                                 ROI_ctx_lh_S_circular_insula_inf,ROI_ctx_rh_S_circular_insula_ant
  )]
  
  #now merge
  
  neural_trial_data<-merge(model_trial_data_wide,neural_data_sample,
                           by.x=c("sid","rid","motivation","TrialID"),
                           by.y=c("subid","runid","Motivation","TrialID"),
                           all.x = TRUE,all.y=FALSE)
  neural_trial_data$trial_expected_val_abs<-abs(neural_trial_data$trial_expected_val)
  neural_trial_data$run_pred_err_c2_abs<-abs(neural_trial_data$run_pred_err_c2)
  
  return(neural_trial_data);
  
}