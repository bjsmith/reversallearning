#this script run during runtime after srm.fit has been obtained for a rl lba single-level model,
#e.g., lba_rl_joint_v13w.R

rawdata_218_r1_p<-rawdata[subid==218 & runid==r & Motivation==m,.(run_pred_err_c2,trial_expected_val)]


srm.fit.summary.dt<-as.data.table(summary(srm.fit)$summary)
srm.fit.summary.dt$param_name<-rownames(summary(srm.fit)$summary)
#rownames(srm.fit.summary.dt)<-
model_trial_data<-srm.fit.summary.dt[union(grep("run_pred_err_c2",srm.fit.summary.dt$param_name),
                                           grep("trial_expected_val",srm.fit.summary.dt$param_name)),]

model_trial_data$TrialID<-as.numeric(substr(model_trial_data$param_name,regexpr("\\[",model_trial_data$param_name)+1,regexpr("\\]",model_trial_data$param_name)-1))
model_trial_data$param_general_name<-substr(model_trial_data$param_name,1,regexpr("\\[",model_trial_data$param_name)-1)

model_trial_data_wide<-model_trial_data[,.(param_general_name,TrialID,mean)] %>% tidyr::spread(param_general_name,mean)

cor.test(model_trial_data_wide$run_pred_err_c2,rawdata_218_r1_p$run_pred_err_c2)
plot(model_trial_data_wide$run_pred_err_c2,rawdata_218_r1_p$run_pred_err_c2)
cor.test(model_trial_data_wide$trial_expected_val,rawdata_218_r1_p$trial_expected_val)
plot(model_trial_data_wide$trial_expected_val,rawdata_218_r1_p$trial_expected_val)
