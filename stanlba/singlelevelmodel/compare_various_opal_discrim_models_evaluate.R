#model.name<-"rl_single_exp_joint_v10"
model.name<-"rl_opal_single_exp_joint_v10"
library(rstan)
source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize_fast.R")
require(R.utils)
options(mc.cores = 6)
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/rl_discrim/lba_rl_discrim_v2_functions_v2.R")
source("stanlba/singlelevelmodel/rl_discrim/rl_discrim_v4_functions.R")

#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.
lba_rl_version<-"rl_opal_20180920_1"

source("lba_rl_evaluate_functions.R")
source("freesurfer_region_naming.R")
source("freesurfer_region_naming.R")

regions<-get_dmn_regions()
#what we really wanna do is extract the sigmas
Sigma_dims<-25
rpe_covarvec<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
rpe_covarvec_df<-paste0("Sigma.",1:Sigma_dims[1],".",1,".")
library(rstan)

model_rl_opal_v10<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="rl_opal_single_exp_joint_v10",
  model_subversion="")


model_rl_control_v10<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="rl_single_exp_joint_v10",
  model_subversion="")

models <- list(
  "opal"=model_rl_opal_v10,
  "control"=model_rl_control_v10)

summary.stats<-NULL
for (model_name in names(models)){#model_name<-"v9"
  model<-models[[model_name]]
  model$results_summary_dt[,FullRunName:=paste0("s",sid,"r",rid,motivation)]
  
  #so we first want to see how many runs each method caught. 
  #Because inclusion in the final results was conditional on actually having a 
  #reasonalbe Rhat value, we can best test whether some methods could not converge
  #by just seeing how many runs made it through that hurdle.
  
  model.summary.stats<-data.frame(model_name)
  model.summary.stats$CompleteRuns=length(unique(model$results_summary_dt$FullRunName))
  
  #now, how does it change the standard deviations associated with our key parameters?
  params_to_check<-c("alpha_pr","beta_pr", "alpha","beta")
  #this tells us how wide those confidence intervals are.
  #let's take the average across subjects, and then use that as our statistic.
  model_param_sds<-model$results_summary_dt[
    param_name %in% params_to_check, # sid==107 & rid==1 & motivation=="reward",
    .(Sub_m_sd=mean(sd)),.(param_name,sid)] %>%
    .[,.(AllSubs_m_sd=mean(Sub_m_sd),model_name=model_name),by=param_name] %>%
    spread(param_name,AllSubs_m_sd)
  colnames(model_param_sds)<-paste0(colnames(model_param_sds),"_sd")
  model.summary.stats<-cbind(model.summary.stats,model_param_sds[,2:length(model_param_sds)])
  
  means_params_to_check<-c(params_to_check, "log_lik")
  
  model_param_means<-model$results_summary_dt[
    param_name %in% means_params_to_check,
    .(Sub_m_mean=mean(mean)),.(param_name,sid)] %>%
    .[,.(AllSubs_m_mean=mean(Sub_m_mean),model_name=model_name),by=param_name] %>%
    spread(param_name,AllSubs_m_mean)
  colnames(model_param_means)<-paste0(colnames(model_param_means),"_mean")
  model.summary.stats<-cbind(model.summary.stats,model_param_means[,2:(length(model_param_means))])
  
  
  ###
  if(is.null(summary.stats)){
    summary.stats<-model.summary.stats
  }else{
    summary.stats<-plyr::rbind.fill(summary.stats,model.summary.stats)
  }
}

summary.stats
#on run count (measuring convergence), v5 (modifying EV calculation) seemed to be a step backwards
#others had roughly the same statistics.


#so nothing particularly distinguishes any of the improvements, and in fact, it's a little worrying how little difference 
#it made to, say, change the scale of the estimated values.

#what can we learn from the posterior estimates?
#prediction error, y_hat (choice), and expected value are available.
y_hat_rows<-model_rl_discrim_v5$results_summary_dt[grepl("y_hat",param_name),]
y_hat_rows$n_start<-str_locate(pattern="\\[",as.character(y_hat_rows$param_name))[,"start"]+1
y_hat_rows$n_end<-str_locate(pattern="\\]",as.character(y_hat_rows$param_name))[,"start"]-1
y_hat_rows[,TrialN:=as.integer(substr(param_name,n_start,n_end))]


library(stringr)
y_hat_rows<-model_rl_discrim_v4$results_summary_dt[grepl("y_hat",param_name),]
y_hat_rows$n_start<-str_locate(pattern="\\[",as.character(y_hat_rows$param_name))[,"start"]+1
y_hat_rows$n_end<-str_locate(pattern="\\]",as.character(y_hat_rows$param_name))[,"start"]-1
y_hat_rows[,TrialN:=as.integer(substr(param_name,n_start,n_end))]

trial_expected_val_rows<-model_rl_discrim_v4$results_summary_dt[grepl("trial_expected_val",param_name),]
trial_expected_val_rows$n_start<-str_locate(pattern="\\[",as.character(trial_expected_val_rows$param_name))[,"start"]+1
trial_expected_val_rows$n_end<-str_locate(pattern="\\]",as.character(trial_expected_val_rows$param_name))[,"start"]-1
trial_expected_val_rows[,TrialN:=as.integer(substr(param_name,n_start,n_end))]

td_var_rows<-model_rl_discrim_v5$results_summary_dt[grepl("L_Omega",param_name),]
unique(td_var_rows$param_name)
#these average values estimates are useful but we need to be able to look at what the correct value was.

#now, let's see how the y_hat posterior estimates matched what subjects actually chose. This will
#tell us how good the model is at assessing subject behavior.

plot(abs(y_hat_rows[sid==106&rid==1 & motivation==m,mean]-model.data$response))
#this looks like...not good at all. The differences should be less than 0.5 if they are good predictions or greater than 0.5 if they are reverse predictions.

#of course if the subject's performance is poor then we don't expect to predict their choices
#we are then going by chance. So we should only expect reasonable accuracy when a subject's performance is good.
#so let's try to get this figure across the subjects.
for (frn in unique(model_rl_discrim_v4$results_summary_dt$FullRunName)){
  #frn<-unique(model_rl_discrim_v4$results_summary_dt$FullRunName)[[11]]
  
  #just get the identifiers for this run
  subid<-model_rl_discrim_v4$results_summary_dt[FullRunName==frn,sid][[1]]
  runid<-model_rl_discrim_v4$results_summary_dt[FullRunName==frn,rid][[1]]
  motiv<-model_rl_discrim_v4$results_summary_dt[FullRunName==frn,motivation][[1]]
  
  #get the raw behavioral data for this run.
  srm.data<-select_rawdata_cols_for_run(rawdata,subid,runid,motiv)
  model.data<-create_standatalist(srm.data = srm.data,theta_count=2)
  
  #yhat values: get the error in predicting the subject's response
  subj_y_hat_rows<-y_hat_rows[sid==subid&rid==runid & motivation==motiv,]
  y_hat_run_mean_error<-mean(abs(subj_y_hat_rows[,mean]-model.data$response))
  
  #should also do trial_expected_val
  subj_trial_ev_rows<-trial_expected_val_rows[sid==subid&rid==runid & motivation==motiv,]
  trial_ev_run_mean_error<-subj_trial_ev_rows$mean
  
  
}


load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/rl_joint_discrim_20180914_1/run_package_106_1_reward_rl_discrim_single_exp_joint_v5.RData")
extdata<-rstan::extract(run_package$fit)
delta_names<-c("Accum1","Accum2","OFC1","OFC2")
heatmap(get_Sigma_m_n(model_rl_discrim_v5$results_summary_dt,theta_names,delta_names))

load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/rl_joint_discrim_20180914_1/run_package_107_1_reward_rl_discrim_single_exp_joint_v5.RData")
extdata<-rstan::extract(run_package$fit)
delta_names<-c("Accum1","Accum2","OFC1","OFC2")
heatmap(get_Sigma_m_n(model_rl_discrim_v5$results_summary_dt,theta_names,delta_names))

