source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize_fast.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v7_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v10_functions.R")

regions<-get_dmn_regions()
#what we really wanna do is extract the sigmas
Sigma_dims<-25
rpe_covarvec<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
rpe_covarvec_df<-paste0("Sigma.",1:Sigma_dims[1],".",1,".")
library(rstan)


lba_rl_version<-"rl_joint_20180927_1"

DeltaThetaLabels=paste0("con_",get_dmn_regions())

library(rstan)



model_rl_joint_v9<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="rl_single_exp_joint_v9",
  model_subversion="")

#seems to be very patchy, timing out occasionally and more often, failing to fully converge.



model_rl_discrim_joint_v4<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="rl_discrim_single_exp_joint_v4",
  model_subversion="")

ggplot(model_rl_discrim_joint_v4$complete_posterior_list,
       aes(alpha,beta,color=UniqueRunCode))+
  geom_point(alpha=0.2)+labs(title="Posterior samples of alpha, beta")

ggplot(model_rl_discrim_joint_v4$complete_posterior_list,aes(alpha,beta))+
  geom_point(alpha=0.2)+labs(title="Posterior samples of alpha, beta")+
  coord_cartesian(ylim=c(0,200))
theta_names<-c("RPE","EV")
delta_names<-paste0("Delta",1:4)
heatmap(get_Sigma_m_n(model_rl_discrim_joint_v4$results_summary_dt,theta_names,delta_names))

#log likelihood comparison; this is not the main thing we're trying to look into
model_rl_discrim_joint_v4$results_summary_dt<-data.table(model_rl_discrim_joint_v4$results_summary)

#model_rl_discrim_joint_v4$results_summary_dt[param_name=="alpha_discrim_pr",.N,by=sid]

mean(model_rl_discrim_joint_v4$results_summary_dt[param_name=="log_lik"]$mean)

mean(model_rl_joint_v9$results_summary_dt[param_name=="log_lik"]$mean)

#so can we look at just runs that we got across both modes to try to compare?
model_rl_discrim_joint_v4$results_summary_dt[,FullRunName:=paste0("s",sid,"r",rid,"m",motivation)]
model_rl_joint_v9$results_summary_dt[,FullRunName:=paste0("s",sid,"r",rid,"m",motivation)]
runs_in_common<-intersect(
  unique(model_rl_discrim_joint_v4$results_summary_dt$FullRunName),
  unique(model_rl_joint_v9$results_summary_dt$FullRunName))

log_lik_comparison<-merge(
  model_rl_discrim_joint_v4$results_summary_dt[FullRunName %in% runs_in_common & param_name=="log_lik",.(FullRunName,mean)],
  model_rl_joint_v9$results_summary_dt[FullRunName %in% runs_in_common & param_name=="log_lik",.(FullRunName,mean)],
  by="FullRunName",suffixes = c("_discrim_joint_v4","_joint_v9"))

hist(log_lik_comparison$mean_discrim_joint_v4-log_lik_comparison$mean_joint_v9)
t.test(log_lik_comparison$mean_discrim_joint_v4-log_lik_comparison$mean_joint_v9)


#now do a comparison by run.
#what do the distributions of the EVs and RPEs look like?
ggplot(model_rl_joint_v9$results_summary_dt[grep("trial_expected_val",param_name),],aes(mean))+
  geom_histogram(binwidth = 0.02)+facet_grid(sid~rid+motivation,scales = "free")

ggplot(model_rl_joint_v9$results_summary_dt[grep("run_pred_err_c2",param_name),],aes(mean))+
  geom_histogram(binwidth = 0.02)+facet_grid(sid~rid+motivation,scales = "free")


ggplot(model_rl_joint_v9$results_summary_dt[param_name=="alpha",],aes(mean))+
  geom_histogram(binwidth = 0.02)+facet_grid(sid~rid+motivation,scales = "free")


#including this here just to look at the distribution of EVs and RPEs for this

model_lba_rl_joint_v13<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180723_1",
  model_filename="lba_rl_single_exp_joint_v13",
  model_subversion="")


ggplot(model_lba_rl_joint_v13$results_summary_dt[grep("trial_expected_val",param_name),],aes(mean))+
  geom_histogram(binwidth = 0.02)+facet_grid(sid~rid+motivation,scales = "free")

ggplot(model_lba_rl_joint_v13$results_summary_dt[grep("run_pred_err_c2",param_name),],aes(mean))+
  geom_histogram(binwidth = 0.02)+facet_grid(sid~rid+motivation,scales = "free")
#brandon's suggestion: try using hte absolute values:
#center distribution by run and parameter so that we get a reasonable estimate of what 
#the model actually has to work with...

model_lba_rl_joint_v13$results_summary_dt[,mean_c:=mean-mean(mean),.(sid,rid,motivation,param_name)]
model_lba_rl_joint_v13$results_summary_dt$abs_mean_c<-abs(model_lba_rl_joint_v13$results_summary_dt$mean_c)
model_lba_rl_joint_v13$results_summary_dt$abs_mean<-abs(model_lba_rl_joint_v13$results_summary_dt$mean)

ggplot(model_lba_rl_joint_v13$results_summary_dt[grep("trial_expected_val",param_name),],aes(abs_mean))+
  geom_histogram(binwidth = 0.02)+facet_wrap(~sid+rid+motivation,scales = "free")

ggplot(model_lba_rl_joint_v13$results_summary_dt[grep("run_pred_err_c2",param_name),],aes(abs_mean))+
  geom_histogram(binwidth = 0.02)+facet_wrap(~sid+rid+motivation,scales = "free")

ggplot(model_lba_rl_joint_v13$results_summary_dt[grep("trial_expected_val",param_name),],aes(mean))+
  geom_histogram(binwidth = 0.02)+facet_wrap(~sid+rid+motivation,scales = "free")+labs(title="Expected Value")

ggplot(model_lba_rl_joint_v13$results_summary_dt[grep("run_pred_err_c2",param_name),],aes(mean))+
  geom_histogram(binwidth = 0.02)+facet_wrap(~sid+rid+motivation,scales = "free")+labs(title="Reward Prediction Error")

source("stanlba/singlelevelmodel/empirical_correlations_estimate.R")
neural_trial_data<-merge_with_neural_data(model_lba_rl_joint_v13$results_summary_dt)
#Alright, so now, let's try grabbing the neural data and running a correlation ourselves.

ggplot(neural_trial_data[(sid %% 5)==0 & run_pred_err_c2>0],aes(run_pred_err_c2,con_ROI_Left.Accumbens.area,color=as.factor(presentation_n)))+geom_point()+
  facet_wrap(~interaction(sid,rid,motivation),scales = "free")+
  labs(title="L Accumbens Activity by RPE for each run", x="RPE",color="Presentation Number")
  
ggplot(neural_trial_data[(sid %% 5)==0],aes(trial_expected_val,con_ROI_Left.Accumbens.area,color=as.factor(presentation_n)))+geom_point()+facet_wrap(~interaction(sid,rid,motivation),scales = "free")+
  labs(title="L Accumbens Activity by EV for each run", x="EV",color="Presentation Number")


ggplot(neural_trial_data[(sid %% 5)==0],aes(run_pred_err_c2,con_fsl_roi_accumbens_r,color=as.factor(presentation_n)))+geom_point()+facet_wrap(~interaction(sid,rid,motivation),scales = "free")+
  labs(title="R Accumbens Activity by abs RPE for each run", x="EV",color="Presentation Number")


#insula activity is what we expect to relate to absolute RPE....
ggplot(neural_trial_data[(sid %% 5)==0],aes(run_pred_err_c2_abs,ROI_ctx_lh_S_circular_insula_ant,color=as.factor(presentation_n)))+geom_point()+facet_wrap(~interaction(sid,rid,motivation),scales = "free")+
  labs(title="insula Activity by abs RPE for each run", x="absRPE",color="Presentation Number")
ggplot(neural_trial_data[(sid %% 5)==0],aes(run_pred_err_c2_abs,ROI_ctx_rh_S_circular_insula_ant,color=as.factor(presentation_n)))+geom_point()+facet_wrap(~interaction(sid,rid,motivation),scales = "free")+
  labs(title="insula Activity by abs RPE for each run", x="absRPE",color="Presentation Number")


ggplot(neural_trial_data[(sid %% 5)==0],
       aes(presentation_n_in_segment,con_ROI_Left.Accumbens.area,color=as.factor(presentation_n)))+
  geom_point()+facet_wrap(~interaction(sid,rid,motivation),scales = "free")+
  labs(title="L Accumbens Activity by presentation_n_in_segment for each run", x="RPE",color="Presentation Number")


ggplot(neural_trial_data[(sid %% 5)==0],
       aes(presentation_n_in_segment,con_ROI_Right.Accumbens.area,color=as.factor(presentation_n)))+
  geom_point()+facet_wrap(~interaction(sid,rid,motivation),scales = "free")+
  labs(title="R Accumbens Activity by presentation_n_in_segment for each run", x="PNIS",color="Presentation Number")

ggplot(neural_trial_data[(sid %% 5)==0],
       aes(presentation_n_in_segment,ROI_ctx_lh_S_circular_insula_ant,color=as.factor(presentation_n)))+
  geom_point()+facet_wrap(~interaction(sid,rid,motivation),scales = "free")+
  labs(title="R Accumbens Activity by presentation_n_in_segment for each run", x="PNIS",color="Presentation Number")



ggplot(neural_trial_data[(sid %% 5)==0],
       aes(presentation_n,con_fsl_roi_accumbens_l))+
  geom_point()+facet_wrap(~interaction(sid,rid,motivation),scales = "free")+
  labs(title="R Accumbens Activity by presentation_n_in_segment for each run", x="PNIS",color="Presentation Number")

ggplot(neural_trial_data[(sid %% 5)==0],
       aes(presentation_n,con_fsl_roi_accumbens_r))+
  geom_point()+facet_wrap(~interaction(sid,rid,motivation),scales = "free")+
  labs(title="R Accumbens Activity by presentation_n_in_segment for each run", x="PNIS",color="Presentation Number")



ggplot(neural_trial_data,aes(run_pred_err_c2,con_ROI_Right.Accumbens.area))+geom_point()+facet_wrap(~sid+rid+motivation,scales = "free")
ggplot(neural_trial_data,aes(trial_expected_val,con_ROI_Right.Accumbens.area))+geom_point()+facet_wrap(~sid+rid+motivation,scales = "free")


ggplot(neural_trial_data,aes(run_pred_err_c2_abs,con_ROI_Left.Accumbens.area))+geom_point()+facet_wrap(~sid+rid+motivation,scales = "free")
ggplot(neural_trial_data,aes(run_pred_err_c2_abs,con_ROI_Left.Accumbens.area))+geom_point()+facet_wrap(~sid+rid+motivation,scales = "free")
ggplot(neural_trial_data,aes(trial_expected_val_abs,con_ROI_Left.Accumbens.area))+geom_point()+facet_wrap(~sid+rid+motivation,scales = "free")

ggplot(neural_trial_data,aes(run_pred_err_c2_abs,con_ROI_Right.Accumbens.area))+geom_point()+facet_wrap(~sid+rid+motivation,scales = "free")
ggplot(neural_trial_data,aes(trial_expected_val_abs,con_ROI_Right.Accumbens.area))+geom_point()+facet_wrap(~sid+rid+motivation,scales = "free")

summary(lm(trial_expected_val_abs~con_ROI_Left.Accumbens.area+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))
summary(lm(trial_expected_val_abs~con_ROI_Right.Accumbens.area+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))
summary(lm(run_pred_err_c2_abs~con_ROI_Left.Accumbens.area+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))
summary(lm(run_pred_err_c2_abs~con_ROI_Right.Accumbens.area+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))

summary(lm(presentation_n_in_segment~con_ROI_Left.Accumbens.area+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))
summary(lm(presentation_n_in_segment~con_ROI_Right.Accumbens.area+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))

summary(lm(trial_expected_val~con_ROI_Left.Accumbens.area+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))
summary(lm(trial_expected_val~con_ROI_Right.Accumbens.area+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))
summary(lm(run_pred_err_c2_abs~con_ROI_Left.Accumbens.area+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))
summary(lm(run_pred_err_c2_abs~con_ROI_Right.Accumbens.area+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))
summary(lm(trial_expected_val~con_ROI_ctx_lh_S_suborbital+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))
summary(lm(trial_expected_val~con_ROI_ctx_rh_S_suborbital+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))
summary(lm(run_pred_err_c2_abs~con_ROI_ctx_lh_S_suborbital+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))
summary(lm(run_pred_err_c2_abs~con_ROI_ctx_rh_S_suborbital+as.factor(sid)*as.factor(rid)*motivation,neural_trial_data))


model_lba_rl_joint_v12<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180725_1",
  model_filename="lba_rl_single_exp_joint_v12",
  model_subversion="")

# model_lba_rl_joint_v12$results_summary_dt[param_name=="alpha",]
# dim(model_lba_rl_joint_v12$complete_posterior_list)
# colnames(model_lba_rl_joint_v12$complete_posterior_list)[4600:4612]

ggplot(model_lba_rl_joint_v12$complete_posterior_list,aes(alpha,p,color=UniqueRunCode))+geom_point(alpha=0.2)+labs(title="Posterior samples of p, alpha,\nacross 13 runs")

#model_lba_rl_joint_v13$results_summary_dt[param_name=="alpha",]

hist(model_lba_rl_joint_v12$results_summary_dt[param_name=="alpha",mean])
hist(model_lba_rl_joint_v13$results_summary_dt[param_name=="alpha",mean])
#learning rate has gotten higher.

theta_names<-c("RPE","EV")
delta_names<-get_dmn_regions()

heatmap(get_Sigma_m_n(model_lba_rl_joint_v12$results_summary_dt,theta_names,delta_names))


