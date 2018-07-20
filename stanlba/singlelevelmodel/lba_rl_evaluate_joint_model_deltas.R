source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize_fast.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")

# model_lba_rl_joint_v7<-single_level_model_summarize_fast(
#   single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
#   model_version="joint_20180702_1",
#   model_filename="lba_rl_single_exp_joint_v7",
#   model_subversion="")

# do_violin_runs(data.table(model_lba_rl_joint_v7[["complete_posterior_list"]]),
#               parameters_to_show=c("alpha","k","tau"),
#               title="JOINT lba_rl model, v7")

#what we really wanna do is extract the sigmas
Sigma_dims<-23
rpe_covarvec<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
rpe_covarvec_df<-paste0("Sigma.",1:Sigma_dims[1],".",1,".")
library(rstan)
#summary(srm.fit)$summary[,rpe_covarvec]
summary(srm.fit)$summary[rpe_covarvec,]

relevant_sigmas<-model_lba_rl_joint_v7$complete_posterior_list[,rpe_covarvec_df,with=FALSE]

regions<-c(
  #ventral striatum
  "ROI_Left.Caudate","ROI_Right.Caudate",
  "ROI_Left.Putamen","ROI_Right.Putamen",
  "ROI_Left.Accumbens.area","ROI_Right.Accumbens.area",
  #vmPFC/OFC
  "ROI_ctx_lh_S_suborbital","ROI_ctx_rh_S_suborbital",
  "ROI_ctx_lh_G_subcallosal","ROI_ctx_rh_G_subcallosal",
  "ROI_ctx_lh_G_rectus","ROI_ctx_rh_G_rectus",
  "ROI_ctx_lh_G_orbital","ROI_ctx_rh_G_orbital",
  "ROI_ctx_lh_S_orbital.H_Shaped","ROI_ctx_rh_S_orbital.H_Shaped",
  "ROI_ctx_lh_S_orbital_lateral","ROI_ctx_rh_S_orbital_lateral",
  "ROI_ctx_lh_S_orbital_med.olfact","ROI_ctx_rh_S_orbital_med.olfact",
  #ACC
  "ROI_ctx_lh_G_and_S_cingul.Ant","ROI_ctx_rh_G_and_S_cingul.Ant"
)

DeltaThetaLabels=c("RewardPredictionError",gsub("ctx_","",gsub("ROI_","",regions)))
show_distribution_of_all_sigmas(data.table(model_lba_rl_joint_v7[["complete_posterior_list"]]),
                                Sigma_dims = c(23,1),
                                ggplot_args = scale_x_continuous(limits = c(-2,2)))



# library(rstan)
# load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180702_1/run_package_107_1_punishment_lba_rl_single_exp_joint_v7.RData")
# dim(summary(run_package$fit)$summary)
# s107r1p_rownames<-rownames(summary(run_package$fit)$summary)
# 
# 
# load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180702_1/run_package_109_2_punishment_lba_rl_single_exp_joint_v7.RData")
# dim(summary(run_package$fit)$summary)
# s109r2p_rownames<-rownames(summary(run_package$fit)$summary)
# setdiff(s107r1p_rownames,s109r2p_rownames)
# setdiff(s109r2p_rownames,s107r1p_rownames)

model_lba_rl_joint_v8<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180703_1",
  model_filename="lba_rl_single_exp_joint_v8",
  model_subversion="")

# myres<-dim(data.table(model_lba_rl_joint_v8[["complete_posterior_list"]]))
DeltaThetaLabels=c("RewardPredictionError",gsub("ctx_","",gsub("ROI_","",regions)))
show_distribution_of_all_sigmas(data.table(model_lba_rl_joint_v8[["complete_posterior_list"]]),
                                Sigma_dims = c(20,1),
                                ggplot_args = scale_x_continuous(limits = c(-2,2)))


model_lba_rl_joint_v8b<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180703_1",
  model_filename="lba_rl_single_exp_joint_v8",
  model_subversion="b")

regions<-c(
  #ventral striatum
  "ROI_Left.Accumbens.area","ROI_Right.Accumbens.area",
  #vmPFC/OFC
  "ROI_ctx_lh_S_suborbital","ROI_ctx_rh_S_suborbital"
)
DeltaThetaLabels=c("RewardPredictionError",gsub("ctx_","",gsub("ROI_","",regions)))
show_distribution_of_all_sigmas(data.table(model_lba_rl_joint_v8[["complete_posterior_list"]]),
                                Sigma_dims = c(5,1),
                                ggplot_args = scale_x_continuous(limits = c(-2,2)))



model_lba_rl_joint_v10<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180706_1",
  model_filename="lba_rl_single_exp_joint_v10",
  model_subversion="")

colnames(model_lba_rl_joint_v10[["complete_posterior_list"]])[1:100]
DeltaThetaLabels=c("RewardPredictionError",gsub("ctx_","",gsub("ROI_","",regions)))

show_distribution_of_all_sigmas_new(
  data.table(model_lba_rl_joint_v10[["complete_posterior_list"]]),
  Sigma_dims = c(26,1),
  ggplot_args = scale_x_continuous(limits = c(-2,2))
  )


