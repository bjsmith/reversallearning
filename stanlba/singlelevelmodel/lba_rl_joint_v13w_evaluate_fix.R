source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize_fast.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v7_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v10_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v11r_functions_fix.R")


regions<-get_dmn_regions()
#what we really wanna do is extract the sigmas
Sigma_dims<-25
rpe_covarvec<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
rpe_covarvec_df<-paste0("Sigma.",1:Sigma_dims[1],".",1,".")
library(rstan)

DeltaThetaLabels=c("Simulated_Accumbens_Left",
                   "Simulated_Accumbens_Right",
                   "Simulated_SubOrbitalLeft",
                   "Simulated_SubOrbitalRight")

library(rstan)



model_lba_rl_joint_v13<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20181004_1",
  model_filename="lba_rl_single_exp_joint_v13",
  model_subversion="w")


ggplot(model_lba_rl_joint_v13$results_summary_dt[grep("trial_expected_val",param_name),],aes(mean))+
  geom_histogram(binwidth = 0.02)+facet_grid(sid~rid+motivation,scales = "free")

ggplot(model_lba_rl_joint_v13$results_summary_dt[grep("run_pred_err_c2",param_name),],aes(mean))+
  geom_histogram(binwidth = 0.02)+facet_grid(sid~rid+motivation,scales = "free")
