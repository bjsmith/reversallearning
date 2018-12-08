source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize_fast.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v7_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v10a_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v11r_functions_fix.R")


source("lba_rl_evaluate_functions.R")
source("freesurfer_region_naming.R")
source("freesurfer_region_naming.R")

regions<-get_dmn_regions()
#what we really wanna do is extract the sigmas
Sigma_dims<-25
rpe_covarvec<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
rpe_covarvec_df<-paste0("Sigma.",1:Sigma_dims[1],".",1,".")
library(rstan)


lba_rl_version<-"joint_20180917_1"

DeltaThetaLabels=paste0("con_",get_dmn_regions())
#load('/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180708_1/run_package_140_1_punishment_lba_rl_single_exp_joint_v11e.RData')
library(rstan)

#Motion+CSF+WM regressed out; freesurfer ROIs all dmn regions
model_lba_rl_joint_v11s<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="lba_rl_single_exp_joint_v11",
  model_subversion="s")
#seems to be very patchy, timing out occasionally and more often, failing to fully converge.

#Motion+CSF+WM regressed out; 4 freesurfer ROIs
regions<-c("ROI_ctx_lh_S_suborbital","ROI_ctx_rh_S_suborbital", "ROI_Left.Accumbens.area", "ROI_Right.Accumbens.area")
model_lba_rl_joint_v11t<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="lba_rl_single_exp_joint_v11",
  model_subversion="t")
#better




#Motion+CSF+WM regressed out; FSL Harvard-Oxford ROIs
model_lba_rl_joint_v11u<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="lba_rl_single_exp_joint_v11",
  model_subversion="u")

source("freesurfer_region_naming.R")

#v

model_lba_rl_joint_v11v<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="lba_rl_single_exp_joint_v11",
  model_subversion="v")
