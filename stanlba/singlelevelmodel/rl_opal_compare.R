source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize_fast.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v7_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v10_functions.R")

source("lba_rl_evaluate_functions.R")
source("freesurfer_region_naming.R")
source("freesurfer_region_naming.R")

regions<-get_dmn_regions()
#what we really wanna do is extract the sigmas
Sigma_dims<-25
rpe_covarvec<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
rpe_covarvec_df<-paste0("Sigma.",1:Sigma_dims[1],".",1,".")
library(rstan)



DeltaThetaLabels=paste0("con_",get_dmn_regions())
#load('/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180708_1/run_package_140_1_punishment_lba_rl_single_exp_joint_v11e.RData')
library(rstan)


#Motion+CSF+WM regressed out; freesurfer ROIs all dmn regions
model_lbarl<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_201807810_1",
  model_filename="rl_single_exp_joint_v6b",
  model_subversion="")
#seems to be very patchy, timing out occasionally and more often, failing to fully converge.
rsdt<-model_lba_rl_joint_v11s$results_summary_dt
length(unique(rsdt$FullRunId))
testres.dt<-do_matrix_t_tests(theta_range = 1:2,delta_range=3:39)
testres.dt$AdjustedPVals<-p.adjust(testres.dt$p.value,method="fdr")
testres.dt$CI95Pct<-unlist(apply(testres.dt,1, function(r){paste0("[",formatC(as.numeric(r[["conf.int1"]]),digits=2),", ",formatC(as.numeric(r[["conf.int2"]]),digits=2),"]")}))
runs_missing(rawdata,rsdt)
write.csv(testres.dt,paste0(localsettings$data.dir,"lba_rl/",lba_rl_version,"/lba_rl_single_exp_joint_v11s_revised.csv"))
save(rsdt,file=paste0(localsettings$data.dir,"lba_rl/",lba_rl_version,"/lba_rl_single_exp_joint_v11s_rsdt.RData"))
