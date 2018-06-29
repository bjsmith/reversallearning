source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")

joint_rl_model<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180628_1",
  model_filename="rl_single_exp_joint",
  model_subversion="_joint_v1")

# parallel_rl_model<-single_level_model_summarize(
#   single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
#   model_version="rl_joint_20180628_1",
#   model_filename="rl_single_exp_parallel",
#   model_subversion="_joint_v1")
# 
# parallel_rl_model_v3<-single_level_model_summarize(
#   single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
#   model_version="rl_joint_20180629_1",
#   model_filename="rl_single_exp_parallel_v3",
#   model_subversion="")

joint_rl_model_v2<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180629_1",
  model_filename="rl_single_exp_joint_v2",
  model_subversion="")

data.table(joint_rl_model_v2$results_summary)[param_name=="alpha_pr",]

do_violin(data.table(joint_rl_model[["complete_posterior_list"]])[SubjectId %in% 106:221],parameters_to_show=c("alpha","beta"))
do_violin(data.table(joint_rl_model_v2[["complete_posterior_list"]])[SubjectId %in% 106:221],parameters_to_show=c("alpha","beta"))
