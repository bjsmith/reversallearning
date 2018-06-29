source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")

#JOINT AND PARALLEL MODELS WITH BIASED ALPHA AND BETA PRIORS

joint_rl_model<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180628_1",
  model_filename="rl_single_exp_joint",
  model_subversion="_joint_v1")

parallel_rl_model<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180628_1",
  model_filename="rl_single_exp_parallel",
  model_subversion="_joint_v1")

#JOINT AND PARALLEL MODELS WITH MODERATE ALPHA AND BETA PRIORS AND CHOLESKY(4)
#Shouldn't make a difference to Alpha and Beta because this is a 'parallel' model.
# parallel_rl_model<-single_level_model_summarize(
#   single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
#   model_version="rl_joint_20180629_1",
#   model_filename="rl_single_exp_parallel_v2",
#   model_subversion="") ###PROCESSING ON SCREEN.


#JOINT AND PARALLEL MODELS WITH MODERATE ALPHA AND BETA PRIORS AND CHOLESKY(1)
#ALPHA=N(0,1.8);BETA=N(0,1.8);cholesky(1);
# joint_rl_model2<-single_level_model_summarize(
#   single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
#   model_version="rl_joint_20180629_1",
#   model_filename="rl_single_exp_joint_v2",
#   model_subversion="")###PROCESSING ON SCREEN.
# parallel_rl_model_v3<-single_level_model_summarize(
#   single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
#   model_version="rl_joint_20180629_1",
#   model_filename="rl_single_exp_parallel_v3",
#   model_subversion="")###PROCESSING ON SCREEN.

#JOINT AND PARALLEL MODELS WITH MODERATE ALPHA, K, TAU PRIORS AND CHOLESKY(1)

#OK now, let's check out the charts of their parameter distributions.


############# OUTLYING-BIASED PRIORS: JOINT AND PARALLEL COMPARISON.

#Is there a difference between joint and parallel models with these poor priors?
do_violin(data.table(joint_rl_model[["complete_posterior_list"]]),parameters_to_show=c("alpha","beta"))
  #: completely uninformative beta. alphas often indistinguishable from zero.  
do_violin(data.table(parallel_rl_model[["complete_posterior_list"]]),parameters_to_show=c("alpha","beta"))
  #: alphas obviously biased too high. betas that are also high but biased.
  # This would be possible if 

########### MODERATE PRIORS (ALPHA, BETA)~N(0,1.8)
