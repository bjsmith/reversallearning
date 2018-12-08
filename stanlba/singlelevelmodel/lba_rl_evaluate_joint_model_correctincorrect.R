source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize_fast.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")

model_lba_rl_joint_v9c<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180705_1",
  model_filename="lba_rl_single_exp_joint_v9",
  model_subversion="")

regions<-c(
  #ventral striatum
  "ROI_Left.Accumbens.area","ROI_Right.Accumbens.area",
  #vmPFC/OFC
  "ROI_ctx_lh_S_suborbital","ROI_ctx_rh_S_suborbital"
)

library(data.table)

DeltaThetaLabels=c("ResponseCorrect",gsub("ctx_","",gsub("ROI_","",regions)))
Sigma_dims<-5
rpe_covarvec<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
show_distribution_of_all_sigmas_new(data.table(model_lba_rl_joint_v9c[["complete_posterior_list"]]),
                                Sigma_dims = c(5,1),
                                ggplot_args = scale_x_continuous(limits = c(-2,2)))
dim(model_lba_rl_joint_v9c$results_summary)

# model_lba_rl_joint_v9c$results_summary[model_lba_rl_joint_v9c$results_summary$sid==106,]
# model_lba_rl_joint_v9c$results_summary[model_lba_rl_joint_v9c$results_summary$sid==108,]

#OK, what if we mined the results summary for means....
#We really have to do this by-subject to get a true measure of power.
rsdt<-data.table(model_lba_rl_joint_v9c$results_summary)
#mean estimate seems to be over 0, which is helpful I guess?

#Left Accumbens
t.test(rsdt[param_name=="Sigma[1,2]",.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
#Right Accumbens
t.test(rsdt[param_name=="Sigma[1,3]",.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
#Left vmPFC
t.test(rsdt[param_name=="Sigma[1,4]",.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
#Right vmPFC
t.test(rsdt[param_name=="Sigma[1,5]",.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)


load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180705_1/run_package_396_2_reward_lba_rl_single_exp_joint_v9.RData")

summary(run_package$fit)$summary
run_package$fit@stanmodel
