source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")

#all these models have similar starting values, so only parallel vs. joint and lbarl vs rl make a difference.

#JOINT AND PARALLEL MODELS WITH MODERATE ALPHA, K, TAU PRIORS AND CHOLESKY(1)
model_rl_parallel_v6<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180701_1",
  model_filename="rl_single_exp_parallel_v6",
  model_subversion="")

model_rl_joint_v6<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180701_1",
  model_filename="rl_single_exp_joint_v6",
  model_subversion="")

model_lba_rl_parallel_v5<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180701_1",
  model_filename="lba_rl_single_exp_parallel_v5",
  model_subversion="")

model_lba_rl_joint_v6<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180701_1",
  model_filename="lba_rl_single_exp_joint_v6",
  model_subversion="")





#OK, and joint rl_discriminability model

model_rl_discrim_joint_v1<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_discrim_joint_20180702_1",
  model_filename="rl_discrim_single_exp_joint_v1",
  model_subversion="")

#making the size of the window smaller.
model_rl_discrim_joint_v1_nb2<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_discrim_joint_20180702_1",
  model_filename="rl_discrim_single_exp_joint_v1nb2",
  model_subversion="")

model_rl_discrim_joint_v2<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_discrim_joint_20180702_1",
  model_filename="rl_discrim_single_exp_joint_v2",
  model_subversion="")

#now take a look at them.
#what do the alpha values look like?
#what do the correlations liook like? I think all of these ??? now have correlation matrices?

library(gridExtra)
p1<-do_violin_sid(data.table(model_rl_parallel_v6[["complete_posterior_list"]])[SubjectId %in% 106:143,],
              parameters_to_show=c("alpha","beta"),
              title="PARALLEL Reinforcement learning model")
unique(model_rl_parallel_v6$results_summary$param_name)


p2<-do_violin_sid(data.table(model_rl_joint_v6[["complete_posterior_list"]])[SubjectId %in% 106:143,],
              parameters_to_show=c("alpha","beta"),
              title="JOINT Reinforcement learning model")
unique(model_rl_joint_v6$results_summary$param_name)


p3<-do_violin_sid(data.table(model_lba_rl_parallel_v5[["complete_posterior_list"]])[SubjectId %in% 106:143,],
              parameters_to_show=c("alpha","k","tau"),
              title="PARALLEL RL+LBA model")

p4<-do_violin_sid(data.table(model_lba_rl_joint_v6[["complete_posterior_list"]])[SubjectId %in% 106:143,],
              parameters_to_show=c("alpha","k","tau"),
              title="JOINT RL+LBA model")


p5<-do_violin_sid(data.table(model_rl_discrim_joint_v1[["complete_posterior_list"]])[SubjectId %in% 106:143,],
                  parameters_to_show=c("alpha","beta","d_prime"),
                  title="JOINT RL+DISCRIMINATION model")

p6<-do_violin_sid(data.table(model_rl_discrim_joint_v1_nb2[["complete_posterior_list"]])[SubjectId %in% 106:143,],
                  parameters_to_show=c("alpha","beta","d_prime"),
                  title="JOINT RL+DISCRIMINATION model, 2-back")


p7<-do_violin_sid(data.table(model_rl_discrim_joint_v2[["complete_posterior_list"]])[SubjectId %in% 106:143,],
                  parameters_to_show=c("alpha","beta","d_prime"),
                  title="JOINT RL+DISCRIMINATION model, 2-back")
grid.arrange(p1,p2,p3,p4, nrow = 2,top="Parameter distributions by subject:\nAlpha values are higher in the RL than RL_LBA model")

grid.arrange(p2,p4,p5,p6,p7, nrow = 2,top="Parameter distributions by subject:\nAlpha values in joint models")


grid.arrange(p5,p6,p7, nrow = 3,top="Parameter distributions by subject:\nAlpha values in joint models")
#now let's take a look at the joint parameters.

show_distribution_of_all_sigmas(data.table(model_lba_rl_parallel_v5[["complete_posterior_list"]]),
                                Sigma_dims = c(4,4),ggplot_args = scale_x_continuous(limits = c(-50,50)))

show_distribution_of_all_sigmas(data.table(model_lba_rl_parallel_v5[["complete_posterior_list"]]),
                                Sigma_dims = c(4,4),ggplot_args = scale_x_continuous(limits = c(-50,50)))


show_distribution_of_all_sigmas(data.table(model_lba_rl_joint_v6[["complete_posterior_list"]]),
                                Sigma_dims = c(5,5),ggplot_args = scale_x_continuous(limits = c(-2,2)))



#discrimination model
show_distribution_of_all_sigmas(data.table(model_rl_discrim_joint_v1[["complete_posterior_list"]]),
                                Sigma_dims = c(5,5),ggplot_args = scale_x_continuous(limits = c(-2,2)))

show_distribution_of_all_sigmas(data.table(model_rl_discrim_joint_v1_nb2[["complete_posterior_list"]]),
                                Sigma_dims = c(5,5),ggplot_args = scale_x_continuous(limits = c(-2,2)))

View(model_rl_discrim_joint_v2$results_summary)
show_distribution_of_all_sigmas(data.table(model_rl_discrim_joint_v2[["complete_posterior_list"]]),
                                Sigma_dims = c(5,5),ggplot_args = scale_x_continuous(limits = c(-2,2)))
#get_sigma_array(fit = model_rl_discrim_joint_v1#,DeltaThetaLabels = DeltaThetaLabels)
#What do the log probabilities tell us about the relative performance of each model?

hist(model_rl_discrim_joint_v1$complete_posterior_list$lp__)
hist(model_lba_rl_joint_v6$complete_posterior_list$lp__)
hist(model_rl_joint_v6$complete_posterior_list$lp__)

hist(model_rl_discrim_joint_v1$complete_posterior_list$lp__-model_rl_joint_v6$complete_posterior_list$lp__)

# hist(model_rl_discrim_joint_v1_nb2$complete_posterior_list$lp__-model_rl_joint_v6$complete_posterior_list$lp__)

hist(model_rl_discrim_joint_v1$complete_posterior_list$lp__-model_rl_joint_v6$complete_posterior_list$lp__)
t.test(model_rl_discrim_joint_v1$complete_posterior_list$lp__-model_rl_joint_v6$complete_posterior_list$lp__)
#we would hope to get a difference of 2 to show that a new model is better.

length(model_rl_discrim_joint_v1$complete_posterior_list$lp__)/600
length(model_lba_rl_joint_v6$complete_posterior_list$lp__)/600
length(model_rl_joint_v6$complete_posterior_list$lp__)/600
mean(model_lba_rl_joint_v6$complete_posterior_list$lp__)
mean(model_rl_joint_v6$complete_posterior_list$lp__)
t.test(model_lba_rl_joint_v6$complete_posterior_list$lp__,model_rl_joint_v6$complete_posterior_list$lp__)
#this does make the lba_rl model a substantial improvement.
t.test(model_rl_discrim_joint_v1$complete_posterior_list$lp__,model_rl_joint_v6$complete_posterior_list$lp__)
t.test(model_rl_discrim_joint_v1_nb2$complete_posterior_list$lp__,model_rl_joint_v6$complete_posterior_list$lp__)
t.test(model_rl_discrim_joint_v2$complete_posterior_list$lp__,model_rl_joint_v6$complete_posterior_list$lp__)
#but we can't say the same for the discrimination model, sadly.

#so, what does our joint modeling distribution look like?

show_distribution_of_all_sigmas(data.table(model_lba_rl_joint_v6[["complete_posterior_list"]]),
                                Sigma_dims = c(5,5),ggplot_args = scale_x_continuous(limits = c(-2,2)))

#unfortunately, no covariance whatsoever across the single-level models.

show_distribution_of_all_sigmas(data.table(model_rl_discrim_joint_v1[["complete_posterior_list"]]),
                                Sigma_dims = c(23,1),ggplot_args = scale_x_continuous(limits = c(-2,2)))



Sigma_dims=23
rpe_covarvec<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
rpe_covarvec_df<-paste0("Sigma.",1:Sigma_dims[1],".",1,".")
library(rstan)
summary(srm.fit)$summary[,rpe_covarvec]
summary(srm.fit)$summary[rpe_covarvec,]
