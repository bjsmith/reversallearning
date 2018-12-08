library(rstan)
source("stanlba/lba_rl_joint_setup.R")
require(R.utils)
options(mc.cores = 6)
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/rl_discrim/lba_rl_discrim_v2_functions.R")
source("stanlba/singlelevelmodel/single_level_model_summarize.R")
source("stanlba/singlelevelmodel/single_level_model_summarize_fast.R")

model_lba_rl_single_exp_discrim_v1<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180716_1",
  model_filename="lba_rl_single_exp_discrim_v1a",
  model_subversion="")

model_lba_rl_single_exp_discrim_v1_rsdt<-data.table(model_lba_rl_single_exp_discrim_v1$results_summary)

model_lba_rl_single_exp_v3<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180716_1",
  model_filename="lba_rl_single_exp_v3a",
  model_subversion="")
model_lba_rl_single_exp_v3_rsdt<-data.table(model_lba_rl_single_exp_v3$results_summary)

#discrim v1 seems to consistently underperform.
model_lba_rl_single_exp_discrim_v2<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180716_1",
  model_filename="lba_rl_single_exp_discrim_v2a",
  model_subversion="")
model_lba_rl_single_exp_discrim_v2_rsdt<-data.table(model_lba_rl_single_exp_discrim_v2$results_summary)


model_lba_rl_single_exp_discrim_v3<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180716_1",
  model_filename="lba_rl_single_exp_discrim_v3a",
  model_subversion="")
model_lba_rl_single_exp_discrim_v3_rsdt<-data.table(model_lba_rl_single_exp_discrim_v3$results_summary)
model_lba_rl_single_exp_v3_rsdt[param_name=="log_lik"]

table(model_lba_rl_single_exp_v3_rsdt$param_name)

compare_lp<-rbind(model_lba_rl_single_exp_v3_rsdt[param_name=="log_lik",.(mean,sid,rid,motivation,ModelName="lba_rl_single_exp_v3")],
      model_lba_rl_single_exp_discrim_v1_rsdt[param_name=="log_lik",.(mean,sid,rid,motivation,ModelName="lba_rl_single_exp_discrim_v1")],
      model_lba_rl_single_exp_discrim_v2_rsdt[param_name=="log_lik",.(mean,sid,rid,motivation,ModelName="lba_rl_single_exp_discrim_v2")],
      model_lba_rl_single_exp_discrim_v3_rsdt[param_name=="log_lik",.(mean,sid,rid,motivation,ModelName="lba_rl_single_exp_discrim_v3")]
      ) %>%
  tidyr::spread(ModelName,mean)

save(compare_lp,file=paste0(localsettings$data.dir,"lba_rl/rl_joint_20180711_1/comparison.RData"))
colMeans(compare_lp[,4:9],na.rm = TRUE)

ggplot(compare_lp[,4:9] %>% tidyr::gather("Model","LogProbabilility"),aes(LogProbabilility,color=Model,group=Model))+geom_density()
View(compare_lp)

hist((compare_lp[,4]-compare_lp[,7])[[1]])
hist((compare_lp[,5]-compare_lp[,7])[[1]])
hist((compare_lp[,6]-compare_lp[,7])[[1]])

colnames(model_lba_rl_single_exp_discrim_v3$complete_posterior_list)[1:20]
model_lba_rl_single_exp_discrim_v3_rsdt[param_name=="alpha",]