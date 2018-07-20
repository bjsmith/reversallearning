source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize_fast.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")

#what we really wanna do is extract the sigmas
Sigma_dims<-25
rpe_covarvec<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
rpe_covarvec_df<-paste0("Sigma.",1:Sigma_dims[1],".",1,".")
library(rstan)

lba_rl_version<-"joint_20180706_1"

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
  "ROI_ctx_lh_G_and_S_cingul.Ant","ROI_ctx_rh_G_and_S_cingul.Ant",
  #a few things that shouldn't be correlated with RPE
  "ROI_ctx_rh_S_occipital_ant","ROI_ctx_lh_S_temporal_sup","ROI_ctx_rh_G_cingul.Post.dorsal"
)

DeltaThetaLabels=c("RewardPredictionError",gsub("ctx_","",gsub("ROI_","",regions)))

model_lba_rl_joint_v10<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180706_1",
  model_filename="lba_rl_single_exp_joint_v10",
  model_subversion="")
length(unique(model_lba_rl_joint_v10$results_summary$sid))
show_distribution_of_all_sigmas_new(
  data.table(model_lba_rl_joint_v10[["complete_posterior_list"]]),
  Sigma_dims = c(26,1),
  ggplot_args = scale_x_continuous(limits = c(-2,2))
)

rsdt<-data.table(model_lba_rl_joint_v10$results_summary)
unique(rsdt$param_name)

length(unique(rsdt$sid))
ttests<-vector("list",25)
#now do a FDR p-value correction.
for(i in 2:26){#i<-2
  tesres<-t.test(rsdt[param_name==paste0("Sigma[1,",i,"]"),.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
  ttests[[i]]<-data.table(t(data.table(tesres)))
}

testres.dt<-rbindlist(ttests)
colnames(testres.dt)<-names(tesres)
source("freesurfer_region_naming.R")
testres.dt$Region<-freesurfer_region_naming(regions)

testres.dt$AdjustedPVals<-p.adjust(testres.dt$p.value,method="fdr")
testres.dt$CI95Pct<-unlist(lapply(testres.dt$conf.int, function(ci){paste0("[",formatC(ci[1],digits=2),", ",formatC(ci[2],digits=2),"]")}))

results<-testres.dt[order(unlist(p.value)),.(Region,CI95Pct,
                                             "FDRadjustedPValue"=format.pval(unlist(AdjustedPVals),digits = 2)
                                             #,"UnadjustedPVal"=format.pval(unlist(p.value))
                                             )]

write.csv(results,paste0(localsettings$data.dir,"lba_rl/",lba_rl_version,"/lba_rl_single_exp_joint_v10_provisional.csv"))
View(results)


load(paste0(localsettings$data.dir,"lba_rl/",lba_rl_version,"/run_package_107_1_reward_lba_rl_single_exp_joint_v10.RData"))
DeltaThetaLabels<-c("RPE",
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
  "ROI_ctx_lh_G_and_S_cingul.Ant","ROI_ctx_rh_G_and_S_cingul.Ant",
  #a few things that shouldn't be correlated with RPE
  "ROI_ctx_rh_S_occipital_ant","ROI_ctx_lh_S_temporal_sup","ROI_ctx_rh_G_cingul.Post.dorsal"
)
srm.fit@par_dims$Sigma

heatmap(get_sigma_array(srm.fit,DeltaThetaLabels = paste0("TD",1:38)))



##### more expansive list of regions


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


lba_rl_version<-"joint_20180706_2"

DeltaThetaLabels=c("RewardPredictionError","ExpectedValue",gsub("ctx_","",gsub("ROI_","",regions)))

model_lba_rl_joint_v10e<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="lba_rl_single_exp_joint_v10",
  model_subversion="e")
length(unique(model_lba_rl_joint_v10$results_summary$sid))
show_distribution_of_all_sigmas_new(
  data.table(model_lba_rl_joint_v10[["complete_posterior_list"]]),
  Sigma_dims = c(26,1),
  ggplot_args = scale_x_continuous(limits = c(-2,2))
)

rsdt<-data.table(model_lba_rl_joint_v10$results_summary)
unique(rsdt$param_name)

length(unique(rsdt$sid))
ttests<-vector("list",25)
#now do a FDR p-value correction.
for(i in 2:26){#i<-2
  tesres<-t.test(rsdt[param_name==paste0("Sigma[1,",i,"]"),.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
  ttests[[i]]<-data.table(t(data.table(tesres)))
}

testres.dt<-rbindlist(ttests)
colnames(testres.dt)<-names(tesres)
source("freesurfer_region_naming.R")
testres.dt$Region<-freesurfer_region_naming(regions)

testres.dt$AdjustedPVals<-p.adjust(testres.dt$p.value,method="fdr")
testres.dt$CI95Pct<-unlist(lapply(testres.dt$conf.int, function(ci){paste0("[",formatC(ci[1],digits=2),", ",formatC(ci[2],digits=2),"]")}))

results<-testres.dt[order(unlist(p.value)),.(Region,CI95Pct,
                                             "FDRadjustedPValue"=format.pval(unlist(AdjustedPVals),digits = 2)
                                             #,"UnadjustedPVal"=format.pval(unlist(p.value))
)]

write.csv(results,paste0(localsettings$data.dir,"lba_rl/",lba_rl_version,"/lba_rl_single_exp_joint_v10_provisional.csv"))
View(results)


load(paste0(localsettings$data.dir,"lba_rl/",lba_rl_version,"/run_package_107_1_reward_lba_rl_single_exp_joint_v10.RData"))
DeltaThetaLabels<-c("RPE",
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
                    "ROI_ctx_lh_G_and_S_cingul.Ant","ROI_ctx_rh_G_and_S_cingul.Ant",
                    #a few things that shouldn't be correlated with RPE
                    "ROI_ctx_rh_S_occipital_ant","ROI_ctx_lh_S_temporal_sup","ROI_ctx_rh_G_cingul.Post.dorsal"
)
srm.fit@par_dims$Sigma

heatmap(get_sigma_array(srm.fit,DeltaThetaLabels = paste0("TD",1:38)))
