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


lba_rl_version<-"joint_20180727_1"


#load('/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180708_1/run_package_140_1_punishment_lba_rl_single_exp_joint_v11e.RData')
library(rstan)

#Motion+CSF+WM regressed out; freesurfer ROIs all dmn regions
model_lba_rl_joint_v11v<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="lba_rl_single_exp_joint_v11",
  model_subversion="v")
#seems to be very patchy, timing out occasionally and more often, failing to fully converge.


run_data_report<-function(rsdt){
  #we need to quantify how many runs missed out because we couldn't calculate good values for them.
  all.runs<-rawdata[,.N,.(subid,Motivation,runid)]
  included_runs<-rsdt[,.N,.(sid,motivation,rid)]
  length(unique(included_runs$sid))
  View(included_runs)
  all.subjs<-rawdata[,.N,.(subid)]
  included.subjs<-rsdt[,.N,.(sid)]
  print(paste0(as.character(dim(all.runs)[1]-dim(included_runs)[1]), 
               " runs missing and ",as.character(dim(all.subjs)[1]-dim(included.subjs)[1])," subjects missing altogether because we couldn't calculate parameters for them. (alternatively, estimation isn't complete yet)"))
  
  #that's an awful lot of runs! We need to get them one by one and work out what's going wrong because I don't think I can proceed without these in the calculations. It could be a substantial bias to the model to just throw out all the data that doesn't fit.
}
library(dplyr)
run_data_report(model_lba_rl_joint_v11v$results_summary_dt)

t.test.report<-function(rsdt,theta_names,delta_names){
  #rsdt<-model_lba_rl_joint_v11s$results_summary_dt
  delta_count<-length(delta_names)
  theta_count <- length(theta_names)
  ttests<-vector("list",theta_count*delta_count)
  for (t in 1:theta_count){
    for(i in (theta_count+1):(theta_count+delta_count)){#i<-2
      tesres<-t.test(rsdt[param_name==paste0("Sigma[",t,",",i,"]"),.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
      testres.dt<-data.table(t(data.table(tesres)))
      colnames(testres.dt)<-names(tesres)
      testres.dt$Region<-delta_names[i-theta_count]
      testres.dt$Theta<-theta_names[t]
      ttests[[i-theta_count+(t-1)*delta_count]]<-testres.dt
    }
  }
  testres.dt<-rbindlist(ttests)
  
  testres.dt$AdjustedPVals<-p.adjust(unlist(testres.dt$p.value),method="fdr")
  testres.dt$CI95Pct<-unlist(lapply(testres.dt$conf.int, function(ci){paste0("[",formatC(ci[1],digits=2),", ",formatC(ci[2],digits=2),"]")}))
  testres.summary.CIs<-tidyr::spread(testres.dt[,.(Region,Theta,CI95Pct)],Theta,CI95Pct)
  testres.summary.AdjustedPVals<-tidyr::spread(testres.dt[,.(Region,Theta,AdjustedPVals)],Theta,AdjustedPVals)
  testres.summary.wide<-merge(testres.summary.CIs,testres.summary.AdjustedPVals,by="Region",suffixes = c("CI95Pct","AdjustedPVals"))
  testres.summary.wide$RPEAdjustedPVals
  results<-testres.summary.wide[order(unlist(RPEAdjustedPVals)),.(Region,EVCI95Pct,
                                                                  "EV_FDRadjustedPValue"=format.pval(unlist(EVAdjustedPVals),digits = 2),
                                                                  RPECI95Pct,
                                                                  "RPE_FDRadjustedPValue"=format.pval(unlist(RPEAdjustedPVals),digits = 2)
                                                                  #,"UnadjustedPVal"=format.pval(unlist(p.value))
  )]
  return(results)
}

source('freesurfer_region_naming.R')
#s
#now do a FDR p-value correction.
theta_names<-c("RPE","EV")
delta_names<- c("Simulated_Accumbens_Left","Simulated_Accumbens_Right", "Simulated_SubOrbitalLeft", "Simulated_SubOrbitalRight")

STD<-t.test.report(model_lba_rl_joint_v11v$results_summary_dt,
              theta_names=theta_names,
              delta_names=delta_names)

write.csv(STD,"lba_rl_joint_v11v_sigma_theta_delta.csv")
heatmap(get_Sigma_m_n(model_lba_rl_joint_v11v$results_summary_dt,theta_names,delta_names))


lba_rl_version<-"joint_20180727_3"

model_lba_rl_joint_v17v<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="lba_rl_single_exp_joint_v17",
  model_subversion="v")

STD<-t.test.report(model_lba_rl_joint_v17v$results_summary_dt,
                   theta_names=theta_names,
                   delta_names=delta_names)

heatmap(get_Sigma_m_n(model_lba_rl_joint_v17v$results_summary_dt,theta_names,delta_names))


model_lba_rl_joint_v17v<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180727_3",
  model_filename="lba_rl_single_exp_joint_v17",
  model_subversion="v")


model_lba_rl_joint_v18v<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180727_3",
  model_filename="lba_rl_single_exp_joint_v18",
  model_subversion="v")

STD<-t.test.report(model_lba_rl_joint_v18v$results_summary_dt,
                   theta_names=theta_names,
                   delta_names=delta_names)

heatmap(get_Sigma_m_n(model_lba_rl_joint_v18v$results_summary_dt,theta_names,delta_names))
