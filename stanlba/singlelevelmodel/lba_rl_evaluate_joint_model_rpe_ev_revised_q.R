source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize_fast.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v7_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v10_functions.R")
source("freesurfer_region_naming.R")
regions<-get_dmn_regions()
#what we really wanna do is extract the sigmas
Sigma_dims<-4
rpe_covarvec<-paste0("Sigma[",1:Sigma_dims[1],",",1,"]")
rpe_covarvec_df<-paste0("Sigma.",1:Sigma_dims[1],".",1,".")
library(rstan)


lba_rl_version<-"joint_20180709_1"

DeltaThetaLabels=c("RewardPredictionError","ExpectedValue",gsub("ctx_","",gsub("ROI_","",regions)))
load('/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180709_1/run_package_125_2_reward_lba_rl_single_exp_joint_v11q.RData')

#heatmap(get_sigma_array(run_package$fit,DeltaThetaLabels=paste0("DT",1:39)))


model_lba_rl_joint_v11q<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="lba_rl_single_exp_joint_v11",
  model_subversion="q")
model_lba_rl_joint_v11q$rsdt<-data.table(model_lba_rl_joint_v11q$results_summary)
rsdt<-model_lba_rl_joint_v11q$rsdt


ttests<-vector("list",4*2)
#now do a FDR p-value correction.
theta_names<-c("RPE","EV")
for (t in 1:2){
  for(i in 3:6){#i<-2
    tesres<-t.test(rsdt[param_name==paste0("Sigma[",t,",",i,"]"),.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
    testres.dt<-data.table(t(data.table(tesres)))
    colnames(testres.dt)<-names(tesres)
    testres.dt$Region<-freesurfer_region_naming(regions)[i-2]
    testres.dt$Theta<-theta_names[t]
    ttests[[i-2+(t-1)*4]]<-testres.dt
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

write.csv(results,paste0(localsettings$data.dir,"lba_rl/",lba_rl_version,"/lba_rl_single_exp_joint_v11f_provisional.csv"))
View(results)

#let's do a matrix.

Sigma_matrix_mean<-matrix(NA,nrow=39,ncol=39)
mat_names<-c(c("RPE","EV"),freesurfer_region_naming(regions))
for (m in 1:length(mat_names)){#m<-1
  for(n in 1:length(mat_names)){#n<-1
    Sigma_m_n<-mean(rsdt[param_name==paste0("Sigma[",m,",",n,"]"),.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
    Sigma_matrix_mean[m,n]<-Sigma_m_n
  }
}
colnames(Sigma_matrix_mean)<-mat_names
rownames(Sigma_matrix_mean)<-mat_names
heatmap(Sigma_matrix_mean,show_labels = FALSE)
save(Sigma_matrix_mean,file=paste0(localsettings$data.dir,"lba_rl/",lba_rl_version,"/lba_rl_single_exp_joint_v11f.RData"))

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

heatmap(get_sigma_array(srm.fit,DeltaThetaLabels = paste0("TD",1:38)))

#now let's take a look at R vs. P contrasts and inter-group comparisons.
# inter-group comparisons will be easiest.
#First: learning rate
#let's follow the same method as before, using the posterior means.

#how to get a summary of each subject's learning rate?
hist(rsdt[param_name=="alpha",.(alpha_SubjectMean=mean(mean)),.(sid)]$alpha_SubjectMean)
#now we need to know about the groups these subjects are in.

key_behavioral_params<-merge(rsdt[param_name %in% c("alpha","k","tau"),.(SubjectMean=mean(mean)),.(sid,param_name)],
      rawdata[,.(Group=unique(SubjectGroup),MethUse=unique(MethUse)),.(subid)],by.x="sid",by.y="subid")

#Do meth-users have slower learning rates than non-meth-users?
t.test(SubjectMean~MethUse,key_behavioral_params[Group %in% c(1,2,3) & param_name=="alpha",])
#I can't find evidence for that in this model.
#how about the other parameters?
t.test(SubjectMean~MethUse,key_behavioral_params[Group %in% c(1,2,3) & param_name=="k",])
t.test(SubjectMean~MethUse,key_behavioral_params[Group %in% c(1,2,3) & param_name=="tau",])

#what about just between group 2 and 3?
t.test(SubjectMean~MethUse,key_behavioral_params[Group %in% c(2,3) & param_name=="alpha",])
#I can't find evidence for that in this model.
#how about the other parameters?
t.test(SubjectMean~MethUse,key_behavioral_params[Group %in% c(2,3) & param_name=="k",])
t.test(SubjectMean~MethUse,key_behavioral_params[Group %in% c(2,3) & param_name=="tau",])

#nope.
#OK, so we can't do anything with the groups here. Our data are probably not well-targeted enough.
#Can we do reward vs. punishment?
#we need to manage this within-subject.

r_v_punish.dt<-rsdt[param_name %in% c("alpha","k","tau"),.(RewardRunsMean=mean(.SD[motivation=="reward",mean]),
                                            PunishmentRunsMean=mean(.SD[motivation=="punishment",mean])),
                                            .(sid,param_name)]

key_behavioral_params_rvp<-merge(r_v_punish.dt,
                             rawdata[,.(Group=unique(SubjectGroup),MethUse=unique(MethUse)),.(subid)],by.x="sid",by.y="subid")

key_behavioral_params_rvp$RPDiff<-key_behavioral_params_rvp$RewardRunsMean-key_behavioral_params_rvp$PunishmentRunsMean
t.test(key_behavioral_params_rvp[Group %in% c(1, 2,3) & param_name=="alpha",]$RPDiff)
t.test(key_behavioral_params_rvp[Group %in% c(1, 2,3) & param_name=="k",]$RPDiff)
t.test(key_behavioral_params_rvp[Group %in% c(1, 2,3) & param_name=="tau",]$RPDiff)
#No evidence of a reward vs. punishment difference in the key learning parameters. I wasn't really expecting to see it though.





for (t in 1:2){
  for(i in 3:39){#i<-2
    tesres<-t.test(rsdt[param_name==paste0("Sigma[",t,",",i,"]"),.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
    testres.dt<-data.table(t(data.table(tesres)))
    colnames(testres.dt)<-names(tesres)
    testres.dt$Region<-freesurfer_region_naming(regions)[i-2]
    testres.dt$Theta<-theta_names[t]
    ttests[[i-2+(t-1)*37]]<-testres.dt
  }
}

#OK here's the deal: we know which subjects were excluded and which were included.
#we can compare the correct vs. incorrect stats to see if poorly performing runs are the ones excluded.
excluded_runs<-setdiff(
  paste0(all.runs$subid,all.runs$Motivation,all.runs$runid),
  paste0(included_runs$sid,included_runs$motivation,included_runs$rid))
#let's take a look at rawdata correct scores.
excludedCorrect<-rawdata[,.(CorrectReponse=sum(correct),
           srmCode=paste0(subid,Motivation,runid)
           ),.(subid,runid,Motivation)]
excludedCorrect$IsIncluded<-excludedCorrect$srmCode %in% excluded_runs
t.test(CorrectReponse~IsIncluded,excludedCorrect)
#no difference in correct response between the ones we excluded and the ones we didn't.
#is it relate to heterogeneity in the cues?

performanceAnalysis<-rawdata[,.(CorrectReponseByCue=sum(correct),
           srmCode=paste0(subid,Motivation,runid)
),.(subid,runid,Motivation,cue)] %>% .[,.(PerformanceSDAcrossCues=sd(CorrectReponseByCue),
                                          PerformanceRangeAcrossCues=max(CorrectReponseByCue)-min(CorrectReponseByCue),
                                          CorrectResponse=sum(CorrectReponseByCue),
                                          MeanCorrectResponseAcrossCues=mean(CorrectReponseByCue),
                                          srmCode=paste0(subid,Motivation,runid)),
                                          .(subid,runid,Motivation)]
performanceAnalysis$IsIncluded<-performanceAnalysis$srmCode %in% excluded_runs

t.test(PerformanceSDAcrossCues~IsIncluded,performanceAnalysis)

t.test(MeanCorrectResponseAcrossCues~IsIncluded,performanceAnalysis)
#no evidence it is related to heterogeneity, in terms of range or SD
t.test(PerformanceRangeAcrossCues~IsIncluded,performanceAnalysis)


#let's do a pairs plot across all the subjects, runs.
#then it might be good to MANUALLY take a look at each of the posteriors 

#let's take a look at sigma variability across subjects.

#what is the distribution for each value?

#a pairs plot might even suffice.
#but first, why are we getting positive log probabilities?
#Can we replicate these? And why are they coming up?


lba_rl_version<-"joint_20180709_1"
model_lba_rl_joint_v11g<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version=lba_rl_version,
  model_filename="lba_rl_single_exp_joint_v11",
  model_subversion="g")
rsdt<-data.table(model_lba_rl_joint_v11g$results_summary)
table(rsdt$param_name)

regions<-c("ROI_ctx_lh_S_suborbital","ROI_ctx_rh_S_suborbital", "ROI_Left.Accumbens.area", "ROI_Right.Accumbens.area")
ttests<-vector("list",6*2)
#now do a FDR p-value correction.
theta_names<-c("RPE","EV")
for (t in 1:2){
  for(i in 3:6){#i<-2
    tesres<-t.test(rsdt[param_name==paste0("Sigma[",t,",",i,"]"),.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
    testres.dt<-data.table(t(data.table(tesres)))
    colnames(testres.dt)<-names(tesres)
    testres.dt$Region<-freesurfer_region_naming(regions)[i-2]
    testres.dt$Theta<-theta_names[t]
    ttests[[i-2+(t-1)*37]]<-testres.dt
  }
}

testres.dt<-rbindlist(ttests)

testres.dt$AdjustedPVals<-p.adjust(unlist(testres.dt$p.value),method="fdr")
testres.dt$CI95Pct<-unlist(lapply(testres.dt$conf.int, function(ci){paste0("[",formatC(ci[1],digits=2),", ",formatC(ci[2],digits=2),"]")}))
testres.summary.CIs<-tidyr::spread(testres.dt[,.(Region,Theta,CI95Pct)],Theta,CI95Pct)
testres.summary.AdjustedPVals<-tidyr::spread(testres.dt[,.(Region,Theta,AdjustedPVals)],Theta,AdjustedPVals)
testres.summary.wide<-merge(testres.summary.CIs,testres.summary.AdjustedPVals,by="Region",suffixes = c("CI95Pct","AdjustedPVals"))
testres.summary.wide$RPEAdjustedPVals
results<-testres.summary.wide[order(unlist(RPEAdjustedPVals)),
                              .(Region,EVCI95Pct,
                                "EV_FDRadjustedPValue"=format.pval(unlist(EVAdjustedPVals),digits = 2),
                                RPECI95Pct,
                                "RPE_FDRadjustedPValue"=format.pval(unlist(RPEAdjustedPVals),digits = 2)
                                #,"UnadjustedPVal"=format.pval(unlist(p.value))
)]
View(results)
write.csv(results,paste0(localsettings$data.dir,"lba_rl/",lba_rl_version,"/lba_rl_single_exp_joint_v11g_provisional.csv"))



###################### 
## look at the variability of individual sigmas in subjects.
## we really want pairs plots.
## we will have to put it into 3-D array format I think, is best.

get_srm_fit<-function(s,r,m,modelnameversion){
  load(paste0(
    '/expdata/bensmith/joint-modeling/data/msm/reversallearning/lba_rl/joint_20180709_1/run_package_',
    s,'_',r,'_',m,'_',modelnameversion,'.RData'))
  return(run_package)
}
srm_fit_eg<-get_srm_fit(106,1,"punishment","lba_rl_single_exp_joint_v11f")


run_ids<-model_lba_rl_joint_v11f$rsdt[order(FullRunId),.N,by=.(sid,FullRunId,rid,motivation)] %>% .[order(FullRunId),paste0("s",sid,"_fr",rid,"_",motivation)]
sigma_array<-array(NA,dim = c(srm_fit_eg$fit@par_dims$Sigma,
                              length(run_ids)))
dimnames(sigma_array)<-list(mat_names,mat_names,run_ids)

#let's iterate through and see how we can copy the values into the array.
for (m in 1:length(mat_names)){#m<-1
  for(n in 1:length(mat_names)){#n<-1
    Sigma_m_n<-rsdt[param_name==paste0("Sigma[",m,",",n,"]"),mean,.(sid,FullRunId)] %>% .[order(FullRunId)]
    sigma_array[m,n,]<-Sigma_m_n$mean
  }
}
sigma_array_list<-vector("list",length(run_ids))
heatmap(sigma_array[,,run_ids[1]])
for(i in 1:length(run_ids)){i<-1
heatmap(sigma_array[,,run_ids[i]])
  sigma_array_list[[i]]<-melt(sigma_array[,,run_ids[i]])
}
names(sigma_array_list)<-run_ids
#right, now we have the array; what's the best way to display it?
#try a pairs plot. This will require long data format.
