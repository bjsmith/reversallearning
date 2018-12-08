source("stanlba/lba_rl_joint_setup.R")
source("stanlba/singlelevelmodel/single_level_model_summarize.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")

source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v7_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v10_functions.R")

regions<-get_dmn_regions()

model_rl_joint_v6<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_joint_20180701_1",
  model_filename="rl_single_exp_joint_v6",
  model_subversion="")

rsdt.v6<-data.table(model_rl_joint_v6$results_summary)

rl_joint_v6_logprob<-rsdt.v6[param_name=="lp__",mean,by=.(sid,rid,motivation)]

model_rl_opal_joint<-single_level_model_summarize(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="rl_opal_joint_20180709_1",
  model_filename="rl_opal_single_exp_joint_v8",
  model_subversion="")

rsdt<-data.table(model_rl_opal_joint$results_summary)
rsdt.opal<-data.table(model_rl_opal_joint$results_summary)
all.runs<-rawdata[,.N,.(subid,Motivation,runid)]
included_runs<-rsdt[,.N,.(sid,motivation,rid)]
length(unique(included_runs$sid))
View(included_runs)
all.subjs<-rawdata[,.N,.(subid)]
included.subjs<-rsdt[,.N,.(sid)]
print(paste0(as.character(dim(all.runs)[1]-dim(included_runs)[1]), 
             " runs missing and ",as.character(dim(all.subjs)[1]-dim(included.subjs)[1])," subjects missing altogether because we couldn't calculate parameters for them. (alternatively, estimation isn't complete yet)"))


source("freesurfer_region_naming.R")
length(unique(rsdt$sid))
regions<-c("AccumbensL","AccumbensR","OFCL","OFCR")
ttests<-vector("list",4)
#now do a FDR p-value correction.
theta_names<-c("RPE")
# for (t in 1:2){
t=1
  for(i in 2:5){#i<-2
    tesres<-t.test(rsdt[param_name==paste0("Sigma[",t,",",i,"]"),.(SigmaSubjectMean=mean(mean)),.(sid)]$SigmaSubjectMean)
    testres.dt<-data.table(t(data.table(tesres)))
    colnames(testres.dt)<-names(tesres)
    testres.dt$Region<-freesurfer_region_naming(regions)[i-1]
    testres.dt$Theta<-theta_names[t]
    ttests[[i-1]]<-testres.dt
  }
#}

testres.dt<-rbindlist(ttests)

testres.dt$AdjustedPVals<-p.adjust(testres.dt$p.value,method="fdr")
testres.dt$CI95Pct<-unlist(lapply(testres.dt$conf.int, function(ci){paste0("[",formatC(ci[1],digits=2),", ",formatC(ci[2],digits=2),"]")}))

results<-testres.dt[order(unlist(p.value)),.(Region,CI95Pct,
                                             "FDRadjustedPValue"=format.pval(unlist(AdjustedPVals),digits = 2)
                                             #,"UnadjustedPVal"=format.pval(unlist(p.value))
)]


write.csv(results,paste0(localsettings$data.dir,"lba_rl/",lba_rl_version,"/lba_rl_single_exp_joint_v11e_provisional.csv"))
View(results)

#we really need the log probability of this model so we can compare that to the other models.
unique(rsdt$param_name)

rl_opal_joint_v8_logprob<-rsdt[param_name=="lp__",mean,by=.(sid,rid,motivation)]
hist(rl_opal_joint_v8_logprob$mean)
hist(rl_joint_v6_logprob$mean)
t.test(rl_opal_joint_v8_logprob$mean,rl_joint_v6_logprob$mean)
#looks like the log probability of the opal model is LESS than the log probability of the rl model.
#unfortunately this means as it stands that the OpAL model is NOT an improvement.
#we could try taking out the response time part and see if that helps;
#it might be worth knowing if the actor-critic model on it's own is an improvement over a more sophisticated model.
#we should be comparing like with like though - are these the same subjects?
runs_to_compare<-intersect(unique(as.character(interaction(rsdt$sid,rsdt$rid,rsdt$motivation))),
                           unique(as.character(interaction(rsdt.v6$sid,rsdt.v6$rid,rsdt.v6$motivation))))

rsdt.opal$srmCode<-as.character(interaction(rsdt.opal$sid,rsdt.opal$rid,rsdt.opal$motivation))
rsdt.v6$srmCode<-as.character(interaction(rsdt.v6$sid,rsdt.v6$rid,rsdt.v6$motivation))

t.test(rsdt.v6[param_name=="lp__" & srmCode %in% runs_to_compare,mean,by=.(sid,rid,motivation)]$mean,
       rsdt.opal[param_name=="lp__" & srmCode %in% runs_to_compare,mean,by=.(sid,rid,motivation)]$mean)
#OK. Opal model still is at a disadvantage, but not by a huge degree.
#I think we need to run this OpAL model next to the other model once we finally perfect it.

#I think we really need to run them *exactly* side-by-side, and using