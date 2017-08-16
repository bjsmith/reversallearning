source("get_risk_group.R")

#install.packages("hBayesDM")
library(hBayesDM)
source("rl_behav_analysis_learning_setup.R")
#output a dataset.
subgroupdata<-get_risk_group()
#trialn<-10
#rl.all.subjects.list<-rl.all.subjects.list[trial==trialn,,]
rl.all.subjects.list<-merge(rl.all.subjects.list,subgroupdata,by.x="subid",by.y="Adjusted_subid")
table(rl.all.subjects.list$presentation_n)
table(rl.all.subjects.list$presentation_n_over_segments)
table(rl.all.subjects.list$response_key,rl.all.subjects.list$score)
table(rl.all.subjects.list$score)

modelPath <- system.file("stan", "prl_rp.stan", package = "hBayesDM")
functionPath <- system.file("prl_rp.R", package = "hBayesDM")
#not sure what was wrong. Do we need to take out all subjects who had a non-response at any point?
table(rl.all.subjects.list$response_key)
library(tidyr)
  
rl.all.subjects.list.complete<-rl.all.subjects.list[
  Condition==1
  ] %>% .[order(subid,onset_time_actual)]
rl.all.subjects.list.reward.complete<-rl.all.subjects.list.complete[Motivation=="reward"]
rl.all.subjects.list.punishment.complete<-rl.all.subjects.list.complete[Motivation=="punishment"]

library(parallel)
#prl_rp
hbayesdmPackage.format<-data.frame(
  "subjID"=rl.all.subjects.list.reward.complete$subid,
  "choice"=rl.all.subjects.list.reward.complete$response_key,
  "outcome"=rl.all.subjects.list.reward.complete$score,
  "cue"=rl.all.subjects.list.reward.complete$trial,
  "SubjectGroup"=as.numeric(rl.all.subjects.list.reward.complete$RiskLabel))
filename<-"../data/all_subjs_datacomplete_reward.txt"
write.table(hbayesdmPackage.format,filename,row.names = FALSE)

hbayesdmPackage.format<-data.frame(
  "subjID"=rl.all.subjects.list.punishment.complete$subid,
  "choice"=rl.all.subjects.list.punishment.complete$response_key,
  "outcome"=rl.all.subjects.list.punishment.complete$score,
  "cue"=rl.all.subjects.list.punishment.complete$trial,
  "SubjectGroup"=as.numeric(rl.all.subjects.list.reward.complete$RiskLabel))
filename<-"../data/all_subjs_datacomplete_punishment.txt"
write.table(hbayesdmPackage.format,filename,row.names = FALSE)
# 
# hbayesdmPackage_image10.format<-data.frame(
#   "subjID"=rl.all.subjects.list.reward.complete$subid,
#   "choice"=rl.all.subjects.list.reward.complete$response_key,
#   "outcome"=rl.all.subjects.list.reward.complete$score,
#   "cue"=rl.all.subjects.list.reward.complete$trial,
#   "SubjectGroup"=as.numeric(rl.all.subjects.list.reward.complete$RiskLabel))
# filename<-"../data/all_subjs_datacomplete_reward.txt"
# write.table(hbayesdmPackage.format,filename,row.names = FALSE)

#listi of parameters
#matrix of posterior estimates

#subject*trial*expected_value
#take posterior distribution 
#then we get expected value for each trial. But is that what we are wanting?
#I think we would want to do several things:
# - correlate subject learning rate with properties in the structural and functional data
# - correlate subject overall accuracy with the same thing to test whether learning rate is more informative or not.
# - so which features of the data?
# - Which activity during *observing* correlates with trial prediction error, subject learning rate?
# - Which activity during *evaluation* correlates with trial prediction error, subject learning rate?

# 
# res<-prl_rp(data=filename,nchain = 24,ncore = max(detectCores()-1,1))
# res$allIndPars
# res$parVals
# res$fit
# modelPath <- system.file("stan", "prl_rp.stan", package = "hBayesDM")
# plot(res, type="trace", fontSize=11)
# plot(res, type="trace", inc_warmup=T)   # traceplot of hyper parameters w/ warmup samples
# plot(res)
# plotInd(output1, "res")  