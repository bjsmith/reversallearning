library(parallel)
source("prlp_ben.R")
source("rl_behav_analysis_learning_setup.R")
trialn<-10
rl.all.subjects.list.trial.n<-rl.all.subjects.list[trial==trialn,,]

table(rl.all.subjects.list.trial.n$response_key)
rl.all.subjects.list.trial.n.complete<-rl.all.subjects.list.trial.n[
  !(subid %in% unique(rl.all.subjects.list.trial.n[response_key==0,subid]))]

hbayesdmPackage.format<-data.frame(
  "subjID"=rl.all.subjects.list.trial.n.complete$subid,
  "choice"=rl.all.subjects.list.trial.n.complete$response_key,
  "outcome"=rl.all.subjects.list.trial.n.complete$score)
filename<-"trial10datacomplete.txt"
write.table(hbayesdmPackage.format,filename,sep="\t")
res<-prl_rp_ben(data=filename,nchain = 4,ncore = 1)
#res<-prl_rp_ben(data=filename,nchain = 4,ncore = max(detectCores()-1,1))

modelPath <- system.file("stan", "prl_rp.stan", package = "hBayesDM")
plot(res, type="trace", fontSize=11)
plot(res, type="trace", inc_warmup=T)   # traceplot of hyper parameters w/ warmup samples
plot(res)
plotInd(output1, "res")  

prl_rp_ben