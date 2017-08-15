#install.packages("hBayesDM")
library(hBayesDM)

#output a dataset.
table(rl.all.subjects.list$trial)
trialn<-10
rl.all.subjects.list.trial.n<-rl.all.subjects.list[trial==trialn,,]
table(rl.all.subjects.list.trial.n$presentation_n)
table(rl.all.subjects.list.trial.n$presentation_n_over_segments)
table(rl.all.subjects.list.trial.n$response_key,rl.all.subjects.list.trial.n$score)
table(rl.all.subjects.list.trial.n$score)

modelPath <- system.file("stan", "prl_rp.stan", package = "hBayesDM")
functionPath <- system.file("prl_rp.R", package = "hBayesDM")
#not sure what was wrong. Do we need to take out all subjects who had a non-response at any point?
table(rl.all.subjects.list.trial.n$response_key)
rl.all.subjects.list.trial.n.complete<-rl.all.subjects.list.trial.n[
  !(subid %in% unique(rl.all.subjects.list.trial.n[response_key==0,subid]))]
table(rl.all.subjects.list.trial.n.complete$subid)
library(parallel)
prl_rp
hbayesdmPackage.format<-data.frame(
  "subjID"=rl.all.subjects.list.trial.n.complete$subid,
  "choice"=rl.all.subjects.list.trial.n.complete$response_key,
  "outcome"=rl.all.subjects.list.trial.n.complete$score)
filename<-"trial10datacomplete.txt"
write.table(hbayesdmPackage.format,filename)
res<-prl_rp(data=filename,nchain = 24,ncore = max(detectCores()-1,1))
res$allIndPars
res$parVals
res$fit
modelPath <- system.file("stan", "prl_rp.stan", package = "hBayesDM")
plot(res, type="trace", fontSize=11)
plot(res, type="trace", inc_warmup=T)   # traceplot of hyper parameters w/ warmup samples
plot(res)
plotInd(output1, "res")  