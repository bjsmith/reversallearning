data_amendment_version<-2
source("rl_behav_analysis_learning_setup.R")

#this admended version of the dataset 
#(I) changes the group classification of several subjects whose risk is indeterminate.
#these are subjects who 
  # (1) had unprotected anal sex in the trial period, and
  # (2) had a primary partner, and
  # (3) who MAY not have had unprotected anal sex with anyone except their primary partner.
# (II) cleans up reaction times.
# (1) Reaction times less than 140 ms; new nonresponse_cleaned column that marks these as non-responses
# (III) poorly performing subjects
#       marks out subjects who do not meet the inclusion criteria of reasonably good performane.

rl.all.subjects.list$HasPrimaryPartner<-factor(rl.all.subjects.list$Q32,levels=c("No","Yes"))
#rl.all.subjects.list$Q39
rl.all.subjects.list$Q39_cleaned=rl.all.subjects.list$Q39
rl.all.subjects.list[Q24.1=="No",Q39_cleaned:=0]
#rl.all.subjects.list$Q39_cleaned

# Q34
# In the past 90 days have you had unprotected receptive anal sex (i.e., been fucked without a condom)...
#rl.all.subjects.list$Q34
# Q41
# How many different people have you fucked without a condom in the past 90 days?
rl.all.subjects.list$Q41_cleaned<-rl.all.subjects.list$Q41
rl.all.subjects.list[Q34=="No",Q41_cleaned:=0]

#the min of this is the greater of these two, and the max would be the sum of the two.
rl.all.subjects.list$MinimumUnprotectedPartnerCount=apply(rl.all.subjects.list,1,function(r){max(as.integer(r[["Q41"]]),as.integer(r[["Q39"]]))})
table(rl.all.subjects.list$RiskCat)
subs.to.reclassify<-rl.all.subjects.list[
#(1) had unprotected anal sex in the trial period, and 
  (RiskCat %in% c(2,3)) &
#(2) had a primary partner, and 
  HasPrimaryPartner=="Yes" &
#(3) who MAY not have had unprotected anal sex with anyone except their primary partner.
  MinimumUnprotectedPartnerCount<=1
  ,.N,by=subid
  ]$subid


# rl.all.subjects.list[subid %in% subs.to.reclassify,.N,by=.(RiskCat,RiskLabel,MethUse,SexRisk)]
# rl.all.subjects.list[,.N,by=.(RiskCat,RiskLabel,MethUse,SexRisk)]
#remove subjects from their categories if their sexual behavior is ambiguous.
rl.all.subjects.list[subid %in% subs.to.reclassify, 
                     c("RiskCat", "RiskLabel","SexRisk") := list(NA,NA,NA)]

#clean up reaction times.

# library(ggplot2)
# library(scales)
 
#The Woods et al. cuttoff point seems a little low. Based on the distribution in our data, a better cutoff point would be 
#around 140 ms.
rl.all.subjects.list$choice<-rl.all.subjects.list$response_key
rl.all.subjects.list[reaction_time<.140 & response_key!=0,choice:=0]
# table(rl.all.subjects.list$response_key,rl.all.subjects.list$choice)

#poorly performing subjects.
rl.all.subjects.list$outcome<-rl.all.subjects.list$score

#exclude subjects who seem to be just repeatedly pressing buttons.
changeDetect<-function(vec,onset_time=1:length(vec)){
  #order vec
  vec<-vec[order(onset_time)]
  
  # print(vec)
  # print("")
  sum(vec[2:length(vec)]!=vec[1:(length(vec)-1)])
}
#buttonChanges<-unlist(lapply(data,function(d){mean(unlist(lapply(d$runs,function(r){changeDetect(r$choice)})))}))
#gonna be assessed as a mean score across subjects.
# buttonChanges<-unlist(lapply(data,function(d){mean(unlist(lapply(d$runs,function(r){changeDetect(r$choice)})))}))
# mean(unlist(lapply(data[[1]]$runs,function(r){changeDetect(r$choice)})))
# 

buttonChangesDtByRun<-rl.all.subjects.list[,.(RunButtonChanges=changeDetect(response_key,onset_time_actual),
                                         ButtonCount=.N
                                         #,ButtonMashLikelihood=log.dens.like.buttonmash(dr = .SD)
                                         ),by=.(runid,subid,Motivation)]
  
buttonChangesDt<-buttonChangesDtByRun[,SubjMeanRunButtonChanges:=mean(RunButtonChanges),by=subid]

#this is a probability problem too. so if a probability of the event once is 0.1, 
#then the expected number of runs with that event is
#with two runs, if the probability of this number of switches is 0.9 then the expected number is
#just multiply by 0.9, right? yes. That's the *expected* number.
#So let's get empirical Ns and the probability distribution...

#there were 218 trials in each run.
N_trials_per_run=218
N_runs<-dim(buttonChangesDtByRun)[1]
#so let's find the counts of changes at each level
RunsWithChanges=data.frame(ChangeNum=min(buttonChangesDtByRun$RunButtonChanges):max(buttonChangesDtByRun$RunButtonChanges),RunCount=sapply(min(buttonChangesDtByRun$RunButtonChanges):max(buttonChangesDtByRun$RunButtonChanges),function(x)sum(buttonChangesDtByRun$RunButtonChanges<=x)))

RunsWithChanges$ExpectedRunCount<-round(pbinom(RunsWithChanges$ChangeNum,size=N_trials_per_run,0.5)*N_runs,3)
buttonChangesThreshold<-max(RunsWithChanges$ChangeNum[RunsWithChanges$ExpectedRunCount<1])

#I want to do this by run, not by subject.

#a more sophisticated model might have a policy detection which probabilitistically detects which policy a subject
#uses to press buttons but I'm not using that for now.
#also accuracy data will be useful.
#overallperformance<-unlist(lapply(data,function(d){mean(unlist(lapply(d$runs,function(r){sum(r$outcome==1)/length(r$outcome)})))}))

OverallPerformanceDt<-rl.all.subjects.list[,.(RunPerformance=sum(outcome==1)/.N),by=.(runid,subid,Motivation)]
OverallPerformanceBySubjDt<-OverallPerformanceDt[,.(RunPerformanceBySubject=mean(RunPerformance)),by=subid]

rl.all.subjects.list.uncleaned<-rl.all.subjects.list
rl.all.subjects.list.uncleaned<-merge(rl.all.subjects.list.uncleaned,buttonChangesDtByRun,by=c("runid","subid","Motivation"))
rl.all.subjects.list.uncleaned<-merge(rl.all.subjects.list.uncleaned,OverallPerformanceBySubjDt,by=c("subid"))
rl.all.subjects.list<-rl.all.subjects.list.uncleaned[RunButtonChanges>buttonChangesThreshold & 
                                                       RunPerformanceBySubject>0.4]
#performance worse than 0.4 may suggest the subject has misunderstood the task.


rl.all.subjects.list[,runmotiveid:=runid]#start this; it'll be more fully defined below, see below.
for (s in unique(rl.all.subjects.list$subid)){
  #get the reward run count so that we can give each run, rew and pun, unique IDs
  rew_runcount <- length(unique(rl.all.subjects.list[subid==s & Motivation=="reward",runid]))
  pun_runcount <- length(unique(rl.all.subjects.list[subid==s & Motivation=="punishment",runid]))
  
  rl.all.subjects.list[subid==s & Motivation=="punishment",runmotiveid:=runmotiveid+rew_runcount]
}
