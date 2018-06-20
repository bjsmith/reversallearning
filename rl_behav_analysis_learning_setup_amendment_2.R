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
changeDetect<-function(vec){sum(vec[2:length(vec)]!=vec[1:(length(vec)-1)])}
#buttonChanges<-unlist(lapply(data,function(d){mean(unlist(lapply(d$runs,function(r){changeDetect(r$choice)})))}))
buttonChangesDt<-rl.all.subjects.list[,.(RunButtonChanges=changeDetect(response_key)),by=.(runid,subid,Motivation)]
#a more sophisticated model might have a policy detection which probabilitistically detects which policy a subject
#uses to press buttons but I'm not using that for now.
#also accuracy data will be useful.
#overallperformance<-unlist(lapply(data,function(d){mean(unlist(lapply(d$runs,function(r){sum(r$outcome==1)/length(r$outcome)})))}))

OverallPerformanceDt<-rl.all.subjects.list[,.(RunPerformance=sum(outcome==1)/.N),by=.(runid,subid,Motivation)]

rl.all.subjects.list.uncleaned<-rl.all.subjects.list
rl.all.subjects.list.uncleaned<-merge(rl.all.subjects.list.uncleaned,buttonChangesDt,by=c("runid","subid","Motivation"))
rl.all.subjects.list.uncleaned<-merge(rl.all.subjects.list.uncleaned,OverallPerformanceDt,by=c("runid","subid","Motivation"))
rl.all.subjects.list<-rl.all.subjects.list.uncleaned[RunButtonChanges>90 & RunPerformance>0.4]
length(unique(rl.all.subjects.list$subid))


#performance worse than 0.4 may suggest the subject has misunderstood the task.
