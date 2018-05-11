data_amendment_version<-1
source("rl_behav_analysis_learning_setup.R")

#this admended version of the dataset changes the group classification of several subjects whose risk is indeterminate.
#these are subjects who 
#(1) had unprotected anal sex in the trial period, and 
#(2) had a primary partner, and 
#(3) who MAY not have had unprotected anal sex with anyone except their primary partner.

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