source("write_nltools_stim_fileset.R")
write_nltools_stim_fileset(rl.all.subjects.list[Motivation=="punishment"],filePrefix="detail")


# View(rl.all.subjects.list[subid==153,])
# 
# 
mydata<-read.csv("/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/runfiles/runfiledetail20170820T000903_s113_punishment_r1.txt")
mydata$OnsetPlusDuration=mydata$Onset+mydata$Duration
# 
# 
max(mydata$Onset[2:(dim(mydata)[1])]-mydata$OnsetPlusDuration[1:(dim(mydata)[1]-1)])
min(mydata$Onset[2:(dim(mydata)[1])]-mydata$OnsetPlusDuration[1:(dim(mydata)[1]-1)])
