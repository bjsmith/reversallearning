source("util/apply_local_settings.R")
apply_local_settings()
dd<-localsettings$data.dir
rawdata <- read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment.txt"), header=T)
#this file has artificially generated pain data.
testfile<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/testing/105_punishment_r1.csv"

nps_output<-read_nps_output(read.csv(file=testfile))
nps_output$OutcomeCorrect<-nps_output$Outcome=="correct"
t.test(Value~OutcomeCorrect,nps_output)
#yeah, this seems to check out. The difference is smaller than I expected, but adequate.