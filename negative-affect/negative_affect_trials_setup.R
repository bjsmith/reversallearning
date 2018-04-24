library(dplyr)
source("../util/apply_local_settings.R")
apply_local_settings()
dd<-localsettings$data.dir
rawdata <- read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment.txt"), header=T)
#do trials where the subject received punishment have a stronger NPS pain signal than trials wher they did not?

source("read_nps_output.R")
pain_data<-get_nps_data_for_subs(100:400)

# tmp$runmotiveid<-tmp$runid
# 
# tmp$runmotiveid[tmp$Motivation=="punishment"] <- 
#   tmp$runid[tmp$Motivation=="punishment"]+rew_runcount#this will increment the run motive ID by the number of reward runs.

names(pain_data)
names(rawdata)
pain_data$ResponseCorrect<-pain_data$Outcome=="correct"

pain_data$Motivation<-tolower(pain_data$Motivation)
rawdata<-data.table(merge(
  rawdata,pain_data,
  by.x=c("subid","presentation_n","image","runid","Motivation","first_reversal","presentation_n_in_segment"),
  by.y=c("subid","presentation_n","image","runid","Motivation","first_reversal","presentation_n_in_segment"),all.x=TRUE,all.y=FALSE))

#iterate through each run and add those values 
#order rawdata

rawdata.ordered<-rawdata[order(subid,runid,Motivation,onset_time_designed),]
rawdata.ordered$ValueScaled <- scale(rawdata.ordered$Value)

rawdata.ordered[,runmotiveid:=runid]#start this; it'll be more fully defined below, see below.
for (s in unique(rawdata.ordered$subid)){
  print(s)
  #get the reward run count so that we can give each run, rew and pun, unique IDs
  rew_runcount <- length(unique(rawdata.ordered[subid==s & Motivation=="reward",runid]))
  pun_runcount <- length(unique(rawdata.ordered[subid==s & Motivation=="punishment",runid]))
  
  rawdata.ordered[subid==s & Motivation=="punishment",runmotiveid:=runmotiveid+rew_runcount]
  for (r in unique(rawdata.ordered$runid)){
    for (m in unique(rawdata.ordered$Motivation)){#s<-105;r=1;m=unique(rawdata.ordered$Motivation)[1]
      if (dim(rawdata.ordered[subid==s & runid==r & Motivation==m])[1]>0){
        
        #print(paste(s,r,m))
        preceeding_vals_1<-rawdata.ordered[subid==s & runid==r & Motivation==m] %>% 
          .[1:(dim(.)[1]-1),Value]
        preceeding_vals<-c(mean(preceeding_vals_1),preceeding_vals_1)
        rawdata.ordered[subid==s & runid==r & Motivation==m,PreviousValue:=preceeding_vals]
        preceeding_Outcome_1<-rawdata.ordered[subid==s & runid==r & Motivation==m] %>%
          .[1:(dim(.)[1]-1),Outcome]
        preceeding_Correct<-c(TRUE,preceeding_Outcome_1=="correct")
        rawdata.ordered[subid==s & runid==r & Motivation==m,PreviousCorrect:=preceeding_Correct]
        preceeding_vals_1s<-rawdata.ordered[subid==s & runid==r & Motivation==m] %>% 
          .[1:(dim(.)[1]-1),ValueScaled]
        preceeding_valss<-c(mean(preceeding_vals_1s),preceeding_vals_1s)
        rawdata.ordered[subid==s & runid==r & Motivation==m,PreviousValueScaled:=preceeding_valss]
        }
    }
  }
}

rawdata.ordered$PreviousValueScaled <- scale(rawdata.ordered$PreviousValue)


#table(rawdata.ordered.complete$PreviousCorrect)
#table(rawdata.ordered.complete$PreviousValue)
rawdata.ordered.complete<-rawdata.ordered[
  !is.na(PreviousValue)&!is.na(PreviousCorrect)
  ]
pain_data<-rawdata.ordered.complete
