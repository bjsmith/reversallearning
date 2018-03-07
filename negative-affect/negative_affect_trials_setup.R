source("util/apply_local_settings.R")
apply_local_settings()
dd<-localsettings$data.dir
rawdata <- read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment.txt"), header=T)
#do trials where the subject received punishment have a stronger NPS pain signal than trials wher they did not?

source("read_nps_output.R")
pain_data<-get_nps_data_for_subs(100:400)
pain_data$Motivation="punishment"

names(pain_data)
names(rawdata)
pain_data$ResponseCorrect<-pain_data$Outcome=="correct"
rawdata<-data.table(merge(
  rawdata,pain_data,
  by.x=c("subid","presentation_n","image","runid","Motivation","first_reversal","presentation_n_in_segment"),
  by.y=c("subid","presentation_n","image","runid","Motivation","first_reversal","presentation_n_in_segment"),all.x=TRUE,all.y=FALSE))

pain_data$ValueScaled <- scale(pain_data$Value)

#iterate through each run and add those values 
#order rawdata
rawdata.ordered<-rawdata[order(subid,runid,Motivation,onset_time_designed),]
for (s in unique(rawdata.ordered$subid)){
  for (r in unique(rawdata.ordered$runid)){#s<-105;r=1;
    print(s)
    if (dim(rawdata.ordered[subid==s & runid==r & Motivation=="punishment"])[1]>0){
      preceeding_vals_1<-rawdata.ordered[subid==s & runid==r & Motivation=="punishment"] %>% 
        .[1:(dim(.)[1]-1),Value]
      preceeding_vals<-c(mean(preceeding_vals_1),preceeding_vals_1)
      rawdata.ordered[subid==s & runid==r & Motivation=="punishment",PreviousValue:=preceeding_vals]
      
      preceeding_Outcome_1<-rawdata.ordered[subid==s & runid==r & Motivation=="punishment"] %>%
        .[1:(dim(.)[1]-1),Outcome]
      preceeding_Correct<-c(TRUE,preceeding_Outcome_1=="correct")
      rawdata.ordered[subid==s & runid==r & Motivation=="punishment",PreviousCorrect:=preceeding_Correct]
      
    }
  }
}

rawdata.ordered.complete<-rawdata.ordered[
  !is.na(PreviousValue)&!is.na(PreviousCorrect) & Motivation=="punishment"
  ]