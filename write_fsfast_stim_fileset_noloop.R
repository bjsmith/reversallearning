library(dplyr)
source("util/apply_local_settings.R")
apply_local_settings()

write_fsfast_stim_fileset<- function(actionlist,filePrefix="",datetimestamp=format(Sys.time(), "%Y%m%dT%H%M%OS"),
                                      combinePresentationFeedback=FALSE,
                                      combineAcrossImages=FALSE,
                                      combineAcrossSections=FALSE,
                                      distinguishCorrectAndIncorrect=TRUE,
                                      minTrialTime=0){

  if(is.null(datetimestamp)){
    datetimestamp=format(Sys.time(), "%Y%m%dT%H%M%OS")
  }

  actionlist <- rl.all.subjects.list
  
      #right, we have the right scope.
      #let's record PRESENTATION and FEEDBACK 
      #stim_dur=1; (or as long as the user took to respond, whichever is less)
      #feedback_dur=0.7;
      #exclude:
      #"PreR" or "PostR" according to reversal_trial
  #actionlist=rl.all.subjects.list
  table(actionlist$reversal_trial)#NA reversal trials are "control" trials.
  table(is.na(actionlist$reversal_trial))
  table(is.na(actionlist$reversal_trial_trimws))
  table(actionlist$reversal_trial_trimws)
  actionlist[,reversal_trial_trimws:=trimws(reversal_trial)]
  actionlist[Condition==1 & reversal_trial_trimws==FALSE,revtrialcode:="PreR"]
  actionlist[Condition==1 & reversal_trial_trimws==TRUE,revtrialcode:="PostR"]
  actionlist[Condition==2,revtrialcode:="PreR"]
  table(actionlist[,.(Condition)])
  table(actionlist[,.(Condition,reversal_trial_trimws)])
  table(actionlist[,.(Condition,reversal_trial_trimws)])

  actionlist[,score_trimws:=trimws(score)]
  if(distinguishCorrectAndIncorrect){
    actionlist[score_trimws==1,behaviorcode:="correct"]
    actionlist[score_trimws==-1,behaviorcode:="error"]
  }else{
    actionlist[score_trimws==1,behaviorcode:="validresponse"]
    actionlist[score_trimws==-1,behaviorcode:="validresponse"]
  }

  actionlist[score_trimws==0,behaviorcode:="nonresponse"]
  table(actionlist[,.(score_trimws,score,behaviorcode)])
      #correct OR error OR nonresponse (according to score)
      # behaviorcode<-""
      # score<-trimws(r[["score"]])
      # if (score==1){
      #   behaviorcode<-"correct"
      # }else if (score==-1){
      #   behaviorcode<-"error"
      # }else if (score==0){
      #   behaviorcode<-"nonresponse"
      # }else{
      #   print(score)
      #   stop("unknown score")
      # }
      
      #....
      # Codes Stimulus Schedule (and Weight)
      # Four Columns
      # Onset Time (Since Acq of 1st Saved Volume)
      # Stimulus Code (0, 1, 2 ,3 …)
      # Stimulus Duration 
      # Stimulus Weight (default is 1)
      # Any other columns ignored
      # Simple Text File
      # Code 0 Always Fixation/NULL
      # Weight for parametric modulation
      
      #conditions should be:
      #0=null
      # we actually in this, need a condition individually for every single separate event. 
      # so all of the unique codes I have already coded will be used.
      # I think we can just use them verbatim, as they are.
      #columns:
      # - onset time - we already have
      # - stimulus code
      # - Stimulus duration
      # - Stimulus weight
      
  
      
      #Pres or Fdbk
      # presentation_lines=""
  
  actionlist[is.na(score),PresentationDuration:=1]    
  actionlist[!is.na(score),PresentationDuration:=reaction_time]
  
  #under what conditions are these separated into presentation and feedback?
  #pretty much all conditions?    
  if (combinePresentationFeedback==FALSE){
    actionlist<-rbind(actionlist[,stimtype:="Pres"],actionlist[,stimtype:="Fdbk"])
    #presentation trial duration
    
    actionlist[stimtype=="Pres",itemduration:=PresentationDuration-0.010]    #for some reason?
    actionlist[stimtype=="Pres",itemtime:=as.numeric(onset_time_actual)]    
    actionlist[stimtype=="Fdbk",itemduration:=0.7]
    actionlist[stimtype=="Fdbk",itemtime:=as.numeric(onset_time_actual)+PresentationDuration]
    
  }else{
    actionlist<-actionlist[,stimtype:="PsFb"]
    actionlist[,itemduration:=PresentationDuration+0.7]
    actionlist[,itemtime:=as.numeric(onset_time_actual)]
  }
  
  
  actionlist[,condition_val:=""]
  actionlist[Condition==2,condition_val:="placeholder_"]
  if (combineAcrossImages){
    actionlist[,segmentNum:="alli"]
  }else{
    actionlist[,segmentNum:=presentation_n_in_segment]
  }
  
  if(combineAcrossSections){
    actionlist[,revtrialcode:="AllS"]
  }
  
  hist(actionlist$itemduration)
  
  discarded_vals<-actionlist[as.numeric(itemduration)<0.05]
  actionlist<-actionlist[as.numeric(itemduration)>=0.05]
  
  actionlist[,onset_str:=trimws(as.character(itemtime))]
  actionlist[,itemduration_str:=as.character(max(minTrialTime,as.numeric(itemduration)))]

    actionlist[Condition==2,condition_val:="placeholder_"]
  
    actionlist[,event_Stimcode:=paste0(
    condition_val,
    paste(
      #c(
        revtrialcode,
        behaviorcode,
        stimtype,
        paste0("i",image),#image
        segmentNum#)
      ,sep="_"))]
  actionlist[,weight:=1]
  
  
  
}


res<-write_fsfast_stim_fileset(rl.all.subjects.list,"/testing",distinguishCorrectAndIncorrect=FALSE)

View(table(res[ ,.(event_Stimcode,Motivation)]))

