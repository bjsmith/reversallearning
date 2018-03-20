library(dplyr)
library(data.table)
source("util/apply_local_settings.R")
apply_local_settings()
#source("rl_behav_analysis_learning_setup.R")


write_fsfast_stim_fileset<- function(actionlist,filePrefix="",
                                     datetimestamp=format(Sys.time(), "%Y%m%dT%H%M%OS"),
                                     combinePresentationFeedback=FALSE,
                                     combineAcrossImages=FALSE,
                                     combineAcrossSections=FALSE,
                                     distinguishCorrectAndIncorrect=TRUE,
                                     minTrialTime=0){
  
  if(is.null(datetimestamp)){
    datetimestamp=format(Sys.time(), "%Y%m%dT%H%M%OS")
  }
  
  #actionlist <- rl.all.subjects.list
  #combinePresentationFeedback=FALSE;combineAcrossImages=FALSE;combineAcrossSections=FALSE;distinguishCorrectAndIncorrect=FALSE;minTrialTime=0
  
  #right, we have the right scope.
  #let's record PRESENTATION and FEEDBACK 
  #stim_dur=1; (or as long as the user took to respond, whichever is less)
  #feedback_dur=0.7;
  #exclude:
  #"PreR" or "PostR" according to reversal_trial
  #actionlist=rl.all.subjects.list
  # table(actionlist$reversal_trial)#NA reversal trials are "control" trials.
  # table(is.na(actionlist$reversal_trial))
  # table(is.na(actionlist$reversal_trial_trimws))
  # table(actionlist$reversal_trial_trimws)
  actionlist[,reversal_trial_trimws:=trimws(reversal_trial)]
  actionlist[Condition==1 & reversal_trial_trimws==FALSE,revtrialcode:="PreR"]
  actionlist[Condition==1 & reversal_trial_trimws==TRUE,revtrialcode:="PostR"]
  actionlist[Condition==2,revtrialcode:="PreR"]
  # table(actionlist[,.(Condition)])
  # table(actionlist[,.(Condition,reversal_trial_trimws)])
  # table(actionlist[,.(Condition,reversal_trial_trimws)])
  
  #correct OR error OR nonresponse (according to score)
  
  actionlist[,score_trimws:=trimws(score)]
  if(distinguishCorrectAndIncorrect){
    actionlist[score_trimws==1,behaviorcode:="correct"]
    actionlist[score_trimws==-1,behaviorcode:="error"]
  }else{
    actionlist[score_trimws==1,behaviorcode:="resp"]
    actionlist[score_trimws==-1,behaviorcode:="resp"]
  }
  
  actionlist[score_trimws==0,behaviorcode:="nonresp"]
  table(actionlist[,.(score_trimws,score,behaviorcode)])
  
  
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
    #eventlist<-rbind(actionlist[,stimtype:="Pres"],actionlist[,stimtype:="Fdbk"])
    
    # preslist <- actionlist
    # preslist$stimtype<-"Pres"
    # fdbklist<-actionlist
    # fdbklist$stimtype<-"fdbk"
    # eventlist<-rbind(preslist,fdbklist)
    
    actionlist$StimType1<-"Pres"
    actionlist$StimType2<-"Fdbk"
    eventlist<-data.table(tidyr::gather(actionlist,"stimtypekey","stimtypevalue",StimType1:StimType2))
    #eventlist<-melt(actionlist,variable.name = "stimtypekey", value.name = "stimtypevalue",StimType1:StimType2))
    #presentation trial duration
    
    eventlist[stimtypevalue=="Pres",itemduration:=PresentationDuration-0.010]    #for some reason?
    eventlist[stimtypevalue=="Pres",itemtime:=as.numeric(onset_time_actual)]    
    eventlist[stimtypevalue=="Fdbk",itemduration:=0.7]
    eventlist[stimtypevalue=="Fdbk",itemtime:=as.numeric(onset_time_actual)+PresentationDuration]
    
  }else{
    eventlist<-eventlist[,stimtypevalue:="PsFb"]
    eventlist[,itemduration:=PresentationDuration+0.7]
    eventlist[,itemtime:=as.numeric(onset_time_actual)]
  }
  
  
  eventlist[,condition_val:=""]
  eventlist[Condition==2,condition_val:="pl_"]
  if (combineAcrossImages){
    eventlist[,segmentNum:="alli"]
  }else{
    eventlist[,segmentNum:=presentation_n_in_segment]
  }
  
  if(combineAcrossSections){
    eventlist[,revtrialcode:="AllS"]
  }
  
  
  
  discarded_vals<-eventlist[as.numeric(itemduration)<0.05]
  eventlist<-eventlist[as.numeric(itemduration)>=0.05]
  
  
  eventlist[,itemduration_str:=as.character(max(minTrialTime,as.numeric(itemduration)))]
  
  eventlist[Condition==2,condition_val:="pl_"]
  
  eventlist[,event_Stimcode:=paste0(
    condition_val,
    paste(
      #c(
      revtrialcode,
      stimtypevalue,
      paste0("i",image),#image
      segmentNum,
      behaviorcode#)
      ,sep="_"))]
  eventlist[,weight:=1]
  eventlist[,event_StimID:=as.integer(as.factor(event_Stimcode))]
  eventlist<-eventlist[order(itemtime)]
  
  
  #need to loop through and now separate into subject,run,Motivation files.
  for (s in unique(eventlist$subid)){
    for (r in unique(eventlist$runid)){
      for (m in unique(eventlist$Motivation)){
        
        outputdata<-tryCatch({
          #s=108;r=1;m="reward"
          eventlist_srm<-eventlist[subid==s & runid==r & Motivation==m]
          if(dim(eventlist_srm)[1]==0){
            print(paste0("no events found for subid=", s, "; r=",r,"; m=",m,"; skipping this run."))
            break
          }
          #need to insert gaps in between all the events.
          #here's how we'll do it: add the duration onset time of each item; 
          #if that is more than 0.1 seconds before the onset of the next item, then create a gapfill item
          eventlist_srm[,itemtimeend:=itemtime+itemduration]
          eventlist_srm$delay_since_last<-c(eventlist_srm$itemtime[1],
                                            eventlist_srm$itemtime[2:dim(eventlist_srm)[1]]-eventlist_srm$itemtimeend[1:(dim(eventlist_srm)[1]-1)])
          eventlist_srm_gaps<-eventlist_srm[delay_since_last>0.1]
          eventlist_srm_gaps$event_Stimcode="gap"
          eventlist_srm_gaps$event_StimID=0
          eventlist_srm_gaps$itemtime<-eventlist_srm_gaps$itemtime- eventlist_srm_gaps$delay_since_last
          eventlist_srm_gaps$itemduration<-eventlist_srm_gaps$delay_since_last
          eventlist_srm_all<-rbind(eventlist_srm,eventlist_srm_gaps) %>% .[order(itemtime)]
          
          eventlist_srm_all[,onset_str:=trimws(as.character(round(itemtime,2)))]
          eventlist_srm_all[,itemduration_str:=trimws(as.character(floor(itemduration*100)/100))]
          #so first 
          #View(eventlist_srm_all[,.(onset_str,event_StimID,itemduration,weight,itemtime,event_Stimcode,stimtypevalue,delay_since_last)])
          #eventlist.stimfilecols<-eventlist_srm_all[,.(onset_str,event_StimID,itemduration,weight,itemtime,event_Stimcode,stimtypevalue)]
          eventlist.stimfilecols<-eventlist_srm_all[,.(onset_str,event_StimID,itemduration_str,weight,event_Stimcode)]
          
          fname=paste0(localsettings$data.dir,"runfiles/fsfast_paradigmfile/",
                       filePrefix,"runfile",
                       datetimestamp,
                       "_s",s,"_",m,"_r",r,".txt")
          print(paste0("writing ",fname))
          
          write.table(eventlist.stimfilecols,file = fname,sep=",",
                      quote=FALSE,row.names=FALSE,col.names=FALSE)
        },error= function(e){
          print(eventlist_srm)
          stop(e)
        })
      }
    }
  }
  
}


