library(dplyr)
library(data.table)
source("util/apply_local_settings.R")
apply_local_settings()
#source("rl_behav_analysis_learning_setup.R")


write_fsfast_stim_fileset<- function(actionlist,filePrefix="",datetimestamp=format(Sys.time(), "%Y%m%dT%H%M%OS"),
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
          
<<<<<<< HEAD
          #Pres or Fdbk
          presentation_lines=""
          stimtypes=c("Pres","Fdbk")
          if(combinePresentationFeedback){
            stimtypes<-c("PsFb")
          }
          for (stimtype in stimtypes){
            #need to set the time.
            item_duration<-""
            if(stimtype=="Pres"){
              if (is.na(r[["score"]])){
                itemduration=1
              }else {
                itemduration=r[["reaction_time"]]
              }
              presduration=itemduration#counter so that we append the duration for this item for the next feedback.
              itemtime=r[["onset_time_actual"]]
              itemduration=as.numeric(itemduration)-0.010
            }else if(stimtype=="PsFb"){
              if (is.na(r[["score"]])){
                itemduration=1
              }else {
                itemduration=r[["reaction_time"]]
              }
              itemduration=as.numeric(itemduration)+0.7
              presduration=itemduration#counter so that we append the duration for this item for the next feedback.
              itemtime=r[["onset_time_actual"]]
            }else if (stimtype=="Fdbk"){
              itemduration="0.7"
              itemtime=as.numeric(r[["onset_time_actual"]])+as.numeric(presduration)
            }else{
              stop("unknown stimtype")
            }
            condition_val=""
            if (r[["Condition"]]==2){#mark off Condition==2
              condition_val="placeholder_"
            }
            segmentNum<-r[["presentation_n_in_segment"]]#presentation_n_in_segment
            if(combineAcrossImages){
              segmentNum<-"allimg"
            }
            if(combineAcrossImages){
              segmentNum<-"alli"
            }
            if(combineAcrossSections){
              revtrialcode<-"AllS"
            }
            if(as.numeric(itemduration)<0.05){
              warning(paste0(" a presentation",sid," trial was removed because it wasn't shown."))
              next
            }
            itemtime_str<-trimws(as.character(itemtime))
            itemduration_str<-trimws(as.character(max(minTrialTime,as.numeric(itemduration))))
            event_Stimcode<-paste0(
              condition_val,
              paste(
                c(
                  revtrialcode,
                  behaviorcode,
                  stimtype,
                  paste0("i",r[["image"]]),#image
                  segmentNum)
                ,collapse="_")) 
            #OK. Do we actually need to sync these up, if we're just trying to grab a unique value for each timepoint? Possibly not.
            #If I was doing a group-level analysis or even combining runs, I'd need to make sure stimcodes match across runs.
            #But because I'm not, I think we can get our numeric stimcode simply by doing an as.factor over the written stimcode.
            #should be all we need!
            presentation_lines=paste0(c(
              presentation_lines,
              paste0(c(itemtime_str,
                       event_Stimcode,#these need to be changed to be numeric. But in order to do that, I need to get a list of ALL in EVERY file so that the stimulus codes are coded the same for all subjects.
                       #do we code NULL times? I'm not sure.
                       itemduration_str,
                       "1",event_Stimcode)
                     ,collapse = "\t"),"\n"))
          }
          return(presentation_lines)
        }))
        runfiletext<-paste0(
          c(#"Stim,Onset,Duration\n",#no header in in fsfast
            applyval))
        writeoutput<-paste0(runfiletext,collapse="")
        #we want one file per session.
        write(writeoutput ,file=
                 paste0(localsettings$data.dir,
                   "runfiles/",
                   "fsfast_paradigmfile",filePrefix,
                   datetimestamp,
                   "_s",sid,"_",m,"_r",r,".txt"))
=======
          write.table(eventlist.stimfilecols,file = fname,sep=",",
                      quote=FALSE,row.names=FALSE,col.names=FALSE)
        },error= function(e){
          print(eventlist_srm)
          stop(e)
        })
>>>>>>> 2da32e838176d05853f3fa0e0762d651f15ee8ef
      }
    }
  }
  
}


