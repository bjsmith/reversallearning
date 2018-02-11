write_fsfast_stim_fileset<- function(actionlist,filePrefix="",datetimestamp=format(Sys.time(), "%Y%m%dT%H%M%OS"),
                                      combinePresentationFeedback=FALSE,
                                      combineAcrossImages=FALSE,
                                      combineAcrossSections=FALSE,
                                      minTrialTime=0){
  #source("rl_behav_analysis_learning_setup.R")
  if(is.null(datetimestamp)){
    datetimestamp=format(Sys.time(), "%Y%m%dT%H%M%OS")
  }
  
  for (sid in unique(actionlist$subid)){
    #sid<-107;m="reward";r=1
    #through motivation conditions
    for (m in unique(actionlist[subid==sid,Motivation])){
      #through runs
      for (r in unique(actionlist
                       [subid==sid & Motivation==m,runid])){
        
        run.ds<-actionlist[subid==sid & Motivation==m & 
                                       runid==r]

        applyval<-unlist(apply(run.ds[order(onset_time_actual)],1,function(r){
          sid=r[["subid"]]
          m=r[["Motivation"]]
          #right, we have the right scope.
          #let's record PRESENTATION and FEEDBACK 
          #stim_dur=1; (or as long as the user took to respond, whichever is less)
          #feedback_dur=0.7;
          #exclude:
          #"PreR" or "PostR" according to reversal_trial
          revtrialcode<-""
          if(r[["Condition"]]==1){
            if (trimws(r[["reversal_trial"]])=="FALSE"){
              revtrialcode<-"PreR"
            }else if (trimws(r[["reversal_trial"]])=="TRUE"){
              revtrialcode<-"PostR"
            }else{
              stop("unrecognized reverasl code")
            }
          }else if (r[["Condition"]]==2){
            revtrialcode<-"PreR"
          }else{
            stop("unknown condition, f-off.")
          }
          
          #correct OR error OR nonresponse (according to score)
          behaviorcode<-""
          score<-trimws(r[["score"]])
          if (score==1){
            behaviorcode<-"correct"
          }else if (score==-1){
            behaviorcode<-"error"
          }else if (score==0){
            behaviorcode<-"nonresponse"
          }else{
            print(score)
            stop("unknown score")
          }
          
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
      }
    }
  }
}
