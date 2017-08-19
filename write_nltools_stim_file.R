#source("rl_behav_analysis_learning_setup.R")
datetimestamp=format(Sys.time(), "%Y%m%dT%H%M%OS")
#files should ideally be one per nii.gz
#through subjects
for (sid in unique(rl.all.subjects.list$subid)){
  #sid<-107;m="reward";r=1
  #through motivation conditions
  for (m in unique(rl.all.subjects.list[subid==sid,Motivation])){
    #through runs
    for (r in unique(rl.all.subjects.list
                     [subid==sid & Motivation==m,runid])){
      
      run.ds<-rl.all.subjects.list[subid==sid & Motivation==m & 
                                   runid==r]
      runfiletext<-apply(run.ds[order(onset_time_actual)],1,function(r){
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
          #Stim,Onset,Duration
          #Stim,onset_time_actual,reaction_time
          #Stim should be one of:
          
          #Pres or Fdbk
        presentation_lines=""
        for (stimtype in c("Pres","Fdbk")){
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
          event_Stimcode<-paste0(
            condition_val,
            paste(
              c(
                revtrialcode,
                behaviorcode,
                stimtype,
                #paste0("i",r[["image"]]),#image
                r[["presentation_n_in_segment"]])#presentation_n_in_segment
              ,collapse="_"))
          presentation_lines=paste0(c(
            presentation_lines,
            paste(c(event_Stimcode,
                    itemtime,
                    itemduration)
                  ,collapse = ","),"\n"))
        }
        return(presentation_lines)
      })
      write( paste0(runfiletext,collapse=""),file=
              paste0(
        "../data/runfiles/",
        "runfile",
        datetimestamp,
        "_s",sid,"_",m,"_r",r,".txt"))
    }
  }
}