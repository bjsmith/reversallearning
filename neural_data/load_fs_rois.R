library(dplyr)
library(data.table)
library(tidyr)

#rm(list=ls())

#right, this time, we need to get some real data.
source("../util/apply_local_settings.R")
source("neural_data/read_roi_file.R")
apply_local_settings("")
dd<-localsettings$data.dir
data_location<-paste0(dd,"freesurfer_rl/roi_event_data/")
list.files(data_location)
#these files are listed one per run.
#iterate through files.
#runfilepath<-paste0(data_location,"20180619T103032sub149_reward_r2_beta_vals.csv")
#runfile<-read.csv(runfilepath)
#dim(runfile)
#each of these can be matched to specific trials. 
#but how?
#runfile$EventName
#need to reference back to how these names were created.
#we already integrated negative affect trials right? how'd that go?



#read_roi_output(pattern_csv = read.csv(runfilepath,header = TRUE,row.names = "EventName"),subid = 149,runid = "N")



get_freesurfer_data_for_subs <- function(subjList){#subjList<-c(105:106)
  data_filepathprefix<-"freesurfer_rl/roi_event_data/20180626T165755sub"
  data_filepathsuffix<-"_beta_vals.csv"
  pattern_data_allsubs<-NULL
  for (s in subjList) {#s<-106
    for (r in 1:2){#r<-1;m="Punishment"
      for (m in c("punishment","reward")){
        #print (paste("getting pain data for subject ",s, "run",r))
        #input and parse CSV
        pr<-paste0(localsettings$data.dir,data_filepathprefix,s,"_",m,"_r",r,data_filepathsuffix)
        if (file.exists(pr)){
          pattern_data<-read_roi_output(read.csv(file=pr,header = TRUE,row.names = "EventName"))#,subid=s,runid=r)
          pattern_data$subid<-s
          pattern_data$runid<-r
          pattern_data$Motivation<-m
          if(any(s!=pattern_data$subid) | any(r!=pattern_data$runid)){
            stop("mismatch between filename and file contents subject id or runid")
          }
          cat(".")
        }else{
          #print(paste("Run",r,"M",m,", data not available for subject ",s))
          next
        }
        if (is.null(pattern_data_allsubs)){
          pattern_data_allsubs<-pattern_data
        }else{
          pattern_data_allsubs<-rbind(pattern_data_allsubs,pattern_data,fill=TRUE)
        }
      }
    }
  }
  print("...done.")
  #normalize this data.
  #pattern_data_allsubs$Value_n<-scale(pattern_data_allsubs$Value,center = 0,scale=TRUE)
  
  return (pattern_data_allsubs)
}

#roi_data<-get_freesurfer_data_for_subs(subjList = c(105:396))
roi_data<-get_freesurfer_data_for_subs(subjList = c(105:399))
for (missing_region in names(which(colSums(is.na(roi_data))>0))){
  print(missing_region)
  print(roi_data[is.na(eval(parse(text=missing_region))),.(.N),.(subid,runid,Motivation)])
}

#why is some output missing the cerebral cortex?
#which subjects?
#should consider excluding subjects with values missing...
save(roi_data,file=paste0(data_location,"20180626T165755","data_allsubjs.RData"))