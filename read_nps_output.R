library(data.table)
read_nps_output <- function(pattern_csv){
  #pattern_csv<-punish_r1_csv
  activity<-data.table(t(pattern_csv),keep.rownames = TRUE,stringsAsFactors = FALSE)
  activity<-plyr::rename(activity,replace = c(rn="rawcode", V1="Value"))
  #View(activity)
  #these should be centered ALREADY, but we should scale them to have a tractable value
  #so, columns we can pull out of this are:
  activity[,IsPlaceholder:=startsWith(activity$rawcode,"placeholder_")]
  #activity[,rawcode2:=sub("placeholder_","",rawcode)]
  #before we use this we'll have to take out the misc values.
  misc_value_headers<-c("subid", "runid","ones","linearterm","cubicterm","quadraticterm")
  misc_values<-activity[rawcode %in% misc_value_headers]
  
  activity.eventvals<-activity[!(rawcode %in% misc_value_headers)]#no placeholder
  activity.eventvals[,c("Segment", "Outcome","ActivityPeriod","image","presentation_n_in_segment","centered"):=
             tstrsplit(sub("placeholder_","",rawcode),"_",fixed=TRUE)]
  activity.eventvals$image<-sapply(activity.eventvals$image,function(x){as.integer(sub("i","",x))})
  activity.eventvals<- merge(
    activity.eventvals,
    activity.eventvals[Segment=="PreR",.(first_reversal=as.integer(max(presentation_n_in_segment))+1),image],
    by="image")
  activity.eventvals$presentation_n_in_segment<- as.integer(activity.eventvals$presentation_n_in_segment)
  activity.eventvals[,presentation_n:=(presentation_n_in_segment+as.integer(Segment=="PostR")*(first_reversal-1))]
  activity.eventvals[,presentation_n_in_segment]
  
  # print(mean(activity.eventvals$Value))
  # if(abs(mean(activity.eventvals$Value))>0.1*sd(activity.eventvals$Value)){
  #   stop("for some reason, activity is not centered. The regression process should have centered it, but it hasn't been, for some reason.")
  # }
  # activity.eventvals$Value <-activity.eventvals$Value/sd(activity.eventvals$Value)#scale(activity$Value,center=FALSE,scale=TRUE)
  
  #activity.eventvals<-cbind(activity.eventvals,misc_values[,.(subid,runid)])
  activity.eventvals$subid=as.integer(misc_values[rawcode=="subid",Value])
  activity.eventvals$runid=as.integer(misc_values[rawcode=="runid",Value])
  
  return(activity.eventvals)
}

get_nps_data_for_subs <- function(subjList,nps_version=NA){
  pattern_data_allsubs<-NULL
  for (s in subjList) {
    for (r in 1:2){
      for (m in c("Punishment","Reward")){
        #print (paste("getting pain data for subject ",s, "run",r))
        #input and parse CSV
        if(is.na(nps_version)){pr<-paste0(localsettings$data.dir,"rlPainNPS/",s,"_",tolower(m),"_r",r,".csv")
        }else{pr<-paste0(localsettings$data.dir,"rlPainNPS/",nps_version,"/",s,"_",tolower(m),"_r",r,".csv")}
        if (file.exists(pr)){
          pattern_data<-read_nps_output(read.csv(file=pr))
          pattern_data$Motivation<-m
          if(any(s!=pattern_data$subid) | any(r!=pattern_data$runid)){
            stop("mismatch between filename and file contents subject id or runid")
          }
        }else{
          print(paste("Run",r,"M",m,", data not available for subject ",s))
          next
        }
        if (is.null(pattern_data_allsubs)){
          pattern_data_allsubs<-pattern_data
        }else{
          pattern_data_allsubs<-rbind(pattern_data_allsubs,pattern_data)
        }
      }
    }
  }
  #normalize this data.
  #pattern_data_allsubs$Value_n<-scale(pattern_data_allsubs$Value,center = 0,scale=TRUE)
  
  return (pattern_data_allsubs)
}