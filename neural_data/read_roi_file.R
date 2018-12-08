#adapted from read_nps_output.R
#there are a few changes from that file, so don't try to treat them as interchangeable.
#generalized to take more than one ROI; doesn't expect to have run information in the file itself.
library(data.table)
read_roi_output <- function(pattern_csv){
  #pattern_csv<-punish_r1_csv
  activity<-data.table(pattern_csv,keep.rownames = TRUE,stringsAsFactors = FALSE)
  activity<-plyr::rename(activity,replace = c(rn="rawcode"))
  
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
  # activity.eventvals$subid=subid#as.integer(misc_values[rawcode=="subid",Value])
  # activity.eventvals$runid=runid#as.integer(misc_values[rawcode=="runid",Value])
  # activity.eventvals$motivation=motivation
  # 
  return(activity.eventvals)
}
