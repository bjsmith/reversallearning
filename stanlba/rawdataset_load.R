rawdata_version = 1

rawdataset_cache_path<-paste0(localsettings$data.dir,"/rawdata_with_rois_version",rawdata_version,".RData")


cat("Loading datasets...")

if (file.exists(rawdataset_cache_path)){
  load(rawdataset_cache_path)
}else{
  rawdata <- data.table(read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment_amendment2.txt"), header=T))
  
  #now also load the neural data.
  #created by load_fs_rois.R
  freesurfer_roi_location<-paste0(dd,"freesurfer_rl/roi_event_data/")
  load(paste0(freesurfer_roi_location,"20180626T165755","data_allsubjs.RData"))
  #which columns are NOT ROIs?
  roi_cols<-!(colnames(roi_data) %in% c("image","rawcode","IsPlaceholder","Segment","Outcome",
                                        "ActivityPeriod","presentation_n_in_segment","centered",
                                        "first_reversal","presentation_n","subid",
                                        "runid","Motivation" ))
  roi_colnames<-paste0("ROI_",colnames(roi_data)[roi_cols])
  colnames(roi_data)[roi_cols]<-roi_colnames
  
  cat("datasets loaded.\n")
  
  cat("Merging neural and behavioral datasets...")
  
  rawdata.norois<-rawdata
  rawdata<-merge(rawdata,roi_data,by=c("subid","runid","Motivation","image","presentation_n"),all.x=TRUE,all.y=FALSE)
  
  
  #how much missing data and how much data do we have?
  #get count of NAs by ROI and run.
  rawdata.rois.long<-
    rawdata[,c(roi_colnames,"subid","runid","Motivation"),with=FALSE] %>%
    tidyr::gather("ROIName","ROIValue",roi_colnames) %>% data.table
  rawdata.NAVals<- rawdata.rois.long[,.(NAValues=sum(is.na(ROIValue))),.(subid,runid,Motivation,ROIName)]
  rawdata.ROIMissingRuns<-rawdata.NAVals[,.(MissingRunCount=sum(NAValues>0)),ROIName]
  rawdata.NACounts<-rawdata.NAVals %>% tidyr::spread(ROIName,NAValues)
  rawdata.RunMissingROICount<-rawdata.NAVals[,.(MissingROICount=sum(NAValues>0)),.(subid,runid,Motivation)]
  rm(rawdata.rois.long)
  rm(rawdata.NAVals)
  
  
  #eliminate altogether the runs that are missing most of the ROIs.
  for (ri in 1:dim(rawdata.RunMissingROICount)[1]){#ri<-1
    row_i<-rawdata.RunMissingROICount[ri,]
    if(row_i$MissingROICount>10){
      rawdata<-rawdata[!(subid==row_i$subid & runid==row_i$runid & row_i$Motivation==Motivation),]
    }
  }
  
  cat("...datasets merged.\n")
  
  missingruns<-data.frame(table(rawdata.ROIMissingRuns$MissingRunCount))
  for (mri in 1:dim(data.frame(missingruns))[1]){
    print(paste0(missingruns[mri,"Freq"], " ROIs are missing from ", missingruns[mri,"Var1"], " runs."))
  }
  
  stanfiledir<-"stanlba/stanfiles/"
  
  #exclude non-response trials.
  rawdata<-rawdata[choice %in% c(1,2),]
  
  save(rawdata,file=rawdataset_cache_path)
}
