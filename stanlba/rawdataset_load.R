rawdata_version = 7

rawdataset_cache_path<-paste0(localsettings$data.dir,"/rawdata_with_rois_version",rawdata_version,".RData")


cat("Loading datasets...")

if (file.exists(rawdataset_cache_path)){
  load(rawdataset_cache_path)
}else{
  rawdata <- data.table(read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment_amendment2.txt"), header=T))
  cat("...main dataset loaded;loading freesurfer data...")
  
  #load regressors.
  
  
  load_and_merge_neural_dataset<-function(roi_locationpath,rawdata,colname_prefix="ROI_"){
    #roi_locationpath<-paste0(dd,"freesurfer_rl/roi_event_data/","20180722T232052","data_allsubjs.RData");
    #now also load the neural data.
    #created by load_fs_rois.R
    load(roi_locationpath)
    #which columns are NOT ROIs?
    roi_cols<-!(colnames(roi_data) %in% c("image","rawcode","IsPlaceholder","Segment","Outcome",
                                          "ActivityPeriod","presentation_n_in_segment","centered",
                                          "first_reversal","presentation_n","subid",
                                          "runid","Motivation" ))
    roi_colnames<-paste0(colname_prefix,colnames(roi_data)[roi_cols])
    colnames(roi_data)[roi_cols]<-roi_colnames
    cat("...dataset loaded...")
    
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
    
    missingruns<-data.frame(table(rawdata.ROIMissingRuns$MissingRunCount))
    for (mri in 1:dim(data.frame(missingruns))[1]){
      print(paste0(missingruns[mri,"Freq"], " ROIs are missing from ", missingruns[mri,"Var1"], " runs."))
    }
    
    return(rawdata);
  }
  
  rawdata<-load_and_merge_neural_dataset(paste0(dd,"freesurfer_rl/roi_event_data/","20180722T232052","data_allsubjs.RData"),rawdata)
  fs_rois<-colnames(rawdata)[grep(pattern = "ROI_",colnames(rawdata))]
  #regress out ventricles, white matter, and CSF
  colnames(rawdata)
  lapply(c("ROI_Left.Cerebral.White.Matter","ROI_Right.Cerebral.White.Matter",
            "ROI_Left.Lateral.Ventricle","ROI_Right.Lateral.Ventricle",
            "ROI_X3rd.Ventricle","ROI_X4th.Ventricle","ROI_X5th.Ventricle","ROI_CSF"),function(roi){sum(is.na(rawdata[,roi,with=FALSE]))})
  
  fs_control_rois<-c("ROI_Left.Cerebral.White.Matter","ROI_Right.Cerebral.White.Matter",
                     "ROI_Left.Lateral.Ventricle","ROI_Right.Lateral.Ventricle",
                     "ROI_X3rd.Ventricle","ROI_X4th.Ventricle","ROI_CSF")
  cat("regressing out WM, ventricles, and CSF from freesurfer ROIs...")
  regress_out_freesurfer_wm.csf.ventricles<-function(rawdata,rois_to_regress){
    for (roi_name in rois_to_regress){
      if (!(roi_name %in% fs_control_rois)){
        modelres<-lm(as.formula(paste(roi_name,"~",paste(fs_control_rois,collapse= " + "))),rawdata)
        valid_rows<-!is.na(rawdata[,roi_name,with=FALSE])[,1]
        new_col_name<-paste0("con_",roi_name)
        rawdata[valid_rows,eval(new_col_name):=modelres$residuals]
        cat(". ")
        #plot(rawdata[,eval(new_col_name),with=FALSE][[1]],rawdata[,eval(roi_name),with=FALSE][[1]])
        
      }
    }
    return(rawdata)
  }
  rawdata<-regress_out_freesurfer_wm.csf.ventricles(rawdata,fs_rois)
  cat("done.\n")
  
  
  cat("...datasets merged...loading fsl_harvardoxford_dataset...")
  rawdata<-load_and_merge_neural_dataset(paste0(dd,"roi_event_data/fsl_roi_event_data/","20180722T224831","data_allsubjs.RData"),rawdata,
                                         colname_prefix = "fsl_")
  fsl_rois<-colnames(rawdata)[grep(pattern = "fsl_",colnames(rawdata))]
  rawdata<-regress_out_freesurfer_wm.csf.ventricles(rawdata,fsl_rois)
  
  
  cat("...datasets merged...loading and merging nltools_harvardoxford_dataset...")
  source("neural_data/load_nltools_harvardoxford_rois.R")
  
  roi_all_dt$Motivation<-tolower(roi_all_dt$Motivation)
  #what exactly do we need to do to merge here?
  intersectcols<-intersect(colnames(roi_all_dt),colnames(rawdata))
  
  rawdata<-merge(rawdata,roi_all_dt,by=intersect(colnames(roi_all_dt),colnames(rawdata)), all.x=TRUE,all.y=FALSE)
  
  nltools_rois<-colnames(rawdata)[grep(pattern = "nltools_",colnames(rawdata))]
  rawdata<-regress_out_freesurfer_wm.csf.ventricles(rawdata,nltools_rois)
  
  
  cat("datasets loaded.\n")
  
  
  
  
  
  stanfiledir<-"stanlba/stanfiles/"
  
  #exclude non-response trials.
  rawdata<-rawdata[choice %in% c(1,2),]
  
  save(rawdata,file=rawdataset_cache_path)
}
