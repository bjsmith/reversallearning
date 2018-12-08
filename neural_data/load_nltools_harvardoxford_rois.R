library(dplyr)
source("../util/apply_local_settings.R")
apply_local_settings()
dd<-localsettings$data.dir

nltools_harvardoxford_version<-1
nps_version<-"20180722_version"
nltools_harvardoxford_repo<-paste0(dd,"roi_event_data/fsl_roi_event_data_nltools/",nps_version,"_",nltools_harvardoxford_version,".RData")
if(file.exists(nltools_harvardoxford_repo)){
  load(nltools_harvardoxford_repo)
  }else{
  source("read_nps_output.R")
  
  roi_names<-c("frontal_medial_cortex","frontal_orbital_cortex", "accumbens_l", "accumbens_r")
  #roi_data_list<-vector("list",length(roi_names))
  roi_all_dt<-NULL
  for (roi_i in 1:length(roi_data_list)){
    #roi_i<-2
    roi_name<-roi_names[[roi_i]]
    print(paste0("processing for ",roi_name))
    roi_dt<-get_nps_data_for_subs(100:400,nps_version=nps_version,data.subdir = "roi_event_data/fsl_roi_event_data_nltools",file_prefix = roi_name,verbose=FALSE)
    setnames(roi_dt,c("Value"),c(paste0("nltools_",roi_name)))
    #roi_dt$region<-roi_name
    #roi_data_list[[roi_i]]<-roi_dt
    if(is.null(roi_all_dt)){
      roi_all_dt<-roi_dt
    }else{
      roi_all_dt<-merge(roi_dt,roi_all_dt,by=intersect(colnames(roi_dt),colnames(roi_all_dt)))
    }
  }
  
  save(roi_all_dt,file=nltools_harvardoxford_repo)
  }