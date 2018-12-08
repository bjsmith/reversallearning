library(rstan)
library(parallel)
library(dplyr)
library(data.table)
library(tidyr)

#rm(list=ls())

#right, this time, we need to get some real data.
source("../util/apply_local_settings.R")
apply_local_settings("")
dd<-localsettings$data.dir
library(data.table)
source('nate_files/fitGroupsV3Onegroup.R')
source("../util/get_my_preferred_cores.R")

rawdata <- data.table(read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment_amendment2.txt"), header=T))
cat("...main dataset loaded;loading freesurfer data...")

load_and_merge_neural_dataset<-function(roi_locationpath,rawdata,colname_prefix="ROI_"){
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

rawdata<-load_and_merge_neural_dataset(paste0(dd,"freesurfer_rl/roi_event_data/","20180626T165755","data_allsubjs.RData"),rawdata,
                                       colname_prefix = "frsurf_")

cat("...datasets merged...loading fsl_harvardoxford_dataset...")
rawdata<-load_and_merge_neural_dataset(paste0(dd,"roi_event_data/fsl_roi_event_data/","20180721T120724","data_allsubjs.RData"),rawdata,
                                       colname_prefix = "fsl_")
cat("...datasets merged...loading and merging nltools_harvardoxford_dataset...")
source("neural_data/load_nltools_harvardoxford_rois.R")

roi_all_dt$Motivation<-tolower(roi_all_dt$Motivation)
#what exactly do we need to do to merge here?
intersectcols<-intersect(colnames(roi_all_dt),colnames(rawdata))

rawdata2<-merge(rawdata,roi_all_dt,by=intersect(colnames(roi_all_dt),colnames(rawdata)), all.x=TRUE,all.y=FALSE)

#correlation was only 0.68 between the nltools measure and the other two
stats::heatmap(cor(rawdata2[,.(nltools_accumbens_l,fsl_roi_accumbens_l,frsurf_Left.Accumbens.area,
                        nltools_accumbens_r,fsl_roi_accumbens_r,frsurf_Right.Accumbens.area,
                        nltools_frontal_orbital_cortex,fsl_roi_frontal_orbital_cortex,
                        nltools_frontal_medial_cortex,fsl_roi_frontal_medial_cortex,
                        frsurf_ctx_lh_G_orbital,frsurf_ctx_rh_G_orbital,frsurf_ctx_lh_S_suborbital,frsurf_ctx_rh_S_suborbital
)],use = "complete.obs"))


source("stanlba/singlelevelmodel/lba_rl_joint_v2_evaluate_functions.R")
library(dplyr)
library(ggplot2)
library(stringi)
heatmap(cor(rawdata2[,.(nltools_accumbens_l,fsl_roi_accumbens_l,frsurf_Left.Accumbens.area,
                    nltools_accumbens_r,fsl_roi_accumbens_r,frsurf_Right.Accumbens.area,
                    nltools_frontal_orbital_cortex,fsl_roi_frontal_orbital_cortex,
                    nltools_frontal_medial_cortex,fsl_roi_frontal_medial_cortex,
                    frsurf_ctx_lh_G_orbital,frsurf_ctx_rh_G_orbital,frsurf_ctx_lh_S_suborbital,frsurf_ctx_rh_S_suborbital)],
        use = "complete.obs"),label="Correlation",order=TRUE,labelsize=4,scale_midpoint=0.5)

#let's do prcomp with the freesurfer variables

frsurf.prcomp<-prcomp(rawdata2[,.(frsurf_Left.Accumbens.area,
                   frsurf_Right.Accumbens.area,
                   frsurf_ctx_lh_G_orbital,frsurf_ctx_rh_G_orbital,frsurf_ctx_lh_S_suborbital,frsurf_ctx_rh_S_suborbital)])

stats::heatmap(frsurf.prcomp$rotation)

#sub-version s: applies Jonas's suggestion to regress out CSF. Uses freesurfer ROIs, all dmn regions.
library(rstan)
source("stanlba/lba_rl_joint_setup.R")

heatmap(cor(rawdata[,.(con_nltools_accumbens_l,con_fsl_roi_accumbens_l,con_ROI_Left.Accumbens.area,
                       con_nltools_accumbens_r,con_fsl_roi_accumbens_r,con_ROI_Right.Accumbens.area,
                       con_nltools_frontal_orbital_cortex,con_fsl_roi_frontal_orbital_cortex,
                       con_nltools_frontal_medial_cortex,con_fsl_roi_frontal_medial_cortex,
                       con_ROI_ctx_lh_G_orbital,con_ROI_ctx_rh_G_orbital,con_ROI_ctx_lh_S_suborbital,con_ROI_ctx_rh_S_suborbital)],
            use = "complete.obs"),label="Correlation",order=TRUE,labelsize=4,scale_midpoint=0.5)


library(rstan)
library(dplyr)
source("stanlba/lba_rl_joint_setup.R")

library(scales)
corrected<-grep("^ROI_",colnames(rawdata))
corrected.isNA<-apply(rawdata[,corrected,with=FALSE],2,function(x){sum(is.na(x))<100})

map1<-heatmap(cor(rawdata[,names(corrected.isNA)[corrected.isNA],with=FALSE],
            use = "complete.obs"),label="Correlation",show_labels=FALSE,order=TRUE,labelsize=0.1,scale_midpoint=0.5,
            extra_ggitems = list(theme(axis.text.x = element_text(size=2),axis.text.y = element_text(size=2.5)),labs(title="Motion correction only\n")),
            map_limits=c(0,1))

corrected<-grep("con_ROI",colnames(rawdata))
corrected.isNA<-apply(rawdata[,corrected,with=FALSE],2,function(x){sum(is.na(x))<100})

map2<-heatmap(cor(rawdata[,names(corrected.isNA)[corrected.isNA],with=FALSE],
                  use = "complete.obs"),label="Correlation",show_labels=FALSE,order=TRUE,labelsize=0.5,scale_midpoint=0.5,
              extra_ggitems = list(theme(axis.text.x = element_text(size=2),axis.text.y = element_text(size=2.5)),labs(title="Motion correction\nWith CSF+WM+Ventricles correction")),
              map_limits=c(0,1))


library(gridExtra)
grid.arrange(map1,map2,ncol=2)
library(savePNG)
ggsave("motion_CSF_WM_V_correction.png",arrangeGrob(map1,map2,ncol = 2),width = 12,height = 6)
