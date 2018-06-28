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


source('stanlba/lba-math.R')


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
colnames(roi_data)[roi_cols]<-paste0("ROI_",colnames(roi_data)[roi_cols])


rawdata.norois<-rawdata
rawdata<-merge(rawdata,roi_data,by=c("subid","runid","Motivation","image","presentation_n"),all.x=TRUE,all.y=FALSE)

stanfiledir<-"stanlba/stanfiles/"
