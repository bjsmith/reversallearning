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





rawdata <- data.table(read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment_amendment1.txt"), header=T))

stanfiledir<-"stanlba/stanfiles/"
