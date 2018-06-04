rm(list=ls())

#right, this time, we need to get some real data.
source("../util/apply_local_settings.R")
apply_local_settings("")
dd<-localsettings$data.dir
library(data.table)



source('stanlba/lba-math.R')
library(rstan)
library(parallel)
library(dplyr)
library(LaplacesDemon)
library(data.table)



rawdata <- data.table(read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment_amendment1.txt"), header=T))

stanfiledir<-"stanlba/stanfiles/"
