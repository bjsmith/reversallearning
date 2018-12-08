
source("stanlba/lba_rl_setup.R")

multisubj_threesubs<-rawdata[subid %in% c(105:125) & Motivation=="reward" & runid==1,
                             .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,UniqueRunID=interaction(subid,runid,Motivation,drop = TRUE))]

