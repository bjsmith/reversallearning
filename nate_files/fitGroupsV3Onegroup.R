library(rstan)
library(loo)
library(dplyr)
library(pROC)
library(ggplot2)
library(data.table)

REVERSAL_LEARNING_REWARD=1
REVERSAL_LEARNING_PUNISHMENT=2


get_fit_desc<-function(use_model,descr,run,rp=c(2),model_rp_separately=TRUE,model_runs_separately=FALSE,use_pain=FALSE,fastDebug=FALSE){
  fit_desc<-""
  if (1 %in% rp & 2 %in% c(rp)){
    #both reward and punishment
    if(model_rp_separately){
      model_separately_string="separate"
    }else{
      model_separately_string="together"
    }
    fit_desc<-paste0(fit_desc,"Fits/", use_model, "_", descr, "_", "rewpun_",model_separately_string)
    
  }else if(rp==REVERSAL_LEARNING_PUNISHMENT & length(rp)==1){
    #the default, 
    fit_desc<-paste0(fit_desc,"Fits/", use_model, "_", descr)
  }
  else if (rp==REVERSAL_LEARNING_REWARD & length(rp)==1){
    #must be rp==REVERSAL_LEARNING_REWARD
    fit_desc<-paste0(fit_desc,"Fits/", use_model, "_", descr, "_", "REWARD")
  }else{
    stop("unrecognized rewardpunishment parameter")
  }
  
  fit_desc<-paste0(fit_desc,"_run",paste0(run,collapse = ""))
  
  if(use_pain){
    fit_desc<-paste0(fit_desc,"_with_pain_data")
  }
  if(model_runs_separately){
    fit_desc<-paste0(fit_desc,"_model_distinct_runs")
  }
  if(fastDebug){
    fit_desc<-paste0(fit_desc,"_fastDebug")
  }
  fit_desc<-paste0(fit_desc,".RData")
  return(fit_desc)
}

lookupOrRunFit<-function(run=1,groups_to_fit,model_to_use="simple_decay_pain",
                         includeSubjGroup,rp=c(2),model_rp_separately=TRUE,model_runs_separately=FALSE,
                         include_pain=FALSE,fastDebug=FALSE){
  #looks up a fit. if it has been run before, just reload it from the hard drive.
  #if it hasn't, then run it.
  group.description<-get_group_description(groups_to_fit)
  fit.fileid<-get_fit_desc(use_model=model_to_use,
                           group.description$descr,
                           run,
                           rp=rp,
                           model_rp_separately=model_rp_separately,
                           model_runs_separately=model_runs_separately,
                           use_pain=include_pain,
                           fastDebug = fastDebug)
  if (file.exists(fit.fileid)){
    print("this has already been fit! Loading...")
    load(fit.fileid)
    #fit_data <- list(fit = fit, plot_object = for_plot)
    return(fit_data)
  }else{
    print("This has not been previously fit. Running full model...")
    fit<-fitGroupsV3Onegroup(run,groups_to_fit,model_to_use,includeSubjGroup,rp,model_rp_separately,model_runs_separately,
                             include_pain,fastDebug=fastDebug)
    #the fit run command actually saves the fit so no need to save it here.
    return(fit)
  }
}

get_group_description<-function(groups_to_fit){
  # Separate groups (Risky Meth == 3, Risky No Meth == 2, Safe Meth == 4, Safe No Meth == 1)
  group<-groups_to_fit
  if (length(groups_to_fit)==1){
    if (groups_to_fit==2){
      descr <- "Risky_NoMeth"
    }else if (groups_to_fit==3){
      descr <- "Risky_Meth"
    }
  } else if (length(setdiff(groups_to_fit,c(2,3)))==0 & length(setdiff(c(2,3),groups_to_fit))==0){
      descr <- "RiskyMethAndNonMeth"
  }else{
    stop("Unsupported group combination. Please try again!")
  }
  return(list(group=group,descr=descr))
  # meth <- fit_meth_group  # Meth T/F
  # risk <- fit_risky_group  # Risky sex T/F
  # 
  # 
  # if (meth & risk) {
  #   group <- 3
  #   descr <- "Risky_Meth"
  # } else if (!meth & risk) {
  #   group <- 2
  #   descr <- "Risky_NoMeth"
  # } else if (!meth & !risk) {
  #   group <- 1
  #   descr <- "Safe_NoMeth"
  # } else {
  #   group <- 4
  #   descr <- "Safe_Meth"
  # }
  # return(list(group=group,descr=descr))
}
fitGroupsV3Onegroup <- function(run=1,groups_to_fit,model_to_use="simple_decay_pain",
                                    includeSubjGroup,rp,model_rp_separately,model_runs_separately,
                                    include_pain,fastDebug=FALSE){
  use_model<-model_to_use
  #setwd("~/Box Sync/MIND_2017/Hackathon/Ben/reversallearning/nate_files")
  #setwd("nate_files")
  source("Misc/freq_replace.R")
  source("Misc/plot_model.R")
  
  # Use pain as outcome?
  pain_outcome <- include_pain
  
  # Which model to use?
  models <- c("simple_delta", "simple_delta_bias", "switch_lr", "simple_decay", 
              "switch_model", "double_update", "switch_decay", "switch_lr_double_update")
  
  # Read in raw data
  if (length(rp)==1){
    if(rp==REVERSAL_LEARNING_PUNISHMENT & length(rp)==1){
      rawdata <- read.table("../../data/all_subjs_datacomplete_punishment.txt", header=T)
    }else if (rp==REVERSAL_LEARNING_REWARD & length(rp)==1){
      rawdata <- read.table("../../data/all_subjs_datacomplete_reward.txt", header=T)
    }
  }else if (rp==c(REVERSAL_LEARNING_REWARD,REVERSAL_LEARNING_PUNISHMENT) & length(rp)==2){
    rawdata <- read.table("../../data/all_subjs_datacomplete_reward_and_punishment.txt", header=T)
  }else{
    print("unrecognized reward-punishment flag!")
  }
  
  print("Preparing data...")
  #remove subject 153
  
  # if(model_rp_separately){
  #   stop("we don't yet support modelling R and P separately")
  # }else{
  #   print("modeling reward and punishment together, if they are both here. nothing to see here; carry on!")
  # }
  #we gotta get the data for reversal learning reward.
  
  # /all_subjs_datacomplete_punishment_rm153.txt", header = T) #do we have to remove subject 153???
  names(rawdata)[1] <- c("subjID")
  
  # Separate groups (Risky Meth == 3, Risky No Meth == 2, Safe Meth == 4, Safe No Meth == 1)
  group.description<-get_group_description(groups_to_fit)
  rawdata <- subset(rawdata, cue != 0 & RiskCat %in% group.description$group & runid %in% run)
  
  # Create cue frequency
  rawdata$cue_freq <- freq_replace(rawdata$cue, by_var = rawdata$subjID)
  rawdata.dt<-data.table(rawdata)
  
  #make sure each subject has two runs (or modify this to simply exclude subjects that don't have two runs)
  if (length(run)>1 & model_runs_separately==TRUE){
    
    #subjects with only one run
    one.run.subjs<-rawdata.dt[,.(runcount=length(unique(runid))),by=subjID][runcount<max(runcount),subjID]
    if(length(one.run.subjs)>0){
      stop(paste0(
        "The following subjects did not have the correct number of runs\n",
        "- I haven't coded the classifier to deal with them, yet:\n"+
          paste0(one.run.subjs,collapse = ",")))
      rawdata<-rawdata[!(rawdata$subjID %in% one.run.subjs)]
      rawdata.dt<-data.table(rawdata)
      
    }
  }
  #exclude any trials that seem to have been aborted and print a warning about them.
  #View(rawdata.dt[subjID %in% unique(rawdata.dt[reaction_time==0& response_key==0 & !is.na(onset_time_designed) & is.na(onset_time_actual),subjID])])
  
  first.aborted.trial.by.run<-rawdata.dt[reaction_time==0& response_key==0 & !is.na(onset_time_designed) & is.na(onset_time_actual),
                                          .(FirstOnsetTime=min(onset_time_designed)),by=.(subjID,runid,Motivation)]
  if(dim(first.aborted.trial.by.run)[1]>0){
    warning(paste0("excluded trials from ", as.character(dim(first.aborted.trial.by.run)[1]), " runs that were designed but not presented."))
    rawdata.dt<-rawdata.dt[!(reaction_time==0& response_key==0 & !is.na(onset_time_designed) & is.na(onset_time_actual))]
    rawdata<-data.frame(rawdata.dt)
  }
  
  # apply(first.aborted.trial.by.run,1,function(r){
  #   
  #   rawdata.dt<-
  #     rawdata.dt[subjID==r[["subjID"]]&runid==r[["runid"]]&&Motivation==r[["Motivation"]] & onset_time_designed<FirstOnsetTime,]
  # })
  # 
  # Individual Subjects
  subjList  <- unique(rawdata[,"subjID"])  # list of subjects x blocks
  numSubjs  <- length(subjList)  # number of subjects
  Tsubj     <- as.vector( rep( 0, numSubjs ) ) # number of trials for each subject
  subjGroup   <- as.vector( rep( 0, numSubjs ) ) #subject group
  N_cues    <- as.vector( rep( 0, numSubjs ) ) # number of trials for each subject
  pain_diff <- as.vector( rep( NA, numSubjs ) ) # number of trials for each subject
  numRuns <- length(unique(rawdata.dt$runid))#number of unique runs 
  
  for ( i in 1:numSubjs )  {
    curSubj   <- subjList[ i ]
    Tsubj[i]  <- sum( rawdata$subjID == curSubj )  # Tsubj[N]
    N_cues[i] <- length(unique(rawdata$cue))
    #classify the subject's group.
    curSubjRiskCat<-rawdata.dt[subjID==curSubj,RiskCat]
    if(!all(curSubjRiskCat[1]==curSubjRiskCat)){
      stop("error: this subject has different rounds listed with different risk categories,but subject risk category is subjectwise so this makes no sense.")
    }
    subjGroup[i] <- curSubjRiskCat[1]
  }
  
  # Setting maxTrials
  maxTrials <- max(Tsubj)
  
  choice   <- array(0, c(numSubjs, maxTrials) )
  outcome  <- array(0, c(numSubjs, maxTrials) )
  outcome_type <- array(0, c(numSubjs, maxTrials) )
  run_id <- array(0, c(numSubjs, maxTrials) )
  cue      <- array(0, c(numSubjs, maxTrials) )
  cue_freq <- array(0, c(numSubjs, maxTrials) )
  reversal <- array(0, c(numSubjs, maxTrials) )
  trial <- array(0, c(numSubjs, maxTrials) )
  subjid <- array(0, c(numSubjs, maxTrials) )
  cue_pos <- array(0, c(numSubjs, maxTrials) )
  cor_resp <- array(0, c(numSubjs, maxTrials) )
  pain <- array(0, c(numSubjs, maxTrials) )
  
  subj_ids_have <- as.numeric(gsub(x = list.files("Data/Pain_Betas"), pattern = "_.*_*.csv", replacement = ""))
  
for (i in 1:numSubjs) {
    curSubj      <- subjList[i]
    useTrials    <- Tsubj[i]
    if (curSubj %in% subj_ids_have) {
      tmp_brain <- read.csv(paste0("Data/Pain_Betas/", curSubj, "_punishment_r1.csv"))
    }
    tmp <- subset(rawdata, rawdata$subjID == curSubj)
    choice[i, 1:useTrials]  <- tmp$choice
    
    cue[i, 1:useTrials]     <- as.numeric(as.factor(tmp$cue))
    cue_freq[i, 1:useTrials] <- tmp$cue_freq
    outcome_type[i, 1:useTrials] <- as.integer(tmp$Motivation=="punishment")+1
    run_id[i, 1:useTrials] <- tmp$runid
    reversal[i, 1:useTrials] <- as.numeric(tmp$reversal_trial)
    cue_pos[i, 1:useTrials] <- tmp$presentation_n_after_reversal
    trial[i, 1:useTrials] <- as.numeric(as.factor(tmp$onset_time_actual))
    if(any(trial[i, 1:useTrials]==0)){
      print("there's an 0 trial value.")
    }
    subjid[i, 1:useTrials] <- tmp$subjID
    cor_resp[i, 1:useTrials] <- tmp$cor_res
    if (pain_outcome) {
      outcome[i, 1:useTrials] <- scale(as.numeric(tmp_brain[,!grepl(pattern = "placeholder", x = names(tmp_brain))])[1:useTrials])
      if (curSubj %in% subj_ids_have) {#pain data
        punishmentTrialsCount<-sum(outcome_type[i,1:useTrials]==REVERSAL_LEARNING_PUNISHMENT)
        punishmentTrialsId<-which(outcome_type[i,1:useTrials]==REVERSAL_LEARNING_PUNISHMENT)
        tmp_noPlace <- tmp_brain[,!grepl(pattern = "placeholder", x = names(tmp_brain))][1:punishmentTrialsCount]#excluding 'placeholder' trials...
        pain[i, punishmentTrialsId] <- scale(as.numeric(tmp_brain[,!grepl(pattern = "placeholder", x = names(tmp_brain))])[1:punishmentTrialsCount])
        #get the array of pain measurements to pass in
        pain_diff[i] <- 
          mean(as.numeric(tmp_noPlace[,grepl(pattern = "error", x = names(tmp_noPlace))])[1:punishmentTrialsCount], na.rm = T) - 
          mean(as.numeric(tmp_noPlace[,grepl(pattern = "correct", x = names(tmp_noPlace))])[1:punishmentTrialsCount], na.rm = T)
        
      } else {
        pain_diff[i] <- NA
      }
    } else {
      outcome[i, 1:useTrials] <- tmp$outcome#ifelse(tmp$outcome==1, 0, tmp$outcome)
    }
  }
  
  trial[which(is.na(trial))] <- 0
  
  dataList <- list(
    N        = numSubjs,
    T        = maxTrials,
    Tsubj    = Tsubj,
    N_cues   = N_cues - 1,
    choice   = choice, 
    outcome  = outcome,
    cue      = cue,
    cue_freq = cue_freq,
    reversal = reversal,
    trial = trial,
    cue_pos = cue_pos,
    cor_resp = cor_resp,
    subjid = subjid,
    numPars  = 2
  )
  if(include_pain){
    dataList[["pain"]]   = pain
  }
  
  #add group to the data list, only if we specified that it should be added.
  if(includeSubjGroup){
    dataList[["subjGr"]]   = subjGroup
    dataList[["Gr_N"]] = length(unique(subjGroup))
  }
  
  if(model_rp_separately){
    dataList[["outcome_type"]] = outcome_type
  }
  
  if(model_runs_separately){
    dataList[["run_id"]] = run_id
    dataList[["R"]] = numRuns
  }
  cat("Building model...")
  m1 <- stan_model(paste0("Final_Models/", use_model,".stan"))
  model_text<-paste0(readLines(paste0("Final_Models/", use_model,".stan")),collapse="\n")
  print("model built.")
  
  cat("Fitting model...")
  if (fastDebug==TRUE){
    warning("\nFast debugging enabled. Results will be highly unreliable and should only be used for debugging purposes.\n")
    fit <- vb(m1, data = dataList, adapt_engaged = F, eta = 1,iter=100,output_samples=100)
  }else{
    fit <- vb(m1, data = dataList, adapt_engaged = F, eta = 1)
  }
  print("...model fit.")
  
  # Compute AUC
  parVals <- rstan::extract(fit)
  
  #shape the data into a format suitable for plotting.
  for_plot <- NULL
  for_plot$subjID <- reshape2::melt(apply(parVals$p_subjID, c(2,3), mean, na.rm=T))[,3]
  for_plot$cor_res <- reshape2::melt(apply(parVals$p_cor_res, c(2,3), mean, na.rm=T))[,3]
  for_plot$outcome <- reshape2::melt(apply(parVals$p_outcome, c(2,3), mean, na.rm=T))[,3]
  for_plot$cue_pos <- reshape2::melt(apply(parVals$p_cue_pos, c(2,3), mean, na.rm=T))[,3]
  for_plot$cue_freq <- reshape2::melt(apply(parVals$p_cue_freq, c(2,3), mean, na.rm=T))[,3]
  for_plot$y_hat <- round(reshape2::melt(apply(parVals$y_hat, c(2,3), mean, na.rm=T))[,3]); for_plot$y_hat[for_plot$y_hat==0] <- NA
  for_plot$choice <- round(reshape2::melt(apply(parVals$p_choice, c(2,3), mean, na.rm=T))[,3]); for_plot$choice[for_plot$choice==0] <- NA
  for_plot <- as.data.frame(for_plot)
  
  all_pred_size<-min((dim(parVals$y_hat)[1]),500)#the lesser of the length of y_hat, or 500
  all_pred <- array(NA, dim = c(all_pred_size, length(for_plot$y_hat)))
  for (i in 1:all_pred_size) {
    tmp_y_hat <- reshape2::melt(parVals$y_hat[i,,])[,3]; tmp_y_hat[tmp_y_hat==0] <- NA
    all_pred[i,] <- ifelse((tmp_y_hat==for_plot$choice & for_plot$outcome==1) | (tmp_y_hat!=for_plot$choice & for_plot$outcome!=1), 1, 0)
  }
  for_plot$all_pred_correct <- apply(all_pred, 2, mean, na.rm=T)
  
  # correct % of subject and model
  for_plot$actual_correct <- ifelse(for_plot$outcome==1, 1, 0)
  for_plot$pred_correct <- ifelse((for_plot$y_hat==for_plot$choice & for_plot$outcome==1) | (for_plot$y_hat!=for_plot$choice & for_plot$outcome!=1), 1, 0)
  
  fit_data <- list(fit = fit, plot_object = for_plot,model_text=model_text)
  
  save(fit_data, file = get_fit_desc(use_model=use_model,
                                     group.description$descr,
                                     run,
                                     rp=rp,
                                     model_rp_separately=model_rp_separately,
                                     model_runs_separately=model_runs_separately,
                                     use_pain=include_pain,
                                     fastDebug=fastDebug))
  
  #next line might fail. Be careful
  tryCatch({
    plot_model(fit_data)
  },
  error=function(e){
    print("couldn't plot the data because there were more than two levels in \"response\". You might wanna see what's going on there.")
  }
  )
  return(fit_data)
}