library(rstan)
library(loo)
library(dplyr)
library(pROC)
library(ggplot2)
library(data.table)

if(length(grep("nate_files",getwd()))>0){
  source("../util/apply_local_settings.R")
  source("../read_nps_output.R")
  modelcode_rl<-""
  apply_local_settings("../")
}else if(length(grep("notebooks",getwd()))>0){
  source("../util/apply_local_settings.R")
  source("../read_nps_output.R")
  modelcode_rl<-"../nate_files/"
  apply_local_settings("../")
}else{
  source("../util/apply_local_settings.R")
  source("read_nps_output.R")
  modelcode_rl<-"nate_files/"
  apply_local_settings()
}



REVERSAL_LEARNING_REWARD=1
REVERSAL_LEARNING_PUNISHMENT=2

ESTIMATION_METHODS<-as.factor(c("variationalbayes","MCMC","DEMCMC"))
ESTIMATION_METHOD.VariationalBayes=ESTIMATION_METHODS[[1]]
ESTIMATION_METHOD.MCMC=ESTIMATION_METHODS[[2]]
ESTIMATION_METHOD.DEMCMC=ESTIMATION_METHODS[[3]]
ESTIMATION_METHODS_STAN<-c(as.character(ESTIMATION_METHOD.MCMC),as.character(ESTIMATION_METHOD.VariationalBayes))


get_fit_desc<-function(use_model,descr,run,rp=c(2),
                       model_rp_separately=TRUE,model_runs_separately=FALSE,
                       use_pain=FALSE,fastDebug=FALSE,fileSuffix="",
                       estimation_method=ESTIMATION_METHOD.VariationalBayes,
                       bseed=bseed,
                       iterations=NA,
                       collateTrialData=TRUE,
                       chainNum=NA,
                       warmup_iter=NA,
                       rl_unique_runids=NA,
                       variable_run_lengths=NA,
                       sample_from_prior=NA,
                       subj_level_params=NA,
                       include_run_ot=NA,
                       pass_rt=NA
                       ){
  fit_desc<-""
  dd<-localsettings$data.dir
  if (1 %in% rp & 2 %in% c(rp)){
    #both reward and punishment
    if(model_rp_separately){
      model_separately_string="separate"
    }else{
      model_separately_string="together"
    }
    fit_desc<-paste0(fit_desc,dd,"Fits/", use_model, "_", descr, "_", "rewpun_",model_separately_string)
    
  }else if(rp==REVERSAL_LEARNING_PUNISHMENT & length(rp)==1){
    #the default, 
    fit_desc<-paste0(fit_desc,dd,"Fits/", use_model, "_", descr)
  }
  else if (rp==REVERSAL_LEARNING_REWARD & length(rp)==1){
    #must be rp==REVERSAL_LEARNING_REWARD
    fit_desc<-paste0(fit_desc,dd,"Fits/", use_model, "_", descr, "_", "REWARD")
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
  if(!is.na(iterations)){
    fit_desc<-paste0(fit_desc,"_itercount",as.character(iterations))
  }
  if(!is.na(chainNum)){
    fit_desc<-paste0(fit_desc,"_nchain",as.character(chainNum))
  }
  if(!is.na(warmup_iter)){
    fit_desc<-paste0(fit_desc,"_wup",as.character(warmup_iter))
  }
  if(!is.na(rl_unique_runids)){
    fit_desc<-paste0(fit_desc,"_rluri",as.character(rl_unique_runids))
  }
  if(!is.na(variable_run_lengths)){
    fit_desc<-paste0(fit_desc,"_vrl",as.character(variable_run_lengths))
  }
  if(!is.na(sample_from_prior)){
    fit_desc<-paste0(fit_desc,"_priorsample",as.character(sample_from_prior))
  }
  
  if(!is.na(subj_level_params)){
    fit_desc<-paste0(fit_desc,"_subj_level_params",as.character(subj_level_params))
  }
  if(!is.na(pass_rt)){
    fit_desc<-paste0(fit_desc,"_incRT",as.character(subj_level_params))
  }
  
  if(estimation_method!=ESTIMATION_METHOD.VariationalBayes){
    fit_desc<-paste0(fit_desc,"_",as.character(estimation_method))
  }
  if(collateTrialData==FALSE){
    fit_desc<-paste0(fit_desc,"_noTrialData")
  }
  
  
  fit_desc<-paste0(fit_desc,fileSuffix)
  fit_desc<-paste0(fit_desc,".RData")
  return(fit_desc)
}

lookupOrRunFit<-function(run=1,groups_to_fit,model_to_use="simple_decay_pain",
                         includeSubjGroup,rp=c(2),model_rp_separately=TRUE,model_runs_separately=FALSE,
                         include_pain=FALSE,fastDebug=FALSE,fileSuffix="",
                         estimation_method=ESTIMATION_METHOD.VariationalBayes,
                         bseed=bseed,
                         iterations=NA,
                         collateTrialData=TRUE,
                         chainNum=NA,
                         warmup_iter=NA,
                         rl_unique_runids=NA,
                         variable_run_lengths=NA,
                         sample_from_prior=NA,
                         subj_level_params=NA,
                         include_run_ot=NA,
                         pass_rt=NA,
                         lookupOnly=FALSE){
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
                           fastDebug = fastDebug,
                           fileSuffix = fileSuffix,
                           estimation_method=estimation_method,
                           bseed=bseed,
                           iterations=iterations,
                           collateTrialData=collateTrialData,
                           chainNum=chainNum,
                           warmup_iter=warmup_iter,
                           rl_unique_runids=rl_unique_runids,
                           variable_run_lengths=variable_run_lengths,
                           sample_from_prior=sample_from_prior,
                           subj_level_params=subj_level_params,
                           include_run_ot=include_run_ot,
                           pass_rt=pass_rt)
  if (file.exists(fit.fileid)){
    print(paste0("file ", fit.fileid, " has already been fit! Loading..."))
    load(fit.fileid)
    #fit_data <- list(fit = fit, plot_object = for_plot)
    return(fit_data)
  }else if (lookupOnly==FALSE){
    print("This has not been previously fit. Running full model...")
    fit<-fitGroupsV3Onegroup(run,groups_to_fit,model_to_use,includeSubjGroup,rp,model_rp_separately,model_runs_separately,
                             include_pain,fastDebug=fastDebug,fileSuffix=fileSuffix,
                             estimation_method=estimation_method,
                             bseed=bseed,iterations.set =iterations,collateTrialData=collateTrialData,
                             chainNum.set = chainNum,
                             warmup_iter.set = warmup_iter,
                             rl_unique_runids=rl_unique_runids,
                             variable_run_lengths=variable_run_lengths,
                             sample_from_prior=sample_from_prior,
                             subj_level_params=subj_level_params,
                             include_run_ot=include_run_ot,
                             pass_rt=pass_rt)
    #the fit run command actually saves the fit so no need to save it here.
    return(fit)
  }
  else{
    return(NA)
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
    }else if (groups_to_fit==1){
      descr <- "Safe_NoMeth"
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
                                    include_pain,fastDebug=FALSE,fileSuffix=fileSuffix,
                                estimation_method=ESTIMATION_METHOD.VariationalBayes,
                                bseed=sample.int(.Machine$integer.max, 1),iterations.set=NA,collateTrialData=TRUE,chainNum.set=NA,
                                warmup_iter.set=NA,
                                rl_unique_runids=FALSE,
                                variable_run_lengths=FALSE,
                                sample_from_prior=FALSE,
                                subj_level_params=FALSE,
                                include_run_ot=FALSE,
                                pass_rt=FALSE){
  use_model<-model_to_use
  #setwd("~/Box Sync/MIND_2017/Hackathon/Ben/reversallearning/nate_files")
  #setwd("nate_files")
  source(paste0(modelcode_rl,"Misc/freq_replace.R"))
  source(paste0(modelcode_rl,"Misc/plot_model.R"))
  
  # Use pain as outcome?
  pain_outcome <- include_pain
  
  # Which model to use?
  models <- c("simple_delta", "simple_delta_bias", "switch_lr", "simple_decay", 
              "switch_model", "double_update", "switch_decay", "switch_lr_double_update")
  dd<-localsettings$data.dir
  # Read in raw data
  if (length(rp)==1){
    if(rp==REVERSAL_LEARNING_PUNISHMENT & length(rp)==1){
      rawdata <- read.table(paste0(dd,"all_subjs_datacomplete_punishment.txt"), header=T)
    }else if (rp==REVERSAL_LEARNING_REWARD & length(rp)==1){
      rawdata <- read.table(paste0(dd,"all_subjs_datacomplete_reward.txt"), header=T)
    }
  }else if (rp==c(REVERSAL_LEARNING_REWARD,REVERSAL_LEARNING_PUNISHMENT) & length(rp)==2){
    rawdata <- read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment.txt"), header=T)
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
  subj_runCounts <-array(0, c(numSubjs))
  cue      <- array(0, c(numSubjs, maxTrials) )
  cue_freq <- array(0, c(numSubjs, maxTrials) )
  reversal <- array(0, c(numSubjs, maxTrials) )
  trial <- array(0, c(numSubjs, maxTrials) )
  subjid <- array(0, c(numSubjs, maxTrials) )
  cue_pos <- array(0, c(numSubjs, maxTrials) )
  cor_resp <- array(0, c(numSubjs, maxTrials) )
  pain_signal <- array(0, c(numSubjs, maxTrials) )
  rt <- array(0, c(numSubjs,maxTrials))
  
  run_id_ot <- array(0, c(numSubjs,4))#because 4 is the max number of runs.
  
  
  if (include_pain){
    pain_data<-get_nps_data_for_subs(subjList)
    if (mean(abs(pain_data$Value))>50){
      pain_data$Value<-pain_data$Value/353#a hack because we're in the middle of having this built-in to the pain files.
      warning("temporary correction applied to pain data magnitude")
    }
    print(paste0("stdev nonzero pain_value data is: ",(sd(abs(pain_data$Value[pain_data$Value!=0])))))
    
    
      #scale this to something that will work tractably in our model.
      #absence of standardized pain scores will affect the interpretability of the pain parameter
      #but it seems better than using SD because that would involve applying different SD corrections to each subject,
      #Based on the variance of their 
    pain_data$Motivation<-"punishment"
    rawdata<-merge(
      rawdata,pain_data,
      by.x=c("subjID","presentation_n","image","runid","Motivation","first_reversal","presentation_n_in_segment"),
      by.y=c("subid","presentation_n","image","runid","Motivation","first_reversal","presentation_n_in_segment"),all.x=TRUE,all.y=FALSE)
  }
  #subj_ids_have <- as.numeric(gsub(x = list.files("Data/Pain_Betas"), pattern = "_.*_*.csv", replacement = ""))

  
for (i in 1:numSubjs) {
    curSubj      <- subjList[i]
    useTrials    <- Tsubj[i]

    # if (curSubj %in% subj_ids_have) {
    #   stop("This seems wrong!")
    #   tmp_brain <- read.csv(paste0("Data/Pain_Betas/", curSubj, "_punishment_r1.csv"))
    # }
    tmp <- subset(rawdata, rawdata$subjID == curSubj)
    choice[i, 1:useTrials]  <- tmp$choice
    rt[i,1:useTrials] <- tmp$reaction_time
    
    cue[i, 1:useTrials]     <- as.numeric(as.factor(tmp$cue))
    cue_freq[i, 1:useTrials] <- tmp$cue_freq
    #two problems here! 
    #1) in instances where there's only one Reward run, we potentially get runs numbered 1 through 4, but 
    #a count of only 3 runs. This might not work with the double update model as I've set it up.
    #2) After I've resolved that issue, just need to figure a neat way to pass in run_ot.
    
    #re-number the outcome types for the trials, 

    
    rew_runcount <- length(unique(tmp$runid[tmp$Motivation=="reward"]))
    pun_runcount <- length(unique(tmp$runid[tmp$Motivation=="punishment"]))
    #arbitrarily starting with the reward trials, then the punishment trials
    #so the reward motivation trial IDs are simple. they're always numbered 1 and 2.
    
    #if there are fewer than 2 reward runs we need to renumber the punishment trials
    tmp$runmotiveid<-tmp$runid
    
    tmp$runmotiveid[tmp$Motivation=="punishment"] <- 
      tmp$runid[tmp$Motivation=="punishment"]+rew_runcount#this will increment the run motive ID by the number of reward runs.
    
    #record a variable keeping track of what the runs are.
    #subj_run_ot<-rep(NA,rew_runcount+pun_runcount)
    if(rew_runcount==1){
      run_id_ot[i,1]<-1
    }else if(rew_runcount==2){
      run_id_ot[i,1]<-1
      run_id_ot[i,2]<-1
    }
    if(pun_runcount==1){
      run_id_ot[i,min(which(run_id_ot[i,]==0))]<-2
    }else if(pun_runcount==2){
      run_id_ot[i,min(which(run_id_ot[i,]==0)):(min(which(run_id_ot[i,]==0))+1)]<-2
    }

    outcome_type[i, 1:useTrials] <- as.integer(tmp$Motivation=="punishment")+1
    #assign unique runids to the runs if necessary.
    if (rl_unique_runids){
      run_id[i, 1:useTrials] <- tmp$runmotiveid
    }else{
      run_id[i, 1:useTrials] <- tmp$runid
      if (include_run_ot){
        stop("include_run_ot can only be used with rl_unique_runids")
      }
    }

    #create the run length record per subject
    subj_runCounts[i] <- length(unique(run_id[i, 1:useTrials]))
    #print(unique(run_id[i, 1:useTrials]))
    
    reversal[i, 1:useTrials] <- as.numeric(tmp$reversal_trial)
    cue_pos[i, 1:useTrials] <- tmp$presentation_n_after_reversal
    trial[i, 1:useTrials] <- as.numeric(as.factor(tmp$onset_time_actual))
    if(any(trial[i, 1:useTrials]==0)){
      print("there's an 0 trial value.")
    }
    subjid[i, 1:useTrials] <- tmp$subjID
    cor_resp[i, 1:useTrials] <- tmp$cor_res
    outcome[i, 1:useTrials] <- tmp$outcome#ifelse(tmp$outcome==1, 0, tmp$outcome)
    pain_signal[i,1:useTrials]<-sapply(tmp$Value,function(x){if(is.na(x)){return(0);}else{return(x);}})
      #It doesn't matter what value reward trials hold here so long as it isn't NA (which can't be processed by stan)
      #because the stan code will just ignore pain values associated with reward trials.
  }
  #print(run_id_ot)
  
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
    dataList[["pain_signal"]]   = pain_signal
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
    all.runids.raw<-unique(c(run_id))
    dataList[["R"]] = max(subj_runCounts)
  }
  if(variable_run_lengths){
    dataList[["R_N"]] = subj_runCounts
  }
  
  if(sample_from_prior){
    dataList[["sample_from_prior"]] = 1
  }else{
    dataList[["sample_from_prior"]] = 0
  }
  
  if(subj_level_params){
    dataList[["subj_level_params"]] = 1
  }else{
    dataList[["subj_level_params"]] = 0
  }
  
  if(include_run_ot){
    dataList[["run_ot"]] = run_id_ot
  }
  if(!is.na(pass_rt)){
    if(pass_rt){
      dataList[["rt"]] = rt
    }
  }
  
  
  
  
  
  #need to add an option to number runs from 1 to 4, including both punishment and reward
  #
  if(estimation_method %in% ESTIMATION_METHODS_STAN){
    cat("Building model...")
    model_text<-paste0(readLines(paste0(modelcode_rl,"Final_Models/", use_model,".stan")),collapse="\n")
    #check if a model under the name exists under "compiled models"
    #only for this particular system [intended to pick out an individual system; not completely fool-proof but should do the trick]
    #in instances where there are different systems 
    compile.environ<-gsub("[\\/(). -#-]","",paste0(Sys.getenv("R_PLATFORM"),Sys.info()["version"],Sys.getenv("USER"),Sys.info()["nodename"]))
    precompiled.location<-paste0(localsettings$data.dir,"compiled_models/", compile.environ,use_model)
    model.loaded<-FALSE
    if (file.exists(paste0(precompiled.location,".stan"))){
      print(precompiled.location)
      print("A compiled stan model with this name already exists. Checking to see if it's identical to model being currently run...")
      m.precompiled.text<-paste0(readLines(paste0(precompiled.location,".stan")),collapse="\n")
      if(model_text==m.precompiled.text){
        print("Models match; loading precompiled model...")
        load(paste0(precompiled.location,".RData"))
        model.loaded<-TRUE
      }
    }
    if(model.loaded==FALSE){
      print("No pre-compiled model exists. Compiling...")
      #if it does, check to see if the text is exactly the same
      #if it is, use the pre-compiled model.
      m1 <- stan_model(paste0(modelcode_rl,"Final_Models/", use_model,".stan"))
      #save away the compiled model and its source into the cache directory.
      #this will allow this model to be loaded already pre-compiled next time! Should save us a bit of time.
      save(m1,file=paste0(precompiled.location,".RData"))
      file.copy(paste0(modelcode_rl,"Final_Models/", use_model,".stan"),paste0(precompiled.location,".stan"),overwrite=TRUE)
    }
    
    
    print("model built.")
  }
  
  if(fastDebug==FALSE){
    
  }
  
  cat("Fitting model...")
  estimation.start<-proc.time()
  if (estimation_method==ESTIMATION_METHOD.VariationalBayes){
    if (fastDebug==TRUE){
      warning("\nFast debugging enabled. Results will be highly unreliable and should only be used for debugging purposes.\n")
      fit <- vb(m1, data = dataList, adapt_engaged = F, eta = 1,iter=100,output_samples=100,seed=bseed)
    }else{
      if(is.na(iterations.set)){
        iterations=10000
      }else{
        iterations<-iterations.set
      }
      if(is.na(warmup_iter.set)){
        warmup_iter<-iterations/2
      }else{
        warmup_iter<-warmup_iter.set
      }
      fit <- vb(m1, data = dataList, adapt_engaged = F, eta = 1,iter=iterations,seed=bseed)
    }
  }else if (estimation_method==ESTIMATION_METHOD.MCMC){
    if (fastDebug==TRUE){
      warning("\nFast debugging enabled. Results will be highly unreliable and should only be used for debugging purposes.\n")
      fit <- sampling(m1, data = dataList, iter=60,warmup=30,chains=4,
                      sample_file=paste0(localsettings$data.dir,"MCMCSample"),
                      diagnostic_file=paste0(localsettings$data.dir,"MCMCdiagnostic"),
                      seed=bseed,
                      verbose=TRUE)
    }else{
      if(is.na(iterations.set)){
        iterations=2000
      }else{
        iterations<-iterations.set
      }
      if(is.na(warmup_iter.set)){
        warmup_iter<-iterations/2
      }else{
        warmup_iter<-warmup_iter.set
      }
      if(is.na(chainNum.set)){
        chainNum=6#the default value. Don't set it at chainNum.set because we only want this number recorded in the filename if it's not the default.
      }else{
        chainNum<-chainNum.set
      }
      fit <- sampling(m1, data = dataList,chains=chainNum,iter=iterations,warmup=warmup_iter,
                      sample_file=paste0(localsettings$data.dir,"MCMCSample"),
                      diagnostic_file=paste0(localsettings$data.dir,"MCMCdiagnostic"),
                      seed=bseed,
                      verbose=TRUE)
    }
    
  }else if (estimation_method==ESTIMATION_METHOD.DEMCMC){
    source(paste0(modelcode_rl,"getBySubjDataList.R"))
    bySubjDataList<-dataList()
    #INSERT DE-MCMC CODE HERE.
    print("have generated the BySubjDataList. Here's some info about it:")
    print(names(bySubjDataList))
    print(lapply(bySubjDataList,dim))
    
  }else{
    stop(paste0("Estimation method \"",as.character(estimation_method),"\" not recognized."))
  }
  estimation.finish<-proc.time()
  estimation.duration<-estimation.finish[["elapsed"]]-estimation.start[["elapsed"]]
  print(paste0("...model fit. Duration was ",as.character(estimation.duration), " s."))
  
  if(collateTrialData){
    for_plot<-collate_trial_data(fit)
  }else{
    for_plot<-NULL
  }
  

  fit_data <- list(fit = fit, plot_object = for_plot,model_text=model_text,general_info=list(estimation_duration=estimation.duration))
  
  cat("\nSaving model...")
  save(fit_data, file = get_fit_desc(use_model=use_model,
                                     group.description$descr,
                                     run,
                                     rp=rp,
                                     model_rp_separately=model_rp_separately,
                                     model_runs_separately=model_runs_separately,
                                     use_pain=include_pain,
                                     fastDebug=fastDebug,
                                     fileSuffix=fileSuffix,
                                     estimation_method=estimation_method,
                                     bseed=bseed,
                                     iterations=iterations.set,
                                     collateTrialData=collateTrialData,
                                     chainNum=chainNum.set,
                                     warmup_iter = warmup_iter.set,
                                     rl_unique_runids = rl_unique_runids,
                                     variable_run_lengths=variable_run_lengths,
                                     sample_from_prior=sample_from_prior,
                                     subj_level_params=subj_level_params,
                                     include_run_ot=include_run_ot,
                                     pass_rt=pass_rt))

  cat("...model saved.\n")
  
  #next line might fail. Be careful
  tryCatch({
    plot_model(fit_data)
  },
  error=function(e){
    print("couldn't plot the data because reasons. You might wanna see what's going on there.")
  }
  )
  return(fit_data)
}

collate_trial_data <- function(fit){
  for_plot<-NULL
  print(paste0("Extracting data to examine..."))
  # Compute AUC
  parVals <- rstan::extract(fit)
  print(paste0("...extracted."))
  if("p_subjID" %in% parVals){
    print(paste0("Collating trial-by-trail data for graphing..."))
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
    
    print("...collated.")
    
  }
  return(for_plot)
}
