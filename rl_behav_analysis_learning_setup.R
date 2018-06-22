
if (!exists("nonResponseTimeAsNA")){
  warning("This codes non-response items as not correct, which could cause problems interpteting the data. do not use the 'correct' column unless you do not wish to distinguish between incorrect and false. Alternatively you can set 'nonResponseTimeAsNA' to TRUE to alleviate this issue.")
}else if (nonResponseTimeAsNA==FALSE){
  warning("This codes non-response items as not correct, which could cause problems interpteting the data. do not use the 'correct' column unless you do not wish to distinguish between incorrect and false. Included only for backwards compatibility.")
}

# a<-readline("This codes non-response items as not correct, which could cause problems interpteting the data. Enter 'disregard' to ignore or any other reponse to cancel.")
# if(tolower(a)!="disregard"){
#   stop("Cancelled execution due to potential code problems.")
# }
library(plyr);library(dplyr)
#install.packages("R.matlab")
require(R.matlab)
#install.packages('data.table')
require(data.table)
#install.packages('ggplot2')
#require(ggplot2)
#install.packages("tidyr")
require(tidyr)
require(ggplot2)
source("../util/apply_local_settings.R")
apply_local_settings()

if(!exists("data_amendment_version")){
  data_amendment_version<-0
}

pwd<-getwd()
if(data_amendment_version>0){
  aggregated.game.data.path<-paste0(localsettings$data.dir,"aggregated_game_data",data_amendment_version,".Rdata")
}else{
  aggregated.game.data.path<-paste0(localsettings$data.dir,"aggregated_game_data.Rdata")
}

if (file.exists(aggregated.game.data.path)){
  load(aggregated.game.data.path)
}else{
  source("/Users/benjaminsmith/Documents/msm-project/ranalysis/data-preprocessing-compilation/load_aggregated_game_data.R")
  all.data.all.cols<-load_aggregated_game_data()
  col.range<-c(1,10:35,541:567,988:1024)
  if(!is.na(data_amendment_version)){
    col.range<-c(col.range,which(colnames(all.data.all.cols) %in% c("Q24.1","Q32", "Q34","Q39","Q41")))
  }
  all.subject.data<-all.data.all.cols[,col.range]
  rm(all.data.all.cols)
  save(all.subject.data,file = aggregated.game.data.path)
}


setwd(pwd)

#install.packages("corrplot")
require(corrplot)

source("get_risk_group.R")

rl.all.subjects.list<-NULL
for (condition in c("reward","punishment")){
  #condition<-"reward"
  
  source("../util/graphics.R")
  if (condition=="reward"){
    filename.pattern<-"^rlr_sub[0-9]+_run[0-9]+_.*\\.mat"
    filename.prefix<-"/rlr_sub"
    filename.prefix2<-"/rlr_"
    graph.title.condition.suffix<-"\n(Reward trials)"
  }else if(condition=="punishment"){
    filename.pattern<-"^rlp_sub[0-9]+_run[0-9]+_.*\\.mat"
    filename.prefix<-"/rlp_sub"
    filename.prefix2<-"/rlp_"
    graph.title.condition.suffix<-"\n(Punishment trials)"
  }
  #get list of subjects
  sub.rec.filenames.f1.untrimmed<-list.files(path=paste0(localsettings$data.dir,"RL_behav/"),pattern=filename.pattern,ignore.case=TRUE)
  sub.rec.filenames.f2.untrimmed<-list.files(path=paste0(localsettings$data.dir,"ReversalLearning/new"),pattern=filename.pattern,ignore.case=TRUE)
  sub.rec.filenames.f3.untrimmed<-list.files(path=paste0(localsettings$data.dir,"ReversalLearning/results"),pattern=filename.pattern,ignore.case=TRUE)
  #OK are there duplicates WITHIN each of these groups?
  table(substr(sub.rec.filenames.f1.untrimmed,0,15))[table(substr(sub.rec.filenames.f1.untrimmed,0,15))>1]
  table(substr(sub.rec.filenames.f2.untrimmed,0,15))[table(substr(sub.rec.filenames.f2.untrimmed,0,15))>1]
  sub.rec.filenames.f1<-sub.rec.filenames.f1.untrimmed[
    !substr(sub.rec.filenames.f1.untrimmed,0,15) %in% names(table(substr(sub.rec.filenames.f1.untrimmed,0,15))[
      table(substr(sub.rec.filenames.f1.untrimmed,0,15))>1])]
  sub.rec.filenames.f2<-sub.rec.filenames.f2.untrimmed[
    !substr(sub.rec.filenames.f2.untrimmed,0,15) %in% names(table(substr(sub.rec.filenames.f2.untrimmed,0,15))[
      table(substr(sub.rec.filenames.f2.untrimmed,0,15))>1])]
  sub.rec.filenames.f3<-sub.rec.filenames.f3.untrimmed[
    !substr(sub.rec.filenames.f3.untrimmed,0,15) %in% names(table(substr(sub.rec.filenames.f3.untrimmed,0,15))[
      table(substr(sub.rec.filenames.f3.untrimmed,0,15))>1])]
  
  #yes. Sub 153 and 154 have duplicates. what do we do about that?
  #Because we don't know which are which, we should just exclude these subjects entirely.
  
  
  get.sub.rl.data <- function(sub.filename,sub.folder){
    mat.data<-readMat(sub.filename)
    sub.data<-as.data.frame(mat.data$RL)
    colnames(sub.data)<-
      c("ID","image","cor_res","Condition","response_key","reaction_time",
        "score","onset_time_designed","onset_time_actual",
        "reversal_trial","runid","blockid")
    #record the actual response key
    subid_from_filename<-as.numeric(substr(basename(sub.filename),regexpr("_sub",basename(sub.filename))[1]+4,regexpr("_sub",basename(sub.filename))[1]+6))
    # if((subid_from_filename%%2)==1){
    #   sub.data$cor_res_Counterbalanced=sub.data$cor_res
    # }else{
    #   sub.data$cor_res_Counterbalanced= 3 - sub.data$cor_res
    # }
    
    #so for each sub.data, 
    sub.data.no0<-sub.data[sub.data$score!=0,c("cor_res","response_key","score")]
    #is there another way I can detect whether the subject's cor_res needs to be reversed???
    # table(sub.data.no0$cor_res,sub.data.no0$response_key,sub.data.no0$score)
    score_for_match<-(sub.data.no0$cor_res==sub.data.no0$response_key)==(sub.data.no0$score==1)
    if(all(score_for_match)){
      #print(paste0("for sub ",subid_from_filename,", cor_res MATCHES response_key for correct items"))
      sub.data$cor_res_Counterbalanced=sub.data$cor_res
    }else if (all(!score_for_match)){
      #print(paste0("for sub ",subid_from_filename,", cor_res MISMATCHES response_key for correct items. Remapping."))
      sub.data$cor_res_Counterbalanced= 3 - sub.data$cor_res
    }else{
      stop(paste0("inconsistent cor_res, response_key, score alignment for sub",subid_from_filename))
    }
    
    
    # MID=1; %
    #   %Mtrial=2; %
    #   % Mcrres=3; % Correct Response
    #  Mcond=4; % 1: reversal; 2: control
    #  Mres=5; % left or right key. counterbalance across subjects; Do NOT try to match this to Mcrres ("cor_res"). MScore records whether there was an *actual* match after taking into account counter-balancing, so if you want to know whether a subject got it right orn ot, you need to look at that.
    #% response key counterbalance across subject;
    #% for subject with odd subjectID: left=L, right=R;
    #% for subject with even subjectID: left=R; right=L;
    #  MRT=6; % reaction time;
    #  Mscore=7; % 1: correct; -1: wrong; 0: no response
    #  Monset=8; % designed trial onset time
    #  MAonset=9; % actually trial onset time
    #  Misrev=10; % is this trial a reversal one?
    #  Mrunid=11; % is this trial a reversal one?
    #  Mblockid=12; % is this trial a reversal one?
    
    #get order for 
    
    aggregate(sub.data$onset_time_actual,
              by=list(sub.data$image),
              order)
    
    sub.data.table<-data.table(sub.data)
    if(any(sub.data.table$onset_time_actual==0)){
      sub.data.table[onset_time_actual==0,onset_time_actual:=NA]
      warning(paste("an onset_time_actual of 0 was found for a subject with sub.filename",sub.filename,"\nThis is not necessarily a problem, but it does indicate some irregulary in that subject's data.\nThe onset times for those presentations havae been modified to be \"NA\"."))
    }
    sub.data.table[,presentation_n:=.(order(onset_time_actual)),by=image]
    #set a presentation order relative to the reversal.
    #presentation 0 will be the first presentation after reversal
    #and other presentations will be numbered relative to that. 
    #get the minimum presentation_n_raw where reversal_trail==1, for each trial
    sub.data.table<-
      merge(sub.data.table,
            sub.data.table[reversal_trial==1,.(first_reversal=min(presentation_n)),by=image],
            by="image",all=TRUE)
    #some trials had NO reversals; their "reversal_trial" ID will be NA.
    #Those ARE appropriate for learning about reversal
    #but they would be INAPPROPRIATE to include in "reversal trial" graphs where we look at learning relative to reversal
    #so we will not give them a presentation_n. They still have presentation_n_raw
    sub.data.table[,presentation_n_after_reversal:=presentation_n-first_reversal]
    sub.data.table[,trial_contains_reversal:=!is.na(first_reversal)]
    
    #additional measure presentation_n_in_segment measures which order this presentation had within the learning sequence
    sub.data.table[,presentation_n_in_segment:=presentation_n]
    #print(class(sub.data.table$presentation_n))
    #print(class(sub.data.table$first_reversal))
    
    sub.data.table[presentation_n>=first_reversal,presentation_n_in_segment:=presentation_n-first_reversal+1L]
    sub.data.table[,reversal_trial:=presentation_n>=first_reversal]
    
    #now plot % correct scores for each presentation across trials
    sub.data.table[,correct:=.(score==1)]
    #this will consider a "no response" as incorrect
    
    
    sub.data.table$fileid<-sub(".mat","",sub(paste0(sub.folder,filename.prefix2),"",sub.filename))
    
    #identify unique subjects
    #we're going to rely on a specific format of filename here
    #I have my reservations, but whatever.
    sub.id<-substr(sub.filename,nchar(paste0(sub.folder,filename.prefix))+1,regexpr("_run",sub.filename)-1)
    sub.data.table$subid<-as.numeric(sub.id)
    run.start<-regexpr(paste0("^",sub.folder,filename.prefix,"[0-9]+_run"),sub.filename)
    run.end<-regexpr(paste0("^",sub.folder,filename.prefix,"[0-9]+_run[0-9]+_"),sub.filename)
    sub.data.table$runid<-as.numeric(substr(sub.filename,attributes(run.start)$match.length+1,attributes(run.end)$match.length-1))
    return(sub.data.table)
  }
  
  
  rl.subjects.f1.list<-do.call("rbind",lapply(paste0(localsettings$data.dir, "RL_behav/",sub.rec.filenames.f1),get.sub.rl.data,paste0(localsettings$data.dir,"RL_behav")))
  rl.subjects.f2.list<-do.call("rbind",lapply(paste0(localsettings$data.dir,"ReversalLearning/new/",sub.rec.filenames.f2),get.sub.rl.data,paste0(localsettings$data.dir,"ReversalLearning/new")))
  rl.subjects.f3.list<-do.call("rbind",lapply(paste0(localsettings$data.dir,"ReversalLearning/results/",sub.rec.filenames.f3),get.sub.rl.data,paste0(localsettings$data.dir,"ReversalLearning/results")))
  
  #iterate through each list.
  #in each list, if there exists duplicate rows, then test to see if the ENTIRE run is duplicated EXACTLY
  #View(rl.subjects.f2.list[rl.subjects.f2.list$subid==153,])
  unique(rl.subjects.f1.list$subid)
  length(unique(rl.subjects.f1.list$subid))
  length(unique(rl.subjects.f2.list$subid))
  unique(rl.subjects.f3.list$subid)
  subs.only.in.rl_behav<-setdiff(unique(rl.subjects.f1.list$subid),unique(rl.subjects.f2.list$subid))
  #RL_behav contains 112, which ReversalLearning/new/ does not.
  subs.only.in.reversallearning.results<-setdiff(unique(rl.subjects.f3.list$subid),unique(rl.subjects.f2.list$subid))
  #ReversalLearning/results/ contains three subjects, 155, 262, and 390, which ReversalLearning/new/ does not.
  
  # 
  # View(rl.subjects.f1.list[subid==112])
  # rl.subjects.f1.list$subid
  # rl.subjects.f1.list[,.(count=.N),.(subid)]
  # View(rl.subjects.f1.list[,.(count=.N),.(subid,runid)])
  # View(rl.subjects.f2.list[,.(count=.N),.(subid)])
  # View(rl.subjects.f3.list[,.(count=.N),.(subid)])
  #for now, let's use ReversalLeraning/New and randomly combine the missing ones from the other sets.
  rl.all.subjects.list.forcondition<-rbind(rl.subjects.f2.list,
                                           rl.subjects.f1.list[subid==subs.only.in.rl_behav],
                                           rl.subjects.f3.list[subid %in% subs.only.in.reversallearning.results])
  
  rl.all.subjects.list.forcondition$Motivation<-condition
  if(is.null(rl.all.subjects.list)){
    rl.all.subjects.list<-rl.all.subjects.list.forcondition
  }else{
    rl.all.subjects.list<-rbind(rl.all.subjects.list,rl.all.subjects.list.forcondition)
  }
}

subgroupdata<-get_risk_group()
rl.all.subjects.list<-merge(rl.all.subjects.list,subgroupdata,by.x="subid",by.y="Adjusted_subid")
all.subject.data$SUBID_ADJ_I<-as.integer(all.subject.data$SUBID_ADJ)
rl.all.subjects.list<-merge(rl.all.subjects.list,all.subject.data,by.x="subid",by.y="SUBID_ADJ_I")[, !"SUBID_ADJ", with=FALSE]
rl.all.subjects.list[,presentation_n_over_segments:=presentation_n_in_segment]
last_nonreversal<-max(rl.all.subjects.list[reversal_trial==FALSE,presentation_n_in_segment])
#aligns so that for every subject, we give the same index to the first reversal trial
rl.all.subjects.list[reversal_trial==TRUE,presentation_n_over_segments:=presentation_n_in_segment+last_nonreversal]
# rl.all.subjects.list[1:100]
# length(unique(rl.all.subjects.list[Motivation=="reward",subid]))
# length(unique(rl.all.subjects.list[Motivation=="punishment",subid]))
if (exists("nonResponseTimeAsNA")){
  if(nonResponseTimeAsNA){
    rl.all.subjects.list[reaction_time==0,reaction_time:=NA]
  }
}

#now let's create a column that indicates the required response for this subject

table(rl.all.subjects.list$cor_res_Counterbalanced,
      rl.all.subjects.list$response_key,
      rl.all.subjects.list$correct)

#We should throw out, then print a warning.
break.labels=c("1\nPre-reversal",2:8,"1\nReversal",2:5)


accuracy.by.pres_seg.subid<-rl.all.subjects.list[,.(prop.correct=sum(correct)/.N,count=.N),.(presentation_n_over_segments,subid,Motivation,RiskLabel,MethUse,SexRisk)]

accuracy.by.pres_seg.image<-
  rl.all.subjects.list[,.(prop.correct=sum(correct)/.N,count=.N),.(presentation_n_over_segments,Motivation,image)]

accuracy.by.pres_seg<-
  rl.all.subjects.list[,.(prop.correct=sum(correct)/.N,count=.N),.(presentation_n_over_segments,Motivation)]


#mean across subjects
# accuracy.by.pres_seg.subid.summary<-accuracy.by.pres_seg.subid[
#   ,
#   .(prop.correct.m=mean(prop.correct),
#     prop.correct.sd=sd(prop.correct),
#     prop.correct.p25=rank(prop.correct)/.N),
#   .(presentation_n_over_segments,Motivation)]

break.labels=c("1\nPre-reversal",2:last_nonreversal,"1\nReversal",2:6)
# accuracy.by.pres_seg.subid.finalpc<-
#   accuracy.by.pres_seg.subid[presentation_n_over_segments==4,.(final.prop.correct=prop.correct),.(subid,Motivation)]
# accuracy.by.pres_seg.subid<-merge(accuracy.by.pres_seg.subid,accuracy.by.pres_seg.subid.finalpc,by=c("subid","Motivation"))
warning("NB to self: stopped the calculation of proportion correct because it was wrong. If you restore this, (a) fix it and (b) make sure fixing this doesn't break other things!")

