#install.packages("R.matlab")
require(R.matlab)
#install.packages('data.table')
require(data.table)
#install.packages('ggplot2')
#require(ggplot2)
#install.packages("tidyr")
require(tidyr)
require(ggplot2)
#install.packages("corrplot")
require(corrplot)

source("get_risk_group.R")

rl.all.subjects.list<-NULL
for (condition in c(
  "reward",
  "punishment"
)){
  #condition<-"reward"
  
  
  break.labels=c("1\nPre-reversal",2:8,"1\nReversal",2:5)
  reversal_learning_timeline_ggplot_commands<-
    list(scale_x_continuous(breaks=-8:4,labels=break.labels),
         labs(x="Presentation"),
         theme(axis.text.x = element_text(hjust=0),
               axis.text = element_text(face="bold")#left-align presentation labels
         ))
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
  sub.rec.filenames.f1<-list.files(path="../data/RL_behav/",pattern=filename.pattern,ignore.case=TRUE)
  sub.rec.filenames.f2<-list.files(path="../data/ReversalLearning/new",pattern=filename.pattern,ignore.case=TRUE)
  sub.rec.filenames.f3<-list.files(path="../data/ReversalLearning/results",pattern=filename.pattern,ignore.case=TRUE)
  get.sub.rl.data <- function(sub.filename,sub.folder){
    mat.data<-readMat(sub.filename)
    sub.data<-as.data.frame(mat.data$RL)
    colnames(sub.data)<-
      c("ID","image","cor_res","Condition","response_key","reaction_time",
        "score","onset_time_designed","onset_time_actual",
        "reversal_trial","runid","blockid")
    
    # MID=1; %
    #   %Mtrial=2; %
    #   % Mcrres=3; % Correct Response
    #  Mcond=4; % 1: reversal; 2: control
    #  Mres=5; % left or right key. counterbalance across subjects;
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
  
  #res<-get.sub.rl.data(paste0("RL_behav/",sub.rec.filenames[1]))
  rl.subjects.f1.list<-do.call("rbind",lapply(paste0("../data/RL_behav/",sub.rec.filenames.f1),get.sub.rl.data,"../data/RL_behav"))
  rl.subjects.f2.list<-do.call("rbind",lapply(paste0("../data/ReversalLearning/new/",sub.rec.filenames.f2),get.sub.rl.data,"../data/ReversalLearning/new"))
  rl.subjects.f3.list<-do.call("rbind",lapply(paste0("../data/ReversalLearning/results/",sub.rec.filenames.f3),get.sub.rl.data,"../data/ReversalLearning/results"))
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
rl.all.subjects.list[,presentation_n_over_segments:=presentation_n_in_segment]
last_nonreversal<-max(rl.all.subjects.list[reversal_trial==FALSE,presentation_n_in_segment])
#aligns so that for every subject, we give the same index to the first reversal trial
rl.all.subjects.list[reversal_trial==TRUE,presentation_n_over_segments:=presentation_n_in_segment+last_nonreversal]
# rl.all.subjects.list[1:100]
# length(unique(rl.all.subjects.list[Motivation=="reward",subid]))
# length(unique(rl.all.subjects.list[Motivation=="punishment",subid]))

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
accuracy.by.pres_seg.subid.finalpc<-
  accuracy.by.pres_seg.subid[presentation_n_over_segments==4,.(final.prop.correct=prop.correct),.(subid,Motivation)]
accuracy.by.pres_seg.subid<-merge(accuracy.by.pres_seg.subid,accuracy.by.pres_seg.subid.finalpc,by=c("subid","Motivation"))

