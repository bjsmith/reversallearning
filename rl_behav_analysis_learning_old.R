#install.packages("R.matlab")
library(R.matlab)
#install.packages('data.table')
library(data.table)
#install.packages('ggplot2')
library(ggplot2)
#install.packages("tidyr")
library('tidyr')

#install.packages("corrplot")
library(corrplot)

for (condition in c(
  "reward"#,
  #"punishment"
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
  sub.rec.filenames.f1<-list.files(path="RL_behav/",pattern=filename.pattern,ignore.case=TRUE)
  sub.rec.filenames.f2<-list.files(path="ReversalLearning/new",pattern=filename.pattern,ignore.case=TRUE)
  sub.rec.filenames.f3<-list.files(path="ReversalLearning/results",pattern=filename.pattern,ignore.case=TRUE)
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
  rl.subjects.f1.list<-do.call("rbind",lapply(paste0("RL_behav/",sub.rec.filenames.f1),get.sub.rl.data,"RL_behav"))
  rl.subjects.f2.list<-do.call("rbind",lapply(paste0("ReversalLearning/new/",sub.rec.filenames.f2),get.sub.rl.data,"ReversalLearning/new"))
  rl.subjects.f3.list<-do.call("rbind",lapply(paste0("ReversalLearning/results/",sub.rec.filenames.f3),get.sub.rl.data,"ReversalLearning/results"))
  unique(rl.subjects.f1.list$subid)
  length(unique(rl.subjects.f1.list$subid))
  length(unique(rl.subjects.f2.list$subid))
  unique(rl.subjects.f3.list$subid)
  setdiff(unique(rl.subjects.f1.list$subid),unique(rl.subjects.f2.list$subid))
  #RL_behav contains 112, which ReversalLearning/new/ does not.
  setdiff(unique(rl.subjects.f3.list$subid),unique(rl.subjects.f2.list$subid))
  #ReversalLearning/results/ contains three subjects, 155, 262, and 390, which ReversalLearning/new/ does not.
  
  # 
  # View(rl.subjects.f1.list[subid==112])
  # rl.subjects.f1.list$subid
  # rl.subjects.f1.list[,.(count=.N),.(subid)]
  # View(rl.subjects.f1.list[,.(count=.N),.(subid,runid)])
  # View(rl.subjects.f2.list[,.(count=.N),.(subid)])
  # View(rl.subjects.f3.list[,.(count=.N),.(subid)])
  #for now, let's use ReversalLeraning/New and randomly combine the missing ones from the other sets.
  rl.all.subjects.list<-rbind(rl.subjects.f2.list,rl.subjects.f1.list[subid==112],rl.subjects.f3.list[subid %in% c(155,262,390)])
  rl.all.subjects.list[,presentation_n_over_segments:=presentation_n_in_segment]
  last_nonreversal<-max(rl.all.subjects.list[reversal_trial==FALSE,presentation_n_in_segment])
  rl.all.subjects.list[reversal_trial==TRUE,presentation_n_over_segments:=presentation_n_in_segment+last_nonreversal]
  rl.all.subjects.list[1:100]
  length(unique(rl.all.subjects.list$subid))
  #we need to align these by trials subsequent or before the reversal trial
  accuracy.by.pres.allsubs<-rl.all.subjects.list[,.(prop.correct=sum(correct)/.N,count=.N),presentation_n_after_reversal]
  
  #actually, this won't really get at what we want with the pre-reversal learning
  #becuase the number of pre-reversal trials varies.
  #and it we want to track learning by number of trials, it makes sense to count from the first trial learning begins, not steps
  #before reversal learning
  
  # ggplot(accuracy.by.pres.allsubs[presentation_n_after_reversal<5&presentation_n_after_reversal>=-8],
  #        aes(x=presentation_n_after_reversal,y=prop.correct,color=sub.id))+
  #   geom_line()
  
  
  
  
  #by subject across subject runs.
  accuracy.by.pres.subid<-rl.all.subjects.list[,.(prop.correct=sum(correct)/.N,count=.N),.(presentation_n_after_reversal,subid)]
  
  #mean across subjects
  accuracy.by.pres.subid.summary<-accuracy.by.pres.subid[
    presentation_n_after_reversal<5&presentation_n_after_reversal>=-8,
    .(prop.correct.m=mean(prop.correct),
      prop.correct.sd=sd(prop.correct),
      prop.correct.p25=rank(prop.correct)/.N),
    .(presentation_n_after_reversal)]
  
  
  
  #we are intersted in SD here, not SE
  ggplot(accuracy.by.pres.subid.summary,aes(x=presentation_n_after_reversal,y=prop.correct.m))+#geom_jitter(width = 0.4, height = 0.04,alpha=0.1)+
    geom_line()+
    geom_errorbar(aes(ymin=prop.correct.m-prop.correct.sd,ymax=prop.correct.m+prop.correct.sd))
  #actually though we are intersted in percentiles
  
  
  #individual subjects
  
  accuracy.by.pres.subid.finalpc<-accuracy.by.pres.subid[presentation_n_after_reversal==4,.(final.prop.correct=prop.correct),subid]
  accuracy.by.pres.subid<-merge(accuracy.by.pres.subid,accuracy.by.pres.subid.finalpc,by="subid")
  #break.labels=c("1\nPre-reversal",2:8,"1\nReversal",2:5)
  pc.cor.main.ggplot<-
    ggplot(accuracy.by.pres.subid[presentation_n_after_reversal<5&presentation_n_after_reversal>=-8],
         aes(x=presentation_n_after_reversal,y=prop.correct,group=subid))+
    geom_line(aes(colour=final.prop.correct),size=1.5,alpha=0.3)+ scale_colour_gradientn(colours=c("red","green","blue","violet"))+
    #scale_x_continuous(breaks=-8:4,labels=break.labels)+
    labs(#x="Presentation",
         y="Proportion correct across all images by user",
         title=paste0("proportion correct across all images by user\n from start to finish of reversal learning",graph.title.condition.suffix))+
    geom_line(data=accuracy.by.pres.subid.summary,aes(x=presentation_n_after_reversal,y=prop.correct.m,group=NULL))+
    reversal_learning_timeline_ggplot_commands
  pc.cor.main.ggplot
  #whether the trend this is because:
  #  different subjects reach high performance at different rates or
  #because all subjects just get consistently better across trials.
  #how to test?
  
  #alternative hypotheses.
  #if the first is true, then within each subject we would see a sharp increase in percent correct at a few steps followed by not much additional improvement
  
  #If the second is true, then within each subject we would see a gradual increase in percent correct across lots of steps.
  
  #but we can't just take the average improvement at each step. That won't tell us anything...what are we looking at?
  #in *how many steps* does each subject improve performance, from R2 to R5?
  
  #need a matrix of % correct where each row is a subject, each column is a presentation.
  accuracy.by.pres.subid.mat<-tidyr::spread(
    accuracy.by.pres.subid[,.(presentation_n_after_reversal,prop.correct,subid)]
    ,"presentation_n_after_reversal","prop.correct")
  accuracy.by.pres.subid.mat.presentations<-data.frame(
    accuracy.by.pres.subid.mat[,2:14,with=FALSE])
  
  accuracy.by.pres.subid.mat.improvement<-
    accuracy.by.pres.subid.mat.presentations[,2:13]-accuracy.by.pres.subid.mat.presentations[,1:12]
  
  #now, did subjects' improvement happen in a distributed manner, or all at once, or not at all?
  #this is how much subjects improved over the three runs...
  last.3.steps<-accuracy.by.pres.subid.mat.improvement[,c("X2","X3","X4")]
  rowSums(last.3.steps)
  
  #how many subjects did more than half of their improving in a single step?
  sum(last.3.steps>rowSums(last.3.steps)/2)/dim(last.3.steps)
  
  #At which step did participants reach a mean of 75% accuracy, during learning and post-reversal learning?
  #75% accuracy was NOT reached by the mean of subjects at any point.
  #At which step can we reject null hypothesis that our participants did not score better than chance?
  #for this, we can probably add up proportion correct across ALL subjects
  #so get a measure of proportion correct across ALL subjects by trial
  #summary(rl.all.subjects.list)
  accuracy.by.pres<-rl.all.subjects.list[
    ,.(prop.correct=sum(correct)/.N,correct=sum(correct),count=.N),.(presentation_n_after_reversal)]
  
  
  
  ######################
  #align by pre-post reversal
  break.labels=c("1\nPre-reversal",2:8,"1\nReversal",2:5)
  reversal_learning_timeline_ggplot_commands<-
    list(scale_x_continuous(breaks=1:length(break.labels),labels=break.labels),
         labs(x="Presentation"),
         theme(axis.text.x = element_text(hjust=0),
               axis.text = element_text(face="bold")#left-align presentation labels
         ))
  
  accuracy.by.pres_seg.subid<-rl.all.subjects.list[,.(prop.correct=sum(correct)/.N,count=.N),.(presentation_n_over_segments,subid)]
  
  accuracy.by.pres_seg<-
    rl.all.subjects.list[,.(prop.correct=sum(correct)/.N,count=.N),.(presentation_n_over_segments)]
  
  rl.all.subjects.list[1:100]
  #mean across subjects
  accuracy.by.pres_seg.subid.summary<-accuracy.by.pres_seg.subid[
    ,
    .(prop.correct.m=mean(prop.correct),
      prop.correct.sd=sd(prop.correct),
      prop.correct.p25=rank(prop.correct)/.N),
    .(presentation_n_over_segments)]
  
  
  break.labels=c("1\nPre-reversal",2:last_nonreversal,"1\nReversal",2:6)
  
  #accuracy.by.pres_seg.subid.finalpc<-accuracy.by.pres_seg.subid[presentation_n_over_segments==13,.(final.prop.correct=prop.correct),subid]
  #accuracy.by.pres_seg.subid<-merge(accuracy.by.pres_seg.subid,accuracy.by.pres_seg.subid.finalpc,by="subid")
  
  accuracy.by.pres_seg.subid.finalpc<-accuracy.by.pres_seg.subid[presentation_n_over_segments==4,.(final.prop.correct=prop.correct),subid]
  accuracy.by.pres_seg.subid<-merge(accuracy.by.pres_seg.subid,accuracy.by.pres_seg.subid.finalpc,by="subid")
  
  
  #mean across subjects
  accuracy.by.pres_seg.subid.summary<-accuracy.by.pres_seg.subid[
    presentation_n_over_segments<=13,
    .(prop.correct.m=mean(prop.correct),
      prop.correct.sd=sd(prop.correct)),
    presentation_n_over_segments]
  
  # accuracy.by.pres_seg.subid[subid==204,]
  # View(rl.all.subjects.list[subid==204 & (trial==94 | trial==93),])
  # View(rl.all.subjects.list[subid==204,])
  # View(rl.all.subjects.list[,.(fileid,runid,subid),.(subid,runid)])
  # table(rl.all.subjects.list[,.(runid,subid)])
  # accuracy.by.pres_seg.subid[1:100]
  main.prop.cor.ggplot<-
    ggplot(accuracy.by.pres_seg.subid[!is.na(presentation_n_over_segments)],
         aes(x=presentation_n_over_segments,y=prop.correct,group=subid))+
    geom_line(aes(colour=final.prop.correct),size=1.5,alpha=0.3)+ scale_colour_gradientn(colours=c("red","green","blue","violet"))+
    #scale_x_continuous(breaks=-8:4,labels=break.labels)+
    labs(#x="Presentation",
      y="Proportion correct across all images by user",
      title=paste0("proportion correct across all images by user\n from start to finish of reversal learning",
                   graph.title.condition.suffix))+
    geom_line(data=accuracy.by.pres_seg.subid.summary,aes(x=presentation_n_over_segments,y=prop.correct.m,group=NULL))+
    scale_x_continuous(breaks=1:length(break.labels),labels=break.labels)+
        labs(x="Presentation")+
    geom_hline(yintercept = accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==5,prop.correct.m],
               linetype=2)+
    geom_hline(yintercept = accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==13,prop.correct.m],
               linetype=2)
  
  main.prop.cor.ggplot
  #it's this shape because for the last few time points, subjects have very few trials (images) over which the score is averaged
  #we don't have any blank spaces in this graph because *all* subjects had *a few * 6,7,8 trials. Just not very many.
  #this is athe better way to analyze the data, though. The pre-reversal trials should definitely be aligned by the trials since first trial,
  #not trials before reversal.
  
  #total accuracy
  
  ggplot(data=accuracy.by.pres_seg[
    !is.na(presentation_n_over_segments)],
    aes(x=presentation_n_over_segments,y=prop.correct))+geom_line()+
    reversal_learning_timeline_ggplot_commands+
    labs(title=paste0("Proportion of correct images at each point across all subjects and images",
         graph.title.condition.suffix))
  
  #OK. Let's try to plot the average subject performance
  ggplot(accuracy.by.pres_seg.subid.summary,aes(x=presentation_n_over_segments,y=prop.correct.m))+#geom_jitter(width = 0.4, height = 0.04,alpha=0.1)+
    geom_line()+
    geom_errorbar(aes(ymin=prop.correct.m-prop.correct.sd,ymax=prop.correct.m+prop.correct.sd))+
    reversal_learning_timeline_ggplot_commands+
    labs(title=
           paste0("Mean and standard deviation of subject mean at each time point",
                  graph.title.condition.suffix))
  
  
  ##need a matrix of % correct where each row is a subject, each column is a presentation.
  accuracy.by.pres_seg.subid.mat<-tidyr::spread(
    accuracy.by.pres_seg.subid[,.(presentation_n_over_segments,prop.correct,subid)]
    ,"presentation_n_over_segments","prop.correct")
  accuracy.by.pres_seg.subid.mat.presentations<-data.frame(
    accuracy.by.pres_seg.subid.mat[,2:14,with=FALSE])
  
  accuracy.by.pres_seg.subid.mat.improvement<-
    accuracy.by.pres_seg.subid.mat.presentations[,2:13]-accuracy.by.pres_seg.subid.mat.presentations[,1:12]
  
  colnames(accuracy.by.pres_seg.subid.mat.improvement)<-
    c(paste0("PreR",c(2:8)),paste0("PostR",c(1:5)))
    
  #so at each level, did subjects improve significantly?
  dim(accuracy.by.pres_seg.subid.mat.improvement)
  
  #let's plot confidence intervals of improvements at each time point across subjects
  t.test(accuracy.by.pres_seg.subid.mat.improvement[,1])
  t.test(accuracy.by.pres_seg.subid.mat.improvement[,2])
  t.test(accuracy.by.pres_seg.subid.mat.improvement[,3])
  t.test(accuracy.by.pres_seg.subid.mat.improvement[,4])
  t.test(accuracy.by.pres_seg.subid.mat.improvement[,5])
  t.test(accuracy.by.pres_seg.subid.mat.improvement[,6])
  improvements.by.timepoint<-data.frame(t(apply(accuracy.by.pres_seg.subid.mat.improvement,2,function(x){unlist(t.test(x))})))
  improvements.by.timepoint$ImprovementRound<-2:13
  for(i in 1:7){
    improvements.by.timepoint[,i]<-as.numeric(as.character(improvements.by.timepoint[,i]))
  }
  
  ggplot(improvements.by.timepoint,aes(x=ImprovementRound,y=estimate.mean.of.x,group=data.name))+#geom_jitter(width = 0.4, height = 0.04,alpha=0.1)+
    geom_line()+
    geom_errorbar(aes(ymin=conf.int1,ymax=conf.int2))+
    reversal_learning_timeline_ggplot_commands+coord_cartesian(ylim=c(-0.1,0.3))+
    labs(title=paste0(
      "improvement in subject performance at each time point\nConfidence intervals of means across subjects",
      graph.title.condition.suffix),
         y="Improvement in subject performance\nover previous time point")+
    geom_hline(aes(yintercept=0),linetype=2)
  improvements.by.timepoint[11,]
  
  
  #how much did reversal learning correlate with initial learning?
  #let's exclude steps 6-8 because there were a smaller number of trials that went for that long.
  #looking at *per subject* scores, how much did subject 5th post-reversal accuracy correlate with
  #5th pre-reversal step accuracy?
  cor.test(accuracy.by.pres_seg.subid[presentation_n_over_segments==5]$prop.correct,
           accuracy.by.pres_seg.subid[presentation_n_over_segments==13]$prop.correct)
  #pretty strongly
  #out of interest, how much does BAD performance at reversal correlate with GOOD performance post-reversal?
  cor.test(accuracy.by.pres_seg.subid[presentation_n_over_segments==9]$prop.correct,
           accuracy.by.pres_seg.subid[presentation_n_over_segments==13]$prop.correct)
  
  cor.test(accuracy.by.pres_seg.subid[presentation_n_over_segments==9]$prop.correct,
           accuracy.by.pres_seg.subid[presentation_n_over_segments==10]$prop.correct)
  
  
  main.prop.cor.ggplot+
    geom_vline(aes(xintercept=5),linetype=2)+geom_vline(aes(xintercept=13),linetype=2)
  
  #Do all subjects tend to learn reversal incrementally or do the best plateau early?
  #To answer this we want to know: how does *improvement* at each time point correlate?
  dim(accuracy.by.pres_seg.subid.mat.improvement)
  corrplot(cor(accuracy.by.pres_seg.subid.mat.improvement),method="number")
  cor.test(accuracy.by.pres_seg.subid.mat.improvement$PostR4,accuracy.by.pres_seg.subid.mat.improvement$PostR5)
  
  #OK but follow-up question: is *performance* at PostR4 also negatively correlated with *improvement* at PostR5
  #in other words, do we have evidence that this is due to bad participants 'catching up'?
  cor.test(accuracy.by.pres_seg.subid[presentation_n_over_segments==12,prop.correct],
           accuracy.by.pres_seg.subid.mat.improvement$PostR5)
  #yes, thre is a moderate relationship between performance at PostR4 and 
  ggplot(data=data.frame(
    "PostR4PropCor"=accuracy.by.pres_seg.subid[presentation_n_over_segments==12,prop.correct],
    "PostR4R5Improvement"=accuracy.by.pres_seg.subid.mat.improvement$PostR5),
    aes(x=PostR4PropCor,y=PostR4R5Improvement)
  )+geom_jitter(width=0.03,height=0.03,alpha=0.6)+
    geom_smooth(method="lm")+
    labs(title=paste0("Score improvement at Post-reversal round 5\nby Score at Post-reversal round 4",
                      graph.title.condition.suffix),
         x="Round 4 Proportion Correct",y="Round 5 Improvement")
  
  # Do subjects tend to learn the reversal association incrementally, or do they have a sudden ‘eureka’ moment for each association and then subsequently get that particular association right each time?
  # 
  # We would have to examine each individual subject*trial, and see - does having got a particular trial right predict being more likely to get that particular trial right in subsequent rounds?
  # We could ask that question for each round, looking across all subjects and trial. Being correct is a binary outcome so we’re thinking of logistic regression - basically, at each round, across all subjects and trials, does getting it right the previous time predict getting it right the next time?
  dim(rl.all.subjects.list)
  colnames(rl.all.subjects.list)
  #so let's start by trying to predict PreR3 from PreR2.
  #we need to keep in mind that subjects are not independent so there is a confound here and I am not exactly sure how to deal with tht
  #one approach would be to calculate a raw score within-subject for each image and then do that across subjects
  #that would be very *underpowered* but it would be valid.
  #but let's start with the easy design
  library(epitools)
  break.labels.alt<-c(paste0("PreR",c(1:8)),paste0("PostR",c(1:5)))
  or.progression<-NULL
  timestep.range<-c(2:5,10:13)
  for (i in timestep.range){
    print(paste0("Odds ratio for correct performance at T",break.labels.alt[i]," compared to T", break.labels.alt[(i-1)]))
    # predict.3.from.2<-glm(rl.all.subjects.list[presentation_n_over_segments==i,correct]~
    #                         rl.all.subjects.list[presentation_n_over_segments==(i-1),correct],family=binomial(link='logit'))
    # 
    #actually, with two binary variables, a chi-square should do.
    presentations<-
      data.frame("Presentation1"=rl.all.subjects.list[presentation_n_over_segments==(i-1),correct],
                 "Presentation2"=rl.all.subjects.list[presentation_n_over_segments==(i),correct])
    
    print(chisq.test(table(presentations)))
    #install.packages("epitools")
    or.res<-oddsratio.wald(presentations$Presentation1,presentations$Presentation2)
    print(or.res)
    #odds ratio for getting it right in round 3 is 1.97 times getting it right in round 2
    if(is.null(or.progression)){
      or.progression<-as.data.frame(t(or.res$measure[2,1:3]))
    }else{
      or.progression<-rbind(or.progression,as.data.frame(t(or.res$measure[2,1:3])))
    }
  }
  or.progression$timestep<-break.labels.alt[timestep.range]
  or.progression$index<-timestep.range
  
  ggplot(or.progression,aes(y=estimate,x=index))+#geom_jitter(width = 0.4, height = 0.04,alpha=0.1)+
    geom_point()+
    geom_errorbar(aes(ymin=lower,ymax=upper))+
    reversal_learning_timeline_ggplot_commands+
    labs(title=paste0("Odds ratio of correct response at time point\ngiven a correct response at previous time point\nover all subjects and trials",
                      graph.title.condition.suffix),
         y="OR improvement over previous timepoint")+
    geom_vline(aes(xintercept=9),linetype=2)


}
