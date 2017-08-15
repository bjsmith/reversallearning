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

source("rl_behav_analysis_learning_setup.R")

######################
#align by pre-post reversal
break.labels=c("1\nPre-reversal",2:8,"1\nReversal",2:5)


accuracy.by.pres_seg.subid<-rl.all.subjects.list[,.(prop.correct=sum(correct)/.N,count=.N),.(presentation_n_over_segments,subid,Motivation)]

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


reversal_learning_timeline_ggplot_commands<-
  list(scale_x_continuous(breaks=1:length(break.labels),labels=break.labels),
       labs(x="Presentation"),
       theme(axis.text.x = element_text(hjust=0),
             axis.text = element_text(face="bold"),#left-align presentation labels
             strip.text = element_text(face="bold"),
             legend.position = "bottom"
       ))
#accuracy.by.pres_seg.subid.finalpc<-accuracy.by.pres_seg.subid[presentation_n_over_segments==13,.(final.prop.correct=prop.correct),subid]
#accuracy.by.pres_seg.subid<-merge(accuracy.by.pres_seg.subid,accuracy.by.pres_seg.subid.finalpc,by="subid")


#mean across subjects
accuracy.by.pres_seg.subid.summary<-accuracy.by.pres_seg.subid[
  presentation_n_over_segments<=13,
  .(prop.correct.m=mean(prop.correct),
    prop.correct.sd=sd(prop.correct)),
  .(presentation_n_over_segments,Motivation)]
  
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
    title=paste0("proportion correct across all images by user\n from start to finish of reversal learning"))+
  geom_line(data=accuracy.by.pres_seg.subid.summary,aes(x=presentation_n_over_segments,y=prop.correct.m,group=NULL))+
      facet_grid(Motivation ~ .)+
  #theme(strip.text.y=element_text(colour="orange"))+
  reversal_learning_timeline_ggplot_commands+
  geom_hline(data=accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==5],
             aes(yintercept = prop.correct.m),
             linetype=2)+
  geom_hline(data=accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==13],
             aes(yintercept = prop.correct.m),
             linetype=2)

main.prop.cor.ggplot


#it's this shape because for the last few time points, subjects have very few trials (images) over which the score is averaged
#we don't have any blank spaces in this graph because *all* subjects had *a few * 6,7,8 trials. Just not very many.
#this is athe better way to analyze the data, though. The pre-reversal trials should definitely be aligned by the trials since first trial,
#not trials before reversal.

#total accuracy

ggplot(data=accuracy.by.pres_seg[
  !is.na(presentation_n_over_segments)],
  aes(x=presentation_n_over_segments,y=prop.correct,colour=Motivation))+geom_line()+
  reversal_learning_timeline_ggplot_commands+
  labs(title=paste0("Proportion of correct images at each point across all subjects and images"))

#OK. Let's try to plot the average subject performance
ggplot(accuracy.by.pres_seg.subid.summary,
       aes(x=presentation_n_over_segments,y=prop.correct.m,colour=Motivation))+#geom_jitter(width = 0.4, height = 0.04,alpha=0.1)+
  geom_line()+
  geom_errorbar(aes(ymin=prop.correct.m-prop.correct.sd,ymax=prop.correct.m+prop.correct.sd))+
  reversal_learning_timeline_ggplot_commands+
  labs(title=
         paste0("Mean and standard deviation of subject mean at each time point"))

  
  ##need a matrix of % correct where each row is a subject, each column is a presentation.
  accuracy.by.pres_seg.subid.mat<-tidyr::spread(
    accuracy.by.pres_seg.subid[,.(presentation_n_over_segments,prop.correct,subid,Motivation)]
    ,"presentation_n_over_segments","prop.correct","Motivation")
   accuracy.by.pres_seg.subid.mat.presentations<-data.frame(
     accuracy.by.pres_seg.subid.mat[,2+1:13,with=FALSE])
  
  accuracy.by.pres_seg.subid.mat.improvement<-
    cbind(accuracy.by.pres_seg.subid.mat[,1:2,with=FALSE],
    apply(accuracy.by.pres_seg.subid.mat.presentations[,2:13],2,as.numeric)-
    apply(accuracy.by.pres_seg.subid.mat.presentations[,1:12],2,as.numeric)
          )
  
  colnames(accuracy.by.pres_seg.subid.mat.improvement)<-
    c(colnames(accuracy.by.pres_seg.subid.mat.improvement)[1:2],
      paste0("PreR",c(2:8)),paste0("PostR",c(1:5)))
    
  #so at each level, did subjects improve significantly?
  dim(accuracy.by.pres_seg.subid.mat.improvement)
  
  #let's plot confidence intervals of improvements at each time point across subjects
  accuracy.by.pres_seg.subid.mat<-data.table(tidyr::gather(
    accuracy.by.pres_seg.subid.mat.improvement,"Presentation","Improvement",3:14))
  #improvements.by.timepoint<-data.frame(t(apply(accuracy.by.pres_seg.subid.mat.improvement[,3:14,with=FALSE],2,function(x){unlist(t.test(x))})))
  #improvements.by.timepoint<-accuracy.by.pres_seg.subid.mat[,.(MeanImprovement:=data.table((t.test(Improvement)))),.(Motivation,Presentation)]
  improvements.by.timepoint<-rbind(
    by(accuracy.by.pres_seg.subid.mat$Improvement,
       list(accuracy.by.pres_seg.subid.mat$Motivation,
            accuracy.by.pres_seg.subid.mat$Presentation)
       ,t.test))
  improvements.by.timepoint.df<-NULL
  for (r in 1:dim(improvements.by.timepoint)[1]){
    for (c in 1:dim(improvements.by.timepoint)[2]){
      rc.df<-data.frame(t(unlist(improvements.by.timepoint[r,c])))
      rc.df$Motivation<-rownames(improvements.by.timepoint)[r]
      rc.df$Presentation<-colnames(improvements.by.timepoint)[c]
      if(is.null(improvements.by.timepoint.df)){
        improvements.by.timepoint.df<-rc.df
      }else{
        improvements.by.timepoint.df<-rbind(improvements.by.timepoint.df,rc.df)
      }
    }
  }
  improvements.by.timepoint.df$Presentation<-
    as.numeric(sapply(improvements.by.timepoint.df$Presentation,
       function(x){
         return(sub(x,pattern = "X",
             replacement = ""))
       }))
  for(i in 1:7){
    improvements.by.timepoint.df[,i]<-as.numeric(as.character(improvements.by.timepoint.df[,i]))
  }
  
  ggplot(improvements.by.timepoint.df,
         aes(x=Presentation,y=estimate.mean.of.x,group=data.name,
             colour=Motivation))+#geom_jitter(width = 0.4, height = 0.04,alpha=0.1)+
    geom_line()+
    geom_errorbar(aes(ymin=conf.int1,ymax=conf.int2))+
    reversal_learning_timeline_ggplot_commands+coord_cartesian(ylim=c(-0.1,0.3))+
    labs(title=paste0(
      "improvement in subject performance at each time point\nConfidence intervals of means across subjects"),
         y="Improvement in subject performance\nover previous time point")+
    geom_hline(aes(yintercept=0),linetype=2)
  
  
  
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
  ggplot(data=merge(
    accuracy.by.pres_seg.subid[presentation_n_over_segments==12,.(prop.correct,Motivation,subid)],
    accuracy.by.pres_seg.subid.mat.improvement[,.(X13,Motivation,subid)],
    by=c("Motivation","subid")
    ),
    aes(x=prop.correct,y=X13,colour=Motivation)
  )+geom_jitter(width=0.03,height=0.03,alpha=0.6)+
    geom_smooth(method="lm")+
    theme(legend.position="bottom")+
    labs(title=paste0("Score improvement at Post-reversal round 5\nby Score at Post-reversal round 4"),
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
  labs(title=paste0("Odds ratio of correct response at time point\ngiven a correct response at previous time point\nover all subjects and trials\n(Reward and punishment trials)"),
       y="OR improvement over previous timepoint")+
  geom_vline(aes(xintercept=9),linetype=2)



#accuracy by image.
library(ggplot2)

accuracy.by.pres_seg.image.finalpc<-
  accuracy.by.pres_seg.image[presentation_n_over_segments==4,.(final.prop.correct=prop.correct),.(trial,Motivation)]
accuracy.by.pres_seg.image<-merge(accuracy.by.pres_seg.image,accuracy.by.pres_seg.image.finalpc,by=c("trial","Motivation"))

library(ggplot2)
main.prop.cor.ggplot<-
  ggplot(accuracy.by.pres_seg.image[!is.na(presentation_n_over_segments)],
         aes(x=presentation_n_over_segments,y=prop.correct,group=trial))+
  geom_line(aes(colour=final.prop.correct),size=1.5,alpha=0.3)+ scale_colour_gradientn(colours=c("red","green","blue","violet"))+
  #scale_x_continuous(breaks=-8:4,labels=break.labels)+
  labs(#x="Presentation",
    y="Proportion correct across all users by image",
    title=paste0("proportion correct across all users by image\n from start to finish of reversal learning"))+
  geom_line(data=accuracy.by.pres_seg.subid.summary,aes(x=presentation_n_over_segments,y=prop.correct.m,group=NULL))+
  facet_grid(Motivation ~ .)+
  #theme(strip.text.y=element_text(colour="orange"))+
  reversal_learning_timeline_ggplot_commands+
  geom_hline(data=accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==5],
             aes(yintercept = prop.correct.m),
             linetype=2)+
  geom_hline(data=accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==13],
             aes(yintercept = prop.correct.m),
             linetype=2)

main.prop.cor.ggplot
