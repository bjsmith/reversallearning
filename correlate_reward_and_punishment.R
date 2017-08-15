#install.packages("R.matlab")
library(R.matlab)
#install.packages('data.table')
library(data.table)
#install.packages('ggplot2')
library(ggplot2)
#install.packages("tidyr")
library(tidyr)

#install.packages("corrplot")
library(corrplot)
source("rl_behav_analysis_learning_setup.R")

#now the question from Emily Barkley-Levenson is: does performance on the reward task correlate with performance on the punishment task across subjects?
#the most principled way to do this is by treating each trial as a separate sample
#but a graphical way to do this is to get the performance on each trial*subject
#and look at correlation between reward and punishment for each subject, at each time point.
#so...
accuracy.by.pres_seg_subid_rp<-spread(accuracy.by.pres_seg.subid[!is.na(presentation_n_over_segments),.(subid,Motivation,presentation_n_over_segments,prop.correct)],Motivation,prop.correct)
#we can plot across time the simple CORRELATION across subjects
accuracy.by.pres_seg_subid_rp.cor<-accuracy.by.pres_seg_subid_rp[,cor.test(punishment,reward,na.rm=TRUE,),by=presentation_n_over_segments]
accuracy.by.pres_seg_subid_rp[,cor.test(punishment,reward,na.rm=TRUE),by=presentation_n_over_segments] %>% 
  merge(.[seq(1,nrow(.),2),],.[seq(1,nrow(.),1)],
        by="p.value",
                               #,
                              #  by.y=colnames(.)[!colnames(.) %in% c("conf.int")],
                               suffixes=c(".lower",".upper"))

tm<-accuracy.by.pres_seg_subid_rp[,cor.test(punishment,reward,na.rm=TRUE),by=presentation_n_over_segments] #%>% 
mergecols<-colnames(tm)[!colnames(tm) %in% c("conf.int")]
accuracy.by.pres_seg_subid_rp_cor<-merge(tm[seq(1,nrow(tm),2),],tm[seq(2,nrow(tm),2),],
      by.x=mergecols,by.y=mergecols,
      suffixes=c(".lower",".upper"))

ggplot(accuracy.by.pres_seg_subid_rp_cor,aes(x=presentation_n_over_segments,y=estimate))+
  labs(#x="Presentation",
    y="R correlation estimate with 95% CI",
    title=paste0("Estimate of Reward-Punishment correlation across subjects\nat each presentation step"))+
  geom_line()+
  geom_errorbar(aes(ymin=conf.int.lower,ymax=conf.int.upper),color="#999999")+
  geom_point()+
  reversal_learning_timeline_ggplot_commands
break.labels.2=c(paste("Pre-rev",1:8),paste("Rev",1:5))
accuracy.by.pres_seg_subid_rp$presentation_n_over_segments<-
  factor(accuracy.by.pres_seg_subid_rp$presentation_n_over_segments,
         levels = 1:13,labels=break.labels.2)
ggplot(accuracy.by.pres_seg_subid_rp,aes(x=punishment,y=reward))+
  facet_wrap(~presentation_n_over_segments)+geom_segment(aes(x=0,y=0,xend=1,yend=1),linetype=2,size=0.02,alpha=0.6)+
  labs(x="Reward Performance across all trials by subject",
    y="Punishment performance across all trials by subject",
    title=paste0("Reward and Punishment performance for each subject"))+
  geom_smooth(method = "lm")+
  geom_jitter(width = 0.03,height=0.03,alpha=0.4)
  
  
