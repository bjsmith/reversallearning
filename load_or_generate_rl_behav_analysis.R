rDataFileName="rl-behavioral-graph-data.Rdata"
if(!file.exists(rDataFileName)){
    source("rl_behav_analysis_learning_setup.R")
    
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
    
    
    accuracy.by.pres_seg.subid.finalpc<-
      accuracy.by.pres_seg.subid[presentation_n_over_segments==4,.(final.prop.correct=prop.correct),.(subid,Motivation)]
    accuracy.by.pres_seg.subid<-merge(accuracy.by.pres_seg.subid,accuracy.by.pres_seg.subid.finalpc,by=c("subid","Motivation"))
    save(accuracy.by.pres_seg.subid,file=rDataFileName)
}else{
  load(rDataFileName)
}

break.labels=c("1\nPre-reversal",2:last_nonreversal,"1\nReversal",2:6)

reversal_learning_timeline_ggplot_commands<-
  list(scale_x_continuous(breaks=1:length(break.labels),labels=break.labels),
       labs(x="Presentation"),
       theme(axis.text.x = element_text(hjust=0),
             axis.text = element_text(face="bold"),#left-align presentation labels
             strip.text = element_text(face="bold"),
             legend.position = "bottom"
       ))
