break.labels=c("1\nPre-reversal",2:8,"1\nReversal",2:5)
reversal_learning_timeline_ggplot_commands<-
  list(scale_x_continuous(breaks=-8:4,labels=break.labels),
       labs(x="Presentation"),
       theme(axis.text.x = element_text(hjust=0),
             axis.text = element_text(face="bold")#left-align presentation labels
       ))


reversal_learning_timeline_ggplot_commands<-
  list(scale_x_continuous(breaks=1:length(break.labels),labels=break.labels),
       labs(x="Presentation"),
       theme(axis.text.x = element_text(hjust=0),
             axis.text = element_text(face="bold"),#left-align presentation labels
             strip.text = element_text(face="bold"),
             legend.position = "bottom"
       ))