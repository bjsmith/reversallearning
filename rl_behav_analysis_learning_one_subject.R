#install.packages("R.matlab")
library(R.matlab)
#install.packages('data.table')
library(data.table)
#install.packages('ggplot2')
library(ggplot2)
mat.data<-readMat("RL_behav/rlp_sub102_run1_09-Mar_10-18.mat")
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

sub.data.table[,presentation_n:=.(order(onset_time_actual)),by=image]

#now plot % correct scores for each presentation across trials
sub.data.table[,correct:=.(score==1)]
#this will consider a "no response" as incorrect

#now we want to calculate proportion of correct responses ACROSS trials for each presentation.
sub.data.table[,sum(correct)/length(correct),presentation_n]
sub.accuracy.by.pres<-sub.data.table[,.(prop.correct=sum(correct)/.N),presentation_n]


ggplot(sub.accuracy.by.pres,aes(x=presentation_n,y=prop.correct))+geom_line()
