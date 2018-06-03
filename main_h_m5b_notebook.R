library(ggplot2)
library(data.table)
alpha_s2_r1<-data.table(t(theta[,param.l1.ids$alpha,2,1,]))
colnames(alpha_s2_r1)<-paste0("Chain",1:24)
alpha_s2_r1l<-data.table(tidyr::gather(alpha_s2_r1,Chain,Alpha,1:24))

alpha_s2_r1l[,.(i=1:.N),Chain]
ggplot(alpha_s2_r1l,aes(x=i,y=Alpha,color=Chain))+geom_line()
View(alpha_s2_r1)
#so nothing for this subject is updated. Why wouldn't it be? What's preventing us from ever finding a better weight?

ggplot(use.theta.dt.l[,.(N=1:.N,Value=Value),by=Param],aes(y=Value,x=Param,group=as.factor(N),color=as.factor(N)))+geom_line()

theta
#find theta chains that are stuck

apply(theta,2,function(x){paste0(as.character(dim(x)),collapse=",")})

dim(theta)
apply(theta[,1,,],c(2,3),function(x){sd(x)})
#no sign of the tightly constructed thetas we see in the generated output.

#turns out run data is not independent. Run2 reliably has better performance than run 1, and run 4 than run 3.

#is this practice effects or reward v. punishment?
unlist(lapply(data,function(d){mean(d$runs[[2]]$outcome,na.rm=TRUE)-mean(d$runs[[1]]$outcome,na.rm=TRUE)}))
unlist(lapply(data,function(d){paste(d$runs[[1]]$motivation,d$runs[[2]]$motivation)}))
#no. Run2 and run 1 are almost always the same motivation (though it differs *which* they are)
#so this must be practice effects.
#

dim(phi_s)
apply(phi_s,c(2,3),function(x){length(unique(x))})


changeDetect<-function(vec){sum(vec[2:length(vec)]!=vec[1:(length(vec)-1)])}
lapply(data,function(d){unlist(lapply(d$runs,function(r){changeDetect(r$choice)}))})
lapply(data,function(d){unlist(lapply(d$runs,function(r){sum(r$outcome==1)/length(r$outcome)}))})

lapply(data,function(d){unlist(lapply(d$runs,function(r){length(r$outcome==1)}))})
lapply(data,function(d){unlist(lapply(d$runs,function(r){length(r$choice)}))})
lapply(data,function(d){unlist(lapply(d$runs,function(r){mean(r$rt,na.rm=TRUE)}))})
lapply(data,function(d){unlist(lapply(d$runs,function(r){sum(is.na(r$rt))}))})
lapply(data,function(d){unlist(lapply(d$runs,function(r){sum(r$choice==0)}))})


accuracy.by.pres_seg.subid<-rl.all.subjects.list[,.(prop.correct=sum(correct)/.N,count=.N),.(presentation_n_over_segments,subid,Motivation,RiskLabel,MethUse,SexRisk,runid)]

accuracy.by.pres_seg<-
  rl.all.subjects.list[,.(prop.correct=sum(correct)/.N,count=.N),.(presentation_n_over_segments,Motivation,runid)]

break.labels=c("1\nPre-reversal",2:last_nonreversal,"1\nReversal",2:6)
accuracy.by.pres_seg.subid[,final_n:=max(presentation_n_over_segments),by=.(subid,Motivation,runid)] 

accuracy.by.pres_seg.subid.finalpc<-accuracy.by.pres_seg.subid[final_n==presentation_n_over_segments,] %>% 
  .[,.(final.prop.correct=prop.correct),
    by=.(subid,Motivation,runid)]

accuracy.by.pres_seg.subid<-merge(accuracy.by.pres_seg.subid,accuracy.by.pres_seg.subid.finalpc,by=c("subid","Motivation","runid"))


#trying to find out why we're unable to find good starting values for SUB147 (#35)
accuracy.by.pres_seg.subid.summary<-accuracy.by.pres_seg.subid[
  presentation_n_over_segments<=13,
  .(prop.correct.m=mean(prop.correct),
    prop.correct.sd=sd(prop.correct)),
  .(presentation_n_over_segments,Motivation)]




  ggplot(accuracy.by.pres_seg.subid[!is.na(presentation_n_over_segments) & subid==147],
         aes(x=presentation_n_over_segments,y=prop.correct,group=interaction(runid,Motivation)))+
  geom_line(aes(colour=interaction(runid,Motivation)),size=1.5,alpha=0.3)+ #scale_colour_gradientn(colours=c("red","green","blue","violet"))+
  #scale_x_continuous(breaks=-8:4,labels=break.labels)+
  coord_cartesian(ylim=c(0,1))+
  labs(#x="Presentation",
    y="Proportion correct across all images by user",
    title=paste0("Learning across trials: why subject 147 might not fit the model."))+
  geom_line(data=accuracy.by.pres_seg.subid.summary,aes(x=presentation_n_over_segments,y=prop.correct.m,group=NULL))+
  #facet_grid(Motivation ~ .)+
  #theme(strip.text.y=element_text(colour="orange"))+
  reversal_learning_timeline_ggplot_commands+
  geom_hline(data=accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==5],
             aes(yintercept = prop.correct.m),
             linetype=2)+
  geom_hline(data=accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==13],
             aes(yintercept = prop.correct.m),
             linetype=2)



#one thing that could have caused a problem: changing tau estimate from based on minimum reaction time to lower quintile:
#makes quite a difference:
hist(unlist(lapply(data,function(d){lapply(d$runs,function(r){quantile(r$rt,c(0.2),na.rm=TRUE)[[1]]})}))-unlist(lapply(data,function(d){lapply(d$runs,function(r){min(r$rt,na.rm=TRUE)})})))

#I think *minimum* reaction time really might be quite important. But we still might want to ignore outliers.
unlist(lapply(data,function(d){lapply(d$runs,function(r){length(r$rt)})}))
#with 200 per run, a 5th percentile would represent the tenth slowest value.
#this seems reasonable; I'll ALSO modify the scaling factor so that on average, we're estimating at round the same level overall.
#so, I'll change the scaling factor to an appropriate level...
mean(0.45*unlist(lapply(data,function(d){lapply(d$runs,function(r){quantile(r$rt,c(0.05),na.rm=TRUE)[[1]]})}))-0.6*unlist(lapply(data,function(d){lapply(d$runs,function(r){min(r$rt,na.rm=TRUE)})})))


infinite.weight.sources<-data.table(read.table("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m5f20180521T114704weaker_sigma/ln_density_likelihood.txt"))

crossover_details_noupdate<-data.table(read.table("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m5f20180521T121654weaker_sigma/no_update_weight.txt"))
colnames(crossover_details_noupdate)<-c("i","like","prior_weight","use.weight","use.data.names")
crossover_details_noupdate$action<-"notupdated"
crossover_details_update<-data.table(read.table("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/output_h_m5f20180521T121654weaker_sigma/updated_weight.txt"))
colnames(crossover_details_update)<-c("i","like","prior_weight","use.weight","use.data.names")
crossover_details_update$action<-"updated"

crossover_details<-rbind(crossover_details_noupdate,crossover_details_update)
crossover_details$like.weight.is.infinite<- is.infinite(crossover_details$like)
crossover_details$prior.weight.is.infinite<- is.infinite(crossover_details$prior_weight)
table(crossover_details$like.weight.is.infinite,crossover_details$use.data.names)
table(crossover_details$prior.weight.is.infinite,crossover_details$use.data.names)
#so it really isn't a matter of having *infinite* weights; they're just too large.
#can we identify one or the other that seems to 'stand out'?
#for alphathreshtau 
crossover_details_l2<-crossover_details[use.data.names=="alphathreshtau",]
#in the successfully updated vs. not successfully updated, does prior or like weight stand out
crossover_details_l2[
  ,
  .(prior.weight.m=mean(prior_weight),
    like.weight.m=mean(like),
    use.weight.m=mean(use.weight),
    prior.weight.sd=sd(prior_weight),
    like.weight.sd=sd(like),
    use.weight.sd=sd(use.weight),
    prior.weight.med=median(prior_weight),
    like.weight.med=median(like),
    use.weight.med=median(use.weight)
    ),by=action]
#so this seems pretty clear. 
#why would we be getting extremely high values

