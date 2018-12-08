source("du_model_rev8_jointrev1.R")
source("visualization/geom_hdi.R")


model.summary.all<-NULL

#iterations
miters<-unlist(lapply(model.summaries,function(m){
  return(length(m$summaryObj$iter))
}))
for(ms.i in 1:length(model.summaries)){
  #ms.i=2
  ms<-model.summaries[[ms.i]]
  ms.summaryObj<-ms$summaryObj
  ms.summaryObj$TestId<-ms.i
  ms.summaryObj$Group<-ms$g
  ms.summaryObj$ModelName<-ms$m
  ms.summaryObj$AnalysisRepetition<-ms$t
  ms.summaryObj$EstimationMethod<-ms$EstimationMethod
  #because when we ran this, we hadn't explicitly recorded estimation methods; 
  #but these are distinguishable by the number of iterations.
  if(is.null(model.summary.all)){
    model.summary.all<-ms.summaryObj
  }else{
    model.summary.all<-rbind(model.summary.all,ms.summaryObj,fill=TRUE)
  }
}
model.summary.all$EstimationMethod<-factor(model.summary.all$EstimationMethod)


ggplot(model.summary.all[Statistic=="mu" & EstimationMethod=="MCMC"],aes(x=Value ,fill=factor(Group),color=factor(Group)
))+
  geom_freqpoly(alpha=0.9,binwidth=0.001)+
  geom_hdi(size=2, lineend = "round",alpha=0.5,credible_mass=0.95)+
  facet_grid(.~Parameter,scales = "free")+
  
  labs(title=paste0("mu statistic (all rounds), variable number of runs model"))




ggplot(model.summary.all[Statistic %in% c("rew_mu","pun_mu") & EstimationMethod=="MCMC"],aes(x=Value ,fill=factor(Group),color=factor(Group)
))+
  geom_freqpoly(alpha=0.9,binwidth=0.001)+
  geom_hdi(size=2, lineend = "round",alpha=0.5,credible_mass=0.95)+
  facet_grid(Statistic~Parameter,scales = "free")+
  
  labs(title=paste0("mu statistic (all rounds), Reward and Punishment Runs"))


###REWARD MINUS PUNISHMENT

model.summary.all.rewpun<-model.summary.all[Statistic %in% c("rew_mu","pun_mu")] %>% tidyr::spread(Statistic,Value)

model.summary.all.rewpun[,rew_minus_pun_mu:=rew_mu-pun_mu]


ggplot(model.summary.all.rewpun[EstimationMethod=="MCMC"],aes(x=rew_minus_pun_mu))+
  geom_freqpoly(alpha=0.9,binwidth=0.001)+
  geom_hdi(size=2, lineend = "round",alpha=0.5,credible_mass=0.95)+
  facet_grid(Group~Parameter,scales = "free")+
  labs(title=paste0("mu statistic (all rounds), alpha, Reward Minus Punishment"))


ggplot(model.summary.all.rewpun[Parameter=="alpha" & EstimationMethod=="MCMC"],aes(x=rew_mu,y=pun_mu))+
  geom_point(alpha=0.1)+
  facet_grid(.~Group,scales = "free")+
  labs(title=paste0("mu statistic (all rounds), alpha, Reward and Punishment"))

ggplot(model.summary.all.rewpun[Parameter=="beta" & EstimationMethod=="MCMC"],aes(x=rew_mu,y=pun_mu ))+
  geom_point(alpha=0.1)+
  facet_grid(.~Group,scales = "free")+
  labs(title=paste0("mu statistic (all rounds), beta, Reward and Punishment"))


model.summary.all.notestid<-model.summary.all[,TestId:=NULL] 

model.summary.all.groupcompare<- tidyr::spread(model.summary.all.notestid,Group,Value,sep="")
model.summary.all.groupcompare$Group3_minus_Group2<-
  model.summary.all.groupcompare$Group3-model.summary.all.groupcompare$Group2

ggplot(model.summary.all.groupcompare[EstimationMethod=="MCMC"],aes(x=Group3_minus_Group2 ))+
  geom_freqpoly(alpha=0.9,binwidth=0.001)+
  geom_hdi(size=2, lineend = "round",alpha=0.5,credible_mass=0.95)+
  facet_grid(Statistic~Parameter,scales = "free")+
  labs(title=paste0("mu statistic (all rounds), Group 3 Minus Group 2"))



model.summary.all.g3g2compare.bypar<- 
  tidyr::spread(
    model.summary.all.groupcompare[,.(iter,Run,Statistic,Parameter,ModelName,
                                      EstimationMethod,Group3_minus_Group2)],
    Parameter,
    Group3_minus_Group2)

ggplot(model.summary.all.g3g2compare.bypar[
  EstimationMethod=="MCMC" & 
    Statistic %in% c("mu","rew_mu","pun_mu")
  ],aes(x=alpha,y=beta 
        #,fill=factor(Statistic),color=factor(Statistic)
  ))+
  geom_point(alpha=0.1)+
  facet_grid(~Statistic,scales = "free")+
  labs(title=paste0("mu statistic (all rounds), Group 3 Minus Group 2"))

