source("du_model_rev8.R")
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
table(model.summary.all$AnalysisRepetition)
table(model.summary.all$EstimationMethod)
model.summary.all.rewpun<-model.summary.all[Statistic %in% c("rew_mu","pun_mu")] %>%
  dcast(iter+Run+Parameter+TestId+Group+ModelName~Statistic,value.var="Value")
#tidyr::spread(Statistic,Value)

model.summary.all.rewpun[,rew_minus_pun_mu:=rew_mu-pun_mu]


ggplot(model.summary.all.rewpun,aes(x=rew_minus_pun_mu))+
  geom_freqpoly(alpha=0.9,binwidth=0.001)+
  geom_hdi(size=2, lineend = "round",alpha=0.5,credible_mass=0.95)+
  facet_grid(Group~Parameter,scales = "free")+
  labs(title=paste0("mu statistic (all rounds), alpha, Reward Minus Punishment"))

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

setwd("BEST/")
source("BEST-original.R")
setwd("../")
model.summary.all.groupcompare$Parameter
#it takes a "mu1","mu2","sigma1", and "sigma2"
#what do we want to test? let's test G3-G2 within the punishment condition.
mcmcChain<-data.frame("mu1"=model.summary.all.groupcompare[Statistic=="pun_mu" & Parameter=="alpha" ,Group3],
                      "mu2"=model.summary.all.groupcompare[Statistic=="pun_mu" & Parameter=="alpha",Group2],
                      "sigma1" = model.summary.all.groupcompare[Statistic=="sigma"  & Parameter=="alpha",Group3],
                      "sigma2" =model.summary.all.groupcompare[Statistic=="sigma"  & Parameter=="alpha",Group2]
                      )


#do we have the sigmas?????
#we're going to assume equal distributions.


y1<-mcmcChain[,"mu1"]
y2<-mcmcChain[,"mu2"]
BESTsummary(y1,y2,mcmcChain)
hist(sort(y1)-sort(y2),breaks=100)
