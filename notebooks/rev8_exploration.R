

source("../util/apply_local_settings.R")
apply_local_settings()
knitr::opts_chunk$set(cache.path = paste0(localsettings$data.dir,"knitrcache"))
source("nate_files/fitGroupsV3Onegroup.R")
source("data_summarize.R")
library(data.table)
library(ggplot2)
source("visualization/geom_hdi.R")

source("du_model_rev8.R")
library(ggplot2)

#arrange all the data into a single data table.
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

model.summary.all.notestid<-model.summary.all[,TestId:=NULL] 

model.summary.all.groupcompare<- tidyr::spread(model.summary.all.notestid,Group,Value,sep="")
model.summary.all.groupcompare$Group3_minus_Group2<-
  model.summary.all.groupcompare$Group3-model.summary.all.groupcompare$Group2

model.summary.all.g3g2compare.bypar<- 
  tidyr::spread(
    model.summary.all.groupcompare[,.(iter,Run,Statistic,Parameter,ModelName,AnalysisRepetition,
                                      EstimationMethod,Group3_minus_Group2)],
    Parameter,
    Group3_minus_Group2)



source("rev8_exploration_functions.R")

model.summary.all.g3g2compare.bypar[
  ,alphabeta_95_ellipse:=get_95_ellipse(cbind(alpha,beta)),by=Statistic]

ggplot(model.summary.all.g3g2compare.bypar[
  EstimationMethod=="MCMC" & 
    Statistic %in% c("mu","rew_mu","pun_mu")
  ],aes(x=alpha,y=beta,color= alphabeta_95_ellipse
  ))+
  geom_point(alpha=0.1)+
  facet_grid(~Statistic,scales = "free")+
  labs(title=paste0("mu statistic (all rounds), Group 3 Minus Group 2"))

hBayesDM::HDIofMCMC(model.summary.all.g3g2compare.bypar$alpha)
hBayesDM::HDIofMCMC(model.summary.all.g3g2compare.bypar[Statistic=="pun_mu",alpha])

#Our multivariate 95% analysis still includes (0,0) within the range of possible densities, 
#so it's not excludable.

#what if we look at not G
