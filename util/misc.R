##helper functions, common libraries

library(data.table)
library(ggplot2)
library(tidyr)
library(corrplot)

lm.beta.lmer <- function(mod) {
  b <- fixef(mod)[-1]
  sd.x <- apply(getME(mod,"X")[,-1],2,sd)
  sd.y <- sd(getME(mod,"y"))
  b*sd.x/sd.y
}

get.model.summary.all <- function(model.summaries){
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
  return(model.summary.all)
}

