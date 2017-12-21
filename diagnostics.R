library(dplyr)
source("visualization/geom_hdi.R")
#convert a summary object into a summaries object


# MCMC Representativness, accuracy, and efficiency

#traceplot: r
#model.summary<-model.summaries[[13]]
#lapply(model.summaries,function(m){print(paste0(m$m,m$g))})
#model.stanfit<-model.stanfits[[13]]
#model.stanfit<-model.stanfits[[1]]
representativeness_stats<-function(model.summary,model.stanfit,cols.to.process=NA){
    #only look at these because looking at models with trial posteriors is unnecessary and timeconsuming.
    traceplot.title<-paste("group=",model.summary$g,model.summary$m,model.summary$t,model.summary$EstimationMethod)
    if(is.na(cols.to.process)){
      cols.to.process<-names(model.stanfit)[!sapply(names(model.stanfit),function(x){
        return(grepl("alpha\\[",x) || grepl("beta\\[",x) || grepl("alpha_pr",x) || grepl("beta_pr",x) ||
                 grepl("alpha_s_r\\[",x) || grepl("beta_s_r",x) || grepl("alpha_s_sigma",x) || grepl("beta_s_sigma",x) ||
                 grepl("beta_r",x) || grepl("alpha_r",x) || grepl("subj_alpha_s",x) || grepl("subj_beta_s",x)
               || grepl("alpha_s",x)
               || grepl("beta_s",x)
               || grepl("log_lik\\[",x))
      })]
    }
    #print(cols.to.process)
    trace<-traceplot(model.stanfit,cols.to.process,alpha=0.5,inc_warmup=FALSE)
    
    print(trace+labs(title=traceplot.title)+scale_color_discrete(guide=guide_legend(nrows=1))+theme(legend.position = "bottom"))
    
    #only look at these because looking at models with trial posteriors is unnecessary and timeconsuming.
    traceplot.title<-paste("group=",model.summary$g,model.summary$m,model.summary$t,model.summary.all[,first(EstimationMethod),by=TestId][i,V1],"vars=",length(names(model.stanfit)))
    #cols.to.process<-[names(model.stanfits[[i]]) %in% 
    #print(cols.to.process)
    print(model.stanfit,pars=cols.to.process)
}


show.dist.by.chain<- function(sf,value,titletext){
  m.mcmc.array<-as.array(sf)
  var.ci.to.show<-which(dimnames(m.mcmc.array)[[3]]==value)
  
  
  #so let's take a look at the distribution....
  
  m.mcmc.bychain<-as.data.table(m.mcmc.array[,,var.ci.to.show]) %>% tidyr::gather("Chain","Value",1:12)
  
  retplot<-ggplot(m.mcmc.bychain,aes(x=Value,fill=factor(Chain),color=factor(Chain)
  ))+
    geom_freqpoly(alpha=0.9,binwidth=0.001)+
    #geom_hdi(size=2, lineend = "round",alpha=0.5,credible_mass=0.95)+
    #scale_x_continuous(limits=c(-0.8,0))+
    labs(title=paste0(value,", variable number of runs model, ",titletext))
  
  return(retplot)
  
}