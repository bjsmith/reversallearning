# ---
# title: "MCMC Analysis Final Report"
# output: html_notebook
# ---
# 
# 
# Although a three-level MCMC analysis proved to be computationally intractable, I did run a two-level MCMC analysis. The two-level analysis examined subjects across each of the individual runs but did not combine runs within subjects. That way, we had four distinct measures of the distributions of subjects across the data - two reward runs and two punishment runs.
# 
# 
# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(
# 	echo = FALSE,
# 	message = FALSE,
# 	warning = FALSE,
# 	tidy = TRUE
# )
# knitr::opts_knit$set(root.dir="../")
# 
# ```
# 
# ```{r setup2}
# 
# source("../util/apply_local_settings.R")
# apply_local_settings()
# knitr::opts_chunk$set(cache.path = paste0(localsettings$data.dir, "knitrcache"), warning = FALSE)
source("nate_files/fitGroupsV3Onegroup.R")
source("data_summarize.R")
source("data_summarize_lbarlmodel.R")
# library(data.table)
# library(ggplot2)
# mainDataDir=localsettings$data.dir
# ```
# 
# 
# 
# ```{r h_m3setup}
library(ggplot2)

source("../joint_msm_combined/bjs_misc_utils.R")
version="h_m3"

source('de_mcmc/functions.R')
source('de_mcmc/main_m1_setup.R')
source('de_mcmc/functions_joint_v2.R')
source(paste0('de_mcmc/functions_',version,'.R'))

source("de_mcmc/functions_h_m3.R")
source("visualization/geom_hdi.R")
# ```

# ```{r h_m3setup4}
source("de_mcmc/fig_base5.R")
source("plot_library.R")

show_histos<-function(res,outer_title){
  par(mfrow=c(3,3),ask=FALSE)
  
  sr_diff<-res[,,,"RiskyNoMeth"]-res[,,,"SafeNoMeth"]
  hist(sr_diff[,,"alpha_mu"],breaks=100,main="Risky-Safe No Meth alpha_mu")
  hist(sr_diff[,,"thresh_mu"],breaks=100,main="Risky-Safe No Meth thresh_mu")
  hist(sr_diff[,,"tau_mu"],breaks=100,main="Risky-Safe No Meth tau_mu")
  meth_diff<-res[,,,"RiskyMeth"]-res[,,,"RiskyNoMeth"]
  table(meth_diff[,,"alpha_mu"]<0)/prod(dim(meth_diff[,,"alpha_mu"]))
  
  
  hist(meth_diff[,,"alpha_mu"],breaks=100,main="Meth-NoMeth (sexually risky) alpha_mu")
  hist(meth_diff[,,"thresh_mu"],breaks=100,main="Meth-NoMeth (sexually risky) thresh_mu")
  hist(meth_diff[,,"tau_mu"],breaks=100,main="Meth-NoMeth (sexually risky) tau_mu")
  
  sr_meth_diff<-res[,,,"RiskyMeth"]-res[,,,"SafeNoMeth"]
  hist(sr_meth_diff[,,"alpha_mu"],breaks=100,main="Risky Meth - Safe NoMeth alpha_mu")
  hist(sr_meth_diff[,,"thresh_mu"],breaks=100,main="Risky Meth - Safe NoMeth thresh_mu")
  hist(sr_meth_diff[,,"tau_mu"],breaks=100,main="Risky Meth - Safe NoMeth tau_mu")
  title(outer_title,outer=TRUE)
}


# ```{r h_m3_reward_run1}
#output_h_m320180716T135813.RData
four_run_files<-c("output_h_m320180730T004210.RData","output_h_m320180730T004226.RData",
                  "output_h_m320180730T004141.RData","output_h_m320180730T004026.RData")
res_list<-vector("list",4)
res_list_metadata<-vector("list",4)
res_dt_list<-vector("list",6*3*4)
tnmc=1001
for (rf_i in 1:length(four_run_files)){
  rf<-four_run_files[[rf_i]]
  load(paste0("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/",rf))
  res<-hyper_param(
    param.names = par.names.l2,
    group.names = l2.groups.list,
    tnmc = tnmc,
    n.chains = n.chains)
  res_list[[rf_i]]<-res
  # dimnames(res)[[1]]<-paste("Chain",1:24)
  # dimnames(res)[[2]]<-paste("Iteration",1:3003)
  res_list_metadata<-list("runid"=runid,"motivation"=mid)
  for (stat_i in 1:dim(res)[[3]]){
    for(group_i in 1:dim(res)[[4]]){
      #stat_i<-1;group_i<-1
      stat_param<-dimnames(res)[[3]][[stat_i]]
      param<-strsplit(stat_param,"_")[[1]][[1]]
      stat<-strsplit(stat_param,"_")[[1]][[2]]
      group<-dimnames(res)[[4]][[group_i]]
      id<-(rf_i-1)*prod(dim(res)[3:4])+(stat_i-1)*dim(res)[[4]]+group_i
      #stat<-"alpha_mu";group<-"SafeNoMeth"
      
      res_dt_list[[id]]<-data.table("Value"=as.vector(res[,,stat_param,group] ),"Stat"=stat,"Param"=param, "Group"=group,
                                    "Run"=runid,"Motivation"=mid,
                                    "Iteration"=1:length(res[,,stat_param,group]))
      # print(paste(sum(!is.finite(res_dt_list[[id]]$Value)), " non-finite and ",
      #             sum(is.finite(res_dt_list[[id]]$Value))," values for ",stat,group,as.character(runid),mid,rf))
      
    }
  }
}
res_dt<-rbindlist(res_dt_list)
#remove non-finite values

ggplot(res_dt,aes(x=Value,color=Group))+geom_density()+facet_grid(Motivation+Param~Stat,scales = "free")

group.diffs<-dcast(res_dt, Iteration+Motivation+Stat+Param+Run~Group,value.var="Value")
group.diffs$RiskyMeth_Minus_RiskyNoMeth<-group.diffs$RiskyMeth-group.diffs$RiskyNoMeth
group.diffs$RiskyMeth_Minus_SafeNoMeth<-group.diffs$RiskyMeth-group.diffs$SafeNoMeth
group.diffs$RiskyNoMeth_Minus_SafeNoMeth<-group.diffs$RiskyNoMeth-group.diffs$SafeNoMeth
colnames(group.diffs.mu)
group.diffs.mu<-group.diffs[Stat=="mu"]

group.diffs.mu.long<-gather(group.diffs.mu[,.(Iteration,Motivation,Stat,Param,Run,
                                              RiskyMeth_Minus_RiskyNoMeth,
                                              RiskyMeth_Minus_SafeNoMeth,
                                              RiskyNoMeth_Minus_SafeNoMeth
                                              )],key = "Comparison","Value", RiskyMeth_Minus_RiskyNoMeth:RiskyNoMeth_Minus_SafeNoMeth)
density_plots<-list(geom_density(),
                    geom_hdi(size=3,alpha=0.3),
                    facet_wrap(Comparison~Param,scales = "free"))
ggplot(group.diffs.mu.long,aes(Value,color=Motivation))+
  density_plots+
  labs(title="Meth Minus NoMeth (Sexually Risky), by Motivation")

ggplot(group.diffs.mu.long,aes(Value))+
  density_plots+
  labs(title="Meth Minus NoMeth (Sexually Risky), by Motivation")

#what about reward punishment differences?
density_plots<-list(geom_density(),
                    geom_hdi(size=3,alpha=0.3),
                    facet_wrap(Stat~Param,scales = "free"))
motivation.diffs<-dcast(res_dt, Iteration+Group+Stat+Param+Run~Motivation,value.var="Value")
motivation.diffs$Reward_Minus_Punishment<-motivation.diffs$reward-motivation.diffs$punishment
ggplot(motivation.diffs,aes(Reward_Minus_Punishment,color=Group))+density_plots+
         labs(title="Reward Minus Punishment, by Group")    
setwd(mainDir)

res<-hyper_param(
  param.names = par.names.l2,
  group.names = l2.groups.list,
  tnmc = tnmc,
  n.chains = n.chains)

show_histos(res,paste(mid, "run",runid))
# 
# ```


# ```
# 
# ```{r h_m3_reward_run2}
#output_h_m320180716T140213.RData
load(paste0("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/",four_run_files[[2]]))

res<-hyper_param(
  param.names = par.names.l2,
  group.names = l2.groups.list,
  tnmc = tnmc,
  n.chains = n.chains)

show_histos(res,paste(mid, "run",runid))
# 
# ```
# 

# ```{r h_m3_reward_run2}
#output_h_m320180716T134722.RData
#output_h_m320180716T133556.RData
#output_h_m320180716T132628.RData
load(paste0("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/",four_run_files[[3]]))

res<-hyper_param(
  param.names = par.names.l2,
  group.names = l2.groups.list,
  tnmc = tnmc,
  n.chains = n.chains)

show_histos(res,paste(mid, "run",runid))


# 
# ```

load(paste0("/expdata/bensmith/joint-modeling/data/msm/reversallearning/de_mcmc/",four_run_files[[4]]))

res<-hyper_param(
  param.names = par.names.l2,
  group.names = l2.groups.list,
  tnmc = tnmc,
  n.chains = n.chains)

show_histos(res,paste(mid, "run",runid))
