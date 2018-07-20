
########################################## load the functions you will use
source("../joint_msm_combined/bjs_misc_utils.R")
version="h_m4"
save.name=paste0("main_", version)
source('de_mcmc/functions.R')
source('de_mcmc/main_m1_setup.R')
source('de_mcmc/functions_joint_v2.R')
source(paste0('de_mcmc/functions_',version,'.R'))
########################################## generate data

source("de_mcmc/raw_data_all_runs_flat.R")
safe_meth_subjs<-unlist(lapply(data,function(d){d$group=="Safe Meth"}))
data.allruns.flat<-data[!safe_meth_subjs]

########################################## load the functions you will use
source("../joint_msm_combined/bjs_misc_utils.R")
version="h_m3"
save.name="main_h_m3"
source('de_mcmc/functions.R')
source('de_mcmc/main_m1_setup.R')
source('de_mcmc/functions_joint_v2.R')
source(paste0('de_mcmc/functions_',version,'.R'))
########################################## generate data

source("de_mcmc/raw_data_reward_only.R")
safe_meth_subjs<-unlist(lapply(data,function(d){d$group=="Safe Meth"}))
data.onerun<-data[!safe_meth_subjs]
##############################################################################################################################
library(data.table)
library(ggplot2)
data.onerun[[1]]
sapply(data.onerun,function(d){length(d$rt)})

data.allruns.flat[[1]]
sapply(data.allruns.flat,function(d){length(d$rt)})

#so we definitely passed in the extra data.

data.allruns.flat[[1]]

#and it is structured exactly the same.
data.group<-sapply(data.allruns.flat,function(d){d$group})
#if we're looking for bad data that might be things like:
#- NA reaction times
na.rt.count<-sapply(data.allruns.flat,function(d){sum(is.na(d$rt))})
rawdata.dt[,sum(is.na(reaction_time)),by=subid]


na.rt.count
#that's interesting - quite a lot of NA reaction times.
data.allruns.flat[[35]]
data.allruns.flat[[69]]


#-very short mean reaction times
mean_rt_range<-sapply(data.allruns.flat,function(d){mean(d$rt,na.rm=TRUE)})
hist(mean_rt_range,breaks=50)
mean(na.rt.count)
cor.test(na.rt.count,mean_rt_range)

# ggplot(data.frame("NACount"=na.rt.count,"MeanRT"=mean_rt_range),aes(NACount,MeanRT,color=data.group))+
#   geom_point()

#so it's not obvious that we have people who have abnormally short reaction times - maybe a few that were really fast- but 
#no obvious outliers.
#however we do have some subjects with a very high NA reaction count and quite a few with a reasonably high count
#and so perhaps these need to be modeled in some sensible way other than ignoring them.

#-large number of trials of a particular cue which would indicate that a cue was erroneously displayed across more than one run
#-the model is written on the assumption this never happens.
max.cue.count<-sapply(data.allruns.flat,function(d){max(table(d$cue))})
data.allruns.flat[[which(max.cue.count==26)]]$cue
table(data.allruns.flat[[which(max.cue.count==26)]]$cue)
#might want to exclude this subject from analysis. 
#looks like it was just one run that was repeated twice and one run missed out altogether.

rawdata.dt[,.(sum(is.na(rt))), by=subid]

#so how could we change our algorithm to do survival?
missing.response.graph<-rawdata.dt[,.(PropNA=sum(reaction_time==0)/.N), by=subid] %>% .[order(PropNA),]
missing.response.graph$MissingRank=as.factor(1:dim(missing.response.graph)[1])

ggplot(missing.response.graph,aes(x=MissingRank,y=PropNA))+geom_col()+labs(title="Proportion of non-responses for each subject",x="Subject")

ggplot(missing.response.graph,aes(PropNA))+geom_histogram(binwidth=0.01)+
  labs(title="Distributions of proportion\n of non-responses by subject",
       x="Proportion of Non-responses",y="Subject Count")

#so anything else wrong with the data that would be stopping us from accurately converging?
