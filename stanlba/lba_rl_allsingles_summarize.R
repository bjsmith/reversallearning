source("stanlba/lba_rl_setup_v2.R")

# sub105data<-rawdata[subid==105 & Motivation=="reward" & runid==1,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]

#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.
lba_rl_version<-"20180609_1"

single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
output_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")
dir.create(single_run_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)
#file_folder<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/lba_rl_single_estimates.RData"
#load(file=file_folder)
results_summary_list_filepath<-paste0(output_dir,"run_package_summary_v2.RData")
if(file.exists(results_summary_list_filepath)){
  load(results_summary_list_filepath)
}else{
  results.summary.list<-list()
}


#lba_rl_single<-stan_model('stanlba/stanfiles/lba_rl_single_exp_v2.stan')

for (sid in unique(rawdata$subid)){#sid<-106
  for (r in unique(rawdata[subid==sid,runid])){#r<-1
    for(m in unique(rawdata[subid==sid & runid==r,Motivation])){#m<-"punishment"
      
      package_filepath<-paste0(output_dir,"run_package_",sid,"_",r,"_",m,"_v2.RData")
      srm.data<-rawdata[subid==sid & Motivation==m & runid==r,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]
      if(any(unlist(lapply(results.summary.list,function(rli){rli$sid==sid & rli$rid==r & rli$motivation==m})))==FALSE & 
         #we haven't already got an entry for this one.
         file.exists(package_filepath)){
        print(paste0("loading from file sid ",sid, "; r ",r, "; m ", m))
        load(package_filepath)
        fit_summary<-summary(run_package$fit)$summary
        run_summary_package<-list("sid"=run_package$sid,"rid"=run_package$rid,"motivation"=run_package$motivation,
                                  fit_summary=fit_summary,duration=run_package$duration)
        
        results.summary.list<-c(results.summary.list,list(run_summary_package))
        save(results.summary.list,file=results_summary_list_filepath)
      }
      
      
    }
    
  }
  
}


#at this point, we need to get a way to extract key datapoints from the results list and save that. Don't save it as it is; it's far too big!
#now we can get all this into a single summary statistics table. In that table, we can have the following columns:
#SID
#rid
#motivation
#parameter name
#mean,se_mean...all the columns in the fit_summary
for (i in 1:length(results.summary.list)){
  results.summary.list[[i]]$FullRunId<-i
}
results.summary.df<-do.call(rbind,lapply(results.summary.list,function(rsli){
  data.frame("sid"=rsli$sid,
             "rid"=rsli$rid,
             "motivation"=rsli$motivation,
             "param_name"=rownames(rsli$fit_summary),
             "FullRunId"=rsli$FullRunId,
             rsli$fit_summary)
  
} ))
#results.summary.df$FullRunId<-1:dim(results.summary.df)[1]
rownames(results.summary.df)<-NULL
#View(results.summary.df)
library(data.table)
results.summary.dt<-data.table(results.summary.df)


#library(ggplot2)
#hist(log(results.summary.dt$Rhat),breaks=100)#Most things converged...

improperly.estimated.runs<-unique(results.summary.dt[which(results.summary.dt$Rhat>1.05),.(sid,rid,motivation,FullRunId)])
#OK great, so about 5% got over the 105 threshold. That's about right.

# #and what sort of alpha values are we seeing?
# table(results.summary.dt$param_name)
# ggplot(results.summary.dt[param_name=="alpha" & !(FullRunId %in% improperly.estimated.runs$FullRunId)],aes(mean))+geom_density()+facet_grid(motivation~. ,margins=TRUE)
# ggplot(results.summary.dt[param_name=="k" & !(FullRunId %in% improperly.estimated.runs$FullRunId)],aes(mean))+geom_density()+facet_grid(motivation~. ,margins=TRUE)
# ggplot(results.summary.dt[param_name=="tau" & !(FullRunId %in% improperly.estimated.runs$FullRunId)],aes(mean))+geom_density()+facet_grid(motivation~. ,margins=TRUE)
# 
# #alpha values are actually very low, in other words, learning doesn't happen very quickly here according to the model.
# 
# #In fact these learning values are much lower than anything else we've seen.
# #looks like we were substantially over-estimating tau, but we got k about right.
# 
# ggplot(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId)],aes(mean))+geom_density()+facet_grid(motivation~. ,margins=TRUE)
# ggplot(results.summary.dt[param_name=="k_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId)],aes(mean))+geom_density()+facet_grid(motivation~. ,margins=TRUE)
# ggplot(results.summary.dt[param_name=="tau_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId)],aes(mean))+geom_density()+facet_grid(motivation~. ,margins=TRUE)
# 
# ggplot(results.summary.dt[param_name=="alpha_pr"],aes(mean))+geom_density()+facet_grid(motivation~. ,margins=TRUE)
# ggplot(results.summary.dt[param_name=="k_pr"],aes(mean))+geom_density()+facet_grid(motivation~. ,margins=TRUE)
# ggplot(results.summary.dt[param_name=="tau_pr"],aes(mean))+geom_density()+facet_grid(motivation~. ,margins=TRUE)

useable_length<-length(sort(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean]))
#alpha
#2.5 percentile. #this figure is relevant to the empirical SD, the gamma value.
sort(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*0.025)]
#50th percentile, two SD lower-bound
sort(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X2.5.])[round(useable_length*.5)]
#50th percentile MEAN
sort(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*.5)]
#50th percentile, two SD upper-bound
sort(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X97.5.])[round(useable_length*.5)]
#97.5th percentile
sort(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*0.975)]



#k
#2.5 percentile.
sort(results.summary.dt[param_name=="k_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*0.025)]
#50th percentile
#50th percentile RUN, 
sort(results.summary.dt[param_name=="k_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X2.5.])[round(useable_length*.5)]
sort(results.summary.dt[param_name=="k_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*.5)]
sort(results.summary.dt[param_name=="k_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X97.5.])[round(useable_length*.5)]

#97.5th percentile
sort(results.summary.dt[param_name=="k_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*0.975)]
sd(results.summary.dt[param_name=="k_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])

#tau
#2.5 percentile.
sort(results.summary.dt[param_name=="tau_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*0.025)]
#50th percentile
sort(results.summary.dt[param_name=="tau_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X2.5.])[round(useable_length*.5)]
sort(results.summary.dt[param_name=="tau_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*.5)]
sort(results.summary.dt[param_name=="tau_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X97.5.])[round(useable_length*.5)]

#97.5th percentile
sort(results.summary.dt[param_name=="tau_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*0.975)]

sd(results.summary.dt[param_name=="tau_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])

# what are good, liberal figures to use for the standard deviations...
#this figure is relevant to the empirical SD, the gamma value.
library(LaplacesDemon)
alpha_sd<-sd(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])
plot(seq(0,10,0.1),phalfcauchy(seq(0,10,0.1),alpha_sd),type="l")

results.summary.dt[param_name=="alpha_pr",] %>% .[order(-mean)]
