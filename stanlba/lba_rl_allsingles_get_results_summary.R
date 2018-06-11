source("stanlba/lba_rl_setup.R")

#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.
lba_rl_version<-"20180609_1"

single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
output_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")
dir.create(single_run_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)
#file_folder<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/lba_rl_single_estimates.RData"
#load(file=file_folder)
results_summary_list_filepath<-paste0(output_dir,"run_package_summary.RData")
if(file.exists(results_summary_list_filepath)){
  load(results_summary_list_filepath)
}else{
  stop("couldn't find summary")
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

rownames(results.summary.df)<-NULL
library(data.table)
results.summary.dt<-data.table(results.summary.df)


#library(ggplot2)
#hist(log(results.summary.dt$Rhat),breaks=100)#Most things converged...

improperly.estimated.runs<-unique(results.summary.dt[which(results.summary.dt$Rhat>1.05),.(sid,rid,motivation,FullRunId)])
#OK great, so about 5% got over the 105 threshold. That's about right.
useable_length<-length(sort(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean]))
#alpha
#mean is the 50th percentil mean.
alpha_pr_mean<-sort(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*.5)]
#mean prior variance should be reflected by the uncertainty in mean values...
alpha_pr_var<-
  max(abs(sort(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X2.5.])[round(useable_length*.5)]-alpha_pr_mean),
      abs(alpha_pr_mean-sort(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X97.5.])[round(useable_length*.5)]))
#2.5 percentile. #this figure is relevant to the empirical SD, the gamma value.
#how widely do we expect SUBJECTS to be spread?
alpha_sd_prior<-sd(results.summary.dt[param_name=="alpha_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])

#so, what's the expected standard deviation WITHIN SUBJECTS across runs?
subject_sd<-results.summary.dt[!(FullRunId %in% improperly.estimated.runs$FullRunId),.(Subject_SD=sd(mean)),by=.(sid,param_name)] %>% tidyr::spread(key = param_name,value=Subject_SD,sep = "_")
#and we probably want to ensure an upper bound across runs si included.
ubound_sd<-sort(subject_sd[,param_name_alpha_pr])[round(dim(subject_sd)[1]*0.975)]
alpha_run_sigma_gamma<-ubound_sd


#k
#mean is the 50th percentil mean.
k_pr_mean<-sort(results.summary.dt[param_name=="k_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*.5)]
#mean prior variance should be reflected by the uncertainty in mean values...
k_pr_var<-
  max(abs(sort(results.summary.dt[param_name=="k_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X2.5.])[round(useable_length*.5)]-k_pr_mean),
      abs(k_pr_mean-sort(results.summary.dt[param_name=="k_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X97.5.])[round(useable_length*.5)]))
#2.5 percentile. #this figure is relevant to the empirical SD, the gamma value.
#how widely do we expect SUBJECTS to be spread?
k_sd_prior<-sd(results.summary.dt[param_name=="k_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])
ubound_sd<-sort(subject_sd[,param_name_k_pr])[round(dim(subject_sd)[1]*0.975)]
k_run_sigma_gamma<-ubound_sd
#tau
#mean is the 50th percentil mean.
tau_pr_mean<-sort(results.summary.dt[param_name=="tau_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])[round(useable_length*.5)]
#mean prior variance should be reflected by the uncertainty in mean values...
tau_pr_var<-
  max(abs(sort(results.summary.dt[param_name=="tau_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X2.5.])[round(useable_length*.5)]-tau_pr_mean),
      abs(tau_pr_mean-sort(results.summary.dt[param_name=="tau_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),X97.5.])[round(useable_length*.5)]))
#2.5 percentile. #this figure is relevant to the empirical SD, the gamma value.
#how widely do we expect SUBJECTS to be spread?
tau_sd_prior<-sd(results.summary.dt[param_name=="tau_pr" & !(FullRunId %in% improperly.estimated.runs$FullRunId),mean])
ubound_sd<-sort(subject_sd[,param_name_tau_pr])[round(dim(subject_sd)[1]*0.975)]
tau_run_sigma_gamma<-ubound_sd
