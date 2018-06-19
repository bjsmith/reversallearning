source("load_lba_rl_allsingles_resultsdata.R")


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
