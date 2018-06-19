generate_lbarl_group_summary_stats <- function(ds_useablesubs){#ds_useablesubs<-ds_boot
  #OK great, so about 5% got over the 105 threshold. That's about right.
  useable_length<-length(sort(ds_useablesubs[param_name=="alpha_pr",mean]))
  #alpha
  #mean is the 50th percentile mean.
  alpha_pr_mean<-sort(ds_useablesubs[param_name=="alpha_pr",mean])[round(useable_length*.5)]
  #mean prior variance should be reflected by the uncertainty in mean values...
  alpha_pr_var<-
    max(abs(sort(ds_useablesubs[param_name=="alpha_pr",X2.5.])[round(useable_length*.5)]-alpha_pr_mean),
        abs(alpha_pr_mean-sort(ds_useablesubs[param_name=="alpha_pr",X97.5.])[round(useable_length*.5)]))
  #2.5 percentile. #this figure is relevant to the empirical SD, the gamma value.
  #how widely do we expect SUBJECTS to be spread?
  alpha_sd_prior<-sd(ds_useablesubs[param_name=="alpha_pr",mean])
  
  #so, what's the expected standard deviation WITHIN SUBJECTS across runs?
  subject_sd<-ds_useablesubs[,.(Subject_SD=sd(mean)),by=.(sid,param_name)] %>% tidyr::spread(key = param_name,value=Subject_SD,sep = "_")
  #and we probably want to ensure an upper bound across runs is included.
  ubound_sd<-sort(subject_sd[,param_name_alpha_pr])[round(dim(subject_sd)[1]*0.975)]
  alpha_run_sigma_gamma<-ubound_sd
  
  
  #k
  #mean is the 50th percentil mean.
  k_pr_mean<-sort(ds_useablesubs[param_name=="k_pr",mean])[round(useable_length*.5)]
  #mean prior variance should be reflected by the uncertainty in mean values...
  k_pr_var<-
    max(abs(sort(ds_useablesubs[param_name=="k_pr",X2.5.])[round(useable_length*.5)]-k_pr_mean),
        abs(k_pr_mean-sort(ds_useablesubs[param_name=="k_pr",X97.5.])[round(useable_length*.5)]))
  #2.5 percentile. #this figure is relevant to the empirical SD, the gamma value.
  #how widely do we expect SUBJECTS to be spread?
  k_sd_prior<-sd(ds_useablesubs[param_name=="k_pr",mean])
  ubound_sd<-sort(subject_sd[,param_name_k_pr])[round(dim(subject_sd)[1]*0.975)]
  k_run_sigma_gamma<-ubound_sd
  #tau
  #mean is the 50th percentil mean.
  tau_pr_mean<-sort(ds_useablesubs[param_name=="tau_pr",mean])[round(useable_length*.5)]
  #mean prior variance should be reflected by the uncertainty in mean values...
  tau_pr_var<-
    max(abs(sort(ds_useablesubs[param_name=="tau_pr",X2.5.])[round(useable_length*.5)]-tau_pr_mean),
        abs(tau_pr_mean-sort(ds_useablesubs[param_name=="tau_pr",X97.5.])[round(useable_length*.5)]))
  #2.5 percentile. #this figure is relevant to the empirical SD, the gamma value.
  #how widely do we expect SUBJECTS to be spread?
  tau_sd_prior<-sd(ds_useablesubs[param_name=="tau_pr",mean])
  ubound_sd<-sort(subject_sd[,param_name_tau_pr])[round(dim(subject_sd)[1]*0.975)]
  tau_run_sigma_gamma<-ubound_sd
  
  #OK; these need to be put into a list format suitable for an initial values object!
  #this should happen the same way that we create the overall priors, which of course are set across all chains
  
  return(list("useable_length"=useable_length,
              "alpha_pr_mean"=alpha_pr_mean,
              "alpha_pr_var"=alpha_pr_var,
              "alpha_sd_prior"=alpha_sd_prior,
              "alpha_run_sigma_gamma"=alpha_run_sigma_gamma,
              "k_pr_mean"=k_pr_mean,
              "k_pr_var"=k_pr_var,
              "k_sd_prior"=k_sd_prior,
              "k_run_sigma_gamma"=k_run_sigma_gamma,
              "tau_pr_mean"=tau_pr_mean,
              "tau_pr_var"=tau_pr_var,
              "tau_sd_prior"=tau_sd_prior,
              "tau_run_sigma_gamma"=tau_run_sigma_gamma))
}
