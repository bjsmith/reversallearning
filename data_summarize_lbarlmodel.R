
efficiency_score<-function(myfit){
  param_num<-length(summary(myfit)$summary[,"n_eff"])
  sum(summary(myfit)$summary[,"n_eff"])/(length(myfit@stan_args)*(myfit@stan_args[[1]]$iter-myfit@stan_args[[1]]$warmup))/param_num
}

efficiency_score_group_params<-function(myfit){#myfit<-rmfit
  sum(summary(myfit)$summary[1:9,"n_eff"])/(length(myfit@stan_args)*(myfit@stan_args[[1]]$iter-myfit@stan_args[[1]]$warmup))/9
}

efficiency_score_by_param<-function(myfit){#myfit<-rmfit
  param_num<-length(summary(myfit)$summary[,"n_eff"])
  summary(myfit)$summary[,"n_eff"]/(length(myfit@stan_args)*(myfit@stan_args[[1]]$iter-myfit@stan_args[[1]]$warmup))
}



rmfit_samples<-function(myfit){
  myfit@stan_args[[1]]$iter-myfit@stan_args[[1]]$warmup
}


draw_sampling_progress<-function(xfit,parameter_name){
  rm_arr<-as.array(xfit)
  rm_ex_subj_mu_alpha<-as.data.table(rm_arr[,,parameter_name]) %>% tidyr::gather(key = "Chain","Value") %>% data.table
  rm_ex_subj_mu_alpha[,iter:=(1:.N),by=Chain]
  ggplot(rm_ex_subj_mu_alpha,aes(x=iter,y=Value,color=Chain,group=Chain))+geom_line()
}
