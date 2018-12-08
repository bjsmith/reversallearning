create_standatalist<-function(srm.data,theta_count,delta_names=NULL,param_A=0.01){#theta_count=1
  if(is.null(delta_names)){#set the legacy default.
    delta_names= c("ROI_Left.Accumbens.area","ROI_Right.Accumbens.area","ROI_ctx_lh_G_front_inf.Orbital","ROI_ctx_rh_G_front_inf.Orbital")
  }
  #let's just add in one or two rois to test:
  datalist<-
    list(
      LENGTH=dim(srm.data)[1],
      NUM_CHOICES=2,
      A=param_A,
      response_time=srm.data$reaction_time,
      response=srm.data$choice,
      required_choice=srm.data$cor_res_Counterbalanced,
      cue=srm.data$cue,
      DELTA_N=length(delta_names),
      THETA_N=theta_count
      #need to revise these!
      )
  
  datalist$td_mu_prior=c(rep(0,theta_count),sapply(delta_names,function(tdn){mean(srm.data[,tdn,with=FALSE][[1]])}))
  datalist$td_sd_prior=c(rep(2,theta_count),sapply(delta_names,function(tdn){sd(srm.data[,tdn,with=FALSE][[1]])}))
  datalist$neural_data=as.matrix(srm.data[,delta_names,with=FALSE])
    
    return(datalist)
}
