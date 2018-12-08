
create_standatalist<-function(srm.data,theta_count){
  #let's just add in one or two rois to test:
  
  
  list(
    LENGTH=dim(srm.data)[1],
    NUM_CHOICES=2,
    A=0.01,
    response_time=srm.data$reaction_time,
    response=srm.data$choice,
    required_choice=srm.data$cor_res_Counterbalanced,
    cue=srm.data$cue,
    cue_presentation_n=srm.data$presentation_n,
    n_back=3,
    #need to revise these!
    td_mu_prior=c(rep(0,theta_count),
                  mean(srm.data$ROI_Left.Accumbens.area),
                  mean(srm.data$ROI_Right.Accumbens.area),
                  mean(srm.data$ROI_ctx_lh_G_front_inf.Orbital),
                  mean(srm.data$ROI_ctx_rh_G_front_inf.Orbital)),
    td_sd_prior=c(rep(1,theta_count),
                  sd(srm.data$ROI_Left.Accumbens.area),
                  sd(srm.data$ROI_Right.Accumbens.area),
                  sd(srm.data$ROI_ctx_lh_G_front_inf.Orbital),
                  sd(srm.data$ROI_ctx_rh_G_front_inf.Orbital)),
    neural_data=as.matrix(srm.data[,.(ROI_Left.Accumbens.area,ROI_Right.Accumbens.area,ROI_ctx_lh_G_front_inf.Orbital,ROI_ctx_rh_G_front_inf.Orbital)]),
    DELTA_N=4
    
    
    )
}
