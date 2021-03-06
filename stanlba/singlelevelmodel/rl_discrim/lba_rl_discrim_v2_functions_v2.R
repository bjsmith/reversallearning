require(dplyr)
#lba_rl_discrim_v2_functions_v2.R
#version 1 of the functions appeared to not order the data in the correct order and this is probably the place it should happen.
#Version 2 ensurs the data selected is in order.

select_rawdata_cols_for_run<-function(rawdata,sid,r,m){
  return(rawdata[subid==sid & Motivation==m & runid==r,] %>% .[order(onset_time_actual),.(
                  reaction_time,outcome,cue,choice,cor_res_Counterbalanced,presentation_n,
                  onset_time_actual,
                   #ACC
                   ROI_ctx_lh_G_and_S_cingul.Ant,ROI_ctx_rh_G_and_S_cingul.Ant,
                   ROI_ctx_lh_G_and_S_cingul.Mid.Ant,ROI_ctx_rh_G_and_S_cingul.Mid.Ant,
                   ROI_ctx_lh_G_and_S_cingul.Mid.Post,ROI_ctx_rh_G_and_S_cingul.Mid.Post,
                   ROI_ctx_lh_S_pericallosal,ROI_ctx_rh_S_pericallosal,
                   #VMPFC+OFC
                   ROI_ctx_lh_G_and_S_transv_frontopol, ROI_ctx_rh_G_and_S_transv_frontopol,
                   ROI_ctx_lh_G_front_inf.Orbital, ROI_ctx_rh_G_front_inf.Orbital,
                   ROI_ctx_lh_G_front_inf.Opercular, ROI_ctx_rh_G_front_inf.Opercular,
                   ROI_ctx_lh_G_front_inf.Triangul, ROI_ctx_rh_G_front_inf.Triangul,
                   ROI_ctx_lh_S_front_inf,ROI_ctx_rh_S_front_inf,
                   ROI_ctx_lh_G_orbital, ROI_ctx_rh_G_orbital,
                   ROI_ctx_lh_G_rectus, ROI_ctx_rh_G_rectus,
                   ROI_ctx_lh_G_subcallosal, ROI_ctx_rh_G_subcallosal,
                   ROI_ctx_lh_S_orbital_lateral, ROI_ctx_rh_S_orbital_lateral,
                   ROI_ctx_lh_S_orbital_med.olfact, ROI_ctx_rh_S_orbital_med.olfact,
                   ROI_ctx_lh_S_orbital.H_Shaped, ROI_ctx_rh_S_orbital.H_Shaped,
                   ROI_ctx_lh_S_suborbital, ROI_ctx_rh_S_suborbital,
                   #DORSO FRONTAL
                   ROI_ctx_lh_G_front_sup, ROI_ctx_rh_G_front_sup,
                   ROI_ctx_lh_S_front_sup, ROI_ctx_rh_S_front_sup,
                   ROI_ctx_lh_S_front_middle, ROI_ctx_rh_S_front_middle,
                   #VISUAL
                   ROI_ctx_lh_G_cuneus,ROI_ctx_rh_G_cuneus,
                   ROI_ctx_lh_G_occipital_middle, ROI_ctx_rh_G_occipital_middle,
                   ROI_ctx_lh_G_occipital_sup, ROI_ctx_rh_G_occipital_sup,
                   ROI_ctx_lh_G_oc.temp_med.Lingual, ROI_ctx_rh_G_oc.temp_med.Lingual,
                   ROI_ctx_lh_G_and_S_occipital_inf, ROI_ctx_rh_G_and_S_occipital_inf,
                   ROI_ctx_lh_Pole_occipital, ROI_ctx_rh_Pole_occipital,
                   ROI_ctx_lh_S_occipital_ant, ROI_ctx_rh_S_occipital_ant,
                   ROI_ctx_lh_S_oc_middle_and_Lunatus, ROI_ctx_rh_S_oc_middle_and_Lunatus,
                   ROI_ctx_lh_S_oc_sup_and_transversal, ROI_ctx_rh_S_oc_sup_and_transversal,
                   ROI_ctx_lh_S_parieto_occipital, ROI_ctx_rh_S_parieto_occipital,
                   #INSULA
                   ROI_ctx_lh_S_circular_insula_ant, ROI_ctx_rh_S_circular_insula_ant,
                   ROI_ctx_lh_S_circular_insula_sup, ROI_ctx_rh_S_circular_insula_sup,
                   ROI_ctx_lh_G_insular_short, ROI_ctx_rh_G_insular_short,
                   #striatum
                   ROI_Left.Accumbens.area, ROI_Right.Accumbens.area,
                   ROI_Left.Amygdala, ROI_Right.Amygdala,
                   ROI_Left.Caudate, ROI_Right.Caudate,
                   ROI_Left.Pallidum, ROI_Right.Pallidum,
                   ROI_Left.Putamen, ROI_Right.Putamen,
                   ROI_Left.Thalamus.Proper, ROI_Right.Thalamus.Proper,
                   #control regions
                   ROI_ctx_rh_S_occipital_ant,ROI_ctx_lh_S_temporal_sup,ROI_ctx_rh_G_cingul.Post.dorsal
                 )])
}

create_standatalist<-function(srm.data){
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
    td_mu_prior=c(0,#0,
                  mean(srm.data$ROI_Left.Accumbens.area),
                  mean(srm.data$ROI_Right.Accumbens.area),
                  mean(srm.data$ROI_ctx_lh_G_front_inf.Orbital),
                  mean(srm.data$ROI_ctx_rh_G_front_inf.Orbital)),
    td_sd_prior=c(2,#2,
                  sd(srm.data$ROI_Left.Accumbens.area),
                  sd(srm.data$ROI_Right.Accumbens.area),
                  sd(srm.data$ROI_ctx_lh_G_front_inf.Orbital),
                  sd(srm.data$ROI_ctx_rh_G_front_inf.Orbital)),
    neural_data=as.matrix(srm.data[,.(ROI_Left.Accumbens.area,ROI_Right.Accumbens.area,ROI_ctx_lh_G_front_inf.Orbital,ROI_ctx_rh_G_front_inf.Orbital)]),
    DELTA_accumbens_lh=1,DELTA_accumbens_rh=2,DELTA_ofc_lh=3,DELTA_ofc_rh=4)
}


get_starting_values_behavioral<-function(n_chains,seed=25902583,thetaDelta_count=6){#n_chains=7
  set.seed(seed)
  alpha_pr<-qnorm1sd(n_chains)-3
  alpha_discrim_pr<-qnorm1sd(n_chains)-3
  k_pr<-qnorm1sd(n_chains)+log(0.5)
  tau_pr<-qnorm1sd(n_chains)+log(0.5)
  td_mu_txc<-do.call(rbind,lapply(1:thetaDelta_count,function(i){qnorm1sd(n_chains)}))
  L_sigma_txc<-do.call(rbind,(lapply(1:thetaDelta_count,function(i){sample(qcauchy(seq(0.5,0.975,0.475/(n_chains+2)),0,2.5)[2:(n_chains+1)],replace=FALSE)})))
  
  return(lapply(1:n_chains,function(i){list(
    "alpha_pr"=alpha_pr[i],
    "k_pr"=k_pr[i],
    "tau_pr"=tau_pr[i],
    "alpha_discrim_pr"=alpha_discrim_pr[i])}))
}
# 
# get_starting_values_alphabeta<-function(n_chains,seed=25902583,theta_count=6){#n_chains=7
#   set.seed(seed)
#   alpha_pr<-qnorm1sd(n_chains)
#   beta_pr<-qnorm2sd(n_chains)
#   # k_pr<-qnorm1sd(n_chains)+log(0.5)
#   # tau_pr<-qnorm1sd(n_chains)+log(0.5)
#   td_mu_txc<-do.call(rbind,lapply(1:theta_count,function(i){qnorm1sd(n_chains)}))
#   L_sigma_txc<-do.call(rbind,(lapply(1:theta_count,function(i){sample(qcauchy(seq(0.5,0.975,0.475/(n_chains+2)),0,2.5)[2:(n_chains+1)],replace=FALSE)})))
#   
#   return(lapply(1:n_chains,function(i){list(
#     "alpha_pr"=alpha_pr[i],
#     "beta_pr"=beta_pr[i],
#     "td_mu"=td_mu_txc[,i],
#     "L_sigma"=L_sigma_txc[,i],
#     "L_Omega"=diag(theta_count))}))
# }