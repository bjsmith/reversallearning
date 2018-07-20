select_rawdata_cols_for_run<-function(rawdata,sid,r,m){
  return(rawdata[subid==sid & Motivation==m & runid==r,
                 c("reaction_time","outcome","cue","choice","cor_res_Counterbalanced",
                   colnames(rawdata)[grepl("ROI_",colnames(rawdata))]),with=FALSE])
}
get_dmn_regions<-function(){
  return(c(
    #ACC
    "ROI_ctx_lh_G_and_S_cingul.Ant","ROI_ctx_rh_G_and_S_cingul.Ant",
    "ROI_ctx_lh_G_and_S_cingul.Mid.Ant","ROI_ctx_rh_G_and_S_cingul.Mid.Ant",
    #"ROI_ctx_lh_G_and_S_cingul.Mid.Post","ROI_ctx_rh_G_and_S_cingul.Mid.Post",
    #"ROI_ctx_lh_S_pericallosal","ROI_ctx_rh_S_pericallosal",
    #VMPFC/OFC
    "ROI_ctx_lh_S_suborbital", "ROI_ctx_rh_S_suborbital",
    "ROI_ctx_lh_G_rectus", "ROI_ctx_rh_G_rectus",
    "ROI_ctx_lh_G_subcallosal", "ROI_ctx_rh_G_subcallosal",
    
    
    
    #"ROI_ctx_lh_G_and_S_transv_frontopol", "ROI_ctx_rh_G_and_S_transv_frontopol",
    #IFG
    # "ROI_ctx_lh_G_front_inf.Orbital", "ROI_ctx_rh_G_front_inf.Orbital",
    # "ROI_ctx_lh_G_front_inf.Opercular", "ROI_ctx_rh_G_front_inf.Opercular",
    # "ROI_ctx_lh_G_front_inf.Triangul", "ROI_ctx_rh_G_front_inf.Triangul",
    # "ROI_ctx_lh_S_front_inf","ROI_ctx_rh_S_front_inf",
    # "ROI_ctx_lh_S_orbital_lateral", "ROI_ctx_rh_S_orbital_lateral",
    # "ROI_ctx_lh_S_orbital_med.olfact", "ROI_ctx_rh_S_orbital_med.olfact",
    # "ROI_ctx_lh_S_orbital.H_Shaped", "ROI_ctx_rh_S_orbital.H_Shaped",
    # "ROI_ctx_lh_G_orbital", "ROI_ctx_rh_G_orbital",
    #DORSO FRONTAL
    "ROI_ctx_lh_G_front_sup", "ROI_ctx_rh_G_front_sup",
    "ROI_ctx_lh_S_front_sup", "ROI_ctx_rh_S_front_sup",
    "ROI_ctx_lh_S_front_middle", "ROI_ctx_rh_S_front_middle",
    #INSULA
    "ROI_ctx_lh_S_circular_insula_ant", "ROI_ctx_rh_S_circular_insula_ant",
    "ROI_ctx_lh_S_circular_insula_sup", "ROI_ctx_rh_S_circular_insula_sup",
    "ROI_ctx_lh_G_insular_short", "ROI_ctx_rh_G_insular_short",
    #striatum
    "ROI_Left.Accumbens.area", "ROI_Right.Accumbens.area",
    "ROI_Left.Amygdala", "ROI_Right.Amygdala",
    "ROI_Left.Caudate", "ROI_Right.Caudate",
    "ROI_Left.Pallidum", "ROI_Right.Pallidum",
    "ROI_Left.Putamen", "ROI_Right.Putamen",
    "ROI_Left.Thalamus.Proper", "ROI_Right.Thalamus.Proper",
    #control regions
    "ROI_ctx_rh_S_occipital_ant","ROI_ctx_lh_S_temporal_sup","ROI_ctx_rh_G_cingul.Post.dorsal"
  ))
}



get_wideset_regions<-function(){
  return(c(
  #ACC
  "ROI_ctx_lh_G_and_S_cingul.Ant","ROI_ctx_rh_G_and_S_cingul.Ant",
  "ROI_ctx_lh_G_and_S_cingul.Mid.Ant","ROI_ctx_rh_G_and_S_cingul.Mid.Ant",
  "ROI_ctx_lh_G_and_S_cingul.Mid.Post","ROI_ctx_rh_G_and_S_cingul.Mid.Post",
  "ROI_ctx_lh_S_pericallosal","ROI_ctx_rh_S_pericallosal",
  #VMPFC+OFC
  "ROI_ctx_lh_G_and_S_transv_frontopol", "ROI_ctx_rh_G_and_S_transv_frontopol",
  "ROI_ctx_lh_G_front_inf.Orbital", "ROI_ctx_rh_G_front_inf.Orbital",
  "ROI_ctx_lh_G_front_inf.Opercular", "ROI_ctx_rh_G_front_inf.Opercular",
  "ROI_ctx_lh_G_front_inf.Triangul", "ROI_ctx_rh_G_front_inf.Triangul",
  "ROI_ctx_lh_S_front_inf","ROI_ctx_rh_S_front_inf",
  "ROI_ctx_lh_G_orbital", "ROI_ctx_rh_G_orbital",
  "ROI_ctx_lh_G_rectus", "ROI_ctx_rh_G_rectus",
  "ROI_ctx_lh_G_subcallosal", "ROI_ctx_rh_G_subcallosal",
  "ROI_ctx_lh_S_orbital_lateral", "ROI_ctx_rh_S_orbital_lateral",
  "ROI_ctx_lh_S_orbital_med.olfact", "ROI_ctx_rh_S_orbital_med.olfact",
  "ROI_ctx_lh_S_orbital.H_Shaped", "ROI_ctx_rh_S_orbital.H_Shaped",
  "ROI_ctx_lh_S_suborbital", "ROI_ctx_rh_S_suborbital",
  #DORSO FRONTAL
  "ROI_ctx_lh_G_front_sup", "ROI_ctx_rh_G_front_sup",
  "ROI_ctx_lh_S_front_sup", "ROI_ctx_rh_S_front_sup",
  "ROI_ctx_lh_S_front_middle", "ROI_ctx_rh_S_front_middle",
  #VISUAL
  "ROI_ctx_lh_G_cuneus",'ROI_ctx_rh_G_cuneus',
  "ROI_ctx_lh_G_occipital_middle", "ROI_ctx_rh_G_occipital_middle",
  "ROI_ctx_lh_G_occipital_sup", "ROI_ctx_rh_G_occipital_sup",
  "ROI_ctx_lh_G_oc.temp_med.Lingual", "ROI_ctx_rh_G_oc.temp_med.Lingual",
  "ROI_ctx_lh_G_and_S_occipital_inf", "ROI_ctx_rh_G_and_S_occipital_inf",
  "ROI_ctx_lh_Pole_occipital", "ROI_ctx_rh_Pole_occipital",
  "ROI_ctx_lh_S_occipital_ant", "ROI_ctx_rh_S_occipital_ant",
  "ROI_ctx_lh_S_oc_middle_and_Lunatus", "ROI_ctx_rh_S_oc_middle_and_Lunatus",
  "ROI_ctx_lh_S_oc_sup_and_transversal", "ROI_ctx_rh_S_oc_sup_and_transversal",
  "ROI_ctx_lh_S_parieto_occipital", "ROI_ctx_rh_S_parieto_occipital",
  #INSULA
  "ROI_ctx_lh_S_circular_insula_ant", "ROI_ctx_rh_S_circular_insula_ant",
  "ROI_ctx_lh_S_circular_insula_sup", "ROI_ctx_rh_S_circular_insula_sup",
  "ROI_ctx_lh_G_insular_short", "ROI_ctx_rh_G_insular_short",
  #striatum
  "ROI_Left.Accumbens.area", "ROI_Right.Accumbens.area",
  "ROI_Left.Amygdala", "ROI_Right.Amygdala",
  "ROI_Left.Caudate", "ROI_Right.Caudate",
  "ROI_Left.Pallidum", "ROI_Right.Pallidum",
  "ROI_Left.Putamen", "ROI_Right.Putamen",
  "ROI_Left.Thalamus.Proper", "ROI_Right.Thalamus.Proper",
  #control regions
  "ROI_ctx_rh_S_occipital_ant","ROI_ctx_lh_S_temporal_sup","ROI_ctx_rh_G_cingul.Post.dorsal"
  ))
}

