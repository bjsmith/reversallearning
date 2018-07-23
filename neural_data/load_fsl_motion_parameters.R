load_fsl_motion_params<-function(){
  motion_param_list<-vector("list",(400-100)*2*2)
  iter<-0
  for (sid in 100:400){
    
    for (rid in 1:2){
      for(motivation in c("Reward","Punish")){
        cat(". ")
        iter<-iter+1
        #sid<-106;rid=1;motivation="Reward"
        param_path<-paste0("/expdata/xfgavin/MSM/sub",sid,"/analysis/ReversalLearning_",motivation,"_run",rid,"_pre.feat/mc/prefiltered_func_data_mcf.par")
        if(file.exists(param_path)){
          motion_param_dt<-load_fsl_motion_param_for_run(sid,rid,motivation)
          colnames(motion_param_dt)<-paste0("Motion",1:6)
          motion_param_dt$SubID<-sid
          motion_param_dt$runid<-rid
          motion_param_dt$motivation<-tolower(motivation)
          motion_param_list[[iter]]<-motion_param_dt
        }
      }
    }
  }
  motion_param_all_dt<-rbindlist(motion_param_list)
}