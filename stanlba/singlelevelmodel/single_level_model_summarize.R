
#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.

single_level_model_summarize <- function(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180628_1",
  model_filename="lba_rl_single_exp_joint_v2",
  model_subversion="_joint_v1"
  ){
  model_name<-model_filename
  
  output_dir<-paste0(single_run_dir,"/",model_version, "/")
  dir.create(single_run_dir, showWarnings = FALSE)
  dir.create(output_dir, showWarnings = FALSE)
  #file_folder<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/lba_rl_single_estimates.RData"
  #load(file=file_folder)
  results_summary_list_filepath<-paste0(output_dir,"run_package_summary_",model_name,"_",model_subversion,".RData")
  grand_posterior_estimate_filepath<-paste0(output_dir,"posteriors_",model_name,"_",model_subversion,".RData")
  if(file.exists(results_summary_list_filepath)){
    load(results_summary_list_filepath)
  }else{
    results.summary.list<-list()
  }
  
  
  #lba_rl_single<-stan_model('stanlba/stanfiles/lba_rl_single_exp_v2.stan')
  if(file.exists(grand_posterior_estimate_filepath)){
    load(grand_posterior_estimate_filepath)
  }else{
    grand_posterior_estimate<-NULL
  }
  
  for (sid in unique(rawdata$subid)){#sid<-106
    for (r in unique(rawdata[subid==sid,runid])){#r<-1
      for(m in unique(rawdata[subid==sid & runid==r,Motivation])){#m<-"reward"
        posterior_estimate_runcode<-paste0("s",sid,"_r",r,"_",m)
        package_filepath<-paste0(output_dir,"run_package_",sid,"_",r,"_",m,"_",model_name,model_subversion,".RData")
        srm.data<-rawdata[subid==sid & Motivation==m & runid==r,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]
        summary_created<-any(unlist(lapply(results.summary.list,function(rli){rli$sid==sid & rli$rid==r & rli$motivation==m})))
        added_to_grand_posterior<-any(grand_posterior_estimate$UniqueRunCode==posterior_estimate_runcode)
        if((!summary_created | !added_to_grand_posterior) & 
           #we haven't already got an entry for this one.
           file.exists(package_filepath)){
          print(paste0("loading from file sid ",sid, "; r ",r, "; m ", m))
          load(package_filepath)
          #create the summary
          if(!summary_created){
            fit_summary<-summary(run_package$fit)$summary
            run_summary_package<-list("sid"=run_package$sid,"rid"=run_package$rid,"motivation"=run_package$motivation,
                                      fit_summary=fit_summary,duration=run_package$duration)
            results.summary.list<-c(results.summary.list,list(run_summary_package))
            save(results.summary.list,file=results_summary_list_filepath)
          }
          #add the posteriors to the posterior grand mean.
          if(!added_to_grand_posterior){
            posterior_estimate<-data.frame(as.matrix(run_package$fit))
            posterior_estimate$UniqueRunCode<-posterior_estimate_runcode
            posterior_estimate$SubjectId<-sid
            if(is.null(grand_posterior_estimate))grand_posterior_estimate<-posterior_estimate
            else grand_posterior_estimate<-rbind(grand_posterior_estimate,posterior_estimate)
            save(grand_posterior_estimate,file=grand_posterior_estimate_filepath)
          }
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
  
  rownames(results.summary.df)<-NULL
  
  return(list("complete_posterior_list"=grand_posterior_estimate,
              "results_summary"=results.summary.df))
}
