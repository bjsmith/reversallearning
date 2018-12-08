library(dplyr)
#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.

single_level_model_summarize_fast <- function(
  
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180628_1",
  model_filename="lba_rl_single_exp_joint_v2",
  model_subversion="_joint_v1",
  avoid_saving=FALSE
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
    print("loading results summary...")
    load(results_summary_list_filepath)
  }else{
    results.summary.list<-list()
  }
  
  
  grand_posterior_estimate<-NULL
  #lba_rl_single<-stan_model('stanlba/stanfiles/lba_rl_single_exp_v2.stan')
  if(file.exists(grand_posterior_estimate_filepath)){
    tryCatch({
      print("loading existing posterior estimates...")
      load(grand_posterior_estimate_filepath)
    },error=function(e){
      print("error loading posterior estimates. starting from scratch.")
    })
  }
  
  #determine which objects to grab according to which objects currently exist.
  
  #maximum size of list we will need is
  run_list<-rawdata[,.N,.(subid,runid,Motivation)]
  run_list$RunIdOverAll<-1:dim(run_list)[1]
  max_list_size<-dim(run_list)[1]
  results.summary.list<-c(results.summary.list,vector("list",max_list_size-length(results.summary.list)))#extned the existing summary list.
  posterior_items_to_add<-vector("list",max_list_size)

  
  save_point<-40
  #posterior_estimate_list<-vector("list",save_point)
  save_new_posteriors<-FALSE
  save_summary<-FALSE
  for (rli in 1:dim(run_list)[1]){#sid<-106;r<-1;m<-"reward"#rli<-8
    cat(".")
    #print("iterating")
    sid=run_list[[rli,"subid"]]
    r=run_list[[rli,"runid"]]
    m=run_list[[rli,"Motivation"]]
    list_counter<-run_list[[rli,"RunIdOverAll"]]
    #print(rli)

    posterior_estimate_runcode<-paste0("s",sid,"_r",r,"_",m)
    package_filepath<-paste0(output_dir,"run_package_",sid,"_",r,"_",m,"_",model_name,model_subversion,".RData")
    
    #print(paste(sid,r,m))
    
    summary_created<-any(unlist(lapply(results.summary.list[!unlist(lapply(results.summary.list,is.null))],
                                       function(rsl){
                                         rsl$sid==sid & rsl$rid==r & rsl$motivation==m
                                         })))
    added_to_grand_posterior<-any(grand_posterior_estimate$UniqueRunCode==posterior_estimate_runcode)
    #print("checking file exists...")
    if(file.exists(package_filepath)){
      # print("getting srm.data")
      # srm.data<-rawdata[subid==sid & Motivation==m & runid==r,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]
      #print("file exists. Checking if a record has been created...")
      if((!summary_created | !added_to_grand_posterior)){ 
        #we haven't already got an entry for this one.{
        print(paste0("loading from file sid ",sid, "; r ",r, "; m ", m))
        load(package_filepath)
        #create the summary
        if(!summary_created){
          fit_summary<-summary(run_package$fit)$summary
          run_summary_package<-list("sid"=run_package$sid,
                                    "rid"=run_package$rid,
                                    "motivation"=run_package$motivation,
                                    fit_summary=fit_summary,duration=run_package$duration)
          rsl_first_space<-min(which(unlist(lapply(results.summary.list,is.null))))
          results.summary.list[[rsl_first_space]]<-run_summary_package
          save_summary<-TRUE
        }
        #add the posteriors to the posterior grand mean.
        
        if(!added_to_grand_posterior){
          
          posterior_estimate<-data.table(as.matrix(run_package$fit))
          posterior_estimate$UniqueRunCode<-posterior_estimate_runcode
          posterior_estimate$SubjectId<-sid
          posterior_items_to_add[[list_counter]]<-posterior_estimate
          save_new_posteriors<-TRUE
          #posterior_items_to_add[list_counter %% save_point + 1]<-posterior_estimate
          
          #posterior_estimate_list<-c(posterior_estimate_list,list(posterior_estimate))
          # if(list_counter%%save_point==0){#only save if we're on a multiple of 20
          #   new_post_items<-rbindlist(posterior_items_to_add[!unlist(lapply(posterior_items_to_add,is.null))])
          #   if(is.null(grand_posterior_estimate)){
          #     grand_posterior_estimate<-new_post_items
          #   }else{
          #     grand_posterior_estimate<-rbind(grand_posterior_estimate,new_post_items)
          #   }
          #   new_post_items<-vector("list",save_point)
          # }
        }
      }else{
        cat("*")
      } 
    }else{
      #print(paste0("no file basis file exists for ",package_filepath))
    } 
    
  }
  
  cat("saving results summary list...")
  #...and once at the end.
  #only save if the list got bigger!
  
  results.summary.list<-results.summary.list[!unlist(lapply(results.summary.list,is.null))]
  if(save_summary & !avoid_saving)  save(results.summary.list,file=results_summary_list_filepath)
  cat("list saved. \nSaving posterior estimates...")
  new_post_items<-rbindlist(posterior_items_to_add[!unlist(lapply(posterior_items_to_add,is.null))],fill=TRUE)
  
  if(is.null(grand_posterior_estimate)){
    grand_posterior_estimate<-new_post_items
  }else{
    grand_posterior_estimate<-rbind(grand_posterior_estimate,new_post_items,fill=TRUE)
  }
  if(save_new_posteriors & !avoid_saving) save(grand_posterior_estimate,file=grand_posterior_estimate_filepath)
  cat("estimates saved.\n")
  #clean up
  #results.summary.list
  
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
              "results_summary"=results.summary.df,
              "results_summary_dt"=data.table(results.summary.df)))
}

