
library(data.table)
#takes from all of the subjects and samples with replacement;
#bootstraps a sample equal to the sample size;
#and repeats this n number of times to get a set of randomized estimates
#suitable for use for n chains
#risks being a bit conservative because it really goes for the central point in any given distribution
#however let's try it and see what happens!
bootstrap_smart_init_vals<-function(n_samples,subid_set,bootstrap_seed=NA){
  #subid_set<-unique(multisubj_multirun_moresubs$subid);n_samples<-3
  #now get the stats associated with that.
  source("load_lba_rl_allsingles_resultsdata.R")
  
  
  rownames(results.summary.df)<-NULL
  
  results.summary.dt<-data.table(results.summary.df)
  results.summary.dt.inrange<-results.summary.dt[sid %in% subid_set]

  n_subs<-length(unique(results.summary.dt.inrange$sid))
  
  #remove subjects with improperly estimated subjects from the sample set.
  improperly.estimated.runs<-unique(results.summary.dt.inrange[which(results.summary.dt.inrange$Rhat>1.05),.(sid,rid,motivation,FullRunId)])
  subid_set_properly_estimated<-subid_set[!(subid_set %in% improperly.estimated.runs$sid)]
  init_val_list<-as.list(rep(NA,times=n_samples))
  if(length(bootstrap_seed)==1){
    if(!is.na(bootstrap_seed)){
      set.seed(bootstrap_seed)
    }#otherwise don't bother setting a seed.
  }
  for (bs in 1:n_samples){#bs<-1
    #sample from the subjects with replacement
    if (length(bootstrap_seed)==n_samples){
      set.seed(bootstrap_seed[bs])
      print("seedsetting")
    }else if (length(bootstrap_seed)!=1 & length(bootstrap_seed)){
      stop("optional argument 'bootstrap_seed' for function bootstrap_smart_init_vals must be NA, length(1), or length(n_samples)")
    }
    subjs_to_sample<-sample(subid_set_properly_estimated,size=n_subs,replace = TRUE)
    print(sort(subjs_to_sample))
    ds_boot<-NULL
    #now create the bootstrapped sample.
    for (bi in 1:length(subjs_to_sample)){#sid<-subjs_to_sample[2]
      sbid<-subjs_to_sample[bi]
      ds_boot_add<-results.summary.dt.inrange[sid==sbid]
      #in order for this to be processed properly later on we have to use the 'bootstrap subject ID' as the actual ID.
      ds_boot_add$RealSubId<-ds_boot_add$sid
      ds_boot_add$BootstrapSubId<-bi
      ds_boot_add$sid<-ds_boot_add$BootstrapSubId
      if(is.null(ds_boot)){ds_boot<-ds_boot_add
      }else{ds_boot<-rbind(ds_boot,ds_boot_add)}
    }  
    
    #now we generate the summary stats
    #we're going to need to give the subjects in the boot
    #source("generate_lbarl_group_summary_stats.R")
    init_val_list[[bs]]<-generate_lbarl_group_summary_stats(ds_boot)
    
  }
  
  return (init_val_list)
  
   
}