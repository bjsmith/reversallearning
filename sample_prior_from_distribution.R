require(rstan)
#load("/expdata/bensmith/joint-modeling/data/msm/reversallearning/test_run_list.Rdata");n.chains<-12
#samples_to_return<-sample_prior_from_distribution(run_list_by_run,n.chains)
#so what now? 
#these will provide our initial values.


sample_prior_from_distribution<-function(run_list_by_run,n.chains){
  #this is brand-new approach.
  #This will:
  # - For a specified group of subjects, and a given number of chains
  # - Go to the initial distribution
  # - For each run, for each chain
  # - Go to the run's distribution and sample a set of parameters from a set of probability distributions that are close to highest likelihood
  # - then, I think, we'll need to draw the initial values from the estimated means and distributions for those
  # - Keep the existing bootstrap process for priors.
  
  posterior_pull_version<-"1"
  # sub105data<-rawdata[subid==105 & Motivation=="reward" & runid==1,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]
  
  #we have problems running all subjects in a single run.
  #so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.
  lba_rl_version<-"20180618_1"
  
  single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
  runpackage_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")
  output_dir<-paste0(localsettings$data.dir,"lba_rl/posterior_sample_summary/")
  
  samples_to_pull<-NULL
  samples_to_return<-NULL
  
  for (ri in 1:dim(run_list_by_run)[1]){#ri<-1
    
    sid<-run_list_by_run$subid[ri]
    withinSubRunId<-run_list_by_run$WithinSubjRunId[ri]
    motiv<-run_list_by_run$Motivation[ri]
    
    posterior_pull_filepath<-paste0(output_dir,"version",posterior_pull_version,"s",sid,"r",withinSubRunId,"m",motiv,".RData")
    #look up this item on cache based on its details
    
    if(file.exists(posterior_pull_filepath)){
      load(posterior_pull_filepath)
    }else{
      package_filepath<-paste0(runpackage_dir,"run_package_",sid,"_",withinSubRunId,"_",motiv,"_v2.RData")
      
      load(package_filepath)
      if(any(summary(run_package$fit)$summary[,"Rhat"]>1.05)){
        warning(paste0("Warning: the starting distribution for ",sid,"_",withinSubRunId,"_",motiv," because the Rhat values indicate a possible lack of convergence."))
        print(summary(run_package$fit)$summary[,"Rhat"])
      }
      #only proceed if the Rhat value is good
      
      posteriors_unsorted<-as.data.frame(run_package$fit)
      posteriors<-posteriors_unsorted[order(posteriors_unsorted$lp__),]
      
      min_position<-round(length(posteriors$lp__)/2)
      
      min_log_likelihood<-posteriors$lp__[min_position]#median
      #which value is closet to this min position
      
      max_position<-length(posteriors$lp__)
      #let's sample from maximum posterior to the median posterior
      #at regular quantiles from the min to the max, including each of those.
      samples_to_pull<-round(seq(min_position, max_position,length.out=12))
      #something like the following would be more weighted according to likelihood:
      #log(seq(exp(-23),exp(-25),length.out = 12))
      #now we return these estimates as our posterior.
      
      samples_to_return_ri<-posteriors[samples_to_pull,c("alpha_pr","k_pr","tau_pr")]
      samples_to_return_ri$subid<-sid
      samples_to_return_ri$withinSubRunId<-withinSubRunId
      samples_to_return_ri$motiv<-motiv
      samples_to_return_ri$ConsecSubId<-run_list_by_run$ConsecSubId[ri]
      samples_to_return_ri$chain<-sample(1:n.chains)
      save(samples_to_return_ri,file=posterior_pull_filepath)
    }
    if(is.null(samples_to_return)){
      samples_to_return<-samples_to_return_ri
    }else{
      samples_to_return<-rbind(samples_to_return_ri,samples_to_return)
    }
  }
  return(samples_to_return)
}
