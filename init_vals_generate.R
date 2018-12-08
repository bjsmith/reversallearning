qnorm2sd<-function(n){
  sample(qnorm(seq(0.025,0.975,0.95/(n-1))),replace=FALSE)
}



source("sample_prior_from_distribution.R")
get_empirical_distribution_allchains<- function(
  n_chains,
  run_list_by_run,run_list_by_run_toplevel=NULL){#bootstrapped_sample_list<-smart_init_vals
  #run_list_by_run_toplevel<-run_list_by_run_all
  #n_chains = n_chains; run_list_by_run=run_list_by_run;run_list_by_run_toplevel= run_list_by_run_all
  
  #bottom-level and top-level are split; 
  #this allows us to run bottom-level calculations with only the required chains
  #while running top-level calculations over all the subjects.
  #the default is to treat them the same, but this can be overridden by passing a value to run_list_by_run_toplevel
  if(is.null(run_list_by_run_toplevel))run_list_by_run_toplevel<-run_list_by_run
  
  #get bottom-level values
  sampled_priors<-data.table(sample_prior_from_distribution(run_list_by_run,n_chains))
  ret_list<-vector("list",n_chains)
  
  #top level values. May be the same as bottom-level or may be different.
  sampled_priors_toplevel<-data.table(sample_prior_from_distribution(run_list_by_run_toplevel,n_chains))
  
  for (i_chain in 1:n_chains){#i_chain<-2
    #maybe move the contents of this loop to a list..?
    subj_means<-sampled_priors[chain==i_chain,.(AlphaSubjMean=mean(alpha_pr),KSubjMean=mean(k_pr),TauSubjMean=mean(tau_pr)),.(subid)]
    subj_sds<-sampled_priors[chain==i_chain,.(AlphaSubjSD=sd(alpha_pr),KSubjSD=sd(k_pr),TauSubjSD=sd(tau_pr)),.(subid)]
    
    warning("This is used when calculating deviance from the group for bottom-level values. Is that really what we want? I think it is..")
    subj_means_toplevel<-sampled_priors_toplevel[chain==i_chain,.(AlphaSubjMean=mean(alpha_pr),KSubjMean=mean(k_pr),TauSubjMean=mean(tau_pr)),.(subid)]
    subj_sds_toplevel<-sampled_priors_toplevel[chain==i_chain,.(AlphaSubjSD=sd(alpha_pr),KSubjSD=sd(k_pr),TauSubjSD=sd(tau_pr)),.(subid)]
    
    subj_mu_toplevel=c(mean(subj_means_toplevel$AlphaSubjMean),mean(subj_means_toplevel$KSubjMean),mean(subj_means_toplevel$TauSubjMean))
    subj_sigma_toplevel=c(sd(subj_means_toplevel$AlphaSubjMean),sd(subj_means_toplevel$KSubjMean),sd(subj_means_toplevel$TauSubjMean))
    
    #what are the empirical run_mu values?
    #well, these cannot be bootstrapped, can they? these must be from the actual data.
    #but they must be from the summaries...
    #which subjects to extract (must be in the right order!)
    #subj_ids<-sort(unique(data_to_use$subid))#sort it so that it's guaranteed to be in a particular order.
    
    run_mu<-subj_means[order(subid),2:4]
    run_sigma<-subj_sds[order(subid),2:4]
    run_sigma_toplevel<-subj_sds_toplevel[order(subid),2:4]
    run_sigma_gamma_toplevel<-colMeans(run_sigma_toplevel,na.rm = TRUE)
    
    #bootstrapped_sample
    #run_mu[s,] = to_vector(run_mu_var[s,]) .* subj_sigma + subj_mu;
    #rearranging:
    run_mu_var = 
      (run_mu-matrix(subj_mu_toplevel,nrow=dim(run_mu)[1],ncol=dim(run_mu)[2],byrow=TRUE))/
      matrix(subj_sigma_toplevel,nrow = dim(run_mu)[1],ncol=dim(run_mu)[2],byrow = TRUE)
    #should be an N_SUBJ*N_PARAM matrix.
    
    
    #alpha[r] = inv_logit(run_mu[run_subjid[r],PARID_alpha] + run_sigma[run_subjid[r],PARID_alpha] * alpha_pr_var[r]);
    #rearranging:
    #pr_var=(alpha_pr-run_mu[run_subjid[r],PARID_alpha])/run_sigma[run_subjid[r],PARID_alpha]
    #this will be one entry for each RUN in the RUNLIST.
    alpha_pr_var<-rep(NA,length(run_list_by_run$StanRunID))
    k_pr_var<-rep(NA,length(run_list_by_run$StanRunID))
    tau_pr_var<-rep(NA,length(run_list_by_run$StanRunID))
    
    for (stanRunID in run_list_by_run$StanRunID){#stanRunID=25
      run_ot<-as.character(run_list_by_run[StanRunID==stanRunID,Motivation])
      withinSubjRunId<-run_list_by_run[StanRunID==stanRunID,WithinSubjRunId]
      runSubjId<-run_list_by_run[StanRunID==stanRunID,subid]
      #the subject for this run...
      alpha_pr<-sampled_priors[subid==runSubjId & motiv==run_ot & withinSubRunId==withinSubjRunId & chain==i_chain,alpha_pr]
      alpha_pr_var[stanRunID]<-unlist((alpha_pr-run_mu[run_list_by_run$ConsecSubId[stanRunID],1])/run_sigma[run_list_by_run$ConsecSubId[stanRunID],1])
      
      k_pr<-sampled_priors[subid==runSubjId & motiv==run_ot & withinSubRunId==withinSubjRunId & chain==i_chain,k_pr]
      k_pr_var[stanRunID]<-unlist((k_pr-run_mu[run_list_by_run$ConsecSubId[stanRunID],2])/run_sigma[run_list_by_run$ConsecSubId[stanRunID],2])
      
      tau_pr<-sampled_priors[subid==runSubjId & motiv==run_ot & withinSubRunId==withinSubjRunId & chain==i_chain,tau_pr]
      tau_pr_var[stanRunID]<-unlist(
        (tau_pr-run_mu[run_list_by_run$ConsecSubId[stanRunID],3])/run_sigma[run_list_by_run$ConsecSubId[stanRunID],3])
      
    }
    ret_list[[i_chain]]<-
      list(
        ################
        ######GROUP LEVEL
        
        subj_mu=subj_mu_toplevel,
        subj_sigma=subj_sigma_toplevel,
        run_sigma_gamma=run_sigma_gamma_toplevel,
        
        #not sure that we really need to define transformed parameters, maybe only sampled parameters.
        #define with an artificially normal, truncated distribution.
        ################
        ####SUBJECT LEVEL
        
        run_mu_var=run_mu_var,
        #run_mu_var=matrix(qnorm2sd(data_to_pass$NUM_SUBJECTS*3),ncol=3),
        
        #NUM_SUBJECTS rows, NUM_PARAMS columns
        #[NUM_SUBJECTS,NUM_PARAMS];
        #might need to get these empirically rather than just trying to filter down.
        #or convert it into non-centered and then see where we go from there.
        run_sigma=run_sigma,
        #I think these cauchys are probably going to screw us!
        #no way we can start with these starting values.
        
        ################
        ######RUN LEVEL
        alpha_pr_var=alpha_pr_var,
        k_pr_var=k_pr_var,
        tau_pr_var=tau_pr_var
        #alpha_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
        #k_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
        #tau_pr_var=qnorm2sd(data_to_pass$NUM_RUNS)
      )
  }
  return(ret_list)
}


get_bootstrapped_init_vals_with_bottomlevel_inits<- function(
  bootstrapped_sample,
  bootseed=1761614456,
  data_to_pass,
  data_to_use,
  results.summary.dt,
  run_list_by_run){#bootstrapped_sample<-smart_init_vals[[1]]
  set.seed(bootseed)
  
  subj_mu=c(bootstrapped_sample$alpha_pr_mean,bootstrapped_sample$k_pr_mean,bootstrapped_sample$tau_pr_mean)
  subj_sigma=c(bootstrapped_sample$alpha_sd_prior,bootstrapped_sample$k_sd_prior,bootstrapped_sample$tau_sd_prior)
  #what are the empirical run_mu values?
  #well, these cannot be bootstrapped, can they? these must be from the actual data.
  #but they must be from the summaries...
  #which subjects to extract (must be in the right order!)
  subj_ids<-sort(unique(data_to_use$subid))#sort it so that it's guaranteed to be in a particular order.
  #now get the sample data
  get_sample_summary<-function(subj_id){
    results.summary.dt[sid==subj_id & param_name %in% c("alpha_pr", "k_pr", "tau_pr"),
                       .(run_mu=mean(mean),run_sigma=sd(mean)),
                       by=.(sid,param_name)]
  }
  run_mu<-
    t(sapply(subj_ids,function(subj_id){
      #run_mu will be the estimate of all the runs together.
      get_sample_summary(subj_id)$run_mu
    }))
  run_sigma<-
    t(sapply(subj_ids,function(subj_id){
      #run_mu will be the estimate of all the runs together.
      get_sample_summary(subj_id)$run_sigma
    }))
  
  
  #bootstrapped_sample
  #run_mu[s,] = to_vector(run_mu_var[s,]) .* subj_sigma + subj_mu;
  #rearranging:
  run_mu_var = 
    (run_mu-matrix(subj_mu,nrow=dim(run_mu)[1],ncol=dim(run_mu)[2],byrow=TRUE))/
    matrix(subj_sigma,nrow = dim(run_mu)[1],ncol=dim(run_mu)[2],byrow = TRUE)#rep(subj_sigma,times=dim(run_mu)[1]),
  #should be an N_SUBJ*N_PARAM matrix.
  
  #alpha[r] = inv_logit(run_mu[run_subjid[r],PARID_alpha] + run_sigma[run_subjid[r],PARID_alpha] * alpha_pr_var[r]);
  #rearranging:
  #pr_var=(alpha_pr-run_mu[run_subjid[r],PARID_alpha])/run_sigma[run_subjid[r],PARID_alpha]
  #this will be one entry for each RUN in the RUNLIST.
  alpha_pr_var<-rep(NA,length(run_list_by_run$StanRunID))
  k_pr_var<-rep(NA,length(run_list_by_run$StanRunID))
  tau_pr_var<-rep(NA,length(run_list_by_run$StanRunID))
  
  for (stanRunID in run_list_by_run$StanRunID){#stanRunID=25
    run_ot<-as.character(run_list_by_run[StanRunID==stanRunID,Motivation])
    withinSubjRunId<-run_list_by_run[StanRunID==stanRunID,WithinSubjRunId]
    runSubjId<-run_list_by_run[StanRunID==stanRunID,subid]
    #the subject for this run...
    #full_runid<-run_list$RunID[consec_runid]
    alpha_pr<-results.summary.dt[param_name=="alpha_pr" & motivation==run_ot & rid==withinSubjRunId & sid==runSubjId,mean]
    alpha_pr_var[stanRunID]<-(alpha_pr-run_mu[run_list_by_run$ConsecSubId[stanRunID],1])/run_sigma[run_list_by_run$ConsecSubId[stanRunID],1]
    
    k_pr<-results.summary.dt[param_name=="k_pr" & motivation==run_ot & rid==withinSubjRunId & sid==runSubjId,mean]
    k_pr_var[stanRunID]<-(k_pr-run_mu[run_list_by_run$ConsecSubId[stanRunID],2])/run_sigma[run_list_by_run$ConsecSubId[stanRunID],2]
    
    tau_pr<-results.summary.dt[param_name=="tau_pr"& motivation==run_ot & rid==withinSubjRunId & sid==runSubjId,mean]
    tau_pr_var[stanRunID]<-(tau_pr-run_mu[run_list_by_run$ConsecSubId[stanRunID],3])/run_sigma[run_list_by_run$ConsecSubId[stanRunID],3]
    
  }
  
  return(
    list(
      ################
      ######GROUP LEVEL
      
      subj_mu=subj_mu,
      subj_sigma=subj_sigma,
      run_sigma_gamma=c(bootstrapped_sample$alpha_run_sigma_gamma,bootstrapped_sample$k_run_sigma_gamma,bootstrapped_sample$tau_run_sigma_gamma),
      
      #not sure that we really need to define transformed parameters, maybe only sampled parameters.
      #define with an artificially normal, truncated distribution.
      ################
      ####SUBJECT LEVEL
      
      #run_mu_var=run_mu_var,
      run_mu_var=matrix(qnorm2sd(data_to_pass$NUM_SUBJECTS*3),ncol=3),
      
      #NUM_SUBJECTS rows, NUM_PARAMS columns
      #[NUM_SUBJECTS,NUM_PARAMS];
      #might need to get these empirically rather than just trying to filter down.
      #or convert it into non-centered and then see where we go from there.
      run_sigma=run_sigma,
      #I think these cauchys are probably going to screw us!
      #no way we can start with these starting values.
      
      ################
      ######RUN LEVEL
      alpha_pr_var=alpha_pr_var,
      k_pr_var=k_pr_var,
      tau_pr_var=tau_pr_var
      #alpha_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
      #k_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
      #tau_pr_var=qnorm2sd(data_to_pass$NUM_RUNS)
    )
  )
}

get_bootstrapped_init_vals<- function(bootstrapped_sample,bootseed=1761614456,data_to_pass){#bootstrapped_sample<-smart_init_vals[[1]]
  set.seed(bootseed)
  
  #print(paste0("running get_bootstrapped_init_vals; data_to_pass$NUM_SUBJECTS=",data_to_pass$NUM_SUBJECTS))
  return(
    list(
      ################
      ######GROUP LEVEL
      subj_mu=c(bootstrapped_sample$alpha_pr_mean,bootstrapped_sample$k_pr_mean,bootstrapped_sample$tau_pr_mean),
      subj_sigma=c(bootstrapped_sample$alpha_sd_prior,bootstrapped_sample$k_sd_prior,bootstrapped_sample$tau_sd_prior),
      run_sigma_gamma=c(bootstrapped_sample$alpha_run_sigma_gamma,bootstrapped_sample$k_run_sigma_gamma,bootstrapped_sample$tau_run_sigma_gamma),
      
      #not sure that we really need to define transformed parameters, maybe only sampled parameters.
      #define with an artificially normal, truncated distribution.
      ################
      ####SUBJECT LEVEL
      run_mu_var=matrix(qnorm2sd(data_to_pass$NUM_SUBJECTS*3),ncol=3),
      
      #NUM_SUBJECTS rows, NUM_PARAMS columns
      #[NUM_SUBJECTS,NUM_PARAMS];
      #might need to get these empirically rather than just trying to filter down.
      #or convert it into non-centered and then see where we go from there.
      run_sigma=cbind(sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*bootstrapped_sample$alpha_run_sigma_gamma,replace=FALSE),
                      sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*bootstrapped_sample$k_run_sigma_gamma,replace=FALSE),
                      sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*bootstrapped_sample$tau_run_sigma_gamma,replace=FALSE)),
      #I think these cauchys are probably going to screw us!
      #no way we can start with these starting values.
      
      ################
      ######RUN LEVEL
      alpha_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
      k_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
      tau_pr_var=qnorm2sd(data_to_pass$NUM_RUNS)
    )
  )
}
get_init_vals<-function(data_to_pass){
  return(
    list(
      ################
      ######GROUP LEVEL
      subj_mu=c(rnorm(1,data_to_pass$priors_alpha,data_to_pass$priors_alpha_spread),
                rnorm(1,data_to_pass$priors_lba_k,data_to_pass$priors_lba_k_spread),
                rnorm(1,data_to_pass$priors_lba_tau,data_to_pass$priors_lba_tau_spread)),
      subj_sigma=c(abs(rnorm(1,0,data_to_pass$priors_alpha_sd_gamma)),
                   abs(rnorm(1,0,data_to_pass$priors_lba_k_sd_gamma)),
                   abs(rnorm(1,0,data_to_pass$priors_lba_tau_sd_gamma))),
      run_sigma_gamma=c(abs(rnorm(1,0,data_to_pass$priors_alpha_run_sigma_gamma)),
                        abs(rnorm(1,0,data_to_pass$priors_lba_k_run_sigma_gamma)),
                        abs(rnorm(1,0,data_to_pass$priors_lba_tau_run_sigma_gamma))),
      
      #not sure that we really need to define transformed parameters, maybe only sampled parameters.
      ################
      ####SUBJECT LEVEL
      run_mu_var=matrix(qnorm2sd(data_to_pass$NUM_SUBJECTS*3),ncol=3),
      
      #NUM_SUBJECTS rows, NUM_PARAMS columns
      #[NUM_SUBJECTS,NUM_PARAMS];
      # run_sigma=cbind(abs(rnorm(data_to_pass$NUM_SUBJECTS,0,data_to_pass$priors_alpha_run_sigma_gamma)),
      #                 abs(rnorm(data_to_pass$NUM_SUBJECTS,0,data_to_pass$priors_k_run_sigma_gamma)),#this seems to be an error.
      #                 abs(rnorm(data_to_pass$NUM_SUBJECTS,0,data_to_pass$priors_tau_run_sigma_gamma))),
      run_sigma=cbind(sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*data_to_pass$priors_alpha_run_sigma_gamma,replace=FALSE),
                      sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*data_to_pass$priors_lba_k_run_sigma_gamma,replace=FALSE),
                      sample(seq(.1,2,(2-.1)/(data_to_pass$NUM_SUBJECTS-1))*data_to_pass$priors_lba_tau_run_sigma_gamma,replace=FALSE)),
      
      #I think these cauchys are probably going to screw us!
      #no way we can start with these starting values.
      
      ################
      ######RUN LEVEL
      alpha_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
      k_pr_var=qnorm2sd(data_to_pass$NUM_RUNS),
      tau_pr_var=qnorm2sd(data_to_pass$NUM_RUNS)
    )
  )
  
}