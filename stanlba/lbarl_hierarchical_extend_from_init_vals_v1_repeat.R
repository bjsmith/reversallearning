
source("stanlba/lba_rl_setup.R")
# source("stanlba/lba_rl_allsingles_get_results_summary.R")

#replace with 
source("load_lba_rl_allsingles_resultsdata.R")
source("generate_lbarl_group_summary_stats.R")
source("init_vals_generate.R")
#have to exclude improperly estimated runs.
improperly.estimated.runs<-unique(results.summary.dt[which(results.summary.dt$Rhat>1.05),.(sid,rid,motivation,FullRunId)])

lba_group_sstats<-generate_lbarl_group_summary_stats(results.summary.dt[!(FullRunId %in% improperly.estimated.runs$FullRunId)])

n_chains<-min(get_my_preferred_cores(),12)
cores_to_use <- get_my_preferred_cores()
options(mc.cores = cores_to_use)
print(paste0("using ", cores_to_use, " cores."))


#Get a minimal amount of data to test a three level model.
multisubj_multirun_moresubs<-rawdata[subid %in% c(105:115) #& Motivation=="reward" 
                                     & reaction_time>0,
                                     .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                       ConsecSubId=as.integer(as.factor(as.character(subid))),
                                       WithinSubjRunId=runid,Motivation=Motivation,
                                       UniqueRunID=as.numeric(interaction(runid,Motivation,subid,drop = TRUE)))]

multisubj_multirun_allsubs<-rawdata[subid #%in% c(105:115) #& Motivation=="reward" 
                                    & reaction_time>0,
                                    .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                      ConsecSubId=as.integer(as.factor(as.character(subid))),
                                      WithinSubjRunId=runid,Motivation=Motivation,
                                      UniqueRunID=as.numeric(interaction(runid,Motivation,subid,drop = TRUE)))]

multisubj_multirun_Group1<-rawdata[SubjectGroup==1
                                   & reaction_time>0,
                                   .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                     ConsecSubId=as.integer(as.factor(as.character(subid))),
                                     WithinSubjRunId=runid,Motivation=Motivation,
                                     UniqueRunID=as.numeric(interaction(runid,Motivation,subid,drop = TRUE)))]


multisubj_multirun_Group1_ex343_362<-rawdata[SubjectGroup==1 & !(subid %in% c(343,362))
                                             & reaction_time>0,
                                             .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                               ConsecSubId=as.integer(as.factor(as.character(subid))),
                                               WithinSubjRunId=runid,Motivation=Motivation,
                                               UniqueRunID=as.numeric(interaction(runid,Motivation,subid,drop = TRUE)))]
#hmmm, before we can speedtest, we need to ensure the damn thing actually works.
bseed<-1236512756#set.seed(as.numeric(Sys.time())); sample.int(.Machine$integer.max-1000, 1)

run_model<-function(model_filename,model_description,filedir="",informative_priors=FALSE,
                    a_delta=0.9,data_to_use=multisubj_multirun_moresubs,warmup_iter=450,iter=500,
                    init_vals="auto",prior_blowup=1){
  #model_filename<-"lba_rl_multi_subj_7_3level_empiricalpriors_noncentered_rsg_n";model_description<-"10subs_bootstrapped_allempirical_init_v2";filedir="incremental/";informative_priors = TRUE;init_vals="bootstrapped_empsampled";warmup_iter=5;iter=10;data_to_use=multisubj_multirun_moresubs
  
  tstart<-Sys.time()
  print(paste0("warmup_iter: ",warmup_iter))
  print(paste0("iter: ",iter))
  
  #run_list_by_run<-data_to_use[,.(RunID=unique(UniqueRunID)),by=.(ConsecSubId,subid)] %>% .[order(RunID)]
  run_list_by_run<-data_to_use[,.(StanRunID=unique(UniqueRunID)),by=.(ConsecSubId,subid,Motivation,WithinSubjRunId)] %>% .[order(StanRunID)]
  
  if(informative_priors){
    data_to_pass<-get_informative_priors(data_to_use,lba_group_sstats,prior_blowup=prior_blowup,run_list_by_run)
  }else{
    data_to_pass<- list(
      NUM_CHOICES=2,
      A=0.01,
      NUM_SUBJECTS=length(unique(data_to_use$subid)),
      NUM_TRIALS=dim(data_to_use)[1],
      NUM_RUNS=length(unique(data_to_use$UniqueRunID)),
      run_subjid= run_list_by_run$ConsecSubId,
      trial_runid=as.numeric(data_to_use$UniqueRunID),
      
      response_time=data_to_use$reaction_time,
      response=data_to_use$choice,
      required_choice=data_to_use$cor_res_Counterbalanced,
      cue=data_to_use$cue,
      
      priors_alpha=-3,
      priors_alpha_spread=3,
      priors_alpha_sd_gamma=5,
      priors_alpha_run_sigma_gamma=4,
      # these priors could probably be set even narrower than this, but let's ease into it.
      
      
      priors_lba_k=log(0.5),
      priors_lba_k_spread=1,
      priors_lba_k_sd_gamma=3,
      priors_lba_k_run_sigma_gamma=2,
      
      priors_lba_tau=log(0.5),
      priors_lba_tau_spread=0.5,
      priors_lba_tau_sd_gamma=2,
      priors_lba_tau_run_sigma_gamma=1
      
    )
  }
  
  
  PARID_alpha = 1;
  PARID_lba_k = 2;
  PARID_lba_tau = 3;
  
  #we want to set initial values...
  #we probably need to set initial values for all the subjects as well as the top level values? So let's try this out...
  #####THINGS TO CHECK:
  #####(1) How to define a vector or matrix of values? assume [M,N], M row, N columns
  #####(2) Are these cauchys going to screw us? I think we need a flatter-tailed distribution. The truncated normals might end up being what we need.
  if(init_vals %in% c("bootstrapped","bootstrapped_allempirical")){
    source("bootstrap_smart_init_vals.R")
    smart_init_vals<-bootstrap_smart_init_vals(n_samples = n_chains,
                                               subid_set = sort(unique(data_to_use$subid)),
                                               bootstrap_seed = c(1973449269))
  }
  
  
  if(init_vals=="auto"){
    init_method="random"
  }else if (init_vals=="randomized"){
    init_method=rep(list(get_init_vals(data_to_pass)),times=12)
  }else if (init_vals=="bootstrapped"){
    init_method=lapply(smart_init_vals,get_bootstrapped_init_vals,1761614456,data_to_pass)
  }else if (init_vals=="bootstrapped_allempirical"){
    init_method=lapply(smart_init_vals,get_bootstrapped_init_vals_with_bottomlevel_inits,
                       1761614456,data_to_pass,data_to_use,results.summary.dt,run_list_by_run)
  }else if (init_vals=="empirical_distribution"){
    init_method=get_empirical_distribution_allchains(n_chains = n_chains,run_list_by_run)
  }else{stop("invalid value for init_vals argument.")}
  #print(init_method)
  #print(paste0("running stan; data_to_pass$NUM_SUBJECTS=",data_to_pass$NUM_SUBJECTS))
  
  rmfit<-stan(file=paste0(stanfiledir,filedir,model_filename,".stan"), 
              #fit=fit_rl_lba_multi_subj_proto1,
              data = data_to_pass,
              warmup = warmup_iter, 
              iter = iter,
              init=init_method,
              chains = n_chains, #run as many chains as we have cores to run them, but no more than 12 necessary.
              seed=bseed,
              refresh=5,
              control = list(max_treedepth = 13,adapt_delta=a_delta))
  
  
  tend<-Sys.time()
  print(tend-tstart)
  model_description<-paste0(model_description,"prior_blowup_",prior_blowup)
  file_save_name<-get_fit_desc(use_model = model_filename,descr = model_description,run=c(1,2),
                               model_rp_separately=TRUE,model_runs_separately=TRUE,
                               use_pain=FALSE,fastDebug=FALSE,fileSuffix="",
                               estimation_method=ESTIMATION_METHOD.MCMC,
                               bseed=bseed,warmup_iter = warmup_iter,
                               iterations=iter)
  save(rmfit,file=file_save_name)
  print(init_method[[1]])
  if(informative_priors){
    print(lba_group_sstats)
  }
  
  print(rmfit)
  
  
  return(rmfit)
}

get_informative_priors<-function(data_to_use,lba_group_sstats,prior_blowup,run_list_by_run){
  data_to_pass<- list(
    NUM_CHOICES=2,
    A=0.01,
    NUM_SUBJECTS=length(unique(data_to_use$subid)),
    NUM_TRIALS=dim(data_to_use)[1],
    NUM_RUNS=length(unique(data_to_use$UniqueRunID)),
    run_subjid= run_list_by_run$ConsecSubId,
    trial_runid=as.numeric(data_to_use$UniqueRunID),
    
    response_time=data_to_use$reaction_time,
    response=data_to_use$choice,
    required_choice=data_to_use$cor_res_Counterbalanced,
    cue=data_to_use$cue,
    priors_alpha=lba_group_sstats$alpha_pr_mean,
    priors_alpha_spread=lba_group_sstats$alpha_pr_var*prior_blowup,
    priors_alpha_sd_gamma=lba_group_sstats$alpha_sd_prior*prior_blowup,
    priors_alpha_run_sigma_gamma=lba_group_sstats$alpha_run_sigma_gamma*prior_blowup,
    # these priors could probably be set even narrower than this, but let's ease into it.
    
    
    priors_lba_k=lba_group_sstats$k_pr_mean,
    priors_lba_k_spread=lba_group_sstats$k_pr_var*prior_blowup,
    priors_lba_k_sd_gamma=lba_group_sstats$k_sd_prior*2*prior_blowup,
    priors_lba_k_run_sigma_gamma=lba_group_sstats$k_run_sigma_gamma*prior_blowup,
    
    priors_lba_tau=lba_group_sstats$tau_pr_mean,
    priors_lba_tau_spread=lba_group_sstats$tau_pr_var*prior_blowup,
    priors_lba_tau_sd_gamma=lba_group_sstats$tau_sd_prior*2*prior_blowup,
    priors_lba_tau_run_sigma_gamma=lba_group_sstats$tau_run_sigma_gamma*prior_blowup
  )
  return(data_to_pass)
}
print("running...")