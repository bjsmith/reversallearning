library(LaplacesDemon)

#the informative priors model took 4.45 hours compared to 12.70 hours for the weakly informative priors model.
#so it may have sped up the model, although this is hard to judge because:
#(1) the server might be doing other things;
#(2) random chance for the very first starting values
print("extending in a 4th direction: including initial values")

source("stanlba/lba_rl_setup.R")
# source("stanlba/lba_rl_allsingles_get_results_summary.R")

#replace with 
source("load_lba_rl_allsingles_resultsdata.R")
source("generate_lbarl_group_summary_stats.R")
#have to exclude improperly estimated runs.
improperly.estimated.runs<-unique(results.summary.dt[which(results.summary.dt$Rhat>1.05),.(sid,rid,motivation,FullRunId)])

lba_group_sstats<-generate_lbarl_group_summary_stats(results.summary.dt[!(FullRunId %in% improperly.estimated.runs$FullRunId)])

#n_chains<-min(get_my_preferred_cores(),6)
n_chains=3
cores_to_use <- n_chains
options(mc.cores = cores_to_use)
print(paste0("using ", cores_to_use, " cores."))


#Get a minimal amount of data to test a three level model.
multisubj_multirun_moresubs<-rawdata[subid %in% c(105:115) #& Motivation=="reward" 
                                     & reaction_time>0,
                                     .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                       ConsecSubId=as.integer(as.factor(as.character(subid))),
                                       UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]

multisubj_multirun_allsubs<-rawdata[subid #%in% c(105:115) #& Motivation=="reward" 
                                    & reaction_time>0,
                                    .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                      ConsecSubId=as.integer(as.factor(as.character(subid))),
                                      UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]

multisubj_multirun_Group1<-rawdata[SubjectGroup==1
                                    & reaction_time>0,
                                    .(reaction_time,outcome,cue,choice,cor_res_Counterbalanced,subid,
                                      ConsecSubId=as.integer(as.factor(as.character(subid))),
                                      UniqueRunID=as.numeric(interaction(subid,runid,Motivation,drop = TRUE)))]

source("bootstrap_smart_init_vals.R")
smart_init_vals<-bootstrap_smart_init_vals(n_samples = n_chains,
                                           subid_set = sort(unique(multisubj_multirun_Group1$subid)),
                                           bootstrap_seed = c(1973449269))


#hmmm, before we can speedtest, we need to ensure the damn thing actually works.
bseed<-1236512756#set.seed(as.numeric(Sys.time())); sample.int(.Machine$integer.max-1000, 1)

run_model<-function(model_filename,model_description,filedir="",informative_priors=FALSE,
                    a_delta=0.9,data_to_use=multisubj_multirun_moresubs,warmup_iter=19,iter=20,
                    init_vals="auto"){
  tstart<-Sys.time()
  print(paste0("warmup_iter: ",warmup_iter))
  print(paste0("iter: ",iter))
  
  if(informative_priors){
    data_to_pass<- list(
      NUM_CHOICES=2,
      A=0.01,
      NUM_SUBJECTS=length(unique(data_to_use$subid)),
      NUM_TRIALS=dim(data_to_use)[1],
      NUM_RUNS=length(unique(data_to_use$UniqueRunID)),
      run_subjid=data_to_use[,.(RunID=unique(UniqueRunID)),by=ConsecSubId] %>% .[order(RunID),ConsecSubId],
      trial_runid=as.numeric(data_to_use$UniqueRunID),
      
      response_time=data_to_use$reaction_time,
      response=data_to_use$choice,
      required_choice=data_to_use$cor_res_Counterbalanced,
      cue=data_to_use$cue,
      priors_alpha=lba_group_sstats$alpha_pr_mean,
      priors_alpha_spread=lba_group_sstats$alpha_pr_var,
      priors_alpha_sd_gamma=lba_group_sstats$alpha_sd_prior,
      priors_alpha_run_sigma_gamma=lba_group_sstats$alpha_run_sigma_gamma,
      # these priors could probably be set even narrower than this, but let's ease into it.
      
      
      priors_lba_k=lba_group_sstats$k_pr_mean,
      priors_lba_k_spread=lba_group_sstats$k_pr_var,
      priors_lba_k_sd_gamma=lba_group_sstats$k_sd_prior*2,
      priors_lba_k_run_sigma_gamma=lba_group_sstats$k_run_sigma_gamma,
      
      priors_lba_tau=lba_group_sstats$tau_pr_mean,
      priors_lba_tau_spread=lba_group_sstats$tau_pr_var,
      priors_lba_tau_sd_gamma=lba_group_sstats$tau_sd_prior*2,
      priors_lba_tau_run_sigma_gamma=lba_group_sstats$tau_run_sigma_gamma
    )
  }else{
    data_to_pass<- list(
      NUM_CHOICES=2,
      A=0.01,
      NUM_SUBJECTS=length(unique(data_to_use$subid)),
      NUM_TRIALS=dim(data_to_use)[1],
      NUM_RUNS=length(unique(data_to_use$UniqueRunID)),
      run_subjid=data_to_use[,.(RunID=unique(UniqueRunID)),by=ConsecSubId] %>% .[order(RunID),ConsecSubId],
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
  #####(3) Do we have to define transformed parameters? A: Probably not.
  
  get_bootstrapped_init_vals<- function(bootstrapped_sample){#bootstrapped_sample<-smart_init_vals[[1]]
    set.seed(1761614456)
    return(
      list(
        ################
        ######GROUP LEVEL
        subj_mu=c(bootstrapped_sample$alpha_pr_mean,bootstrapped_sample$k_pr_mean,bootstrapped_sample$tau_pr_mean),
        subj_sigma=c(bootstrapped_sample$alpha_sd_prior,bootstrapped_sample$k_sd_prior,bootstrapped_sample$tau_sd_prior),
        run_sigma_gamma=c(bootstrapped_sample$alpha_run_sigma_gamma,bootstrapped_sample$k_run_sigma_gamma,bootstrapped_sample$tau_run_sigma_gamma),
        
        #not sure that we really need to define transformed parameters, maybe only sampled parameters.
        ################
        ####SUBJECT LEVEL
        run_mu_var=matrix(rnorm(data_to_pass$NUM_SUBJECTS*3,0,1),ncol=3),
        
        #NUM_SUBJECTS rows, NUM_PARAMS columns
        #[NUM_SUBJECTS,NUM_PARAMS];
        #might need to get these empirically rather than just trying to filter down.
        #or convert it into non-centered and then see where we go from there.
        run_sigma=cbind(sample(seq(.1,2,2/data_to_pass$NUM_SUBJECTS)*bootstrapped_sample$alpha_run_sigma_gamma,replace=FALSE),
                        sample(seq(.1,2,2/data_to_pass$NUM_SUBJECTS)*bootstrapped_sample$k_run_sigma_gamma,replace=FALSE),
                        sample(seq(.1,2,2/data_to_pass$NUM_SUBJECTS)*bootstrapped_sample$tau_run_sigma_gamma,replace=FALSE)),
        #I think these cauchys are probably going to screw us!
        #no way we can start with these starting values.
        
        ################
        ######RUN LEVEL
        alpha_pr_var=rnorm(data_to_pass$NUM_RUNS,0,1),
        k_pr_var=rnorm(data_to_pass$NUM_RUNS,0,1),
        tau_pr_var=rnorm(data_to_pass$NUM_RUNS,0,1)
      )
    )
  }
  get_init_vals<-function(){
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
        run_mu_var=matrix(rnorm(data_to_pass$NUM_SUBJECTS*3,0,1),ncol=3),
        
        #NUM_SUBJECTS rows, NUM_PARAMS columns
        #[NUM_SUBJECTS,NUM_PARAMS];
        # run_sigma=cbind(abs(rnorm(data_to_pass$NUM_SUBJECTS,0,data_to_pass$priors_alpha_run_sigma_gamma)),
        #                 abs(rnorm(data_to_pass$NUM_SUBJECTS,0,data_to_pass$priors_k_run_sigma_gamma)),#this seems to be an error.
        #                 abs(rnorm(data_to_pass$NUM_SUBJECTS,0,data_to_pass$priors_tau_run_sigma_gamma))),
        run_sigma=cbind(sample(seq(.1,2,2/data_to_pass$NUM_SUBJECTS)*data_to_pass$priors_alpha_run_sigma_gamma,replace=FALSE),
                        sample(seq(.1,2,2/data_to_pass$NUM_SUBJECTS)*data_to_pass$priors_lba_k_run_sigma_gamma,replace=FALSE),
                        sample(seq(.1,2,2/data_to_pass$NUM_SUBJECTS)*data_to_pass$priors_lba_tau_run_sigma_gamma,replace=FALSE)),
        
        #I think these cauchys are probably going to screw us!
        #no way we can start with these starting values.
        
        ################
        ######RUN LEVEL
        alpha_pr_var=rnorm(data_to_pass$NUM_RUNS,0,1),
        k_pr_var=rnorm(data_to_pass$NUM_RUNS,0,1),
        tau_pr_var=rnorm(data_to_pass$NUM_RUNS,0,1)
      )
    )
    
  }
  # 
  # get_init_vals_imitate_default<-function(){
  #   return(
  #     list(
  #       ################
  #       ######GROUP LEVEL
  #       subj_mu=c(runif(1,-2,2),
  #                 runif(1,-2,2),
  #                 runif(1,-2,2)),
  #       subj_sigma=c(exp(runif(1,-2,2)),
  #                    exp(runif(1,-2,2)),
  #                    exp(runif(1,-2,2))),
  #       run_sigma_gamma=c(exp(runif(1,-2,2)),
  #                         exp(runif(1,-2,2)),
  #                         exp(runif(1,-2,2))),
  #       
  #       #not sure that we really need to define transformed parameters, maybe only sampled parameters.
  #       ################
  #       ####SUBJECT LEVEL
  #       run_mu_var=matrix(runif(data_to_pass$NUM_SUBJECTS*3,-2,2),ncol=3),
  #       
  #       #NUM_SUBJECTS rows, NUM_PARAMS columns
  #       #[NUM_SUBJECTS,NUM_PARAMS];
  #       run_sigma=cbind(exp(runif(data_to_pass$NUM_SUBJECTS,-2,2)),
  #                       exp(runif(data_to_pass$NUM_SUBJECTS,-2,2)),
  #                       exp(runif(data_to_pass$NUM_SUBJECTS,-2,2))),
  #       #I think these cauchys are probably going to screw us!
  #       #no way we can start with these starting values.
  #       
  #       ################
  #       ######RUN LEVEL
  #       alpha_pr_var=runif(data_to_pass$NUM_RUNS,-2,2),
  #       k_pr_var=runif(data_to_pass$NUM_RUNS,-2,2),
  #       tau_pr_var=runif(data_to_pass$NUM_RUNS,-2,2)
  #     )
  #   )
  #   
  # }
  
  if(init_vals=="auto"){
    init_method="random"
  }else if (init_vals=="randomized"){
    init_method=get_init_vals
  }else if (init_vals=="bootstrapped"){
    init_method=lapply(smart_init_vals,get_bootstrapped_init_vals)
  }else
  {stop("invalid value for init_vals argument.")}
  print(init_method)
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
  file_save_name<-get_fit_desc(use_model = model_filename,descr = model_description,run=c(1,2),
                               model_rp_separately=TRUE,model_runs_separately=TRUE,
                               use_pain=FALSE,fastDebug=FALSE,fileSuffix="",
                               estimation_method=ESTIMATION_METHOD.MCMC,
                               bseed=bseed,warmup_iter = warmup_iter,
                               iterations=iter)
  save(rmfit,file=file_save_name)
  print(rmfit)
  
  return(rmfit)
}
print("running...")

print("------------------------")
print("Running the informative priors model WITHOUT INITIAL VALUES SPECIFIED.")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","G1_auto_init",
                                filedir="incremental/",informative_priors = TRUE,
                                init_vals="auto")

print("------------------------")
print("Running the informative priors model WITH RANDOM INITIAL VALUES SPECIFIED.")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","informativepriors_G1_randomized_init",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="randomized")


print("------------------------")
print("Running the informative priors model WITH BOOTSTRAPPED INITIAL VALUES SPECIFIED.")
fit_with_manual_init_vals <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","informativepriors_G1_bootstrapped_init",
                                       filedir="incremental/",informative_priors = TRUE,
                                       init_vals="bootstrapped")

#what about the exp'd values.


#save(fit_normalsds,fit_widevariablecauchys,fit_base,file=paste0(dd, "Fits/hierarchical_stanforum_suggestion_results.RData"))
#save(fit_informative_priors,fit_weakly_informative_priors,file=paste0(dd, "Fits/informative_priors_test_allsubs_450.RData"))
