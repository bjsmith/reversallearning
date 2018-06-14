library(LaplacesDemon)

#the informative priors model took 4.45 hours compared to 12.70 hours for the weakly informative priors model.
#so it may have sped up the model, although this is hard to judge because:
#(1) the server might be doing other things;
#(2) random chance for the very first starting values
print("extending in a 4th direction: including initial values")

source("stanlba/lba_rl_setup.R")
source("stanlba/lba_rl_allsingles_get_results_summary.R")
chains<-min(get_my_preferred_cores(),6)
cores_to_use <- chains
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


#hmmm, before we can speedtest, we need to ensure the damn thing actually works.
bseed<-712363934#set.seed(as.numeric(Sys.time())); sample.int(.Machine$integer.max-1000, 1)


run_model<-function(model_filename,model_description,filedir="",informative_priors=FALSE,
                    a_delta=0.9,data_to_use=multisubj_multirun_moresubs,warmup_iter=450,iter=500){
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
      priors_alpha=alpha_pr_mean,
      priors_alpha_spread=alpha_pr_var,
      priors_alpha_sd_gamma=alpha_run_sigma_gamma,
      priors_alpha_run_sigma_gamma=alpha_run_sigma_gamma,
      # these priors could probably be set even narrower than this, but let's ease into it.
      
      
      priors_lba_k=k_pr_mean,
      priors_lba_k_spread=k_pr_var,
      priors_lba_k_sd_gamma=k_sd_prior*2,
      priors_lba_k_run_sigma_gamma=k_run_sigma_gamma,
      
      priors_lba_tau=tau_pr_mean,
      priors_lba_tau_spread=tau_pr_var,
      priors_lba_tau_sd_gamma=tau_sd_prior*2,
      priors_lba_tau_run_sigma_gamma=tau_run_sigma_gamma
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
  stop("still need to program stan to accept initial vals")
  rmfit<-stan(file=paste0(stanfiledir,filedir,model_filename,".stan"), 
              #fit=fit_rl_lba_multi_subj_proto1,
              data = data_to_pass,
              warmup = warmup_iter, 
              iter = iter,
              chains = chains, #run as many chains as we have cores to run them, but no more than 12 necessary.
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


# 
# print("Running the informative priors model with a MODIFIED DELTA.")
# fit_modified_delta <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_informativepriors_ad08",filedir="incremental/",informative_priors = TRUE,a_delta=0.8)
# # 
# print("------------------------")
# print("Running the informative priors model with 500 warmup, 1000 real iterations.")
# fit_modified_delta <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_informativepriors_1000iter",filedir="incremental/",informative_priors = TRUE,warmup=500,iter=1000)
# 
# print("------------------------")
# print("Running the informative priors model with ALL SUJBJECTS.")
# fit_modified_delta <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_informativepriors_allsubjs",filedir="incremental/",informative_priors = TRUE,data_to_use=multisubj_multirun_allsubs)

print("------------------------")
print("Running the informative priors model with INITIAL VALUES SPECIFIED.")
fit_modified_delta <- run_model("lba_rl_multi_subj_7_3level_empiricalpriors_noncentered","10subs_informativepriors_allsubjs",
                                filedir="incremental/",informative_priors = TRUE,
                                use_init_vals=TRUE)


#save(fit_normalsds,fit_widevariablecauchys,fit_base,file=paste0(dd, "Fits/hierarchical_stanforum_suggestion_results.RData"))
save(fit_informative_priors,fit_weakly_informative_priors,file=paste0(dd, "Fits/informative_priors_test_allsubs_450.RData"))

####INIT VALS PLAY
initf1 <- function() {
  list(mu = 1, sigma = 4, z = array(rnorm(6), dim = c(3,2)), alpha = 1)
}
# function form 2 with an argument named `chain_id`
initf2 <- function(chain_id = 1) {
  # cat("chain_id =", chain_id, "\n")
  
  list(mu = 1, sigma = 4, z = array(rnorm(6), dim = c(3,2)), alpha = chain_id)
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
function(chain_id){
  return(
    list(
      ################
      ######GROUP LEVEL
      subj_mu=c(rnorm(1,data_to_pass$priors_alpha,data_to_pass$priors_alpha_spread),
                rnorm(1,data_to_pass$priors_lba_k,data_to_pass$priors_lba_k_spread),
                rnorm(1,data_to_pass$priors_lba_tau,data_to_pass$priors_lba_tau_spread)),
      subj_sigma=c(rhalfcauchy(1,data_to_pass$priors_alpha_sd_gamma),
                   rhalfcauchy(1,data_to_pass$priors_lba_k_sd_gamma),
                   rhalfcauchy(1,data_to_pass$priors_lba_tau_sd_gamma)),
      run_sigma_gamma=c(rhalfcauchy(1,data_to_pass$priors_alpha_run_sigma_gamma),
                        rhalfcauchy(1,data_to_pass$priors_lba_k_run_sigma_gamma),
                        rhalfcauchy(1,data_to_pass$priors_lba_tau_run_sigma_gamma)),
      
      #not sure that we really need to define transformed parameters, maybe only sampled parameters.
      ################
      ####SUBJECT LEVEL
      run_mu_var=rnorm(data_to_pass$NUM_SUBJECTS,0,1),
      
      #NUM_SUBJECTS rows, NUM_PARAMS columns
      #[NUM_SUBJECTS,NUM_PARAMS];
      run_sigma=cbind(rhalfcauchy(data_to_pass$NUM_SUBJECTS,data_to_pass$priors_alpha_run_sigma_gamma),
                      rhalfcauchy(data_to_pass$NUM_SUBJECTS,data_to_pass$priors_alpha_run_sigma_gamma),
                      rhalfcauchy(data_to_pass$NUM_SUBJECTS,data_to_pass$priors_alpha_run_sigma_gamma)),
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

# generate a list of lists to specify initial values
n_chains <- 4
init_ll <- lapply(1:n_chains, function(id) initf2(chain_id = id))


