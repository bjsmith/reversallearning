#sub-version u: applies Jonas's suggestion to regress out CSF+WM+Motion; and uses FSL-created Harvard-Oxford ROIs
#sub-version q: uses FSL-created Harvard-Oxford ROIs instead of freesurfer ROIs.
#sub-version v: uses simulated data to test whether the code I've created can actually recover an actually-existing relationship.
#sub-version 13v: like v11v but collects the posterior sample, including EV&RPE, and does not include random noise in the synthetic data.
library(rstan)
source("stanlba/lba_rl_joint_setup.R")
require(R.utils)
options(mc.cores = 6)
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v7_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v10_functions.R")
source("stanlba/singlelevelmodel/lba_rl_joint_v11r_functions.R")

#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.
lba_rl_version<-"joint_20180820_1"
model.subversion<-"v"
single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
output_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")
dir.create(single_run_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)
#file_folder<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/lba_rl_single_estimates.RData"
#load(file=file_folder)
Rhat_corevals_limit=1.05 # I don't care about this at the moment. I just want to take a look at what we'regetting!
Rhat_general_limit=1.1

source("stanlba/singlelevelmodel/single_level_model_summarize_fast.R")
model_lba_rl_joint_v13<-single_level_model_summarize_fast(
  single_run_dir=paste0(localsettings$data.dir,"lba_rl"),
  model_version="joint_20180723_1",
  model_filename="lba_rl_single_exp_joint_v13",
  model_subversion="")
#View(model_lba_rl_joint_v13$results_summary_dt[,.N,by=.(sid,rid,motivation)])
source("stanlba/singlelevelmodel/empirical_correlations_estimate.R")
neural_res<-merge_with_neural_data(model_lba_rl_joint_v13$results_summary_dt)

rawdata<-merge(rawdata,
  neural_res[,.(sid,rid,motivation,TrialID,FullRunId,run_pred_err_c2,trial_expected_val)],
  by.x = c("subid","runid","Motivation","TrialID"),by.y=c("sid","rid","motivation","TrialID"))
   
# View(rawdata[order(subid,runmotiveid,TrialID),.(TrialID,onset_time_actual)])
# summary(rawdata$Simulated_Accumbens_Right)
rawdata$Simulated_Accumbens_Left<-rawdata$run_pred_err_c2#+rnorm(dim(rawdata)[1],0,sd(rawdata$run_pred_err_c2,na.rm = TRUE))
rawdata$Simulated_Accumbens_Right<-rawdata$run_pred_err_c2#+rnorm(dim(rawdata)[1],0,sd(rawdata$run_pred_err_c2,na.rm = TRUE))
rawdata$Simulated_SubOrbitalLeft<-rawdata$trial_expected_val#+rnorm(dim(rawdata)[1],0,sd(rawdata$trial_expected_val,na.rm = TRUE))
rawdata$Simulated_SubOrbitalRight<-rawdata$trial_expected_val#+rnorm(dim(rawdata)[1],0,sd(rawdata$trial_expected_val,na.rm = TRUE))
# plot(neural_res$run_pred_err_c2,neural_res$Simulated_Accumbens_Left)
# plot(neural_res$run_pred_err_c2,neural_res$Simulated_Accumbens_Right)
# plot(neural_res$Simulated_Accumbens_Left,neural_res$Simulated_Accumbens_Right)
# plot(neural_res$trial_expected_val,neural_res$Simulated_SubOrbitalLeft)
# plot(neural_res$trial_expected_val,neural_res$Simulated_SubOrbitalRight)
results.list<-list()

model.name<-"lba_rl_single_exp_joint_v13"
cat("Compiling model...")
lba_rl_single_joint<-stan_model(paste0('stanlba/stanfiles/incremental/',model.name,'.stan'))
cat("compiled.\n")
colnames(rawdata)
regions<-c("Simulated_Accumbens_Left","Simulated_Accumbens_Right", "Simulated_SubOrbitalLeft", "Simulated_SubOrbitalRight")
#regions<-paste0("con_fsl_roi_",c("frontal_medial_cortex","frontal_orbital_cortex", "accumbens_l", "accumbens_r"))
#is there 

#100,140,218,261,334
#ll=100;ul=250
#ll=100;ul=217
#ll=218;ul=400
#ll=140;ul=217
ll=218;ul=260
#ll=261;ul=334
#ll=335;ul=400
#ll=100;ul=400
for (sid in unique(rawdata$subid)[unique(rawdata$subid)>=ll & unique(rawdata$subid)<=ul]){
  for (r in unique(rawdata[subid==sid,runid])){#r<-1
    motivations<-unique(rawdata[subid==sid & runid==r,Motivation])
    for(m in motivations){#m<-"punishment"
      package_filepath<-paste0(output_dir,"run_package_",sid,"_",r,"_",m,"_",model.name,model.subversion,".RData")
      srm.data<-select_rawdata_cols_for_run(rawdata,sid,r,m)
      if(!file.exists(package_filepath)){
        baseseed<-53171370#129221139#sample.int(.Machine$integer.max, 1)
        roundseed<-sid+r+which(motivations==m)+baseseed
        model_attempts=0
        warmup=400
        iterations=500
        #warmup=10;iterations=20
        try_this_model=TRUE
        while(try_this_model==TRUE){
          model_attempts<-model_attempts+1
          start_time<-Sys.time()
          n_chains=6
          srm.fit <- tryCatch(
            expr = {
              withTimeout({
                sampling(lba_rl_single_joint, 
                         data = create_standatalist(srm.data = srm.data,theta_count = 2, delta_names = regions),
                         warmup = warmup, 
                         iter = iterations,
                         init= get_starting_values(n_chains,thetaDelta_count=length(regions)+2),
                         chains = n_chains,
                         seed = roundseed,
                         control = list(max_treedepth = 12,adapt_delta=0.85))
              },timeout = 60*120,onTimeout = "error")
            }, 
            TimeoutException = function(ex){cat(paste0("could not run calculation for sid",sid," rid", r, " m", m, " within 120 minutes. skipping!"))
            cat(ex)
            cat("\n")
            return(NULL)
            }
          )
          end_time<-Sys.time()
          if(!is.null(srm.fit)){
            #only save it if Rhat is within the accepted range.
            test_rhat_vals<-function(rhat_vector){
              #ignore "nan" values as long as they're not all NaN
              if (all(is.nan(rhat_vector)))return(FALSE)
              if (all(rhat_vector[!is.nan(rhat_vector)]<=Rhat_general_limit)){
                if(all(rhat_vector[1:3]<=Rhat_corevals_limit)){
                  return(TRUE)
                }
              }
              return(FALSE)
            }
            if(test_rhat_vals(summary(srm.fit)$summary[,"Rhat"])){
              try_this_model<-FALSE#we've found...no need to try again.
              run_package<-list("sid"=sid,"rid"=r,"motivation"=m,fit=srm.fit,duration=as.numeric(end_time-start_time),"iterations"=iterations,
                                "baseseed"=baseseed,"roundseed"=roundseed)
              save(run_package,file=package_filepath)
              results.list<-c(results.list,list(run_package))
            }else if (iterations<5000){
              print(paste0("One or more Rhat values for sid",sid," rid", r, " m", m, " were outside the accepted range. Trying again with more iterations and a different seed."))
              baseseed=baseseed+1
              iterations=iterations+warmup*2
              warmup=warmup+warmup*2
              
            }else{
              print(paste0("One or more Rhat values for sid",sid," rid", r, " m", m, " were outside the accepted range even for up to 5000 iterations. Giving up."))
              try_this_model<-FALSE
            }
          }else{
            print("model couldn't be calculated within the selected time limit. The policy for this run is not to repeated timed-out models.")
            try_this_model<-FALSE
          }
          if(model_attempts>=3){
            try_this_model<-FALSE
          }
        }
      }else{
        print(paste0("loading from file sid ",sid, "; r ",r, "; m ", m))
        load(package_filepath)
        results.list<-c(results.list,list(run_package))
      }
      
    }
    
  }
  
}

