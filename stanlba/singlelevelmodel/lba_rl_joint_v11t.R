#sub-version s: applies Jonas's suggestion to regress out CSF+WM+Motion. Uses freesurfer ROIs, all dmn regions
#revival of subversion t: uses freesurfer ROIs, just four critical areas.
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
lba_rl_version<-"joint_20180709_1"
model.subversion<-"t"
single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
output_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")
dir.create(single_run_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)
#file_folder<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/lba_rl_single_estimates.RData"
#load(file=file_folder)
Rhat_corevals_limit=1.05 # I don't care about this at the moment. I just want to take a look at what we'regetting!
Rhat_general_limit=1.1



results.list<-list()

model.name<-"lba_rl_single_exp_joint_v11"
cat("Compiling model...")
lba_rl_single_joint<-stan_model(paste0('stanlba/stanfiles/incremental/',model.name,'.stan'))
cat("compiled.\n")
colnames(rawdata)
regions<-c("ROI_ctx_lh_S_suborbital","ROI_ctx_rh_S_suborbital", "ROI_Left.Accumbens.area", "ROI_Right.Accumbens.area")
#regions<-paste0("con_",get_dmn_regions())
regions<-paste0("con_",regions)
#100,140,218,261,334
#ll=100;ul=139
#ll=140;ul=217
#ll=218;ul=260
#ll=261;ul=334
#ll=335;ul=400
ll=100;ul=400
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

