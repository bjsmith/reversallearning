library(rstan)
source("stanlba/lba_rl_joint_setup.R")
require(R.utils)
options(mc.cores = 6)
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")
source("stanlba/singlelevelmodel/rl_discrim/lba_rl_discrim_v2_functions_fixed.R")
source("stanlba/singlelevelmodel/rl_discrim/rl_discrim_v4_functions.R")


#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.
lba_rl_version<-"rl_joint_20180927_1"

single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
output_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")
dir.create(single_run_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)
#file_folder<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/lba_rl_single_estimates.RData"
#load(file=file_folder)
Rhat_corevals_limit=1.05 # I don't care about this at the moment. I just want to take a look at what we'regetting!
Rhat_general_limit=1.1


results.list<-list()

model.vector<-list()

failure_count<-0
n_chains=6
#for (model.name in model.name.vector){

cat("Compiling model...")
lba_rl_single_joint<-stan_model(paste0('stanlba/stanfiles/incremental/',model.name,'.stan'))
cat("compiled.\n")
#100,140,218,261,334
#ll=100;ul=139
#ll=140;ul=217
#ll=218;ul=260
#ll=261;ul=334
ll=335;ul=400
#ll=106;ul=106

for (sid in unique(rawdata$subid)[unique(rawdata$subid)>=ll & unique(rawdata$subid)<=ul]){
  for (r in unique(rawdata[subid==sid,runid])){#r<-1
    motivations<-unique(rawdata[subid==sid & runid==r,Motivation])
    for(m in motivations){#m<-"punishment"
      roundseed<-sid+r+which(motivations==m)+38085906
      package_filepath<-paste0(output_dir,"run_package_",sid,"_",r,"_",m,"_",model.name,".RData")
      srm.data<-select_rawdata_cols_for_run(rawdata,sid,r,m)
      
      model.data<-create_standatalist(srm.data = srm.data,theta_count=2)
      thetaDelta_count=6
      
      if(length(grep("lba_rl",model.name))==1){
        model.init<-get_starting_values(n_chains,theta_count=thetaDelta_count)
      }else{
        model.init<-get_starting_values_alphabeta(n_chains,theta_count=thetaDelta_count)
      }
      
      if(!file.exists(package_filepath)){
        
        model_attempts=0
        warmup=400
        iterations=500
        #warmup=80;iterations=100
        #warmup=10;iterations=20
        #warmup=180;iterations=200
        try_this_model=TRUE
        while(try_this_model==TRUE){
          roundseed=roundseed+1
          if(model_attempts>=1) failure_count = failure_count+1
          model_attempts<-model_attempts+1
          start_time<-Sys.time()
          
          srm.fit <- tryCatch(
            expr = {
              withTimeout({
                sampling(lba_rl_single_joint, 
                         data = model.data,
                         warmup = warmup, 
                         iter = iterations,
                         init=model.init,
                         chains = n_chains,
                         control = list(max_treedepth = 12,adapt_delta=0.8))
              },timeout = 60*10,onTimeout = "error")
            }, 
            TimeoutException = function(ex){cat(paste0("could not run calculation for sid",sid," rid", r, " m", m, " within 10 minutes. skipping!"))
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
              run_package<-list("sid"=sid,"rid"=r,"motivation"=m,fit=srm.fit,duration=as.numeric(end_time-start_time))
              save(run_package,file=package_filepath)
              results.list<-c(results.list,list(run_package))
            }else if (iterations<5000){
              print(paste0("One or more Rhat values for sid",sid," rid", r, " m", m, " were outside the accepted range. Trying again with larger values."))
              iterations=iterations+warmup*0.2
              warmup=warmup+warmup*0.2
              
            }else{
              print(paste0("One or more Rhat values for sid",sid," rid", r, " m", m, " were outside the accepted range even for up to 5000 iterations. Giving up."))
              try_this_model<-FALSE
            }
          }else{
            print("model couldn't be calculated within the selected time limit. Trying again with new initial values and new seed; will try up to 3 times total.")
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
#}

#at this point, we need to get a way to extract key datapoints from the results list and save that. Don't save it as it is; it's far too big!
