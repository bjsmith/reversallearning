source("stanlba/lba_rl_joint_setup.R")
require(R.utils)
options(mc.cores = 6)
source("stanlba/singlelevelmodel/lba_rl_joint_v1_functions.R")

# sub105data<-rawdata[subid==105 & Motivation=="reward" & runid==1,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]

#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.
lba_rl_version<-"joint_20180627_1"

single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
output_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")
dir.create(single_run_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)
#file_folder<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/lba_rl_single_estimates.RData"
#load(file=file_folder)
Rhat_limit=1.05
results.list<-list()
lba_rl_single_joint<-stan_model('stanlba/stanfiles/incremental/lba_rl_single_exp_joint_v1.stan')

for (sid in unique(rawdata$subid)[unique(rawdata$subid)>ll & unique(rawdata$subid)<=ul]){#c(105,106,107)){#unique(rawdata$subid)[1:3]){#sid<-106
  for (r in unique(rawdata[subid==sid,runid])){#r<-1
    for(m in unique(rawdata[subid==sid & runid==r,Motivation])){#m<-"punishment"
      package_filepath<-paste0(output_dir,"run_package_",sid,"_",r,"_",m,"_joint_v1.RData")
      srm.data<-select_rawdata_cols_for_run(rawdata,sid,r,m)
      if(!file.exists(package_filepath)){
        model_attempts=0
        warmup=20
        iterations=40
        try_this_model=TRUE
        while(try_this_model==TRUE){
          model_attempts<-model_attempts+1
          start_time<-Sys.time()
          srm.fit <- tryCatch(
            expr = {
              withTimeout({
                sampling(lba_rl_single_joint, 
                         data = create_standatalist(srm.data = srm.data),
                         warmup = warmup, 
                         iter = iterations,
                         chains = 6,
                         control = list(max_treedepth = 15))
              },timeout = 1200,onTimeout = "error")
            }, 
            TimeoutException = function(ex){cat(paste0("could not run calculation for sid",sid," rid", r, " m", m, " within 20 minutes. skipping!"))
            cat(ex)
            cat("\n")
            return(NA)
            }
          )
          end_time<-Sys.time()
          if(!is.na(srm.fit)){
            #only save it if Rhat is within the accepted range.
            if(all(summary(srm.fit)$summary[,"Rhat"]<=Rhat_limit)){
              try_this_model<-FALSE#we've found...no need to try again.
              run_package<-list("sid"=sid,"rid"=r,"motivation"=m,fit=srm.fit,duration=as.numeric(end_time-start_time))
              save(run_package,file=package_filepath)
              results.list<-c(results.list,list(run_package))
            }else if (iterations<5000){
              print(paste0("One or more Rhat values for sid",sid," rid", r, " m", m, " were outside the accepted range. Trying again with larger values."))
              warmup=warmup+500
              iterations=iterations+500
            }else{
              print(paste0("One or more Rhat values for sid",sid," rid", r, " m", m, " were outside the accepted range even for up to 5000 iterations. Giving up."))
              try_this_model<-FALSE
            }
          }else{
            print("model couldn't be calculated within the selected time limit. Trying again with new initial values and new seed; will try up to 10 times total.")
          }
          if(model_attempts>=10){
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


#at this point, we need to get a way to extract key datapoints from the results list and save that. Don't save it as it is; it's far too big!
