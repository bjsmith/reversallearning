source("stanlba/lba_rl_setup_v2.R")

options(mc.cores = 3)

# sub105data<-rawdata[subid==105 & Motivation=="reward" & runid==1,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]

#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.
lba_rl_version<-"20180610_2_noninformative"

single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
output_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")
dir.create(single_run_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)
#file_folder<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/lba_rl_single_estimates.RData"
#load(file=file_folder)

results.list<-list()
lba_rl_single<-stan_model('stanlba/stanfiles/lba_rl_single_exp_v3.stan')
for (sid in c(107,222,115)){#unique(rawdata$subid)){#unique(rawdata$subid)[1:3]){#sid<-105
  for (r in unique(rawdata[subid==sid,runid])){#r<-1
    for(m in unique(rawdata[subid==sid & runid==r,Motivation])){#m<-"punishment"
      
      package_filepath<-paste0(output_dir,"run_package_",sid,"_",r,"_",m,".RData")
      srm.data<-rawdata[subid==sid & Motivation==m & runid==r,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]
      if(!file.exists(package_filepath)){
        start_time<-Sys.time()
        srm.fit <- tryCatch({
          sampling(lba_rl_single, 
                   data = list(
                     LENGTH=dim(srm.data)[1],
                     NUM_CHOICES=2,
                     A=0.01,
                     response_time=srm.data$reaction_time,
                     response=srm.data$choice,
                     required_choice=srm.data$cor_res_Counterbalanced,
                     cue=srm.data$cue
                   ),
                   warmup = 500, 
                   iter = 1000,
                   chains = 3,
                   control = list(max_treedepth = 15))
        },error=function(e){
          print(paste0("could not run calculation for sid",sid," rid", r, " m", m))
          print(e)
          return(NA)
        })
        end_time<-Sys.time()
        if(!is.na(srm.fit)){
          run_package<-list("sid"=sid,"rid"=r,"motivation"=m,fit=srm.fit,duration=as.numeric(end_time-start_time))
          save(run_package,file=package_filepath)
          results.list<-c(results.list,list(run_package))
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
