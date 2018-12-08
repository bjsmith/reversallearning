source("stanlba/lba_rl_setup_v2.R")
require(R.utils)
options(mc.cores = 6)

# sub105data<-rawdata[subid==105 & Motivation=="reward" & runid==1,.(reaction_time,outcome,cue,choice,cor_res_Counterbalanced)]

#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.
lba_rl_version<-"20180624_1"

single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
output_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")
dir.create(single_run_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)
#file_folder<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/lba_rl_single_estimates.RData"
#load(file=file_folder)
Rhat_limit<-1.10
results.list<-list()
#lba_rl_single<-stan_model('stanlba/stanfiles/lba_rl_single_exp_v3.stan')
for (sid in unique(rawdata$subid)){#c(105,106,107)){#unique(rawdata$subid)[1:3]){#sid<-1053#
  for (r in unique(rawdata[subid==sid,runid])){#r<-1
    for(m in unique(rawdata[subid==sid & runid==r,Motivation])){#m<-"punishment"
      package_filepath<-paste0(output_dir,"run_package_",sid,"_",r,"_",m,"_v2.RData")
      #load it
      if(file.exists(package_filepath)){
        load(package_filepath)
        #check it out.
        if (any(summary(run_package$fit)$summary[,"Rhat"]>Rhat_limit)){
          print(paste0(sid,"_",r,"_",m, " has Rhat value outside accepted range. Deleting..."))
          file.remove(package_filepath)
        }
      }
    }
  }
}
