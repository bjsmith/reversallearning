
#right, this time, we need to get some real data.
source("../util/apply_local_settings.R")
apply_local_settings("")
dd<-localsettings$data.dir
library(data.table)

#we have problems running all subjects in a single run.
#so let's have this save as we go, and then reload and avoid re-saving if there's already a saved file.
lba_rl_version<-"20180618_1"

single_run_dir<-paste0(localsettings$data.dir,"lba_rl")
output_dir<-paste0(single_run_dir,"/",lba_rl_version, "/")
dir.create(single_run_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)
#file_folder<-"/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/lba_rl_single_estimates.RData"
#load(file=file_folder)
results_summary_list_filepath<-paste0(output_dir,"run_package_summary_v2",lba_rl_version,".RData")
if(file.exists(results_summary_list_filepath)){
  load(results_summary_list_filepath)
}else{
  stop("couldn't find summary")
}


#at this point, we need to get a way to extract key datapoints from the results list and save that. Don't save it as it is; it's far too big!
#now we can get all this into a single summary statistics table. In that table, we can have the following columns:
#SID
#rid
#motivation
#parameter name
#mean,se_mean...all the columns in the fit_summary
for (i in 1:length(results.summary.list)){
  results.summary.list[[i]]$FullRunId<-i
}
results.summary.df<-do.call(rbind,lapply(results.summary.list,function(rsli){
  data.frame("sid"=rsli$sid,
             "rid"=rsli$rid,
             "motivation"=rsli$motivation,
             "param_name"=rownames(rsli$fit_summary),
             "FullRunId"=rsli$FullRunId,
             rsli$fit_summary)
  
} ))

rownames(results.summary.df)<-NULL
library(data.table)
results.summary.dt<-data.table(results.summary.df)