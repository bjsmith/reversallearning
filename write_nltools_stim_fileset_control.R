source("rl_behav_analysis_learning_setup.R")
source("write_nltools_stim_fileset.R")
write_nltools_stim_fileset(rl.all.subjects.list,filePrefix = "detail",combinePresentationFeedback = TRUE,minTrialTime = 2)