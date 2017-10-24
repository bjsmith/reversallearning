source("write_nltools_stim_fileset.R")
write_nltools_stim_fileset(rl.all.subjects.list[Motivation=="punishment"],filePrefix="detail",
                           combinePresentationFeedback=TRUE,
                           minTrialTime=2)
write_nltools_stim_fileset(rl.all.subjects.list[Motivation=="reward"],filePrefix="detail",
                           combinePresentationFeedback=TRUE,
                           minTrialTime=2)

