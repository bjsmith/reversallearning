options(mc.cores = ceiling(parallel::detectCores()/2))
#options(mc.cores = NULL)
source("nate_files/fitGroupsV3Onegroup.R")
lookupOrRunFit(
  run=c(1),groups_to_fit=2, model_to_use="double_update",includeSubjGroup = FALSE,
  rp=c(2),,model_runs_separately = TRUE, include_pain=FALSE,
  fastDebug=TRUE,
  fileSuffix=paste0("testingMCMC"),
  estimation_method = ESTIMATION_METHOD.MCMC,
  bseed=sample(.Machine$integer.max,1)#546770702
)
#