
source("nate_files/fitGroupsV3Onegroup.R")

fit.RiskyNoMeth<-lookupOrRunFit(
  run=c(1,2),groups_to_fit=2, model_to_use="double_update_rpo_repeated_runs",includeSubjGroup = FALSE,
  rp=c(REVERSAL_LEARNING_REWARD,REVERSAL_LEARNING_PUNISHMENT),
  model_rp_separately=TRUE,model_runs_separately = TRUE, include_pain=FALSE)
fit.RiskyNoMeth.ex<-rstan::extract(fit.RiskyNoMeth$fit)
rm(fit.RiskyNoMeth)#these are large files; let's not keep them in memory where unnecessary.

fit.RiskyMeth<-lookupOrRunFit(
  run=c(1,2),groups_to_fit=3, model_to_use="double_update_rpo_repeated_runs",includeSubjGroup = FALSE,
  rp=c(REVERSAL_LEARNING_REWARD,REVERSAL_LEARNING_PUNISHMENT),
  model_rp_separately=TRUE,model_runs_separately = TRUE, include_pain=FALSE)
fit.RiskyMeth.ex<-rstan::extract(fit.RiskyMeth$fit)
rm(fit.RiskyMeth)#these are large files; let's not keep them in memory where unnecessary.

fit.SafeNoMeth<-lookupOrRunFit(
  run=c(1,2),groups_to_fit=1, model_to_use="double_update_rpo_repeated_runs",includeSubjGroup = FALSE,
  rp=c(REVERSAL_LEARNING_REWARD,REVERSAL_LEARNING_PUNISHMENT),
  model_rp_separately=TRUE,model_runs_separately = TRUE, include_pain=FALSE)
fit.SafeNoMeth.ex<-rstan::extract(fit.SafeNoMeth$fit)
rm(fit.SafeNoMeth)#these are large files; let's not keep them in memory where unnecessary.

source("data_summarize.R")

RiskyNoMeth.grouplevel.dt<- data_summarize_double_update_rpo_repeated_runs(fit.RiskyNoMeth.ex)
RiskyNoMeth.grouplevel.dt$Group<-"RiskyNoMeth"

RiskyMeth.grouplevel.dt<- data_summarize_double_update_rpo_repeated_runs(fit.RiskyMeth.ex)
RiskyMeth.grouplevel.dt$Group<-"RiskyMeth"
SafeNoMeth.grouplevel.dt<- data_summarize_double_update_rpo_repeated_runs(fit.SafeNoMeth.ex)
SafeNoMeth.grouplevel.dt$Group<-"SafeNoMeth"
grouplevel.dt<-rbind(RiskyNoMeth.grouplevel.dt,RiskyMeth.grouplevel.dt,SafeNoMeth.grouplevel.dt)
