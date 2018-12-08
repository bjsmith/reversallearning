library(R.matlab)
library(data.table)
library(ggplot2)
library('tidyr')

library(corrplot)
library(Matrix)
require(lme4)
nonResponseTimeAsNA<-TRUE
source("rl_behav_analysis_learning_setup_amendment_2.R")

source("../util/apply_local_settings.R")
apply_local_settings()

rl.all.subjects.dt<-data.table(rl.all.subjects.list)

# nrow(rl.all.subjects.dt[,.N,.(subid,Motivation,runid)])
# rl.all.subjects.dt[,.N,.(subid,RiskCat)] %>% .[,.N,RiskCat]
# rl.all.subjects.dt[,.N,.(subid,runmotiveid, RiskCat)] %>% .[,.N,RiskCat]

fixedef.se<-function(fit){
  return(summary(fit)$coefficients[,"Std. Error"])
}

lm.beta.lmer <- function(mod) {
  b <- fixef(mod)[-1]
  sd.x <- apply(getME(mod,"X")[,-1],2,sd)
  sd.y <- sd(getME(mod,"y"))
  b*sd.x/sd.y
}

rl.all.subjects.list$reaction_time_adj<-rl.all.subjects.list$reaction_time
rl.all.subjects.list[is.na(reaction_time),reaction_time_adj:=1]
# table(is.na(rl.all.subjects.list$reaction_time))
# table(is.na(rl.all.subjects.list$reaction_time_adj))

library(rstanarm)
library(parallel)


m.rp.1.stan<-stan_glmer(correct~
                          Motivation + reaction_time + (reaction_time<0.1)+
                          presentation_n_in_segment + 
                          MethUse+SexRisk+
                          (1+presentation_n_in_segment | subid/runmotiveid) + 
                          (1 | image),rl.all.subjects.list,cores=detectCores(),
                        family = binomial(link = "logit"))

save(m.rp.1.stan,file=paste0(localsettings$data_dir,"behavioral_linear_stan_model_logistic.RData"))
#load(paste0(localsettings$data_dir,"behavioral_linear_stan_model.RData"))

summary(m.rp.1.stan)
# rownames(summary(m.rp.0.stan))
summary(m.rp.1.stan)[1:8,]
# #seems like we have a result here.

