library(R.matlab)
library(data.table)
library(ggplot2)
library('tidyr')

library(corrplot)
library(Matrix)
require(lme4)
source("rl_behav_analysis_learning_setup.R")

rl.all.subjects.dt<-data.table(rl.all.subjects.list)
rl.all.subjects.list.bysubj<-
  rl.all.subjects.list[,.(PropCorrect=mean(correct),
                          Mean_reaction_time=mean(reaction_time)),by=subid] %>% 
  merge(all.subject.data,by.x="subid",by.y="SUBID_ADJ_I")
#logistic regression assumptions to check:
# - no multicolinearity of IVs
# - IVs linearly related to log odds

#other ones we don't need to check:
# - large sample size
# - binary outcome
# - independent observations 
#   (not all are independent, but we have treated non-independence 
#     appropriately within the model)

# not perfect because we're doin hierarchical regression 
# which will have its own
# set of assumptions but it's a start

#check correlation among IVs
#per trial IVs
per.trial.ivs<-c("reaction_time","presentation_n")
corrplot(cor(rl.all.subjects.dt[,per.trial.ivs,with=FALSE]),method="number",type="upper")

#per subject IVs
per.subject.ivs<-colnames(rl.all.subjects.list.bysubj)[c(1,2,4:14,16:17,19:20,22:23,25:26,28:29,85)+1]
corrplot(cor(rl.all.subjects.list.bysubj[,per.subject.ivs,with=FALSE],use="pairwise.complete.obs")
         ,method="number",type="upper",diag=FALSE)
#OK. So we can't put (particular combinations of) multiple personality vars in the model
#that's OK.
#mild correlations between mean reaction time and openness (intelligence, openness)

#to test the relationship with log odds 
#we'd have to actually go and run the model to calculate the log odds.
#for the per-subject IVs, we know that none  are strongly related to Propcorrect except the group vars.
#for the individual trial IVs, we can check the relationship between Correct and reaction time
reaction_time_model<-glm(correct~reaction_time,rl.all.subjects.dt,
   family=binomial(link="logit"))
log.odds<-predict(reaction_time_model,rl.all.subjects.dt)
plot(log.odds,rl.all.subjects.dt$reaction_time)
plot(reaction_time_model)
