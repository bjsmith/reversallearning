library(R.matlab)
library(data.table)
library(ggplot2)
library('tidyr')

library(corrplot)
library(Matrix)
require(lme4)
nonResponseTimeAsNA<-TRUE
source("rl_behav_analysis_learning_setup_amendment_2.R")

rl.all.subjects.dt<-data.table(rl.all.subjects.list)

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
table(is.na(rl.all.subjects.list$reaction_time))
table(is.na(rl.all.subjects.list$reaction_time_adj))

#what does reaction time look like in a distribution?
library(ggplot2)
ggplot(rl.all.subjects.list,aes(reaction_time,color=correct))+geom_density()+#facet_grid(SexRisk~.,margins=TRUE)
  scale_x_continuous(breaks=seq(0.2,1.2,0.2))+
  labs(title="Reaction time density by outcome")
ggplot(rl.all.subjects.list,aes(reaction_time_adj,color=correct))+geom_density()+#facet_grid(SexRisk~.,margins=TRUE)
  scale_x_continuous(breaks=seq(0.2,1.2,0.2))+
  labs(title="Reaction time density by outcome")
rl.all.subjects.list[,.N,by=.(subid,MethUse)] %>% .()

mA2c.rt0<-glmer(correct~ 
                 reaction_time+presentation_n_in_segment+ #trial-level
                 MethUse+SexRisk+(1|subid), #subject-level
               data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(mA2c.rt0)

chisq.test(rl.all.subjects.list[MethUse==TRUE,correct],rl.all.subjects.list[MethUse==FALSE,correct])

lm.beta.lmer(mA2c.rt0)

mA2c.nm<-glmer(correct~ 
                 reaction_time_adj+presentation_n_in_segment+ #trial-level
                 MethUse+SexRisk+(1|subid), #subject-level
               data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(mA2c.nm)

mA2c.nm2<-glmer(correct~ 
                 reaction_time_adj+I(reaction_time_adj^2)+presentation_n_in_segment+ #trial-level
                 MethUse+SexRisk+(1|subid), #subject-level
               data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(mA2c.nm2)

mA2c.nm3<-glmer(correct~ 
                  reaction_time_adj+I(reaction_time_adj^2)+I(reaction_time_adj^3)+presentation_n_in_segment+ #trial-level
                  MethUse+SexRisk+(1|subid), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(mA2c.nm3)

# does eliminating reaction time change the prediction?
# in other words, does reaction time carry useful information which might be moderating the rleationship between corrct and MethUse/SexRisk; is reaction time a NEGATIVE CONFOUNDER?

mA2c.noreactiontime<-glmer(correct~ 
                             presentation_n_in_segment+ #trial-level
                             MethUse+SexRisk+(1|subid), #subject-level
                           data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(mA2c.noreactiontime)

#looking at this - the regression coefficients for MethUse and SexRisk appear to disappear
#Reaction time may be a negative confounder.
#https://www.researchgate.net/post/Why_in_regression_analysis_does_the_inclusion_of_a_new_variable_makes_other_variables_that_previously_were_not_statistically_significant

mA2c.rt.interact<-glmer(correct~ 
                 reaction_time+presentation_n_in_segment+ #trial-level
                 MethUse*reaction_time+SexRisk*reaction_time+(1|subid), #subject-level
               data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(mA2c.rt.interact)
#quite interesting: allowing an interaction term between reaction_time and MethUse,
# - MethUse becomes more significant
# - Impact of SexRisk on performance disappears
# - The interaction of RT with MethUse and SexRisk are not themselves significant.

rt.A<-lmer(reaction_time~ 
                          +presentation_n_in_segment+ #trial-level
                          MethUse+(1|subid), #subject-level
                        data = rl.all.subjects.list)
summary(rt.A)
#MethUse doesn't predict reaction time....
rt.A1<-lmer(reaction_time~ correct*MethUse
             +presentation_n_in_segment+ #trial-level
             MethUse+(1|subid), #subject-level
           data = rl.all.subjects.list)
summary(rt.A1)

#But MethUse interacting with correct might be a predictor; i.e.,...
rt.A1.cor<-lmer(reaction_time~ MethUse
            +presentation_n_in_segment+ #trial-level
              (1|subid), #subject-level
            data = rl.all.subjects.list[correct==TRUE])
summary(rt.A1.cor)

rt.A1.ncor<-lmer(reaction_time~ MethUse
                +presentation_n_in_segment+ #trial-level
                  (1|subid), #subject-level
                data = rl.all.subjects.list[correct==FALSE])
summary(rt.A1.ncor)
#Nope. Neither in Correct nor incorrect trials alone is MethUse a predictor of reaction time.


#many false results are due to participants responding far too quickly to make a good response.
#we might want to include this in the model.
#Specifically: we model correct~reaction time according to two regressors: whether RT is less than 0.1
#and whether RT predicts %c
mB<-glmer(correct~ (reaction_time<0.1)+
                 reaction_time+presentation_n_in_segment+ #trial-level
                 MethUse+SexRisk+(1|subid), #subject-level
               data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(mB)
lm.beta.lmer(mB)
#SO, it is clear here that:
# - reaction time is STILL a predictor of performanace
# - getting a RT score less than 0.1 is strongly associated with bad performance

mB.1<-glmer(correct~ (reaction_time<0.1)+
            reaction_time+presentation_n_in_segment:(reaction_time>0.1)+ #trial-level
            MethUse:(reaction_time>0.1)+SexRisk:(reaction_time>0.1)+(1|subid), #subject-level
          data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(mB.1)
lm.beta.lmer(mB.1)

#DO A LINEAR MODEL PREDICTING NON-RESPONSE RATES FROM METH USE AND OTHER RELEVANT VARIABLES.
#THIS MIGHT IMPACT ON THE WAY WE TREAT THE DATA.


#Here, where we include interactions of reaction time being less than 0.1 or more than 0.1,
#we can see that when raection time is less than 0.1, nothing is particularly predictive of score.
#however, when reaction time is greater than 0.1, MethUse, presentation_n, etc, are predictive.
#digging a bit deeper, performance these trials with low reaction times is worse than chance:

table(rl.all.subjects.list[,.(reaction_time>0.1,correct)])

#This is 98% INCORRECT! Something is off here. No one's performance could be this bad by chance.
#what's going on?
#- this is 

ggplot(rl.all.subjects.list,aes(reaction_time,color=correct))+geom_density()+#facet_grid(SexRisk~.,margins=TRUE)
  coord_cartesian(xlim=c(0,0.2))
  labs(title="Reaction time density by outcome")

hist(rl.all.subjects.list[reaction_time<0.2,reaction_time],breaks = 100)
#ahh. So there is a mistake in the data.
#We have 8000 trials where the response is recorded as incorrect but their reaction time is 0.
table(rl.all.subjects.list$correct)
#need to work out why these were coded as incorrect when they were really non-responses!

#I think I can't publish this without an attempt to control for both subject and run-level factors.