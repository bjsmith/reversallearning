#install.packages("R.matlab")
library(R.matlab)
#install.packages('data.table')
library(data.table)
#install.packages('ggplot2')
library(ggplot2)
#install.packages("tidyr")
library('tidyr')

#install.packages("corrplot")
library(corrplot)
library(Matrix)
require(lme4)
source("rl_behav_analysis_learning_setup.R")

#let's try to measure reaction time as a function of Group and Reward or Punishment trial.

m0<-lme4::lmer(reaction_time~1+(1| subid),data=rl.all.subjects.list)
summary(m0)

#reaction time varying by trial type (reward or punishment)
m1<-lme4::lmer(reaction_time~Motivation+(1| subid),data=rl.all.subjects.list)
summary(m1)
#doesn't seem to make a differenct at all! OK. Can we lookat something else?

#what about the order of presentation?
m1a<-lme4::lmer(reaction_time~presentation_n_over_segments+(1| subid),data=rl.all.subjects.list)
summary(m1a)
#doesn't make a difference to reaction time either.

#or onset time?
m1b<-lme4::lmer(reaction_time~onset_time_actual+(1| subid),data=rl.all.subjects.list)
summary(m1b)



#OK. we might have to try a logistic regression model and predict Correct. That's more interesting anyway.
mA0<-glmer(correct~ (1|subid),
      data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))

summary(mA0)
#Alright, great. Now let's predict Correct based on Reaction Time - that might be interesting!

mA1<-glmer(correct~ reaction_time+(1|subid),
           data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mA1)
#alright. Definitely a nice link there but interesting that actually, the relationship between Reaction_time and correct is *positive*.
#(what does that look like in the other direction?)
m1c<-lme4::lmer(reaction_time~correct+(1| subid),data=rl.all.subjects.list)
summary(m1c)

#let's stick with linear rather than logistic for now...seeing as it might be more interesting to predict being correct than
#reaction time, anyway.

#let's add a group-level predictor - the subject's sex risk
m2<-lme4::lmer(reaction_time~correct+SexRisk+(1| subid),data=rl.all.subjects.list)
summary(m2)
#OK. Sex risk doesn't relate to reaction time in the task.  What about Meth use?
m2a<-lme4::lmer(reaction_time~correct+MethUse+(1| subid),data=rl.all.subjects.list)
summary(m2a)
#nah. that's too bad. But we would actually be more interested if SexRisk or MethUse predicted correct rather than reaction_time.
mA2<-glmer(correct~ reaction_time+SexRisk+(1|subid),
           data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mA2)

mA2a<-glmer(correct~ reaction_time+MethUse+SexRisk+(1|subid),
           data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mA2a)
#OK. MethUse makes a small but respectable difference to correct, in the expected direction
#we should also include SexRisk in the model to help ensure that this isn't due to some conflation of MethUse with SexRisk
mA2b<-glmer(correct~ reaction_time+MethUse+SexRisk+(1|subid),
            data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mA2b)
#let's add the presentation's position in the segment, too.
table(rl.all.subjects.list$reversal_trial)
mA2c<-glmer(correct~ 
              reaction_time+presentation_n_in_segment+ #trial-level
              MethUse+SexRisk+(1|subid), #subject-level
            data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mA2c)

coef(mA2c)
fixef(mA2c)
ranef(mA2c)
#get the intercept and slope for each subject
#OK, let's restrict to fewer variables first as I'm learning
mA2d<-glmer(correct~ 
              reaction_time+ #trial-level
              MethUse+(1|subid), #subject-level
            data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mA2d)

coef(mA2d)
fixef(mA2d)
ranef(mA2d)
#model for a subject, say the first subject, 105
#coefficients
coef(mA2d)$subid[1,]
#fixed effects
fixef(mA2d)
#random effects intercept

library(data.table)
rl.all.subjects.dt<-data.table(rl.all.subjects.list)

#get a model for predicting the subject's likelihood of being correct
library(boot)
#
hist(inv.logit(fixef(mA2d)[1]+fixef(mA2d)["reaction_time"]*rl.all.subjects.dt[subid==105]$reaction_time+
                 fixef(mA2d)["MethUseTRUE"]*rl.all.subjects.dt[subid==105]$SexRisk+ranef(mA2d)$subid[1,]))

hist(inv.logit(fixef(mA2d)[1]+fixef(mA2d)["reaction_time"]*rl.all.subjects.dt[subid==126]$reaction_time+
                 fixef(mA2d)["MethUseTRUE"]*rl.all.subjects.dt[subid==126]$SexRisk+ranef(mA2d)$subid[1,]))
#standard deviations?

summary(mA2d)
#SD of random effects is 0.2931;
#SD of fixed effects...
fixedef.se<-function(fit){
  return(summary(fit)$coefficients[,"Std. Error"])
}
fixedef.se(mA2d)
#can this really be used as proof of meth use at the random effects level? I suspect not; we'd need to put methuse at the random level to see that
#ahh yes - see Gelman & Hill, 2006, p271...actually it doesn't sya anything about tha
#it only says we can't interpret the intercepts of individual subjects


#------
rl.all.subjects.list$presentation_n_in_segment_minus1<-rl.all.subjects.list$presentation_n_in_segment-1
mA2e<-glmer(correct~ 
              scale(reaction_time,scale = FALSE)+presentation_n_in_segment_minus1+reversal_trial+ #trial-level
              MethUse+SexRisk+(1|subid), #subject-level
            data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mA2e)

lm.beta.lmer <- function(mod) {
  b <- fixef(mod)[-1]
  sd.x <- apply(getME(mod,"X")[,-1],2,sd)
  sd.y <- sd(getME(mod,"y"))
  b*sd.x/sd.y
}
length(unique(rl.all.subjects.list$subid))
lm.beta.lmer(mA2e)
#approximate likelihoods (see Gelman & Hill, 2006, p82)
lm.beta.lmer(mA2e)/4


mA2f<-glmer(correct~ 
              scale(reaction_time,scale = FALSE)+presentation_n_in_segment_minus1*reversal_trial+ #trial-level
              MethUse+SexRisk+(1|subid), #subject-level
            data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mA2f)

lm.beta.lmer(mA2f)
#approximate likelihoods (see Gelman & Hill, 2006, p82)
lm.beta.lmer(mA2f)/4

rl.all.subjects.list$reaction_time_ms<-rl.all.subjects.list$reaction_time
rl.all.subjects.list$presentation_n_in_segment_minus5<-rl.all.subjects.list$presentation_n_in_segment-5
mA2g<-glmer(correct~ 
              scale(reaction_time,scale = FALSE)+presentation_n_in_segment_minus5*reversal_trial+ #trial-level
              MethUse+SexRisk+(1|subid), #subject-level
            data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mA2g)
hist(rl.all.subjects.list$reaction_time)
lm.beta.lmer(mA2g)
#approximate likelihoods (see Gelman & Hill, 2006, p82)
lm.beta.lmer(mA2g)/4

table(rl.all.subjects.dt$correct,rl.all.subjects.dt$MethUse)
chisq.test(rl.all.subjects.dt$correct,rl.all.subjects.dt$MethUse)
percent.correct.by.meth.use<-rl.all.subjects.dt[,sum(correct)/.N,by=MethUse]
percent.correct.by.meth.use$V1[1]/percent.correct.by.meth.use$V1[2]
mA2h<-glmer(correct~ 
              MethUse+(1|subid), #subject-level
            data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mA2h)


#TIME SERIES ANALYSIS

#VARYING SLOPES

mB.time<-glmer(correct~ 
            MethUse+ reversal_trial+presentation_n+(1|subid), #subject-level
          data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mB.time)

#now, try varying the slope across subjects!
mB.time2<-glmer(correct~ 
                  MethUse+ reversal_trial+presentation_n+(presentation_n + 1|subid), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mB.time2)

#and include group-level predictor (METH USE), interacting with teh slope.
mB.time3<-glmer(correct~ 
                  MethUse+ reversal_trial+ reversal_trial:presentation_n+presentation_n*MethUse+(presentation_n + 1|subid), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
#FAILED TO CONVERGE!

mB.time4<-glmer(correct~ scale(reaction_time,scale = FALSE)+
                  MethUse+ reversal_trial+ reversal_trial:presentation_n+presentation_n*MethUse+(presentation_n + 1|subid), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mB.time4)


mB.time5<-glmer(correct~ scale(reaction_time,scale = FALSE)+
                  MethUse+ reversal_trial+ reversal_trial*presentation_n+presentation_n:MethUse+(reversal_trial*presentation_n + 1|subid), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mB.time5)


#

mB.time6<-glmer(correct~ scale(reaction_time,scale = FALSE)+
                  reversal_trial*presentation_n+(reversal_trial*presentation_n + 1|subid), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mB.time6)


#try again...
mC.time<-glmer(correct~ scale(reaction_time,scale = FALSE)+presentation_n_in_segment_minus1+(1|subid), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time)

mC.time2<-glmer(correct~ 
                  scale(reaction_time,scale = FALSE)+
                  presentation_n_in_segment_minus1+
                  (presentation_n_in_segment_minus1 + 1|subid), #subject-level
               data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time2)

mC.time2a<-glmer(correct~ 
                  scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment,scale=FALSE)+
                  (scale(presentation_n_in_segment_minus1,scale=FALSE) + 1|subid), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time2a)

#alright, we get something here. But perhaps we should be separating out by image. 
#We can nest each image within subjects.
mC.time2b<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus1,scale=FALSE)+
                   (scale(presentation_n_in_segment_minus1,scale=FALSE) + 1|subid/image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time2b)
#but images occur across subjects as well, so we really shouldn't nest images within subjects.
mC.time2c<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus1,scale=FALSE)+
                   (scale(presentation_n_in_segment_minus1,scale=FALSE) + 1|subid) + 
                   (scale(presentation_n_in_segment_minus1,scale=FALSE) + 1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time2c)
coef(mC.time2c)

#a group level predictor, METH
mC.time2d<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus1,scale=FALSE)*MethUse+
                   (scale(presentation_n_in_segment_minus1,scale=FALSE)*MethUse + 1|subid) + 
                   (scale(presentation_n_in_segment_minus1,scale=FALSE)*MethUse + 1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time2d)
coef(mC.time2d)
#at this point, I should try to get the slopes. What do the slopes look like for the different subjects?
#let's graph them.


mC.time2e<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus1,scale=FALSE)*MethUse+
                   (scale(presentation_n_in_segment_minus1,scale=FALSE)*MethUse + 1|subid) + 
                   (scale(presentation_n_in_segment_minus1,scale=FALSE)*MethUse + 1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(mC.time2e)


mC.time2f<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus1,scale=FALSE)*MethUse+
                   (scale(presentation_n_in_segment_minus1,scale=FALSE) + 1|subid) + 
                   (scale(presentation_n_in_segment_minus1,scale=FALSE) + 1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(mC.time2f)