library(R.matlab)
library(data.table)
library(ggplot2)
library('tidyr')

library(corrplot)
library(Matrix)
require(lme4)
source("rl_behav_analysis_learning_setup.R")

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

rl.all.subjects.list$reaction_time_ms<-rl.all.subjects.list$reaction_time
rl.all.subjects.list$presentation_n_in_segment_minus5<-rl.all.subjects.list$presentation_n_in_segment-5
#Alright, great. Now let's predict Correct based on Reaction Time - that might be interesting!

mA1<-glmer(correct~ reaction_time+(1|subid),
           data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mA1)

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


mC.time2a<-glmer(correct~ 
                  scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment,scale=FALSE)+
                  (scale(presentation_n_in_segment_minus1,scale=FALSE) + 1|subid), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time2a)

#alright, we get something here. But perhaps we should be separating out by image. 
#images occur across subjects as well, so we really shouldn't nest images within subjects.
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


#let's try to integrate the best of everything done so far
#probably can't do an integration of meth use interactions but we can do MethUse+SexRisk+Reversal
mC.time3<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus1,scale=FALSE)+MethUse+SexRisk+
                   (scale(presentation_n_in_segment_minus1,scale=FALSE) + 1|subid) + 
                   (scale(presentation_n_in_segment_minus1,scale=FALSE) + 1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3)
coef(mC.time3)

#we lost much of the effect of methuse and sexrisk. is that due to the addition of the per-image measure?
mC.time3a<-glmer(correct~ 
                  scale(reaction_time,scale = FALSE)+
                  scale(presentation_n_in_segment_minus1,scale=FALSE)+MethUse+SexRisk+
                  (scale(presentation_n_in_segment_minus1,scale=FALSE) + 1|subid) + 
                  (1|image), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3a)
coef(mC.time3a)

mC.time3b<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+MethUse+SexRisk+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3b)
coef(mC.time3b)

#adding scale into the *subject level* tends to make things fall apart, unfortunately. 
mC.time3c<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus1,scale=FALSE)+MethUse+SexRisk+
                   (1|subid), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3c)
coef(mC.time3c)

#we still get a meth use effect though!

#can we look at personality now?
#so...what would we look at exactly?
colnames(rl.all.subjects.list)
#what if we start by converting the sexrisk var into continuous?
mC.time3d<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+MethUse+rl.sex.uai90+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3d)
#converge failure! I don't know why this fails while it works with dichotomous, but OK.

#let's try negative urgency, since it was so important for sex-risk itself.
mC.time3e<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+MethUse+SexRisk+
                   scale(URGNEG,scale=FALSE)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3e)
#negative murgency makes this whole model worse...methuse is no longer a significant factor
#and negative urgency itself isn't significant.

#how about intelligence?
mC.time3f<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+MethUse+SexRisk+
                   scale(O_INTELL,scale=FALSE)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3f)

#if we start from including image?
mC.time3g<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+MethUse+SexRisk+
                   scale(URGNEG,scale=FALSE)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3g)

#sooooo little difference. almost none at all.
mC.time3h<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+MethUse+SexRisk+
                   scale(O_INTELL,scale=FALSE)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3h)

#reaction time is a clear negative co-founder:
colnames(rl.all.subjects.list)
mC.time3i<-glmer(correct~ 
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+MethUse+SexRisk+
                   scale(O_INTELL,scale=FALSE)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3i)

#BAS_RR
#strange that just about any personality variable seems to reduce the influence of meth use.
mC.time3j<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+MethUse+SexRisk+
                   scale(BAS_RR,scale=FALSE)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3j)

#OK so never mind about that. But...did we ever try Motivation (reward/punishment)
colnames(rl.all.subjects.list)
mC.time3k<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+MethUse+SexRisk+
                   scale(BAS_RR,scale=FALSE)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid)+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time3k)

mC.time4<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)*MethUse+
                   Motivation+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(mC.time4)
mC.time4a<-glmer(correct~ 
                  scale(reaction_time,scale = FALSE)+
                  scale(presentation_n_in_segment_minus5,scale=FALSE)*MethUse+
                  Motivation+
                  (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                  (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|image), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time4a)

#you know what--do we really need to control for the presentation number across the image level?
#I don't think that's right and we're losing power unnecessarily.
mC.time5<-glmer(correct~ 
                  scale(reaction_time,scale = FALSE)+
                  scale(presentation_n_in_segment_minus5,scale=FALSE)*MethUse+
                  Motivation+
                  (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                  (1|image), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time5)

mC.time5a<-glmer(correct~ 
                  scale(reaction_time,scale = FALSE)+
                  scale(presentation_n_in_segment_minus5,scale=FALSE)*MethUse+SexRisk+
                  Motivation*BAS_RR+
                  (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                  (1|image), #subject-level
                data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time5a)

mC.time5b<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+
                   MethUse+SexRisk+
                   Motivation*BAS_RR+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                   (1|image), #subject-level
                 data = rl.all.subjects.list,family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(mC.time5b)

mC.time5c<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+
                   MethUse+SexRisk+
                   Motivation+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                   (1|image), #subject-level
                 data = rl.all.subjects.list,
                 family = binomial,
                 control=glmerControl(optimizer="bobyqa"))
summary(mC.time5c)

mC.time5d<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+
                   MethUse+SexRisk+
                   Motivation+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|image), #subject-level
                 data = rl.all.subjects.list,
                 family = binomial,
                 control=glmerControl(optimizer="bobyqa"))
summary(mC.time5d)


mC.time5e<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+
                   MethUse+SexRisk+
                   Motivation+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|image), #subject-level
                 data = rl.all.subjects.list,
                 family = binomial,
                 control=glmerControl(optimizer="Nelder_Mead"))
summary(mC.time5e)
#Nelder_MElt doesn't help with convergence

#removing the presentation_n_in_segment_minus5 does.


mC.time5f<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+
                   MethUse+SexRisk+
                   Motivation+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                   (1|image), #subject-level
                 data = rl.all.subjects.list,
                 family = binomial,
                 control=glmerControl(optimizer="bobyqa"))
summary(mC.time5f)

#what about looking at various personality measures as interactions of learning rate?
mC.time5g<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)*BAS_RR+
                   MethUse+SexRisk+
                   Motivation+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                   (1|image), #subject-level
                 data = rl.all.subjects.list,
                 family = binomial,
                 control=glmerControl(optimizer="bobyqa"))
summary(mC.time5g)

mC.time5h<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)*O_INTELL+
                   MethUse+SexRisk+
                   Motivation+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                   (1|image), #subject-level
                 data = rl.all.subjects.list,
                 family = binomial,
                 control=glmerControl(optimizer="bobyqa"))
summary(mC.time5h)

#OK. So it might be problematic to include meth use and sex risk in the model 
#because meth use and sex risk are pretty strongly related to a lot of personality variables
#so if we want to look at personality, we ought to take out meth use and sex risk and put personality in.
#we can insert the rest later on.


class(rl.all.subjects.list$subid)

mC.personality1<-glmer(correct~ 
                   scale(reaction_time,scale = FALSE)+
                   scale(presentation_n_in_segment_minus5,scale=FALSE)+
                     O_INTELL+BAS_RR+O_OPEN+
                   (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                   (1|image), #subject-level
                 data = rl.all.subjects.list,
                 family = binomial,
                 control=glmerControl(optimizer="bobyqa"))
summary(mC.personality1)

mC.personality2<-glmer(correct~ 
                         scale(presentation_n_in_segment_minus5,scale=FALSE)+
                         scale(O_OPEN,scale=FALSE)+
                         scale(O_INTELL,scale=FALSE)+
                         scale(BAS_RR,scale=FALSE)+
                         (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                         (1|image), #subject-level
                       data = rl.all.subjects.list,
                       family = binomial,
                       control=glmerControl(optimizer="bobyqa"))
summary(mC.personality2)

mC.personality3<-glmer(correct~ 
                         scale(reaction_time,scale = FALSE)+
                         scale(presentation_n_in_segment_minus5,scale=FALSE)+
                         scale(O_OPEN,scale=FALSE)+
                         (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                         (1|image), #subject-level
                       data = rl.all.subjects.list,
                       family = binomial,
                       control=glmerControl(optimizer="bobyqa"))
summary(mC.personality3)

mC.personality4<-glmer(correct~ 
                         scale(reaction_time,scale = FALSE)+
                         scale(presentation_n_in_segment_minus5,scale=FALSE)+
                         scale(O_INTELL,scale=FALSE)+
                         (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                         (1|image), #subject-level
                       data = rl.all.subjects.list,
                       family = binomial,
                       control=glmerControl(optimizer="bobyqa"))
summary(mC.personality4)

mC.personality4a<-glmer(correct~ 
                         scale(reaction_time,scale = FALSE)*scale(O_INTELL,scale=FALSE)+
                         scale(presentation_n_in_segment_minus5,scale=FALSE)*scale(O_INTELL,scale=FALSE)+
                          scale(presentation_n_in_segment_minus5,scale=FALSE):scale(AGREE,scale=FALSE)+
                          scale(presentation_n_in_segment_minus5,scale=FALSE):scale(E_ASSERT,scale=FALSE)+
                         (1|subid) + 
                         (1|image), #subject-level
                       data = rl.all.subjects.list,
                       family = binomial,
                       control=glmerControl(optimizer="bobyqa"))
summary(mC.personality4a)

mC.personality5<-glmer(correct~ 
                         scale(reaction_time,scale = FALSE)+
                         scale(presentation_n_in_segment_minus5,scale=FALSE)+
                         scale(BAS_RR,scale=FALSE)+
                         (scale(presentation_n_in_segment_minus5,scale=FALSE) + 1|subid) + 
                         (1|image), #subject-level
                       data = rl.all.subjects.list,
                       family = binomial,
                       control=glmerControl(optimizer="bobyqa"))
summary(mC.personality5)

#hold-up. What if we do some fishing for strong personality correlations with correct performance across subjects 
#and then further investigate any that pass a FDR threshold?
rl.all.subjects.list.bysubj<-
  rl.all.subjects.list[,.(PropCorrect=mean(correct)),by=subid] %>% 
  merge(all.subject.data,by.x="subid",by.y="SUBID_ADJ_I")
#merge this with all subject data

propcorrect.cor<-NULL
for (col in colnames(rl.all.subjects.list.bysubj)[c(4:14,16:17,19:20,22:23,25:26,28:29)]){
  #missing.vals<-is.na(rl.all.subjects.list.bysubj[,cor,with=FALSE])
  corres<-cor.test(rl.all.subjects.list.bysubj[,col,with=FALSE][[col]],
                   rl.all.subjects.list.bysubj[,"PropCorrect",with=FALSE][["PropCorrect"]]
                   )
  corresrow<-as.data.frame(t(unlist(corres)))
  corresrow$cor.var<-col
  if (is.null(propcorrect.cor)){
    propcorrect.cor<-corresrow
  }else{
    propcorrect.cor<-rbind(propcorrect.cor,corresrow)
  }
}
propcorrect.cor$p.value<-as.numeric(as.character(propcorrect.cor$p.value))
#now need to do an FDR test

library(stats)
propcorrect.cor$p.value.adjusted<-p.adjust(propcorrect.cor$p.value,method="BH")
View(propcorrect.cor)

#no personality variable can predict, significantly, after adjustment for multiple comparisons, Proportion Correct.
