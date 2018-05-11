source("negative-affect/negative_affect_trials_setup.R")

warning("Should test several more runs to determine that the method is reliable.")

#descriptives
lapply(pain_data,length)
lapply(pain_data,function(x){length(unique(x))})
table(pain_data$image)
table(pain_data$IsPlaceholder)
table(pain_data$Segment)
table(pain_data$Outcome)
table(pain_data$presentation_n_in_segment)
table(pain_data$first_reversal)
table(pain_data$presentation_n)
table(pain_data$subid)
table(pain_data$runid)

table(pain_data$ResponseCorrect)
ggplot(pain_data,aes(ResponseCorrect,Value))+
  geom_boxplot()+ylim(c(-100,100))

ggplot(pain_data,aes(ResponseCorrect,Value))+
  geom_violin()+coord_cartesian(ylim=c(-600,0))
#the difference is in the right direction
summary(lm(Value~ResponseCorrect,pain_data))
pain_data$runid<-as.factor(pain_data$runid)
pain_data$subid<-as.factor(pain_data$subid)
pain_data$image<-as.factor(pain_data$image)
#hierarchical
library(lme4)


m.sronly<-lmer(ValueScaled~presentation_n_in_segment+ (1 | subid/runid),pain_data)
summary(m.sronly)
m.withReponseCorrect<-lmer(ValueScaled~ResponseCorrect + presentation_n_in_segment + (1+ResponseCorrect | subid/runid),pain_data)
summary(m.withReponseCorrect)

fixed_effects.w.p<-function(m){
  # extract coefficients
  coefs <- data.frame(coef(summary(m.withReponseCorrect)))
  # use normal distribution to approximate p-value
  coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
  return(coefs)
}
fixed_effects.w.p(m.withReponseCorrect)

anova(m.withReponseCorrect)
anova(m.sronly,m.withReponseCorrect)

anova(m.sronly,m.withReponseCorrect)

#Is there anything we're missing here that might improve?
#pain_data$
#lmer(Value~ResponseCorrect + presentation_n_in_segment + (1+ResponseCorrect | subid/runid),pain_data))
m.withImage<-lmer(Value~ResponseCorrect + presentation_n_in_segment + (1+ResponseCorrect | subid/runid) + (1 | image),pain_data)
summary(m.withImage)
fixed_effects.w.p(m.withImage)

m.withImage2<-lmer(Value~ResponseCorrect * presentation_n_in_segment + (1+ResponseCorrect | subid/runid) + (1 | image),pain_data)

summary(m.withImage2)
fixed_effects.w.p(m.withImage2)

#Doesn't really help much. We're still left with very very little relationship between ResponseCorrect and Value.

#should also test to see other theories about pain signal, like: does it distract and *worsen* performance in the subsequent round?
#will probably need reward trials to control for INCORRECT result here.
#so to work this out, we need a unique ID for each trial and then indicate whether or not there was an electricity shock

#alright, how would this work?

#get a list and record the "previous pain signature value" AND the "previous punished" value
#these will simply be two extra values added to the equation.
#to get the order we'll need to know the sequential order of these things.



m2.0<-
  glmer(ResponseCorrect ~ (1 | subid/runid) + (1 | image),
        rawdata.ordered,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(m2.0)
#rawdata.ordered
m2.1<-
  glmer(ResponseCorrect ~ PreviousValue + (1 | subid/runid) + (1 | image),
        rawdata.ordered,family = binomial, control=glmerControl())
summary(m2.1)

m2.2<-
  glmer(ResponseCorrect ~ PreviousValue + PreviousCorrect + (1 | subid/runid) + (1 | image),
        rawdata.ordered,family = binomial, control=glmerControl())
summary(m2.2)

m2.2a<-
  lmer(ResponseCorrect ~ PreviousValue + PreviousCorrect + (PreviousValue | subid/runid) +
         (PreviousValue | image),rawdata.ordered)
summary(m2.2a)

m2.3<-
  lmer(ResponseCorrect ~ PreviousValue*PreviousCorrect + (1 | subid/runid) +
         (1 | image),rawdata.ordered)
summary(m2.3)

anova(m2.1,m2.2,m2.3)


m2.4<-
  lmer(ResponseCorrect ~ PreviousValueScaled*PreviousCorrect + (1+ PreviousValueScaled*PreviousCorrect | subid/runid) +
         (1 + PreviousValueScaled*PreviousCorrect | image),rawdata.ordered)
summary(m2.4)

anova(m2.1,m2.2,m2.3,m2.4)

