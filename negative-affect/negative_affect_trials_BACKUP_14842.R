<<<<<<< HEAD
source("util/apply_local_settings.R")
apply_local_settings()
dd<-localsettings$data.dir
rawdata <- read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment.txt"), header=T)
#do trials where the subject received punishment have a stronger NPS pain signal than trials wher they did not?

source("read_nps_output.R")
pain_data<-get_nps_data_for_subs(100:218)
pain_data$Motivation="punishment"

names(pain_data)
names(rawdata)
pain_data$ResponseCorrect<-pain_data$Outcome=="correct"
rawdata<-data.table(merge(
  rawdata,pain_data,
  by.x=c("subid","presentation_n","image","runid","Motivation","first_reversal","presentation_n_in_segment"),
  by.y=c("subid","presentation_n","image","runid","Motivation","first_reversal","presentation_n_in_segment"),all.x=TRUE,all.y=FALSE))

warning("NEED TO RE-RUN WITH ALL SUBJECTS ONCE RECALCULATION OF NEURAL DATA IS COMPLETE")
=======
source("negative_affect/negative_affect_trials_setup.R")

warning("Should test several more runs to determine that the method is reliable.")
>>>>>>> 309586672523d3343f88ef7ea6f3b19d7aa518d5
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
  geom_boxplot()+ylim(c(-10,10))
#the difference is in the right direction
summary(lm(Value~ResponseCorrect,pain_data))
pain_data$runid<-as.factor(pain_data$runid)
pain_data$subid<-as.factor(pain_data$subid)
pain_data$image<-as.factor(pain_data$image)
#hierarchical
library(lme4)
<<<<<<< HEAD
m.sronly<-lmer(Value~presentation_n_in_segment+ (1 | subid/runid),pain_data)
summary(m.sronly)
m.withReponseCorrect<-lmer(Value~ResponseCorrect + presentation_n_in_segment + (1+ResponseCorrect | subid/runid),pain_data)
=======

m.sronly<-lmer(ValueScaled~presentation_n_in_segment+ (1 | subid/runid),pain_data)
summary(m.sronly)
m.withReponseCorrect<-lmer(ValueScaled~ResponseCorrect + presentation_n_in_segment + (1+ResponseCorrect | subid/runid),pain_data)

>>>>>>> 309586672523d3343f88ef7ea6f3b19d7aa518d5
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

#ResponseCorrect should track the Value, because Value is a function of whether the shock was delivered or not
#and ResponseCorrect tells us whether or not a shock was delivered
#The fact we can't get a finding here suggests that our NPS signature is not that good at detecting a punishment signal.
anova(m.sronly,m.withReponseCorrect)

#Is there anything we're missing here that might improve?
<<<<<<< HEAD
#pain_data$
#lmer(Value~ResponseCorrect + presentation_n_in_segment + (1+ResponseCorrect | subid/runid),pain_data))
m.withImage<-lmer(Value~ResponseCorrect + presentation_n_in_segment + (1+ResponseCorrect | subid/runid) + (1 | image),pain_data)
summary(m.withImage)
fixed_effects.w.p(m.withImage)

m.withImage2<-lmer(Value~ResponseCorrect * presentation_n_in_segment + (1+ResponseCorrect | subid/runid) + (1 | image),pain_data)
=======


m.withImage<-lmer(ValueScaled~ResponseCorrect + presentation_n_in_segment + (1+ResponseCorrect | subid/runid) + (1 | image),pain_data)
summary(m.withImage)
fixed_effects.w.p(m.withImage)

m.withImage2<-lmer(ValueScaled~
                     ResponseCorrect * presentation_n_in_segment + 
                     (1+ResponseCorrect * presentation_n_in_segment | subid/runid) + 
                     (1+ResponseCorrect * presentation_n_in_segment | image),pain_data)
>>>>>>> 309586672523d3343f88ef7ea6f3b19d7aa518d5
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
