source("util/apply_local_settings.R")
apply_local_settings()
dd<-localsettings$data.dir
rawdata <- read.table(paste0(dd,"all_subjs_datacomplete_reward_and_punishment.txt"), header=T)
#do trials where the subject received punishment have a stronger NPS pain signal than trials wher they did not?

source("read_nps_output.R")
pain_data<-get_nps_data_for_subs(100:400)
pain_data$Motivation="punishment"

names(pain_data)
names(rawdata)
pain_data$ResponseCorrect<-pain_data$Outcome=="correct"
rawdata<-data.table(merge(
  rawdata,pain_data,
  by.x=c("subid","presentation_n","image","runid","Motivation","first_reversal","presentation_n_in_segment"),
  by.y=c("subid","presentation_n","image","runid","Motivation","first_reversal","presentation_n_in_segment"),all.x=TRUE,all.y=FALSE))


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
rawdata.ordered$ValueScaled<-scale(rawdata.ordered$Value)

table(pain_data$ResponseCorrect)
ggplot(pain_data,aes(ResponseCorrect,Value))+
  geom_boxplot()+ylim(c(-10,10))

summary(lm(Value~ResponseCorrect,pain_data))
summary(glm(ResponseCorrect~ValueScaled,pain_data,family = binomial(link="logit")))
#OK - this seems promising. Now we need to fill out to a hierarchical model.

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
rawdata.ordered$PreviousValueScaled<-scale(rawdata.ordered$PreviousValue)
fixed_effects.w.p(m.withReponseCorrect)

anova(m.withReponseCorrect)
anova(m.sronly,m.withReponseCorrect)
#so, this doesn't show us that the pain value has any EFFECT on learning. It *does* show us that correct response predicts pain signal
#i.e., the pain signal is tracking something related to value.
#I really should test this on the reward trials too.

#ResponseCorrect should track the Value, because Value is a function of whether the shock was delivered or not
#and ResponseCorrect tells us whether or not a shock was delivered
#The fact we can't get a finding here suggests that our NPS signature is not that good at detecting a punishment signal.


#Is there anything we're missing here that might improve?
#pain_data$
#
m.withImage<-lmer(ValueScaled~ResponseCorrect + presentation_n_in_segment + (1+ResponseCorrect | subid/runid) + (1 | image),pain_data,
                  control=lmerControl(optimizer="Nelder_Mead"))
summary(m.withImage)
fixed_effects.w.p(m.withImage)

m.withImage2<-lmer(ValueScaled~ResponseCorrect * presentation_n_in_segment + (1+ResponseCorrect | subid/runid) + (1 | image),pain_data)
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

#iterate through each run and add those values 
#order rawdata
rawdata.ordered<-rawdata[order(subid,runid,Motivation,onset_time_designed),]
for (s in unique(rawdata.ordered$subid)){
  for (r in unique(rawdata.ordered$runid)){#s<-105;r=1;
    print(s)
    if (dim(rawdata.ordered[subid==s & runid==r & Motivation=="punishment"])[1]>0){
      preceeding_vals_1<-rawdata.ordered[subid==s & runid==r & Motivation=="punishment"] %>% 
        .[1:(dim(.)[1]-1),Value]
      preceeding_vals<-c(mean(preceeding_vals_1),preceeding_vals_1)
      rawdata.ordered[subid==s & runid==r & Motivation=="punishment",PreviousValue:=preceeding_vals]
      
      preceeding_Outcome_1<-rawdata.ordered[subid==s & runid==r & Motivation=="punishment"] %>%
        .[1:(dim(.)[1]-1),Outcome]
      preceeding_Correct<-c(TRUE,preceeding_Outcome_1=="correct")
      rawdata.ordered[subid==s & runid==r & Motivation=="punishment",PreviousCorrect:=preceeding_Correct]
      
    }
  }
}
rawdata.ordered.complete<-rawdata.ordered[
  !is.na(PreviousValue)&!is.na(PreviousCorrect) & Motivation=="punishment"
]

m2.0<-
  glmer(ResponseCorrect ~ (1 | subid/runid) + (1 | image),
        rawdata.ordered,family = binomial, control=glmerControl(optimizer="Nelder_Mead"))
summary(m2.0)
#rawdata.ordered

m2.1<-
  glmer(ResponseCorrect ~ PreviousValueScaled + (1 | subid/runid) + (1 | image),
        rawdata.ordered,family = binomial, control=glmerControl())
summary(m2.1)

m2.2<-
  glmer(ResponseCorrect ~ PreviousValueScaled + PreviousCorrect + (1 | subid/runid) + (1 | image),
        rawdata.ordered,family = binomial, control=glmerControl())
summary(m2.2)

m2.2a<-
  lmer(ResponseCorrect ~ PreviousValueScaled + PreviousCorrect + (PreviousValueScaled | subid/runid) +
         (PreviousValueScaled | image),rawdata.ordered)
summary(m2.2a)

m2.3<-
  lmer(ResponseCorrect ~ PreviousValueScaled*PreviousCorrect + (1 | subid/runid) +
         (1 | image),rawdata.ordered)
summary(m2.3)

m2.4<-
  lmer(ResponseCorrect ~ PreviousValueScaled*PreviousCorrect + (1+ PreviousValueScaled*PreviousCorrect | subid/runid) +
         (1 + PreviousValueScaled*PreviousCorrect | image),rawdata.ordered)
summary(m2.4)

anova(m2.1,m2.2,m2.3,m2.4)
