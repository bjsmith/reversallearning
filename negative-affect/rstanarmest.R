source("negative-affect/negative_affect_trials_setup.R")
library(rstanarm)
library(parallel)
#should use this in future if you want stan code.
#https://cran.r-project.org/web/packages/brms/index.html


library(lme4)
m.rp<-lmer(ValueScaled~
             (ResponseCorrect==FALSE)*(Motivation=="punishment") + 
             presentation_n_in_segment + 
             (1+presentation_n_in_segment | subid/runmotiveid) + 
             (1 | image),rawdata.ordered.complete,
)


m.rp.2<-lmer(ValueScaled~
               (ResponseCorrect==FALSE)*(Motivation=="punishment") + 
               presentation_n_in_segment + 
               (1+presentation_n_in_segment+(ResponseCorrect==FALSE) | subid/runmotiveid) + 
               (1 | image),rawdata.ordered.complete)

m.rp.contrast<-lmer(ValueScaled~
                      (ResponseCorrect==FALSE)*(Motivation=="punishment") + 
                      presentation_n_in_segment + 
                      (1+ | runmotiveid) + 
                      (1+presentation_n_in_segment+(ResponseCorrect==FALSE):(Motivation=="punishment")  | subid) + 
                      (1 | image),rawdata.ordered.complete)

m.rp.3<-lmer(ValueScaled~
               (ResponseCorrect==FALSE)*(Motivation=="punishment") + 
               presentation_n_in_segment + 
               (1 | runmotiveid) + 
               (1+presentation_n_in_segment+(ResponseCorrect==FALSE) | subid) + 
               (1 | image),rawdata.ordered.complete)
