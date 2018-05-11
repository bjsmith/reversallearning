library(rstanarm)
library(parallel)
#should use this in future if you want stan code.
#https://cran.r-project.org/web/packages/brms/index.html

m.rp.0.stan<-stan_glmer(ValueScaled~
               (ResponseCorrect==FALSE) + 
               presentation_n_in_segment + 
               (1+presentation_n_in_segment | subid/runmotiveid) + 
               (1 | image),rawdata.ordered.complete,cores=detectCores())
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
