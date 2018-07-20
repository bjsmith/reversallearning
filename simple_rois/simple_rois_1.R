#purpose of this doc is to try to run some simple ROI correlations in order to
#attempt to validate the' ROIs responsivity to behavioral data.

library(lme4)
library(ggplot2)
library(data.table)

source("stanlba/lba_rl_joint_setup.R")
require(R.utils)

#now we are simply going to ask whether there is a correlation between accumbens activity and a positive response.

#colnames(rawdata)


t.test(ROI_Left.Accumbens.area~correct,rawdata)
cor.test(rawdata$ROI_Left.Accumbens.area,as.integer(rawdata$correct))

t.test(ROI_Right.Accumbens.area~correct,rawdata)
cor.test(rawdata$ROI_Right.Accumbens.area,as.integer(rawdata$correct))
#rawdata$


t.test(ROI_CC_Posterior~correct,rawdata)
t.test(ROI_Left.Cerebral.Cortex~correct,rawdata)
cor.test(rawdata$ROI_Left.Cerebral.Cortex,as.integer(rawdata$correct))
t.test(ROI_ctx_lh_Pole_occipital~correct,rawdata)
cor.test(rawdata$ROI_ctx_lh_Pole_occipital,as.integer(rawdata$correct))
t.test(ROI_ctx_lh_G_insular_short~correct,rawdata)

fixedef.se<-function(fit){
  return(summary(fit)$coefficients[,"Std. Error"])
}

require(lme4)
lm.beta.lmer <- function(mod) {
  b <- fixef(mod)[-1]
  sd.x <- apply(getME(mod,"X")[,-1],2,sd)
  sd.y <- sd(getME(mod,"y"))
  b*sd.x/sd.y
}

m.0<-lmer(ROI_Left.Accumbens.area~
            presentation_n_in_segment.x + 
            (1+presentation_n_in_segment.x | subid/runmotiveid) + 
            (1 | image),rawdata)
summary(m.0)


m.withResponseCorrect<-lmer(ROI_Left.Accumbens.area~
                              correct + 
                              presentation_n_in_segment.x + 
                              (1+presentation_n_in_segment.x | subid/runmotiveid) + 
                              (1 | image),rawdata)
summary(m.withResponseCorrect)

anova(m.0,m.withResponseCorrect)
# 
# ttest_a_c_list<-
#   aggregate(rawdata[subid %in% 106:115,],
#             list(rawdata[subid %in% 106:115,]$subid,rawdata[subid %in% 106:115,]$runmotiveid),
#             function(srmd){
#   #return(t.test(ROI_Left.Accumbens.area~correct,srmd))
#   return(mean(srmd$ROI_Left.Accumbens.area))
#   #return(data.table(t(data.table(t.test(ROI_Left.Accumbens.area~correct,srmd)))))
# })

accumbensL_list<-list()
accumbensR_list<-list()
cerebral_cortexL_list<-list()

regions<-c(
  #ventral striatum
  "ROI_Left.Caudate","ROI_Right.Caudate",
  "ROI_Left.Putamen","ROI_Right.Putamen",
  "ROI_Left.Accumbens.area","ROI_Right.Accumbens.area",
  #vmPFC/OFC
  "ROI_ctx_lh_S_suborbital","ROI_ctx_rh_S_suborbital",
  "ROI_ctx_lh_G_subcallosal","ROI_ctx_rh_G_subcallosal",
  "ROI_ctx_lh_G_rectus","ROI_ctx_rh_G_rectus",
  "ROI_ctx_lh_G_orbital","ROI_ctx_rh_G_orbital",
  "ROI_ctx_lh_S_orbital.H_Shaped","ROI_ctx_rh_S_orbital.H_Shaped",
  "ROI_ctx_lh_S_orbital_lateral","ROI_ctx_rh_S_orbital_lateral",
  "ROI_ctx_lh_S_orbital_med.olfact","ROI_ctx_rh_S_orbital_med.olfact",
  #ACC
  "ROI_ctx_lh_G_and_S_cingul.Ant","ROI_ctx_rh_G_and_S_cingul.Ant"#,
  #control region.
  #"ROI_Left.Cerebral.Cortex"
)

roi_full_list<-list()
accumbensL_list<-list()
accumbensR_list<-list()
cerebral_cortexL_list<-list()
hippocampusR_list<-list()
suborbitalL_list<-list()
rawdata$ROI_ctx_lh_S_suborbital
for (roi in regions){
  roi_full_list=c(roi=list("hey"),roi_full_list)
}
table(is.na(rawdata$correct))
for(sid in unique(rawdata$subid)){
  print(sid)
  for (rm in unique(rawdata[subid==sid,]$runmotiveid)){
    srmd<-rawdata[subid==sid&runmotiveid==rm]
    # for (roi in regions){
    #   #print(rm)
    #   print(roi)
    #   print(paste0(sid,rm))
    #   res<-data.table(t(data.table(t.test(as.formula(paste0(roi," ~ correct")),srmd))))
    #   res$subid=sid
    #   res$runmotiveid=rm
    #   res$roi=roi
    #   roi_full_list<-c(roi_full_list,list(res))
    # }
    # 
    accumbensL_list<-c(accumbensL_list,list(data.table(t(data.table(t.test(ROI_Left.Accumbens.area~correct,srmd))),"sid"=sid)))
    accumbensR_list<-c(accumbensR_list,list(data.table(t(data.table(t.test(ROI_Right.Accumbens.area~correct,srmd))))))
    hippocampusR_list<-c(hippocampusR_list,list(data.table(t(data.table(t.test(ROI_Right.Hippocampus~correct,srmd))))))
    suborbitalL_list<-c(suborbitalL_list,list(data.table(t(data.table(t.test(ROI_ctx_lh_S_suborbital~correct,srmd))))))
    #cerebral_cortexL_list<-c(cerebral_cortexL_list,list(data.table(t(data.table(t.test(ROI_Left.Cerebral.Cortex~correct,srmd))))))
  }
}

accumbensL.dt<-data.table(do.call(rbind,accumbensL_list))
accumbensR.dt<-data.table(do.call(rbind,accumbensR_list))
hippocampusR.dt<-data.table(do.call(rbind,hippocampusR_list))
suborbitalL.dt<-data.table(do.call(rbind,suborbitalL_list))
roi_all<-data.table(do.call(rbind,roi_full_list))
names(accumbensL.dt)[1:9]<-names(t.test(ROI_Left.Accumbens.area~correct,rawdata))
names(accumbensR.dt)[1:9]<-names(t.test(ROI_Left.Accumbens.area~correct,rawdata))
names(hippocampusR.dt)[1:9]<-names(t.test(ROI_Left.Accumbens.area~correct,rawdata))
names(suborbitalL.dt)[1:9]<-names(t.test(ROI_Left.Accumbens.area~correct,rawdata))
names(roi_full_list)[1:9]<-names(t.test(ROI_Left.Accumbens.area~correct,rawdata))

bysubj<-aggregate(unlist(accumbensL.dt$statistic ) ,by=list(unlist(accumbensL.dt$sid )),mean)
length(bysubj$x)
t.test(bysubj$x)

names(roi_all)[1:9]<-names(t.test(ROI_Left.Accumbens.area~correct,rawdata))
accumbensL.dt
accumbensL.dt
t.test(unlist(accumbensL.dt$statistic))
t.test(unlist(accumbensR.dt$statistic))
t.test(unlist(suborbitalL.dt$statistic))

bysubj.rhippocampus<-aggregate(unlist(hippocampusR.dt$statistic ) ,by=list(unlist(hippocampusR.dt$sid )),mean)
t.test(unlist(hippocampusR.dt$statistic))

hist(unlist(accumbensL.dt$V1))
gglist<-list()
for (roi in regions){#roi<-regions[[1]]
  gglist<-
  ggplot(roi_all[roi==roi,],aes(statistic))+geom_histogram()
}



#good sign! This also works 
hist(unlist(accumbens_correct$statistic))
