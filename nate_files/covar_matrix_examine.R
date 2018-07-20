myfit<-fitpackage$fit
save(myfit,file="/expdata/bensmith/joint-modeling/data/msm/reversallearning/testdoubleupdatejointrev3.RData")
load(file="/expdata/bensmith/joint-modeling/data/msm/reversallearning/testdoubleupdatejointrev3.RData")


#fitSummary<-summary(fitpackage$fit)$summary
fitSummary<-summary(myfit)$summary
dim(fitSummary)

rownames(summary(fitpackage$fit)$summary)[1:200]
fitSummary

fitSummary[1:10,]
dim(fitSummary)
thetaDeltaMatrix<-array(NA,dim=c(2318,6))
#so let's extract thetadelta matrix from the summary.
for (rn in 1:2318){
  for (cn in 1:6){#rn<-5;cn=5
    thetaDeltaMatrix[rn,cn]<-fitSummary[paste0("theta_delta[",rn,",",cn,"]"),"mean"]
  }
}
thetaDeltaMatrix[1:50,1:6]
tdcov<-cov(thetaDeltaMatrix)
cov2cor(tdcov)


tdcov.run1<-cov(thetaDeltaMatrix[1:198,])
cov2cor(tdcov.run1)

# theta_delta[run_first_trial_id:run_last_trial_id,THETA_rpe=1]=logit(trial_PE/4+0.5);
# theta_delta[run_first_trial_id:run_last_trial_id,THETA_ev=2]=logit(trial_ev/2+0.5);
library(boot)
((inv.logit(thetaDeltaMatrix[1:100,1])-0.5)*4)
#reward prediction error this makes sense. we would expect that reward prediction error would go to the maximum or minimum values.
hist(((inv.logit(thetaDeltaMatrix[1:2318,1])-0.5)*4),breaks = 100)
hist(thetaDeltaMatrix[1:2318,1],breaks = 100)
#what about EV?
(inv.logit(thetaDeltaMatrix[1:100,2])-0.5)*2
hist(thetaDeltaMatrix[1:2318,2],breaks = 100)
hist(thetaDeltaMatrix[1:2318,2],breaks = 100)
#well, this is ALL going to reach the max values. So probably transforming them into logit space was the wrong move. they might be as normally distributed as they can get just as they are.

#can we find out which N cues these are in?
#maybe not. 
#include_pain | isTRUE(pass_rt) | any(runs!=c(1,2)) | includeSubjGroup  | !model_runs_separately | !variable_run_lengths
recovereddata<-format_dataset_style2(rp = c(1,2),groups_to_fit = g,
                                     include_pain=FALSE,pass_rt=FALSE,includeSubjGroup=FALSE,
                                     model_runs_separately=TRUE,
                                     variable_run_lengths=TRUE,
                                     sample_from_prior=FALSE,
                                     subj_level_params=FALSE,
                                     include_run_ot=TRUE,
                                     subjectLimit = 3)
names(recovereddata)
data_with_EPRPE<-data.table(data.frame("RPE"=((inv.logit(thetaDeltaMatrix[,1])-0.5)*4),
                                       "EV"=(inv.logit(thetaDeltaMatrix[,2])-0.5)*2,
                                       "Cue"=recovereddata$cue,
                                       "trial_runid"=recovereddata$trial_runid,
                                       "outcome"=recovereddata$outcome))
data_with_EPRPE[,CueAppearanceInRun:=1:.N,by=.(Cue,trial_runid)]
data_with_EPRPE[,ReversalTrial:=ifelse(test = CueAppearanceInRun>5 & CueAppearanceInRun <=9,"MAYBE","NO")]

ggplot(data_with_EPRPE,aes(RPE,group=factor(CueAppearanceInRun),fill=factor(CueAppearanceInRun)))+
  geom_histogram(binwidth=.2, position="dodge")+labs(title="Histogram of Reward Prediction Error by trial",fill="Trial")

ggplot(data_with_EPRPE,aes(RPE))+
  geom_histogram(binwidth=.2, position="dodge")+labs(title="Histogram of Reward Prediction Error")


ggplot(data_with_EPRPE,aes(EV,group=factor(CueAppearanceInRun),fill=factor(CueAppearanceInRun)))+
  geom_histogram(binwidth=.2, position="dodge")+labs(title="Histogram of Expected Value by trial",fill="Trial")

ggplot(data_with_EPRPE,aes(EV))+
  geom_histogram(binwidth=.05)+labs(title="Histogram of Expected Value")

thetaDeltaMatrix.dt<-data.table(thetaDeltaMatrix)
thetaDeltaMatrix.dt$N<-1:dim(thetaDeltaMatrix.dt)[1]
tddt<- thetaDeltaMatrix.dt %>% tidyr::gather("Var","Value",V1,V2,V3,V4,V5,V6)

ggplot(tddt,aes(x=N,y=Value,group=Var,color=Var))+geom_line()+coord_cartesian(ylim=c(-50,50),xlim=c(1,198))
plot(thetaDeltaMatrix[,3:4])

rois_cleaned<-rawdata[,
                      c(which(colnames(rawdata)=="ROI_Left.Cerebral.White.Matter"),
                        which(colnames(rawdata)=="ROI_Left.Lateral.Ventricle"):which(colnames(rawdata)=="ROI_Left.VentralDC"),
                        which(colnames(rawdata)=="ROI_Left.choroid.plexus"):which(colnames(rawdata)=="ROI_Right.Cerebral.White.Matter"),
                        which(colnames(rawdata)=="ROI_Right.Lateral.Ventricle"),
                        which(colnames(rawdata)=="ROI_Right.Cerebellum.White.Matter"):which(colnames(rawdata)=="ROI_ctx_rh_S_temporal_transverse")),with=FALSE]

apply(rois_cleaned,2,function(c){sum(is.na(c))})
roi_cov<-cov(rois_cleaned)
which(is.na(roi_cov))
View(roi_cov)

roi_cor<-cor(roi_cov)
stats::heatmap(roi_cor,cexRow=.8,cexCol=0.8)
mean(roi_cor)
hist(roi_cor)

diag(roi_cor)
