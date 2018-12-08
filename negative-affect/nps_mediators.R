
library(data.table)

library(lme4)
library(ggplot2)
library(data.table)
require(lme4)
library(dplyr)
#getwd()
source("negative-affect/negative_affect_trials_setup_amendment2.R")

load("pain_stan_output_3.Rdata")

fixedef.se<-function(fit){
  return(summary(fit)$coefficients[,"Std. Error"])
}


lm.beta.lmer <- function(mod) {
  b <- fixef(mod)[-1]
  sd.x <- apply(getME(mod,"X")[,-1],2,sd)
  sd.y <- sd(getME(mod,"y"))
  b*sd.x/sd.y
}



    

    load("pain_stan_output_3_no_motivation.Rdata")
    pander::pander(summary(m.rp.1.nomotivation.stan)[1:3,c(-5,-7)])
    
    load("pain_stan_output_3_no_correctincorrect.Rdata")
    
    summary(m.rp.1.nomotivation.stan)[1:3,c(-5,-7)]
    
    
    load("pain_stan_output_3.Rdata")
    
    summary(m.rp.1.stan)[1:5,c(-5,-7)]
    
    
    #can we only include subjects who got both a reward and a punishment run?
    #average valuescaled by subject
    rawdata.ordered.complete.dt<-data.table(rawdata.ordered.complete)
    data.to.graph<-rawdata.ordered.complete.dt[
      !is.na(ResponseCorrect) & !is.na(Motivation),
      .(ValueScaledBySub=mean(ValueScaled,na.rm=TRUE)),.(subid,ResponseCorrect,Motivation)]
    data.to.graph[,MotivationConditions:=length(unique(.SD[,Motivation])),by=subid]
    data.to.graph<-data.to.graph[MotivationConditions==2]
    data.to.graph[,Response:=sapply(ResponseCorrect,function(x){ifelse(x,"Correct","Wrong")})]
    data.to.graph$Motivation<-factor(data.to.graph$Motivation,levels=c("reward","punishment"))
    
    ggplot(data.to.graph,
           aes(y=ValueScaledBySub,x=interaction(Response,Motivation),color=interaction(Response,Motivation)))+
      scale_colour_manual(values=c("#888888", "#888888","#888888","#ff0000"),guide=FALSE)+
      geom_violin()+geom_jitter(width=0.3,height=0.0,alpha=0.2)+
      stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                   width = 2, linetype = "dashed")+
      scale_x_discrete(labels=c("Reward condition\nCORRECT","Reward condition\nWRONG","Punishment condition\nCORRECT","Punishment condition\nWRONG"),
                       name="Condition\nSUBJECT RESPONSE")+
      scale_y_continuous(name="Neurologic Pain Signature Value\n(Subject Mean)")+
      labs(title="Neurologic Pain Signature Value in Reversal learning\nBy Subject Response and Condition")
    
    
    data.to.graph.2<-tidyr::spread(data.to.graph,Motivation,ValueScaledBySub)
    data.to.graph.2$PunishmentMinusReward<-data.to.graph.2$punishment-data.to.graph.2$reward
    
    ggplot(data.to.graph.2,
           aes(y=PunishmentMinusReward,x=Response,color=Response))+
      scale_colour_manual(values=c("#888888", "#ff0000","#888888","#888888"),guide=FALSE)+
      geom_hline(yintercept = 0,color="#888888",alpha=0.5)+
      geom_violin(alpha=0.5)+geom_jitter(width=0.3,height=0.0,alpha=0.2)+
      stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                   width = 2, linetype = "dashed")+
      # scale_x_discrete(labels=c("WRONG","CORRECT"),
      #                  name="SUBJECT RESPONSE")+
      scale_y_continuous(name="Neurologic Pain Signature Value Punishment Minus Reward\n(Subject Mean)")+
      labs(title="Neurologic Pain Signature Value in Reversal learning\nBy Subject Response and Condition")
    
    
    
    
    data.to.graph.3<-data.to.graph[,ResponseCorrect:=NULL] %>% tidyr::spread(Response,ValueScaledBySub)
    data.to.graph.3$WrongMinusCorrect<-data.to.graph.3$Wrong-data.to.graph.3$Correct
    
    
    ggplot(data.to.graph.3,
           aes(y=WrongMinusCorrect,x=Motivation,color=Motivation))+
      scale_colour_manual(values=c("#888888", "#ff0000","#888888","#888888"),guide=FALSE)+
      geom_hline(yintercept = 0,color="#888888",alpha=0.5)+
      geom_violin(alpha=0.5)+geom_jitter(width=0.3,height=0.0,alpha=0.2)+
      stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                   width = 2, linetype = "dashed")+
      scale_x_discrete(labels=c("Reward","Punishment"))+
      scale_y_continuous(name="Neurologic Pain Signature Value Wrong Minus Correct\n(Subject Mean)")+
      labs(title="Neurologic Pain Signature Wrong-Correct\nDifference in Reversal learning\nBy Subject Response and Condition")
    
    
#What are we doing to look at personality exactly? 

#BISBAS (particularly BIS): 1) BIS
#Big5 (particularly neuroticism, conscientiousness): 2) Neuroticism
#Negative urgency; Perseverence; Sensation Seeking; 
#Attachment styles: anxiety, avoidance


    
#
source("rl_behav_analysis_learning_setup_amendment_2.R")

personality.comparison <- 
  merge(
    data.to.graph.3[Motivation=="punishment"],
  rl.all.subjects.list[,.SD[1],subid] %>% .[,.(subid, BIS,NEUR, URGNEG,PERSEV,SENSEEK,AVOID,ANXIETY)],
  by="subid"
  )

correlations<-cor(personality.comparison[, .(WrongMinusCorrect, )],use="complete.obs")


cor.test(personality.comparison$WrongMinusCorrect, personality.comparison$URGNEG,use="complete.obs")

pers.neg<-c("BIS","NEUR", "URGNEG","PERSEV","SENSEEK","AVOID","ANXIETY")
cor.list<-list()
for (pers in pers.neg){
  corres<-cor.test(personality.comparison$WrongMinusCorrect,personality.comparison[,get(pers)])
  cor.row<-as.data.frame(t(unlist(corres)))
  cor.row$data.name.1<-"WrongMinusCorrect"
  cor.row$data.name.2<-pers
  cor.list[[pers]]<-cor.row
}
cor.dt<-rbindlist(cor.list)
cor.dt$p.value<-as.numeric(as.character(cor.dt$p.value))
cor.dt$p.value.FDRAdjust<-p.adjust(cor.dt$p.value,method="fdr")
View(cor.dt[,.(statistic.t,parameter.df,estimate.cor,conf.int1,conf.int2,data.name.2,p.value,p.value.FDRAdjust)])

ggplot(personality.comparison,aes(x=URGNEG,y=WrongMinusCorrect))+
  geom_point()+geom_smooth(method = "lm")+
  labs(title="Neural pain sensitivity to electric shock\ncorrelates with negative urgency\n(r=0.24, p<0.01)")



personality.behavior.comparison <- 
  merge(
    data.to.graph.3[Motivation=="punishment"],
    rl.all.subjects.list[,.SD[1],subid] %>% .[,.(subid, URGNEG,MethUse,BIS,BAS_RR,BAS_DR,BAS_FUN)],
    by="subid"
  )

not.na.rows<- apply(!is.na(personality.behavior.comparison[,c("BIS","BAS_RR","BAS_DR","BAS_FUN")]),1,all)
BIS.predict<-lm(BIS~BAS_RR+BAS_DR+BAS_FUN,personality.behavior.comparison[not.na.rows,])
personality.behavior.comparison$BIS.controlling.bas[not.na.rows]<-BIS.predict$residuals
cor.test(personality.behavior.comparison$BIS.controlling.bas,
         personality.behavior.comparison$WrongMinusCorrect)

t.test(WrongMinusCorrect~MethUse,personality.behavior.comparison)
t.test(WrongMinusCorrect~MethUse,personality.behavior.comparison)

personality.behavior.comparison.extend<-
  merge(
    data.to.graph.3[Motivation=="punishment"],
    rl.all.subjects.list[,.SD[1],subid] %>% .[,.(subid, URGNEG,URGPOS,MethUse,BIS,BAS_RR,BAS_DR,BAS_FUN)],
    by="subid"
  )

not.na.rows<- apply(!is.na(personality.behavior.comparison.extend[,c("URGNEG","URGPOS")]),1,all)
NEGURG.residual<-lm(URGNEG~URGPOS,personality.behavior.comparison.extend[not.na.rows])
personality.behavior.comparison.extend$NEGURG.residual[not.na.rows]<-NEGURG.residual$residuals
cor.test(personality.behavior.comparison.extend$NEGURG.residual,personality.behavior.comparison.extend$WrongMinusCorrect)


ggplot(personality.behavior.comparison.extend,aes(x=NEGURG.residual,y=WrongMinusCorrect))+
  geom_point()+geom_smooth(method = "lm")+
  labs(title="Neural pain sensitivity to electric shock\ncorrelates with negative urgency*\n(r=0.29, p<0.001)",
       caption="*controlling for positive urgency score")


ggplot(personality.behavior.comparison.extend,aes(x=BIS,y=WrongMinusCorrect))+
  geom_point()+geom_smooth(method = "lm")+
  labs(title="Neural pain sensitivity to electric shock\nmarginally correlates with BIS\n(r=0.17, p=0.052)"
       )

#can we look at reversal trials specifically to examine the pain signature felt by subjects then?
#there may have been anticipatory pain felt on reversal trials...
#or because the RPE is greater on a reversal trial, receiving a shock on a reversal learning trial
#involves even greater negative RPE while not receiving a shock involves less RPE
#so let's plot the WrongMinusCorrect as a function of trial num.

data.to.graph.bytrial<-rawdata.ordered.complete.dt[
  !is.na(ResponseCorrect) & !is.na(Motivation),
  .(ValueScaledBySub=mean(ValueScaled,na.rm=TRUE)),.(subid,ResponseCorrect,Motivation,presentation_n_after_reversal)]
data.to.graph.bytrial[,MotivationConditions:=length(unique(.SD[,Motivation])),by=subid]
data.to.graph.bytrial<-data.to.graph.bytrial[MotivationConditions==2]
data.to.graph.bytrial[,Response:=sapply(ResponseCorrect,function(x){ifelse(x,"Correct","Wrong")})]
data.to.graph.bytrial$Motivation<-factor(data.to.graph.bytrial$Motivation,levels=c("reward","punishment"))

data.to.graph.bytrial.contrast<-data.to.graph.bytrial[,ResponseCorrect:=NULL] %>% tidyr::spread(Response,ValueScaledBySub)
data.to.graph.bytrial.contrast$WrongMinusCorrect<-data.to.graph.bytrial.contrast$Wrong-data.to.graph.bytrial.contrast$Correct


ggplot(data.to.graph.bytrial.contrast,
       aes(y=WrongMinusCorrect,x=Motivation,color=Motivation))+
  scale_colour_manual(values=c("#888888", "#ff0000","#888888","#888888"),guide=FALSE)+
  geom_hline(yintercept = 0,color="#888888",alpha=0.5)+
  geom_violin(alpha=0.5)+geom_jitter(width=0.3,height=0.0,alpha=0.2)+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 2, linetype = "dashed")+
  scale_x_discrete(labels=c("Reward","Punishment"))+
  scale_y_continuous(name="Neurologic Pain Signature Value Wrong Minus Correct\n(Subject Mean)")+
  coord_cartesian(ylim=c(-1,1))+
  labs(title="Neurologic Pain Signature Wrong-Correct Difference in Reversal learning\nBy Subject Response and Condition")+
  facet_wrap(~presentation_n_after_reversal)

ggplot(data.to.graph.bytrial.contrast[Motivation=="punishment"],
       aes(x=presentation_n_after_reversal, y=WrongMinusCorrect,group=presentation_n_after_reversal))+
  scale_colour_manual(values=c("#888888", "#ff0000","#888888","#888888"),guide=FALSE)+
  geom_hline(yintercept = 0,color="#888888",alpha=0.5)+
  geom_violin(alpha=0.5)+geom_jitter(width=0.3,height=0.0,alpha=0.2)+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 2, linetype = "dashed")+
  scale_y_continuous(name="Neurologic Pain Signature Value Wrong Minus Correct\n(Subject Mean)")+
  coord_cartesian(ylim=c(-1,1))+
  labs(title="Neurologic Pain Signature Wrong-Correct Difference in Reversal learning\nBy Subject Response and Condition")#+
  #facet_wrap(~presentation_n_after_reversal)


#Dhruv's suggestions:
#PCA or other dimensitonality reduction on personality measures;
#then polynomial investigation on it.
colnames(rl.all.subjects.list)


personality.vars<-c(
  "ANXIETY",                               
  "AVOID",                                  "BIS",                                    "BAS_DR",                                
  "BAS_FUN",                                "BAS_RR",                                 "URGNEG",                                
  "PREMED",                                 "PERSEV",                                 "SENSEEK",                               
  "URGPOS",                                 "N_WDRAW",                               
  "N_VOLAT",                                "A_COMP",                                
  "A_POLITE",                               "C_INDUST",                              
  "C_ORDER",                                "E_ENTHUS",                              
  "E_ASSERT",                               "O_INTELL",                              
  "O_OPEN"
)

pca_poly_data<-merge(
  rl.all.subjects.list[,.SD[1],subid] %>% .[,c(personality.vars,"subid"),with=FALSE],
  data.to.graph.3[Motivation=="punishment"],by="subid")
not.na.rows<- apply(!is.na(pca_poly_data),1,all)
pca_poly_data<-pca_poly_data[not.na.rows]
#now, can we do PCA on this personality data?
pca_data<-copy(pca_poly_data)
pca_data<-pca_poly_data[,personality.vars,with=FALSE]

#pca
prcompres<-prcomp(pca_data,center=TRUE,scale=TRUE)
plot(prcompres$sdev,type="l")
summary(prcompres)
heatmap(prcompres$rotation)

# library(devtools)
# install.packages("scales")
# install_github("vqv/ggbiplot")

library(ggbiplot)

ggbiplot(prcompres,choices = c(3,4))
pca_poly_data<-cbind(pca_poly_data,prcompres$x[,1:4])

modelres<-lm(WrongMinusCorrect~poly(PC1,PC2,PC3,PC4,degree=2),pca_poly_data)
summary(modelres)
#yeah--we threw in way too much here. I wouldn't really expect this to work.
#start with two-way correlations 
twoways<-cor(cbind("WrongMinusCorrect"=pca_poly_data[,WrongMinusCorrect],
                   poly(as.matrix(pca_poly_data[,.(PC1,PC2,PC3,PC4)]),degree=2)))
View(twoways)
heatmap(twoways)
colnames(twoways)
twoways[1,]

#what about an expansion with degree 3?
polyexp.deg4<-cor(cbind("WrongMinusCorrect"=pca_poly_data[,WrongMinusCorrect],
                   poly(as.matrix(pca_poly_data[,.(PC1,PC2,PC3,PC4)]),degree=4)))
best.expansions<-colnames(polyexp.deg4)[rank(-abs(polyexp.deg4[1,])) %in% 2:8]

modelres<-lm(as.formula(paste0("WrongMinusCorrect~",paste(paste0("`",best.expansions,"`"),collapse = "+"))),as.data.frame(polyexp.deg4))
summary(modelres)
summary(lm(WrongMinusCorrect~URGNEG+URGPOS,pca_poly_data))

#weird. the URGNEG on its own actually predicts WrongMinusCorrect better than any of the polynomial expansions do!

#what if we only ran the PCA and expansion on the several variables I actually thought were important?
not.na.rows<- apply(!is.na(personality.comparison),1,all)
pers.comp.notna<-personality.comparison[not.na.rows]
pca_data<-copy(pers.comp.notna)
pca_data<-pca_poly_data[,pers.neg,with=FALSE]

#But what if I followed this same process on *half* the data only; then tested the model on the other half?

test<-sample(nrow(pers.comp.notna),nrow(pers.comp.notna)/2,replace=FALSE)
train<-(1:nrow(pers.comp.notna))[1:nrow(pers.comp.notna) %in% test]
#right.
#now do PCA on the training data.
pers.comp.notna.train<-pers.comp.notna[train]
pca_data.train<-pers.comp.notna.train[,pers.neg,with=FALSE]
prcompres<-prcomp(pca_data.train,center=TRUE,scale=TRUE)
summary(prcompres)
ggbiplot(prcompres,choices = c(1,2))
pca_poly_data.train<-cbind(pers.comp.notna.train,prcompres$x[,1:4])
poly.res.train<-poly(as.matrix(pca_poly_data.train[,.(PC1,PC2,PC3,PC4)]),degree=2)
polyexp.deg4.train<-as.data.frame(cbind("WrongMinusCorrect"=pca_poly_data.train[,WrongMinusCorrect],
                    poly.res.train))
polyexp.deg4.correlations<-cor(polyexp.deg4.train)
best.expansions<-colnames(polyexp.deg4.correlations)[rank(-abs(polyexp.deg4.correlations[1,])) %in% 2:8]
expansionlist<-best.expansions
repeat{
  modelres.step<-lm(as.formula(paste0("WrongMinusCorrect~",paste(paste0("`",expansionlist,"`"),collapse = "+"))),as.data.frame(polyexp.deg4.train))
  max.coefficient<-which(summary(modelres.step)$coefficients[,"Pr(>|t|)"]==max(summary(modelres.step)$coefficients[,"Pr(>|t|)"]))
  if(summary(modelres.step)$coefficients[max.coefficient,"Pr(>|t|)"]>0.05){
    expansionlist<-expansionlist[-(max.coefficient-1)]
  }else{
    break
  }
}
summary(modelres.step)

train.predict<-predict(modelres.step,polyexp.deg4.train)
#get an overall r-squared of this...
cor(polyexp.deg4.train$WrongMinusCorrect, train.predict)^2


#now, test.
pers.comp.notna.test<-pers.comp.notna[test]
#we have to re-fit the PCA to the new test group
#test with training....
#try to recover PC1...
pca_data.test<-pers.comp.notna.test[,pers.neg,with=FALSE]

#check my code is correct...
#plot((scale(pca_data.train)%*%prcompres$rotation[,1:2])[,1],pca_poly_data$PC1)
#plot((scale(pca_data.train)%*%prcompres$rotation[,1:2])[,2],pca_poly_data$PC2)

#so that's how it's done; matrix multiplication of the rotation
#now rotate the test data using the principal components created with the training data...
prcompres.test.x<-scale(pca_data.test)%*%prcompres$rotation[,1:4] #rotated.
pca_poly_data.test<-cbind(pers.comp.notna.test,prcompres.test.x) #combined with the test data.
#now apply polynomial expansion on the test data PCs
poly.test.res<-poly(as.matrix(pca_poly_data.test[,.(PC1,PC2,PC3,PC4)]),degree=4)
polyexp.deg4.test<-as.data.frame(cbind("WrongMinusCorrect"=pca_poly_data.test[,WrongMinusCorrect],
                    poly.test.res))
#now we pick the linear model associated with this....
test.predict<-predict(modelres3,poly.test.res)
cor(polyexp.deg4.test$WrongMinusCorrect, test.predict)^2

#what about polynomial expansion on the raw data (without PCA?)

poly.rawdata.train<-poly(as.matrix(pca_data.train),degree=2)
polyexp.deg4.correlations<-cor(poly.rawdata.train)
best.expansions<-colnames(polyexp.deg4.correlations)[rank(-abs(polyexp.deg4.correlations[1,])) %in% 2:8]
expansionlist<-best.expansions
polyexp.raw.deg4.train<-as.data.frame(cbind("WrongMinusCorrect"=
                                              pca_poly_data.train[,WrongMinusCorrect],
                                            poly.rawdata.train))
repeat{
  modelres.step<-lm(as.formula(paste0("WrongMinusCorrect~",paste(paste0("`",expansionlist,"`"),collapse = "+"))),as.data.frame(polyexp.raw.deg4.train))
  max.coefficient<-which(summary(modelres.step)$coefficients[,"Pr(>|t|)"]==max(summary(modelres.step)$coefficients[,"Pr(>|t|)"]))
  if(summary(modelres.step)$coefficients[max.coefficient,"Pr(>|t|)"]>0.05){
    expansionlist<-expansionlist[-(max.coefficient-1)]
  }else{
    break
  }
}
summary(modelres.step)
#what? this doens't make sense. negative urge was 0.29....

summary(lm(WrongMinusCorrect~URGNEG,pca_poly_data.train))
summary(lm(WrongMinusCorrect~URGNEG,pca_poly_data))
#mmmm, not enough data in the subset.

#Polynomial expansion *followed* by PCA?

neural.selfreport.data.raw<-merge(
  rl.all.subjects.list[,.SD[1],subid] %>% .[,c(pers.neg,"subid"),with=FALSE],
  data.to.graph.3[Motivation=="punishment"],by="subid")
not.na.rows<- apply(!is.na(neural.selfreport.data.raw),1,all)
neural.selfreport.data <-neural.selfreport.data.raw[not.na.rows]
test<-sample(nrow(neural.selfreport.data),nrow(neural.selfreport.data)/2,replace=FALSE)
train<-(1:nrow(neural.selfreport.data))[1:nrow(neural.selfreport.data) %in% test]

#OK, polynomial expansion of the predictors and the predictor.
nsf.matrix<-as.matrix(neural.selfreport.data[,c("WrongMinusCorrect",pers.neg),with=FALSE])
poly.exp<-poly(nsf.matrix,degree=2)
#now we do PCA....
persneg.7.prcomp<-prcomp(poly.exp)
ggbiplot(persneg.7.prcomp,choices = c(1,2))
ggbiplot(persneg.7.prcomp,choices = c(3,4))

nsf.pc.df<-data.table(cbind(nsf.matrix,poly.exp,persneg.7.prcomp$x))
#alright--looks like we have 2 main PCs again...
summary(lm(WrongMinusCorrect~PC1+PC2,nsf.pc.df))
summary(lm(WrongMinusCorrect~PC1,nsf.pc.df))

#what if we did the same process but with all of the personality variables?
neural.selfreportall.data.raw<-merge(
  rl.all.subjects.list[,.SD[1],subid] %>% .[,c(personality.vars,"subid"),with=FALSE],
  data.to.graph.3[Motivation=="punishment"],by="subid")
not.na.rows<- apply(!is.na(neural.selfreportall.data.raw),1,all)
neural.selfreportall.data <-neural.selfreportall.data.raw[not.na.rows]
test<-sample(nrow(neural.selfreportall.data),nrow(neural.selfreportall.data)/2,replace=FALSE)
train<-(1:nrow(neural.selfreportall.data))[1:nrow(neural.selfreportall.data) %in% test]

#OK, polynomial expansion of the predictors and the predictor.
nsf.matrix<-as.matrix(neural.selfreportall.data[,c(personality.vars),with=FALSE])
poly.exp<-poly(nsf.matrix,degree=2)
#now we do PCA....
persneg.all.prcomp<-prcomp(poly.exp)
ggbiplot(persneg.7.prcomp,choices = c(1,2))
ggbiplot(persneg.7.prcomp,choices = c(3,4))

nsf.pc.df<-data.table(cbind(nsf.matrix,poly.exp,persneg.7.prcomp$x))
#alright--looks like we have 2 main PCs again...
summary(lm(WrongMinusCorrect~PC1+PC2,nsf.pc.df))
summary(lm(WrongMinusCorrect~PC1,nsf.pc.df))
summary(lm(WrongMinusCorrect~NEGURG,nsf.pc.df))
