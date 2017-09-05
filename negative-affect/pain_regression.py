#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 19 17:53:46 2017

@author: benjaminsmith
"""

#do the regression in the 

#we need a design matrix
#with linear, square, cubic time point regressors
#plus intercept
#plus whatever Design_Matrix files we want to put in.

onsets_convolved.head()
onsets_convolved['linearterm']=range(1,361)
onsets_convolved['quadraticterm']=[pow(x,2) for x in onsets_convolved['linearterm']]
onsets_convolved['cubicterm']=[pow(x,3) for x in onsets_convolved['linearterm']]
onsets_convolved['ones']=[1]*360

c.head()
onsets_convolved.heatmap()

onsets_convolved

#add in the Design Matrix 
msmrl1.X=onsets_convolved#=pd.DataFrame([msmrl1.X,onsets_convolved])
#msmrl1.X=pd.DataFrame(msmrl1.X)
#msmrl1.X
regression=msmrl1.regress()


msm_predicted_pain=regression['t'].similarity(stats['weight_map'],'correlation')

for brainimg in regression['t']:
    plotBrain(brainimg)
    
regression['t'].shape()
plotBrain(regression['t'][1,])
onsets_convolved.head()
plotBrain(regression['t'][1,])
plotBrain(regression['t'][9,])
plotBrain(regression['t'][13,])
#regress out the linear trends
#then dot product with the pain map
#


msm_predicted_pain=regression['beta'].similarity(stats['weight_map'],'dot_product')
plt(msm_predicted_pain)
np.shape(msm_predicted_pain)
#raw data.
msm_predicted_pain=msmrl1.similarity(stats['weight_map'],'dot_product')

onsets_convolved.columns.tolist()

ggplot(
       pd.DataFrame(data={
               'PainByBeta':msm_predicted_pain[0:9],
               'RegressionBetaNum':range(0,9)
               }),
    aes('RegressionBetaNum','PainByBeta')) +\
    geom_line() +\
    stat_smooth(colour='blue', span=0.2)+ \
  scale_x_continuous(breaks=[1,2,3],  \
                     labels=["horrible", "ok", "awesome"])
