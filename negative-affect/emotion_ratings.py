#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 17 16:51:53 2017

@author: benjaminsmith
"""

from ggplot import *
from nltools.datasets import fetch_pain
import numpy as np
import pandas
from nltools.data import Brain_Data
from nltools.file_reader import onsets_to_dm
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd

#could do it here or could grab a set of raw (per-study) images from neurosynth
#and repeatedly 
pdata = fetch_pain()

pdata = Brain_Data(pdata)

print(len(pdata))

#data_nifti=pdata.to_nifti()

#some interesting things about how to plot brains with this package
from nilearn.plotting import plot_glass_brain

plot_glass_brain(pdata.mean().to_nifti())

pdata.mean().plot()


from nltools.plotting import plotBrain
plotBrain(pdata.X)

data_subjs=pdata.X["SubjectID"].unique()
len(data_subjs)
train_subjs=pandas.DataFrame(data_subjs).sample(frac=0.9,random_state=2057)
test_subjs= [s for s in data_subjs if s not in train_subjs[0]]

pdata.Y=pdata.X["PainLevel"]

data_train = pdata[train_subjs[0]]
data_test = pdata[test_subjs]



#train!
stats = data_train.predict(algorithm='ridge',plot=False)





#test
predicted_pain=data_test.similarity(stats['weight_map'],'dot_product') + stats['intercept']
dat = pd.DataFrame(data={'SubjectID':data_test.X['SubjectID'],'Predicted':predicted_pain,'PainLevel':data_test.X['PainLevel']})

with sns.plotting_context(context='paper',font_scale=2):
    sns.factorplot(data=dat,x='PainLevel',y='Predicted')
#need to regress on this value PainLevel to build our regression of Pain


#we have number of subjects and we ought to build a model where 
#we correlate pain within-subject, then average across subjects
#or possibly a linear model in which we regress out each subject's mean then calculate across subjects
#or we could run a bunch of within-subject correlations of pain-level with voxel activity
#then train on the 84/3=18 images.
#ideally a linear repeated-measures model or something
#but Luke 

#sub113
sub113_onset_file='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/runfiles/runfilepunishmentcompare20170819T170218_s113_punishment_r1.txt'
msmrl1=Brain_Data('/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/preprocessed/sub113/ReversalLearningPunishrun1.nii.gz')
type(msmrl1)
msmrl1.shape()


datamean=np.mean(msmrl1.data,axis=1)

with sns.plotting_context(context='paper',font_scale=2):
    sns.factorplot(data=pd.DataFrame(data=
                    {'ImageMean':datamean,
                     'Timepoint':range(1,361)})
    ,x='Timepoint',y='ImageMean')
    

msmrl1_demeaned=msmrl1-datamean
datamean.shape()
msmrl1.shape()
data_demeaned_mean=np.mean(msmrl1.data,axis=1)
with sns.plotting_context(context='paper',font_scale=2):
    sns.factorplot(data=pd.DataFrame(data=
                    {'ImageMean':data_demeaned_mean,
                     'Timepoint':range(1,361)})
    ,x='Timepoint',y='ImageMean')
datamean
meanres.X
msmrl1.X["TimePoint"]=range(1,len(msmrl1)+1)
msmrl1.X=pd.DataFrame(data={'TimePoint':range(1,len(msmrl1)+1)})

msm_predicted_pain=msmrl1.similarity(stats['weight_map'],'dot_product') + stats['intercept']
msm_dat = pd.DataFrame(data=
                    {'PredictedPain':msm_predicted_pain,
                     'Timepoint':msmrl1.X['TimePoint']})


#this is not ideal at all but...
msmrl1_detrended=msmrl1.detrend()


data_detrended_mean=np.mean(msmrl1_detrended.data,axis=1)
with sns.plotting_context(context='paper',font_scale=2):
    sns.factorplot(data=pd.DataFrame(data=
                    {'ImageMean':data_detrended_mean,
                     'Timepoint':range(1,361)})
    ,x='Timepoint',y='ImageMean')
    
with sns.plotting_context(context='paper',font_scale=2):
    sns.factorplot(data=msm_dat,x='Timepoint',y='PredictedPain')
    

msm_predicted_pain=msmrl1_detrended.similarity(stats['weight_map'],'correlation')# + stats['intercept']
msm_dat = pd.DataFrame(data=
                    {'PredictedPain':msm_predicted_pain,
                     'Timepoint':msmrl1_detrended.X['TimePoint']})
with sns.plotting_context(context='paper',font_scale=2):
    sns.factorplot(data=msm_dat
    ,x='Timepoint',y='PredictedPain')
    

#file should have headers: Stim,Onset,Duration
#Onset and Duration should both be set in seconds.
onsets=onsets_to_dm(
        sub113_onset_file,
             TR=2,
             runLength=msmrl1.shape()[0]
)
onsets.sampling_rate=2
#I believe there is a bug in the code here which means we have to set Sampling Rate twice.
onsets_convolved=onsets.convolve()

list(onsets_convolved)

#graph the pain activity against the pain regressor
msm_dat = pd.DataFrame(data=
                    {'PredictedPain':msm_predicted_pain,
                     'Timepoint':msmrl1_detrended.X['TimePoint']})
with sns.plotting_context(context='paper',font_scale=2):
    sns.factorplot(data=msm_dat
    ,x='Timepoint',y='PredictedPain')
    
    
msmrl1_detrended
    
np.corrcoef(onsets_convolved["AllS_error_Fdbk_alli_c0"],
            msm_predicted_pain
            )

np.corrcoef(onsets_convolved["AllS_correct_Fdbk_alli_c0"],
            msm_predicted_pain
            )

with sns.plotting_context(context='paper',font_scale=2):
    sns.factorplot(data=pd.DataFrame(data=
                    {'PredictedPain':msm_predicted_pain,
                     'Timepoint':msmrl1_detrended.X['TimePoint'],
                     'AllS_error_Fdbk_alli_c0':onsets_convolved["AllS_correct_Fdbk_alli_c0"]
                     }),x='Timepoint',y=['PredictedPain','AllS_error_Fdbk_alli_c0'])