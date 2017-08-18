#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 17 16:51:53 2017

@author: benjaminsmith
"""


from nltools.datasets import fetch_pain
import numpy
import pandas
from nltools.data import Brain_Data

#could do it here or could grab a set of raw (per-study) images from neurosynth
#and repeatedly 
pdata = fetch_pain()

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



import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd

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


msmrl1=Brain_Data('/Users/benjaminsmith/Documents/MIND/data/reversallearning/ReversalLearningPunishrun1.nii.gz')

msmrl1.X["TimePoint"]=range(1,len(msmrl1)+1)
msmrl1.X=pd.DataFrame(data={'TimePoint':range(1,len(msmrl1)+1)})

msm_predicted_pain=msmrl1.similarity(stats['weight_map'],'dot_product') + stats['intercept']
msm_dat = pd.DataFrame(data=
                    {'PredictedPain':msm_predicted_pain,
                     'Timepoint':msmrl1.X['TimePoint']})

with sns.plotting_context(context='paper',font_scale=2):
    sns.factorplot(data=msm_dat,x='Timepoint',y='PredictedPain')