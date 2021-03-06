#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#from ggplot import *
from nltools.datasets import fetch_pain
import numpy as np
import pandas
from nltools.data import Brain_Data
from nltools.file_reader import onsets_to_dm
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
from nilearn.plotting import plot_glass_brain
from nltools.plotting import plotBrain
import os.path
import csv
import pickle
import warnings
import gc

"""
Created on Sat Aug 19 19:05:48 2017

@author: benjaminsmith
"""

class TimeseriesProcessor:
    

    def __init__(self):
        #learning model
        #self.pain_decoder=''
        #self.fMRI_dir=''
        self.onset_dir=''
        self.regressor_output_filepathprefix=''
        #self.decoder_file = ''
        self.stats=None
        #self.decoder=None
        #self.decoder_origin=''

        #self.nps_map_dir=''
        self.onset_file_version='20170819T170218'

    def process_detailed_regressors(self,subid_range=range(1,500),motivations_all=False):


        if motivations_all is None or motivations_all==False:
            motivations = ['Punishment']
        elif motivations_all == True:
            motivations = ['Punishment', 'Reward']
        else:
            motivations = motivations_all

            #csvfile=None
        header_written=False
        for sid in subid_range:
            print sid
            for rid in [1,2]:
                for m in motivations:
                    nifti_file=self.fMRI_dir + 'sub'+str(sid) + 'ReversalLearning' + m[0:6] + 'run'+str(rid)
                    #print(nifti_file)
                    if os.path.isfile(nifti_file+'.nii.gz'):
                        print(nifti_file)
                        #got an nii.gz, check tosee if there's also a onset file for this.
                        onset_file=self.onset_dir + '/runfiledetail'+self.onset_file_version+'_s'+str(sid)+'_' + m.lower() + '_r'+str(rid)+'.txt'
                        print(onset_file)
                        if (os.path.isfile(onset_file)):
                            print ('we have a match; '+ onset_file)
                            #print("done the regressing :-)")

                            msm_predicted_pain_dict = self.get_trialtype_pain_regressors(nifti_file, onset_file)
                            msm_predicted_pain_dict['subid'] = sid
                            msm_predicted_pain_dict['runid'] = rid
                            with open(self.regressor_output_filepathprefix + str(sid) + '_' + m.lower() + '_r' + str(rid) + '.csv',
                                      'w') as csvfile:
                                w = csv.DictWriter(csvfile, msm_predicted_pain_dict.keys())
                                w.writeheader()
                                w.writerow(msm_predicted_pain_dict)
            gc.collect()

    def process_all_punishment_subjects(self):
        for sid in range(1,500):
            for rid in [1,2]:
                nifti_file=self.fMRI_dir + '/sub'+str(sid) + 'ReversalLearningPunishrun'+str(rid)
                if os.path.isfile(nifti_file+'.nii.gz'):
                    print(self.fMRI_dir)
                    #got an nii.gz, check tosee if there's also a onset file for this.
                    onset_file=self.onset_dir + '/runfilepunishmentcompare'+self.onset_file_version+'_s'+str(sid)+'_punishment_r'+str(rid)+'.txt'
                    if (os.path.isfile(onset_file)):
                        print ('we have a match; '+ onset_file)
                        predicted_pain=self.get_trialtype_pain_regressors(nifti_file+'.nii.gz',onset_file)

                        with open(self.regressor_output_filepathprefix +str(sid)+'_punishment_r'+str(rid)+'.csv', 'w') as csvfile:
                            spamwriter = csv.writer(csvfile, delimiter=',',
                                                    quotechar='|', quoting=csv.QUOTE_MINIMAL)
                            spamwriter.writerow(['subid', 'runid', 'pain_regressor'])
                            for r in predicted_pain:
                                spamwriter.writerow([sid,rid,r])
                        #attach the subject and run ID to the output and concatenate.

    #the standard paine regressor from nlTools
    def get_trialtype_pain_regressors(self,nifti_data,onset_file):
        print("importing nifti")
        #import the nifti
        #load the nltools prepped file if it's available.
        if (os.path.isfile(nifti_data + "nltoolstandard.nii.gz")):
            msmrl1 = Brain_Data(
                nifti_data + "nltoolstandard.nii.gz")
        else:#but if it's not; no worries; just load the original one.
            msmrl1 = Brain_Data(
                nifti_data + ".nii.gz")
            msmrl1.write(nifti_data + "nltoolstandard.nii.gz")

        #I want to standardize globally; this will preserve the relative strengths of each time point
        #and preserve the relative activity at each voxel.
        #and let's use the mean standard deviation across all the images.
        #msmrl1.data = msmrl1.data - np.tile(msmrl1.mean().mean(),msmrl1.data.shape)
        #msmrl1.data = msmrl1.data / np.tile(np.std(msmrl1.data,axis=1).mean(),msmrl1.data.shape)
        # OR we could apply the standardization to the OUTPUT.
        #grand_mean=msmrl1.mean().mean()
        #grand_sd=np.std(msmrl1.data,axis=1).mean()

        #preprocess the nifti?
        print("importing onsets")
        #import the onset
        onsets = onsets_to_dm(
            onset_file,
            TR=2,
            runLength=msmrl1.shape()[0]
        )

        #process the onset files
        #
        onsets.sampling_rate=2

        onsets_convolved=onsets.convolve()

        #delete columns with no information in them.
        for c in onsets_convolved.columns:
            if sum(onsets_convolved.ix[:, c]) <= 0:
                print('deleting '+ str(c))
                del onsets_convolved[c]


        rowcount=onsets_convolved.__len__()
        if rowcount != 360:
            warnings.warn("Just a friendly FYI: expected number of rows is 360 but this subject had " + str(
                rowcount) + ". Probably this subject got cut off the task half-way through.")
        onsets_convolved['linearterm'] = range(1, rowcount + 1)

        onsets_convolved['quadraticterm']=[pow(x,2) for x in onsets_convolved['linearterm']]
        onsets_convolved['cubicterm']=[pow(x,3) for x in onsets_convolved['linearterm']]
        onsets_convolved['ones']=[1]*rowcount
        msmrl1.X=onsets_convolved
        print("convolved onsets; regressing...")
        #regress the file on each of the onsets. So then, when we compare similarity to the regression, we'll be getting the
        #regression to the each event, not to each TR.
        regression=msmrl1.regress()
        print("Regressing; calculating similarity to the pain map from " + self.decoder_origin + "...")
        msm_predicted_pain = regression['beta'].similarity(self.decoder, 'dot_product')
        msm_predicted_pain_scaled=msm_predicted_pain-msmrl1.data.std()
        onset_colnames = onsets_convolved.columns.tolist()
        msm_predicted_pain_dict={}
        for i, b in enumerate(msm_predicted_pain_scaled):
            msm_predicted_pain_dict[onset_colnames[i]] = b
        return msm_predicted_pain_dict



