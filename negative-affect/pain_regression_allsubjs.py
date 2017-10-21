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

"""
Created on Sat Aug 19 19:05:48 2017

@author: benjaminsmith
"""

class RLPain:
    

    def __init__(self):
        #learning model
        self.pain_decoder=''
        self.fMRI_dir=''
        self.onset_dir=''
        self.regressor_output_filepathprefix=''
        self.decoder_file = ''
        self.stats=None
        self.decoder=None
        self.decoder_origin=''

        self.nps_map_dir=''
        self.onset_file_version='20170819T170218'

    def get_wager_nps_map(self):
        if(os.path.isfile(self.nps_map_filepath)):
            nps = Brain_Data(self.nps_map_filepath)
            self.decoder=nps
            self.decoder_origin='nps'
        else:
            raise Exception("error; cannot find NPS map")


    def compile_pain_decoder(self,pain_dir=None):
        print("compiling pain dir")

        if os.path.isfile(self.decoder_file):
            print("pain data pre-saved, loading...")
            stats = pickle.load(open(self.decoder_file, "rb"))

            # pdata = Brain_Data(self.decoder_file)
        else:
            print("pain data doesn't exist, getting...")
            if (pain_dir is None):
                pdata = fetch_pain()
            else:
                pdata = fetch_pain(pain_dir)
            pdata.Y = pdata.X["PainLevel"]
            stats = pdata.predict(algorithm='ridge', plot=False)
            with open(self.decoder_file, "wb") as f:
                pickle.dump(stats, f, pickle.HIGHEST_PROTOCOL)
        self.decoder_origin='chang_pain_data'

        print ("pain data loaded.")

        #train_subjs=pandas.DataFrame(data_subjs).sample(frac=0.9,random_state=2057)
        #test_subjs= [s for s in data_subjs if s not in train_subjs[0]]

        #data_train = pdata[train_subjs[0]]
        #data_test = pdata[test_subjs]
        #stats = data_train.predict(algorithm='ridge',plot=False)

        self.stats = stats
        self.decoder = stats['weight_map']

    def process_detailed_regressors(self):
        #csvfile=None
        header_written=False
        for sid in range(1,500):
            for rid in [1,2]:
                nifti_file=self.fMRI_dir + '/sub'+str(sid) + 'ReversalLearningPunishrun'+str(rid)
                if os.path.isfile(nifti_file+'.nii.gz'):
                    print(self.fMRI_dir)
                    #got an nii.gz, check tosee if there's also a onset file for this.
                    onset_file=self.onset_dir + '/runfiledetail'+self.onset_file_version+'_s'+str(sid)+'_punishment_r'+str(rid)+'.txt'
                    if (os.path.isfile(onset_file)):
                        print ('we have a match!')
                        #print("done the regressing :-)")

                        msm_predicted_pain_dict = self.get_trialtype_pain_regressors(nifti_file, onset_file)
                        msm_predicted_pain_dict['subid'] = sid
                        msm_predicted_pain_dict['runid'] = rid
                        with open(self.regressor_output_filepathprefix + str(sid) + '_punishment_r' + str(rid) + '.csv',
                                  'w') as csvfile:
                            w = csv.DictWriter(csvfile, msm_predicted_pain_dict.keys())
                            w.writeheader()
                            w.writerow(msm_predicted_pain_dict)

                            #for key, value in msm_predicted_pain_dict.items():
                            #    w.writerow([key, value])

             #               if (header_written==False):
             #                   w.writeheader()
             #                   header_written=True

                        #attach the subject and run ID to the output and concatenate.
        #if(csvfile is not None):
        #    csvfile.close(self)

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

        onsets_convolved['linearterm']=range(1,361)
        onsets_convolved['quadraticterm']=[pow(x,2) for x in onsets_convolved['linearterm']]
        onsets_convolved['cubicterm']=[pow(x,3) for x in onsets_convolved['linearterm']]
        onsets_convolved['ones']=[1]*360
        msmrl1.X=onsets_convolved
        print("convolved onsets; regressing...")
        #regress the file on each of the onsets. So then, when we compare similarity to the regression, we'll be getting the
        #regression to the each event, not to each TR.
        regression=msmrl1.regress()
        print("Regressing; calculating similarity to the pain map from " + self.decoder_origin + "...")
        msm_predicted_pain = regression['beta'].similarity(self.decoder, 'dot_product')
        onset_colnames = onsets_convolved.columns.tolist()
        msm_predicted_pain_dict={}
        for i, b in enumerate(msm_predicted_pain):
            msm_predicted_pain_dict[onset_colnames[i]] = b
        return msm_predicted_pain_dict



