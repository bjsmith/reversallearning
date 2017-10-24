#!/usr/bin/env python3

from nltools.datasets import fetch_pain
import pandas
from nltools.data import Brain_Data
from nltools.file_reader import onsets_to_dm
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
from nilearn.plotting import plot_glass_brain

from pain_regression_allsubjs import *
rlp=RLPain()

rlp.fMRI_dir='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/preprocessed'
rlp.onset_dir='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/runfiles'
rlp.decoder_file='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/pain_decoder.pkl'
rlp.compile_pain_decoder()

#rlp.stats['weight_map']



rlp.stats['weight_map'].shape()
plot_glass_brain(rlp.stats['weight_map'].to_nifti(),alpha=1)

plotBrain(rlp.stats['weight_map'],alpha=0.4)