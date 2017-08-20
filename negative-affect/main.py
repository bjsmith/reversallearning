from pain_regression_allsubjs import *

rlPain=RLPain()

rlPain.fMRI_dir='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/preprocessed'
rlPain.onset_dir='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/runfiles'
rlPain.decoder_file='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/pain_decoder.pkl'
rlPain.compile_pain_decoder()
rlPain.get_trialtype_pain_regressors(113,1)
#rlPain.process_detailed_regressors()
