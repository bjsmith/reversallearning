from pain_regression_allsubjs import *

rlPain=RLPain()

rlPain.fMRI_dir='/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/preprocessed'
rlPain.onset_dir='/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/runfiles'
rlPain.decoder_file='/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/pain_decoder.pkl'
rlPain.compile_pain_decoder()
rlPain.onset_file_version='20180220T031755'
#sid=113
#rid=2
#nifti_file = rlPain.fMRI_dir + '/sub' + str(sid) + 'ReversalLearningPunishrun' + str(rid)
#onset_file = rlPain.onset_dir + '/runfiledetail20170820T001729_s' + str(sid) + '_punishment_r' + str(
#    rid) + '.txt'
#print(rlPain.get_trialtype_pain_regressors(nifti_file,onset_file))

rlPain.process_detailed_regressors(range(0,199))
#rlPain.process_detailed_regressors()
