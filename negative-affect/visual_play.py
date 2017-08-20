from pain_regression_allsubjs import *

rlp=RLPain()

rlp.fMRI_dir='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/preprocessed'
rlp.onset_dir='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/runfiles'
rlp.decoder_file='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/pain_decoder.pkl'
rlp.compile_pain_decoder()

sid=113
rid=1

nifti_file = rlp.fMRI_dir + '/sub' + str(sid) + 'ReversalLearningPunishrun' + str(rid)

#briefer items
#onset_file = rlp.onset_dir + '/runfilepunishmentcompare20170819T170218_s' + str(sid) + '_punishment_r' + str(
#    rid) + '.txt'
#predicted_pain = rlp.get_trialtype_pain_regressors(nifti_file, onset_file)

#detailed

onset_file = rlp.onset_dir + '/runfiledetail20170819T232956_s' + str(sid) + '_punishment_r' + str(
    rid) + '.txt'
predicted_pain_detailed = rlp.get_trialtype_pain_regressors(nifti_file, onset_file)