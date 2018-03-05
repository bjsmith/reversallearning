from pain_regression_allsubjs import *
rlPain=RLPain()

rlPain.fMRI_dir='/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/preprocessed/'
rlPain.onset_dir='/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/runfiles'
#rlPain.decoder_file='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/pain_decoder.pkl'
rlPain.nps_map_filepath = '/Users/benjaminsmith/Dropbox/joint-modeling/wager-pain-dataset/NPS_share/subject_space_masks/weights_NSF_grouppred_cvpcr_sub105_ReversalLearning_Punish_run1_pre.feat.nii.gz'
rlPain.regressor_output_filepathprefix = '/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/testing/'
rlPain.get_wager_nps_map()
rlPain.onset_file_version='20180220T031755'

#sid=113
#rid=2
#nifti_file = rlPain.fMRI_dir + '/sub' + str(sid) + 'ReversalLearningPunishrun' + str(rid)
#onset_file = rlPain.onset_dir + '/runfiledetail20170820T001729_s' + str(sid) + '_punishment_r' + str(
#    rid) + '.txt'
#print(rlPain.get_trialtype_pain_regressors(nifti_file,onset_file))


rlPain.process_detailed_regressors(
    range(105,114),
    lambda sid,rid,m: ('/Users/benjaminsmith/Dropbox/joint-modeling/wager-pain-dataset/NPS_share/subject_space_masks/' +
                      'weights_NSF_grouppred_cvpcr_sub' + str(sid) + '_ReversalLearning_' + m +
                      '_run' + str(rid) + '_pre.feat.nii.gz'),
    motivations_all=['Reward']
    )

#rlPain.process_all_punishment_subjects()


