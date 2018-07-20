from pain_regression_allsubjs import *
rlPain=RLPain()
#july2018: updated this to use a custom mask for each subject data instead of the *misaligned* standard space mask.

#don't see how there's a syntax error here.
rlPain.fMRI_dir='/expdata/bensmith/joint-modeling/data/msm/behavioral-analysis/reversallearning/preprocessed_fMRI_symlinks'
rlPain.onset_dir='/expdata/bensmith/joint-modeling/data/msm/behavioral-analysis/reversallearning/runfiles'
#rlPain.decoder_file='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/pain_decoder.pkl'
rlPain.nps_map_filepath = '/expdata/bensmith/joint-modeling/code/wagertools/NPS_share/weights_NSF_grouppred_cvpcr.img'
rlPain.regressor_output_filepathprefix = '/expdata/bensmith/joint-modeling/data/msm/behavioral-analysis/reversallearning/rlPainNPS/20180720_version/'
rlPain.get_wager_nps_map()
rlPain.onset_file_version='20180220T031755'
rlPain.data_fmri_space='subjectspace_cropped'
#sid=113
#rid=2
#nifti_file = rlPain.fMRI_dir + '/sub' + str(sid) + 'ReversalLearningPunishrun' + str(rid)
#onset_file = rlPain.onset_dir + '/runfiledetail20170820T001729_s' + str(sid) + '_punishment_r' + str(
#    rid) + '.txt'
#print(rlPain.get_trialtype_pain_regressors(nifti_file,onset_file))


rlPain.process_detailed_regressors(
    range(100,500),
    lambda sid,rid,m: ('/expdata/bensmith/joint-modeling/code/wagertools/NPS_share/subject_space_masks/' +
                      'weights_NSF_grouppred_cvpcr_sub' + str(sid) + '_ReversalLearning_' + m +
                      '_run' + str(rid) + '_pre.feat.nii.gz'),
    custom_data_mask_lambda= lambda sid,rid,m: ('/expdata/bensmith/joint-modeling/data/msm/reversallearning/masks/wholebrain/' +
                      'sub' + str(sid) + '_ReversalLearning_' + m +
                      '_run' + str(rid) + '_pre.feat.nii.gz')
    )

#rlPain.process_all_punishment_subjects()


