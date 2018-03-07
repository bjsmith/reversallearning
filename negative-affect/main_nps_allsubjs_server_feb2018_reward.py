from pain_regression_allsubjs import *
rlPain=RLPain()


rlPain.fMRI_dir='/expdata/bensmith/joint-modeling/data/msm/behavioral-analysis/reversallearning/preprocessed_fMRI_symlinks/'
rlPain.onset_dir='gcrunfiles'
#rlPain.decoder_file='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/pain_decoder.pkl'
rlPain.nps_map_filepath = '/expdata/bensmith/joint-modeling/code/wagertools/NPS_share/weights_NSF_grouppred_cvpcr.img'
rlPain.regressor_output_filepathprefix = '/expdata/bensmith/joint-modeling/data/msm/behavioral-analysis/reversallearning/rlPainNPS/'
rlPain.get_wager_nps_map()
rlPain.onset_file_version='20180220T031755'

#sid=113
#rid=2
#nifti_file = rlPain.fMRI_dir + '/sub' + str(sid) + 'ReversalLearningPunishrun' + str(rid)
#onset_file = rlPain.onset_dir + '/runfiledetail20170820T001729_s' + str(sid) + '_punishment_r' + str(
#    rid) + '.txt'
#print(rlPain.get_trialtype_pain_regressors(nifti_file,onset_file))


rlPain.process_detailed_regressors(
    range(100,400),
    lambda sid,rid,m: ('/expdata/bensmith/joint-modeling/code/wagertools/NPS_share/subject_space_masks/' +
                      'weights_NSF_grouppred_cvpcr_sub' + str(sid) + '_ReversalLearning_' + m +
                      '_run' + str(rid) + '_pre.feat.nii.gz'),
    motivations_all=['Reward']
    )

#rlPain.process_all_punishment_subjects()


