from pain_regression_allsubjs import *

#uses rlPain to extract actually not a pain signature but a neural mask.
## This should be as effective as using fsl to extract a mask and if the correlations between this and the FSL masks don't work out we whould be worried.

for fsl_harvardoxford_region in ["frontal_medial_cortex","frontal_orbital_cortex", "accumbens_l", "accumbens_r"]:
    rlPain = RLPain()
    rlPain.fMRI_dir='/expdata/bensmith/joint-modeling/data/msm/reversallearning/preprocessed_fMRI_symlinks/'
    rlPain.onset_dir='/expdata/bensmith/joint-modeling/data/msm/reversallearning/runfiles'
    #rlPain.decoder_file='/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/pain_decoder.pkl'
    rlPain.nps_map_filepath = '/expdata/bensmith/joint-modeling/code/wagertools/NPS_share/weights_NSF_grouppred_cvpcr.img'
    rlPain.regressor_output_filepathprefix = '/expdata/bensmith/joint-modeling/data/msm/reversallearning/roi_event_data/fsl_roi_event_data_nltools/20180722_version/'+fsl_harvardoxford_region
    rlPain.onset_file_version='20180220T031755'
    rlPain.data_fmri_space='subjectspace_cropped'

    rlPain.process_detailed_regressors(
        range(100,500),
        custom_pain_map_lambda= lambda sid,rid,m: ('/expdata/bensmith/joint-modeling/data/msm/reversallearning/masks/harvardoxford_rois/sub' +
                          str(sid) + '_ReversalLearning_' + m +
                          '_run' + str(rid) + '_pre.feat_'+fsl_harvardoxford_region+'.nii.gz'),
        custom_data_mask_lambda= lambda sid,rid,m: ('/expdata/bensmith/joint-modeling/data/msm/reversallearning/masks/nltoolswholebrain/' +
                          'sub' + str(sid) + '_ReversalLearning_' + m +
                          '_run' + str(rid) + '_pre.feat.nii.gz'),
        motivations_all=True
        )

    #rlPain.process_all_punishment_subjects()


