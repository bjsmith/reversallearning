print("started to access python")
import sys

from pain_regression_allsubjs import *

rlp=RLPain()

rlp.fMRI_dir='/mnt/fMRI_data'
rlp.onset_dir='/mnt/runfiles'
rlp.decoder_file='/mnt/fMRI_data/pain_decoder.pkl'
rlp.compile_pain_decoder("../../fMRI_data/")

sid=sys.argv[1]
rid=sys.argv[2]


nifti_file = rlp.fMRI_dir + '/sub' + str(sid) + 'ReversalLearningPunishrun' + str(rid)
if os.path.isfile(nifti_file + '.nii.gz'):
    print(rlp.fMRI_dir)
    # got an nii.gz, check tosee if there's also a onset file for this.
    onset_file = rlp.onset_dir + '/runfiledetail20170820T012610_s' + str(sid) + '_punishment_r' + str(
        rid) + '.txt'
    if os.path.isfile(onset_file):
        print('we have a match!')
        msm_predicted_pain_dict = rlp.get_trialtype_pain_regressors(nifti_file, onset_file)
        msm_predicted_pain_dict['subid'] = sid
        msm_predicted_pain_dict['runid'] = rid
        with open(rlp.regressor_output_dir + str(sid) + '_punishment_r' + str(rid) + '.csv',
                  'w') as csvfile:
            w = csv.DictWriter(csvfile, msm_predicted_pain_dict.keys())
            w.writeheader()
            w.writerow(msm_predicted_pain_dict)
