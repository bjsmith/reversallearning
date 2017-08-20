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

onset_file = rlp.onset_dir + '/runfiledetail20170820T001729_s' + str(sid) + '_punishment_r' + str(
    rid) + '.txt'
predicted_pain_detailed = rlp.get_trialtype_pain_regressors(nifti_file, onset_file)


#get_trialtype_pain_regressors:::

if (os.path.isfile(nifti_data + "nltoolstandard.nii.gz")):
    msmrl1 = Brain_Data(
        nifti_file + "nltoolstandard.nii.gz")
else:
    msmrl1 = Brain_Data(
        nifti_data + ".nii.gz")
    msmrl1.write(nifti_data + "nltoolstandard.nii.gz")
# preprocess the nifti?
print("importing onsets")
# import the onset
onsets = onsets_to_dm(
    onset_file,
    TR=2,
    runLength=360
)

# process the onset files
#
onsets.sampling_rate = 2
onsets_convolved = onsets.convolve()

onsets_convolved['linearterm'] = range(1, 361)
onsets_convolved['quadraticterm'] = [pow(x, 2) for x in onsets_convolved['linearterm']]
onsets_convolved['cubicterm'] = [pow(x, 3) for x in onsets_convolved['linearterm']]
onsets_convolved['ones'] = [1] * 360
msmrl1.X = onsets_convolved
print("convolved onsets; regressing...")
# regress
regression = msmrl1.regress()
print("Regressing; calculating similarity...")
msm_predicted_pain = regression['beta'].similarity(rlp.stats['weight_map'], 'dot_product')
onset_colnames=onsets_convolved.columns.tolist()
msm_predicted_pain_dict={}#
for i,b in enumerate(msm_predicted_pain):
    msm_predicted_pain_dict[onset_colnames[i]]=b
msm_predicted_pain_dict['subid']=sid
msm_predicted_pain_dict['runid']=rid





    spamwriter = csv.writer(csvfile, delimiter=',',
                            quotechar='|', quoting=csv.QUOTE_MINIMAL)
    spamwriter.writerow(['subid', 'runid', 'pain_regressor'])
    spamwriter.writerow([sid, rid, msm_predicted_pain_dict])