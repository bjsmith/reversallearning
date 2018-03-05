import os
import pandas as pd
import subprocess
from nltools.data import Brain_Data
#this file creates a 'test' pain dataset artificially.
#we get real data and mix the pain regressor into it. This will require the following steps:

#Get nii.gz data
#get behavioral data
#convolve
#mix behavioral data into appropriate points and save.

#then the idea is that I should use the EXISTING functions to read the dataset.

#Get nii.gz data
fMRI_dir='/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/preprocessed'
nps_map_filepath = '/Users/benjaminsmith/Dropbox/joint-modeling/wager-pain-dataset/NPS_share/weights_NSF_grouppred_cvpcr.img'
#get behavioral data
#onset_dir='/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/runfiles'
behav_data_path='/Users/benjaminsmith/Dropbox/joint-modeling/reversal-learning/behavioral-analysis/data/all_subjs_datacomplete_reward_and_punishment.txt'
behav_data= pd.read_csv(behav_data_path,header=0,sep=" ")

nps_brain_data=Brain_Data(nps_map_filepath)
filtered_func_data=Brain_Data(fMRI_dir + '/sub105ReversalLearningPunishrun1.nii.gz')
from nilearn.plotting import plot_glass_brain
plot_glass_brain(nps_brain_data.to_nifti())
plot_glass_brain(filtered_func_data.mean().to_nifti())
def insert_pain_image(timepoints,nifti_file):

    # split up the nifti image into a whole set of images,

    # round these to the nearest even number to get to TR=2
    punishment_tr_list=round(timepoints/2,0)*2
    current_tr=0
    #get the length of the nifti file.
    fslnvols_query = subprocess.run(['fslnvols', nifti_file], stdout=subprocess.PIPE)
    vol_length = int(fslnvols_query.stdout.strip())
    # go through the whole series to produce a list of splices with start times, end times, and whether or not they're punishment.
    current_splice_start_tr=0
    current_splice_is_punishment=any(punishment_tr_list == 0)
    splice_list = []
    for i in range(1,vol_length):
        tr=i*2
        #this algorithm could be more efficient but it'll do the job fine.
        current_tr_is_punishment=any(punishment_tr_list==tr)
        if current_splice_is_punishment != current_tr_is_punishment:
            #we have a change between the current splice and the current TR.
            #save the last one.
            splice_to_save_start_tr=current_splice_start_tr
            splice_to_save_end_tr=tr-2
            current_splice_record={'start_tr':splice_to_save_start_tr,'end_tr':splice_to_save_end_tr,'punishment':current_splice_is_punishment}
            splice_list.append(current_splice_record)
            #now increment the current splice
            current_splice_start_tr=tr
            current_splice_is_punishment=current_tr_is_punishment

    splice_df = pd.DataFrame(splice_list)

    subprocess.call(['fslsplit', nifti_file + '.nii.gz', nifti_file, '-t'])
    #great, now we can iterate through that list of dicts.
    for i, splice in enumerate(splice_list):
        #
        print("extracting splice " + str(i) + ' of '+str(len(splice_list)))
        #split - create one file for EVERY TR.

        if splice['punishment']==True:
            # loop through each of those TRs in THIS splice
            for img_3d_i in range(splice['start_tr'],splice['end_tr']+2,2):
                # add the punishment image to the splice
                img_3d_filepath = nifti_file + format(img_3d_i,'04.0f') + '.nii.gz'
                subprocess.call(['fslmaths', img_3d_filepath, '-add', nps_map_filepath,img_3d_filepath])

    subprocess.call(['fslmerge',
                     nifti_file + 'groundtruth.nii.gz'] + [nifti_file + format(img_3d_i,'04.0f') + '.nii.gz' for i in range(0,vol_length)],
                     )
    #now delete the files we created.
    for i in range(0,vol_length):
        subprocess.call(['rm',nifti_file + format(img_3d_i,'04.0f') + '.nii.gz'])
    #make sure out output has the length our input did.
    assert(vol_length==subprocess.run(['fslnvols', nifti_file + 'groundtruth.nii.gz'], stdout=subprocess.PIPE).stdout.strip())







    # while current_tr<(len(punishment_tr_list)):
    #     consecutive_punishment_trs=0
    #     start_splice=current_tr
    #     while (punishment_tr_list[current_tr+consecutive_punishment_trs]==consecutive_punishment_trs*2 and
    #                    (current_tr + consecutive_punishment_trs) < len(punishment_tr_list)):
    #         consecutive_punishment_trs = consecutive_punishment_trs+1
    #         #provisionally set end_splice; this will be replaced if we go onward.
    #         end_splice=current_tr+consecutive_punishment_trs


    # add the punisher image to the punishment items
    # resplace them all together.




#convolve



#mix behavioral data into appropriate points and save.


subid_range = range(105,114)
for sid in subid_range:
    for rid in [1, 2]:
        #get the nifti dataset
        #nifti_file = fMRI_dir + '/sub' + str(sid) + 'ReversalLearningPunishrun' + str(rid)
        nifti_file = fMRI_dir + '/filtered_func_data'

        #get the behavioral data we'll use to generate our data
        behav_data_s_r = behav_data[(behav_data['runid'] == rid) & (behav_data['subid'] == sid) & (behav_data.Motivation=="punishment")]
        if os.path.isfile(nifti_file + '.nii.gz') and behav_data_s_r.shape[0]>0:
            print(fMRI_dir)

            # or do we use another method, other than the runfile? I think we should use another method.
            # right, now
            punishment_timepoints_unconvolved = behav_data_s_r.loc[behav_data_s_r.correct==False,'onset_time_actual']
            #a very basic pseudoconvolution; because we're just testing, this approximate test should suffice.

            # to do this we will
            # get a list of the punishment periods
            punishment_timepoints_convolved = pd.concat([(punishment_timepoints_unconvolved + 4),
                                                         (punishment_timepoints_unconvolved + 6)],
                                                        ignore_index=True).sort_values()

            insert_pain_image(punishment_timepoints_convolved,nifti_file)



