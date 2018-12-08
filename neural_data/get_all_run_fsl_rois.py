
#this script gets the FSL-generated reversallearning rois and uses modified code from
#Luke Chang's package to extract the data. It's the same code I used to extract data
#for the freesurfer ROIs.


import pandas
from nltools.file_reader import onsets_to_dm
import os.path
import csv
import pickle

import warnings
from regress_rois import *

from scipy.stats import ttest_1samp, t, norm
import datetime
import time


self_onset_file_version='20180220T031755'

action_timestamp=datetime.datetime.fromtimestamp(time.time()).strftime('%Y%m%dT%H%M%S')


roi_data_version = '20180720'

#server

# roi_data_dir = '/expdata/bensmith/joint-modeling/data/msm/freesurfer/reversallearning/segstats_out/'
# roi_event_data_dir='/expdata/bensmith/joint-modeling/data/msm/freesurfer/reversallearning/roi_event_data'
# self_onset_dir='/expdata/bensmith/joint-modeling/data/msm//reversallearning/runfiles'
# run_id_map_path=

roi_data_dir='/expdata/bensmith/joint-modeling/data/msm/reversallearning/roits/'
roi_event_data_dir='/expdata/bensmith/joint-modeling/data/msm/reversallearning/roi_event_data/fsl_roi_event_data'
run_id_map_path='/expdata/bensmith/joint-modeling/data/msm/freesurfer/functional_preprocessed/run_id_key.csv'
self_onset_dir='/expdata/bensmith/joint-modeling/data/msm/reversallearning/runfiles'
motion_param_dir = "/expdata/xfgavin/MSM"
motion_param_file = 'mc/prefiltered_func_data_mcf.par'

#local
#roi_data_dir='/Users/benjaminsmith/Dropbox/joint-modeling/data/fsl_roi_local/'
#roi_event_data_dir='/Users/benjaminsmith/Dropbox/joint-modeling/data/fsl_roi_event_data'
#run_id_map_path='/Users/benjaminsmith/Dropbox/joint-modeling/data/freesurfer-testing-local/run_id_key.csv'
#self_onset_dir='/Users/benjaminsmith/Dropbox/joint-modeling/data/freesurfer-testing-local/runfiles'
#motion_param_dir = "/expdata/xfgavin/MSM"
#motion_param_file = 'mc/prefiltered_func_data_mcf.par'

# #this is a table containing all the names of all the freesurfer items.
# freesurfer_LUT_table_path="/Users/benjaminsmith/Dropbox/joint-modeling/data/freesurfer-testing-local/FreeSurferColorLUT.txt"


# how to map rois using the two numbering systems here?
# get teh csv file.
#run_id_map=pandas.read_csv('/expdata/bensmith/joint-modeling/data/msm/freesurfer/functional_preprocessed/run_id_key.csv')

#get a list of the folders we'll be looking at.
roi_folders=os.listdir(roi_data_dir+roi_data_version)
# freesurfer_LUT_df=pandas.read_table(freesurfer_LUT_table_path,
#                                     delim_whitespace=True,comment="#",
#                                     names=["RegionId","Label","R","G","B","A"])
run_id_map=pandas.read_csv(run_id_map_path)

for idx, row in run_id_map.iterrows():
    #idx=0
    #row=run_id_map[0,:]

    sid=row['subid']
    #freesurferRunId=row['freesurferRunId']
    runid=row['runid'] #this is just the simple runid which is accessible in the behavioral data.
    if row['motivation'] == 'Reward':
        m="Reward"
        m_short = "Reward"
    elif row['motivation'] == 'Punish':
        m="Punishment"
        m_short = "Punish"
    else: raise Exception("Unrecognized motivation type")

    onset_file=self_onset_dir + '/runfiledetail'+self_onset_file_version+'_s'+str(sid)+'_' + m.lower() + '_r'+str(runid)+'.txt'
    print(onset_file)

    #concatenate all the ROIs for this subject/run from the files
    sub_r_m_roi_dt = pandas.concat([pandas.read_table(roi_data_dir+roi_data_version+'/'+folder +'/s'+str(sid)+"r" + str(runid ) + "_" + m_short + ".txt",
                       delim_whitespace=True, header=None,names=[folder])
                                    for folder in roi_folders],axis=1)

    #sub_r_m_roi_dt=pandas.read_csv(roi_file,delim_whitespace=True, header=None)


    #let's get the sum file as well which gives us ROI names.
    # sub_r_m_roi_sum_dt = pandas.read_table(
    #     roi_data_dir + roi_data_version + '/sub' + str(sid) + '_run' + str(freesurferRunId )+ '_sum.dat',
    #     delim_whitespace=True, header=None, comment='#',
    #     names=["Index","SegId","NVoxels","Volume_mm3","StructName","Mean","StdDev","Min","Max","Range"])

    # great and now add the labels to the summary file.
    # sub_r_m_roi_sum_dt=sub_r_m_roi_sum_dt.merge(freesurfer_LUT_df[["RegionId","Label"]],how='left',left_on='SegId',right_on='RegionId')

    colnames =  sub_r_m_roi_dt.columns
    #onset_file='/Users/benjaminsmith/Dropbox/joint-modeling/data/runfiledetail20171020T012224_s214_reward_r1.txt'
    if (os.path.isfile(onset_file)):
        print('we have a match; ' + onset_file)
        print("importing onsets")
        # import the onset
        onsets = onsets_to_dm(
            onset_file,
            TR=2,
            runLength=sub_r_m_roi_dt.shape[0] #we don't know this for sure. But in this instance it doesn't matter.
            # import the run_length data from the data.
        )

        onsets.sampling_rate = 2

        onsets_convolved = onsets.convolve()

        #removing columns with nothing in them.
        for c in onsets_convolved.columns:
            if sum(onsets_convolved.loc[:, c]) <= 0:
                print('deleting ' + str(c))
                del onsets_convolved[c]

        rowcount = onsets_convolved.__len__()

        if rowcount != 360:
            warnings.warn("Just a friendly FYI: expected number of rows is 360 but this subject had " + str(
                rowcount) + ". Probably this subject got cut off the task half-way through.")

        # high pass filters
        onsets_convolved['linearterm'] = range(1, rowcount + 1)
        onsets_convolved['quadraticterm'] = [pow(x, 2) for x in onsets_convolved['linearterm']]
        onsets_convolved['cubicterm'] = [pow(x, 3) for x in onsets_convolved['linearterm']]
        #mean center
        onsets_convolved['ones'] = [1] * rowcount

        #every row in the onsets files is timepoint; every column is a specific event with the mapping of that
        #column's expected activity
        #In principle, we can treat the data in sub_r_m_roi_dt just as we could a 4D nii.gz fMRI time series
        #it's not exactly like that though because it is a pandas dt and not a nifti object so
        #we might or might not be able to use the same functions. Let's see.

        motion_param_path = motion_param_dir + "/sub" + str(sid) + "/analysis/ReversalLearning_" + m_short + "_run" + str(runid) + "_pre.feat/" + motion_param_file
        #print motion_param_path
        motion_params = pandas.read_table(motion_param_path, names=["Motion" + str(i) for i in range(1, 7)],
                                          delim_whitespace=True)
        onsets_convolved=pandas.concat([onsets_convolved, motion_params],axis=1)

        roi_data = regress_rois(onsets_convolved,sub_r_m_roi_dt)
        #so this has given us a beta value with a row for each event, and a column for each ROI. that's what we want!
        #we could have just multiplied each ROI time course with the convolution...in other words, done the dot product
        #but without the regression equation. However the regression equation buys us a few nice things,
        # including throwing in the linear, quadratic, cubic, and ones terms.
        # it also gives us a beta value, which is a measure of the degree to which each convolution (X) *predicts*
        # activity in each ROI (Y).
        #normally the code would be measuring the degree to which each convolution predicts activity in each
        #voxel (allowing for all the others, which is important and helpful) but we have replaced voxels here with ROIs.
        #I think this is what we want.

        roi_dfs={}

        roi_output_partialpath =roi_event_data_dir + '/' + action_timestamp + 'sub' + str(sid) + '_' + m.lower() + '_r' + str(runid) + '_'


        #give these row and column names.
        for event_by_roi_t in ['beta_vals','t_vals','p_vals']:
            roi_dfs[event_by_roi_t]= pd.DataFrame(data=roi_data[event_by_roi_t],
                                                columns = colnames)
            roi_dfs[event_by_roi_t]['EventName']=pd.Series(onsets_convolved.columns)
            roi_dfs[event_by_roi_t].set_index('EventName',inplace=True)

            roi_dfs[event_by_roi_t] = pd.DataFrame(data=roi_data[event_by_roi_t],
                                                columns=colnames)
            roi_dfs[event_by_roi_t]['EventName'] = pd.Series(onsets_convolved.columns)
            roi_dfs[event_by_roi_t].set_index('EventName', inplace=True)
            roi_dfs[event_by_roi_t].to_csv(roi_output_partialpath + event_by_roi_t + '.csv')

        for roi_v in ['df_vals', 'sigma_vals']:
            roi_dfs[roi_v] = pd.DataFrame(roi_data[roi_v])
            roi_dfs[roi_v].to_csv(roi_output_partialpath + roi_v + '.csv')

        #we ddn't get the column and row names in but I don't care.
        roi_dfs['residual_vals']=pd.DataFrame(data=roi_data['residual_vals'])
        roi_dfs['residual_vals'].to_csv(roi_output_partialpath + 'residual_vals' + '.csv')

        #what's the next thing we'll want to do with these things????
        #I think once we have the betas stored for each event, then we will want to connect the events to the event records in R.
