#!/bin/bash
#this script aims to create wholebrain subject masks from standard masks
#based on the binary masks from nltools.
#STANDARD_MASK_DIR="/expdata/bensmith/dmn-masks"
#MASK_SUBFOLDERS=("/insula/" "/networks/" "/regions/" "/")
#mkdir -p subject_mask_task_output
#cd subject_mask_task_output
#loop through each mask type
#{1..500}
#loop through each possible subject
mask_to_use=/home/bensmith/.conda/envs/msm_behav_revlearn_pyenv/lib/python2.7/site-packages/nltools/resources/MNI152_T1_2mm_brain_mask.nii.gz
for s in {100..500}
do
	#and each possible run
	for r in 1 2
	do
		for m in 'Punish' 'Reward'
		do
			#if we have pre-processed data for this subject*run
	#		/expdata/xfgavin/MSM/sub113/data
			#/expdata/xfgavin/MSM/sub105/analysis/ReversalLearning_Punish_run1_slicetiming_pre.feat
			RUN_DIR="ReversalLearning_${m}_run${r}_pre.feat"
			SUBJECT_INPUT="/expdata/xfgavin/MSM/sub$s/analysis/${RUN_DIR}"
			if [ -e "$SUBJECT_INPUT" ];
			then
				echo "$SUBJECT_INPUT exists; creating whole-brain mask."
				#create link
				#ln -s $SUBJECT_INPUT /expdata/bensmith/joint-modeling/data/msm/behavioral-analysis/reversallearning/preprocessed_fMRI_symlinks/sub${s}ReversalLearningPunishrun${r}.nii.gz
				#ReversalLearning_Punish_run1_slicetiming_nosmooth_pre.feat
				#flirt -applyxfm -init /expdata/xfgavin/MSM/sub106/analysis/ReversalLearning_Punish_run1_pre.feat/reg/standard2example_func.mat
				# -in /opt/fmritools/fsl/data/standard/MNI152_T1_2mm_brain_mask.nii.gz -ref /expdata/xfgavin/MSM/sub106/analysis/ReversalLearning_Punish_run1_pre.feat/filtered_func_data.nii.gz -out /expdata/bensmith/joint-modeling/data/msm/reversallearning/masks/ReversalLearning_Punish_run1_pre_right_wholebrain_testmask.nii.gz
				
				output_path=/expdata/bensmith/joint-modeling/data/msm/reversallearning/masks/nltoolswholebrain/sub${s}_${RUN_DIR}
				flirt -applyxfm -init ${SUBJECT_INPUT}/reg/standard2example_func.mat -in ${mask_to_use} -ref ${SUBJECT_INPUT}/filtered_func_data.nii.gz -out ${output_path}_unthresh
				#using a relatively liberal threshold.
				fslmaths ${output_path}_unthresh -thr 0.2 -bin ${output_path}
				rm ${output_path}_unthresh.nii.gz
			fi
		done
	done
done
