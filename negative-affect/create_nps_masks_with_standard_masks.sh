#this script aims to create subject masks from standard masks, using several different collections of masks.
#!/bin/bash
STANDARD_MASK_DIR="/expdata/bensmith/dmn-masks"
#MASK_SUBFOLDERS=("/insula/" "/networks/" "/regions/" "/")
#mkdir -p subject_mask_task_output
#cd subject_mask_task_output
#loop through each mask type
#{1..500}
#loop through each possible subject
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
				echo "$SUBJECT_INPUT exists; creating NPS file. for security reasons this is stored in the NPS dir"
				#create link
				#ln -s $SUBJECT_INPUT /expdata/bensmith/joint-modeling/data/msm/behavioral-analysis/reversallearning/preprocessed_fMRI_symlinks/sub${s}ReversalLearningPunishrun${r}.nii.gz
				#ReversalLearning_Punish_run1_slicetiming_nosmooth_pre.feat
				flirt -applyxfm -init ${SUBJECT_INPUT}/reg/standard2example_func.mat -in /expdata/bensmith/joint-modeling/code/wagertools/NPS_share/weights_NSF_grouppred_cvpcr.img -ref ${SUBJECT_INPUT}/filtered_func_data.nii.gz -out /expdata/bensmith/joint-modeling/code/wagertools/NPS_share/subject_space_masks/weights_NSF_grouppred_cvpcr_sub${s}_${RUN_DIR}
			fi
		done
	done
done
