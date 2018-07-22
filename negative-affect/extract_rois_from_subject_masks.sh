#!/bin/bash
#this script aims to EXTRACT TIME COURSES for specific pre-created masks from the dataset
#followup to create_subject_dmn_masks_with_standard_masks.sh
#should have already created the masks.
#STANDARD_MASK_DIR="/expdata/bensmith/dmn-masks"
#MASK_SUBFOLDERS=("/insula/" "/networks/" "/regions/" "/")
#just the regions themselves for now
#MASK_SUBFOLDERS=("/")
MASKS=('accumbens_l' 'accumbens_r' 'putamen_l' 'putamen_r' 'frontal_orbital_cortex' 'frontal_medial_cortex')
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
			    echo "$SUBJECT_INPUT exists; checking for mask..."
			    for mask in "${MASKS[@]}"
			    do
			      echo "From subject $s run $r $m extracting ROI for $mask"
     		   	MASKFOLDER=/expdata/bensmith/joint-modeling/data/msm/reversallearning/roits/20180720/roi_$mask/
		   	    #make the directory to store if it doesn't exist.
		   	    mkdir -p $MASKFOLDER/
			      # mask_name=`basename ${mask_path%%.*}`
			      mask_path=/expdata/bensmith/joint-modeling/data/msm/reversallearning/masks/harvardoxford_rois/sub${s}_ReversalLearning_${m}_run${r}_pre.feat_${mask}.nii.gz
            #flirt -applyxfm -init ${SUBJECT_INPUT}/reg/standard2example_func.mat -in $mask_path -ref ${SUBJECT_INPUT}/filtered_func_data.nii.gz -out /expdata/bensmith/joint-modeling/data/msm/reversallearning/masks/harvardoxford_rois/sub${s}_${RUN_DIR}_${mask_name}
            fslmeants -i ${SUBJECT_INPUT}/filtered_func_data.nii.gz -m ${mask_path} -o $MASKFOLDER/s${s}r${r}_${m}.txt
          done
			fi
		done
	done
done
