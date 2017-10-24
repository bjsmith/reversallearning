#this script aims to create subject masks from standard masks, using several different collections of masks.
#!/bin/bash
STANDARD_MASK_DIR="/expdata/bensmith/dmn-masks"
MASK_SUBFOLDERS=("/insula/" "/networks/" "/regions/" "/")
mkdir -p subject_mask_task_output
cd subject_mask_task_output
#loop through each mask type
#{1..500}
#loop through each possible subject
for s in {100..500}
do
	#and each possible run
	for r in 1 2
	do
		#if we have pre-processed data for this subject*run
#		/expdata/xfgavin/MSM/sub113/data
		SUBJECT_INPUT="/expdata/xfgavin/MSM/sub${s}/data/ReversalLearningPunishrun${r}.nii.gz"
		if [ -e "$SUBJECT_INPUT" ];
		then
			echo "$SUBJECT_INPUT exists; creating symbolic link..."
			#create link
			ln -s $SUBJECT_INPUT /expdata/bensmith/joint-modeling/data/msm/behavioral-analysis/reversallearning/preprocessed_fMRI_symlinks/sub${s}ReversalLearningPunishrun${r}.nii.gz
		fi
	done
done
