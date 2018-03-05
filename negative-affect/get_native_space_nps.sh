#thanks to Vita Droutman, modified this from her script.

for SUBJECT in /expdata/bensmith/data/subjects/sub*; do 

	 subid=`echo $SUBJECT|sed -e "s#/expdata/xfgavin/MSM/sub##g"`

	# convert ROI to native space
	flirt -applyxfm -init /expdata/vita/CUPS/model/sid${subid}CUPS.feat/reg/standard2example_func.mat -in /expdata/vita/CUPS/GroupByRisknew.gfeat/cope2.feat/cluster_mask_zstat13.nii.gz -ref /expdata/vita/CUPS/model/sid${subid}CUPS.feat/filtered_func_data.nii.gz -out /expdata/vita/CUPS/model/sid${subid}CUPS.feat/PPI_cope2zstat13 
  
	#Binarizing ROI
	fslmaths /expdata/vita/CUPS/model/sid${subid}CUPS.feat/PPI_cope2zstat13  -bin -thr 0.5  /expdata/vita/CUPS/model/sid${subid}CUPS.feat/PPI_cope2zstat13_bin 
    
	# Extract the time course for ROI (this will be 2d regressor EV2, PHYS)
	fslmeants -i /expdata/vita/CUPS/model/sid${subid}CUPS.feat/filtered_func_data.nii.gz -m /expdata/vita/CUPS/model/sid${subid}CUPS.feat/PPI_cope2zstat13_bin  -o /expdata/vita/CUPS/model/sid${subid}CUPS.feat/PPI_cope2zstat13_timecourse.txt 

done;

