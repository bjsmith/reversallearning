#for sid in {100..400}; 
for sid in {146..148}; 
do 
	echo $sid;
	for rid in {1..2}; 
	do 
		#echo "./run_sub.sh $sid $rid"
        echo "/dartfs-hpc/scratch/mind_hackathon/reversallearning-project/reversallearning/negative-affect/run_sub.sh $sid $rid" | qsub
	done
done