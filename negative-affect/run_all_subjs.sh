for sid in {100..400}; 
do 
	#echo $sid;
	for rid in {1..2}; 
	do 
		#echo $rid;
		echo "./run_sub.sh $sid $rid"
		#echo "./run_sub.sh $sid $rid" | qsub
	done
done