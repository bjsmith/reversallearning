#for sid in {100..400}; 
for sid in {146..148}; 
do 
	echo $sid;
	for rid in {1..2}; 
	do 
		#echo "./run_sub.sh $sid $rid"
        #run_sub.sh $sid $rid
        python run_for_job_and_rid_server.py $sid $rid
	done
done