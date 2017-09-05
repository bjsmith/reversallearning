module load singularity
cd /admin/class/rc/rcomp03/
./run_container
cd /mnt/reversallearning/negative-affect/
PYTHONPATH="/mnt/reversallearning/negative-affect"; export PYTHONPATH
python run_for_job_and_rid.py $1 $2
