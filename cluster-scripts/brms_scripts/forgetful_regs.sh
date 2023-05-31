#!/bin/bash
#SBATCH --ntasks=1                # Number of tasks (see below)
#SBATCH --cpus-per-task=4        # Number of CPU cores per task
#SBATCH --nodes=1                 # Ensure that all cores are on one machine
#SBATCH --time=3-00:00            # Runtime in D-HH:MM
#SBATCH --partition=cpu-preemptable
#SBATCH --mem=10G 				# If this parameter is omitted, the smallest amount is allocated, usually 100 MB. And chances are good that your job will be killed as it will likely go over this amount.
#SBATCH --output=/mnt/qb/work/wu/sbreit80/logs/%A_%a.out  # File to which STDOUT will be written - make sure this is not on $HOME
#SBATCH --error=/mnt/qb/work/wu/sbreit80/logs/%A_%a.err   # File to which STDERR will be written - make sure this is not on $HOME
#SBATCH --mail-type=ALL
#SBATCH --mail-user=sebastian.breit@student.uni-tuebingen.de
#SBATCH --array=6

# print info about current job
scontrol show job $SLURM_JOB_ID 


singularity exec /home/wu/sbreit80/agingexploration/cluster-scripts/r_brms.sif Rscript /home/wu/sbreit80/agingexploration/plotting/brms_plots.R ${SLURM_ARRAY_TASK_ID} 

# Run with 
# sbatch /home/wu/sbreit80/agingexploration/cluster-scripts/forgetful_regs.sh 