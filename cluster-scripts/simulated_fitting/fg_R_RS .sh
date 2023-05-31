#!/bin/bash
#SBATCH --ntasks=1                # Number of tasks (see below)
#SBATCH --cpus-per-task=1        # Number of CPU cores per task
#SBATCH --nodes=1                 # Ensure that all cores are on one machine
#SBATCH --time=3-00:00            # Runtime in D-HH:MM
#SBATCH --partition=cpu-long
#SBATCH --mem=10G 				# If this parameter is omitted, the smallest amount is allocated, usually 100 MB. And chances are good that your job will be killed as it will likely go over this amount.
#SBATCH --output=/mnt/qb/work/wu/sbreit80/logs/%A_%a.out  # File to which STDOUT will be written - make sure this is not on $HOME
#SBATCH --error=/mnt/qb/work/wu/sbreit80/logs/%A_%a.err   # File to which STDERR will be written - make sure this is not on $HOME
#SBATCH --mail-type=ALL
#SBATCH --mail-user=sebastian.breit@student.uni-tuebingen.de
#SBATCH --array=1-30


# print info about current job
scontrol show job $SLURM_JOB_ID 

# Participant
# Fitting Model - 1:recency, 2:surprise+, 3:full, 4:null, 5:all, 6: all except full (long runtimes)
# Condition - 1:Memory, 2:NoMemory, 3:Both
# Cv round from which to re-run
# Number of initializations
singularity exec /home/wu/sbreit80/agingexploration/cluster-scripts/r_env.sif Rscript /home/wu/sbreit80/agingexploration/modeling/simulation_fitting.R ${SLURM_ARRAY_TASK_ID} 1 6

# Run with 
# sbatch /home/wu/sbreit80/agingexploration/cluster-scripts/forgetful_regs.sh 