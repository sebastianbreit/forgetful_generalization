#!/bin/bash
#SBATCH --job-name agingexploration
#SBATCH --time 72:0:0
#SBATCH --partition long 
#SBATCH --mem 500mb
#SBATCH --cpus-per-task 1
#SBATCH --workdir .
#SBATCH --array=1-346
#SBATCH -o /home/mpib/cwu/agingexploration/logs/%A_%a.out
#SBATCH -e /home/mpib/cwu/agingexploration/logs/%A_%a.err   


module load R

# clusterid currently is participant
#clusterid <- as.integer(args[1])

#1:recency, 2:surprise+, 3:full, 4:null, 5:all, 6: all except full (long runtimes)
#fitting_model_id <- as.integer(args[2])

#1:Memory, 2:NoMemory, 3:Both
#experiment_model_id <- as.integer(args[3])


# Whether or not asymmetry is only positive (exponentiated) or not
#exp_asym <- as.integer(args[4])

# Whether to use overall_prior_mean for surprise or observed_prior
#surprise_model <- as.integer(args[5])

Rscript --vanilla modeling/behaviour_fitting.R $SLURM_ARRAY_TASK_ID 3 3 1 3

## MPIB cluster
