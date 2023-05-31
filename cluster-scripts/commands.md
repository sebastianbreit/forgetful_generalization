# Commands for executing scripts on cluster

## Copying files
Execute from simulations folder
### Data
scp -r data/* sbreit80@134.2.168.52:/home/wu/sbreit80/data




### Code
scp -r models.R sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/models.R
scp -r data_munging.R sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/data_munging.R
scp -r modeling/fitting_functions.R sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/modeling/fitting_functions.R
scp -r modeling/behaviour_fitting.R sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/modeling/behaviour_fitting.R
scp -r modeling/simulation_fitting.R sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/modeling/simulation_fitting.R

scp -r cluster-scripts/fg_robust_worst_surprise_basic_averaging_long.sh sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/cluster-scripts/fg_robust_worst_surprise_basic_averaging_long.sh
 
### Copy Output
rsync -avh -e ssh  sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/output/*  '/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/simulations/output'

rsync -avh -e ssh  sbreit80@134.2.168.52:/mnt/qb/work/wu/sbreit80/logs/301*out  '/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/simulations/output/logs'
rsync -avh -e ssh  sbreit80@134.2.168.52:/mnt/qb/work/wu/sbreit80/logs/302*out  '/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/simulations/output/logs'



### Copy brms Output
rsync -avh -e ssh /source/path/ host:/destination/path

scp -r sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/brmsModels/brm_unscaled_distance_agebin_x_prevz_x_condition_4kiter.brm '/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/simulations/brmsModels'


rsync -avh -e ssh sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/brmsModels/brm_unscaled_ageScenarioDist_excl_SA.brm '/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/simulations/brmsModels/brm_unscaled_ageScenarioDist_excl_SA.brm'

rsync -avh -e ssh sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/brmsModels/brm_unscaled_distance_age_x_prevz_x_condition_4kiter_excl_SA.brm '/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/simulations/brmsModels/brm_unscaled_distance_age_x_prevz_x_condition_4kiter_excl_SA.brm'

rsync -avh -e ssh sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/brmsModels/lmer_unscaled_distance_age_x_prevz_x_condition_4kiter_excl_SA.brm '/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/simulations/brmsModels/lmer_unscaled_distance_age_x_prevz_x_condition_4kiter_excl_SA.brm'



scp -r sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/brmsModels '/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/simulations/brmsModels'

scp -r '/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/simulations/cluster-scripts/brms_scripts' sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/cluster-scripts/brms_scripts

scp -r '/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/simulations/plotting/bonus_brms.R' sbreit80@134.2.168.52:/home/wu/sbreit80/agingexploration/plotting/bonus_brms.R


### Base R
Rscript model-recovery/behaviour_fitting.R 1

### With Singularity
singularity exec --bind '/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/simulations':/home/ cluster-scripts/r_env.sif Rscript '/home/model-recovery/modelComparison.R' '1' 

## Slurm execution
sbatch cluster-scripts/run_modelfitting.sh 1-900
squeue | grep sbreit80

### Singularity execution on cluster
singularity exec --bind /mnt/qb/work/wu/sbreit80/batch_exponential_2pars:/home/data/batch_exponential_2pars cluster-scripts/r_env.sif Rscript /home/wu/sbreit80/modelComparison.R '1' 

# Files and folders necessary
models.R
data_munging.R
model-recovery/*
output/
data/experiment_data_full/*

cluster-scripts/*
tardis.sh