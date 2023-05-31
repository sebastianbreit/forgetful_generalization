rm(list=ls()) #house keeping

#load packages
packages <- c('plyr', 'jsonlite', 'DEoptim', "matrixcalc", "fields") # 
lapply(packages, require, character.only = TRUE)

#Source dependencies
homePath= paste0(getwd(),"/")
modelpath <-paste(homePath,'modeling/fitting_functions.R',sep="")
source(modelpath)

# Input arguments for the script
args = commandArgs(TRUE)
# clusterid currently is participant
clusterid <- as.integer(args[1])
#1:recency, 2:surprise+, 3:full, 4:null, 5:all, 6: all except full (long runtimes)
fitting_model_id <- as.integer(args[2])
#1:Memory, 2:NoMemory, 3:Both
experiment_model_id <- as.integer(args[3])


# Whether or not asymmetry is only positive (exponentiated) or not
exp_asym <- as.integer(args[4])
if(exp_asym==1){
  EXPONENTIAL_ASYMMETRY=TRUE
}else{
  EXPONENTIAL_ASYMMETRY=FALSE
}

# Whether to use overall_prior_mean for surprise or observed_prior
surprise_model <- as.integer(args[5])


#Cv round from which to re-run
missing_rounds_from <- as.integer(args[6])
if(is.na(missing_rounds_from)){
  missing_rounds_from<-1
}
# Number of initializations
initialization <- as.integer(args[7])
if(is.na(initialization)){
  initialization<-1
}

# If different amount of iterations is given, use that one
max_iter <- as.integer(args[8])
if(is.na(max_iter)){
  MAX_ITER_FITTING=100 # 100 is our default, 200 is general default
}else{
  MAX_ITER_FITTING=max_iter
}


homePath= paste0(getwd(),"/")
input_path <- paste0(homePath,"data/experiment_data_full/experiment_data_full.csv")
output_path <- paste0(homePath,"output/")


############## DEBUG PARAMS ##################
##############################################
ROUNDS_CV=7 #Full fitting is 7, our default is 7
FIT_NORMAL_GP=TRUE #CONST to determine whether GP params should be fit too
DEFAULT_ERR_VAR=0.0001

# GRIDSIZE=10 #0-10 (11x11)
GRIDSIZE=11 #0-7 (8x8)
NUM_CLICKS=25


NUM_PARTICIPANTS <- 346
# #
# clusterid <- 1
# fitting_model_id <- 4
# experiment_model_id <- 1
# exp_asym<-1
# EXPONENTIAL_ASYMMETRY=TRUE
# surprise_model <- 3
# MAX_ITER_FITTING=1
# missing_rounds_from<-1
# initialization<-1

print(paste0('Averaging surprise: ',surprise_model,' | Positive asymmetry: ',EXPONENTIAL_ASYMMETRY ))

print(paste0('Cv-rounds from: ',missing_rounds_from,', No initializations: ',initialization,', No fitting iterations: ', MAX_ITER_FITTING))

##############################################################################################################
#Compile Experimental Data
##############################################################################################################

d_raw <- load_exp_data(includeLast = FALSE)

##############################################################################################################
#Cluster configuration: (1 subject x model combination) per CPU
##############################################################################################################

#create list of all fitting models
experimentModelList <- c("Memory","NoMemory")
fittingModelList<-c("recency","surprise+","full","null") # "surprise+" for asymmetric surprise
modelCombs <- expand.grid(fittingModelList, experimentModelList) #fittingModelList

num_fitting_models <- length(fittingModelList)
num_experiment_models <- length(experimentModelList)


fitting_routine_single_config <- function(subjectId,modelCombIndex){
  
  print(c("Fitting model, Input model: ",modelCombs[modelCombIndex,]))
  print(c("Participant:",subjectId))
  
  fittingModel <- modelCombs[modelCombIndex,1]
  experimentModel <- modelCombs[modelCombIndex,2]
  
  data <- d_raw %>% 
    filter(scenario==ifelse(experimentModel=="Memory",'Memory Condition','Baseline Condition')) %>% 
    mutate(id=participant)
  
  for(p in unique(data$UID)){
    fitting_round=1
    sub <- data %>% filter(UID==p)
    for(r in unique(sub$experiment_round)){
      # Distinguish between experiment round (actual round) and fitting round (round in cv routine)
      data[data$UID==p & data$experiment_round==r,] <- data %>% filter(UID==p & experiment_round==r) %>% mutate(round=fitting_round)
      fitting_round=fitting_round+1
    }
  }
  
  ##############################################################################################################
  #OPTIMIZATION ROUTINE
  ##############################################################################################################
  
  # Extend output with folder and name of input script
  ifelse(!dir.exists(output_path), dir.create(output_path), "Folder exists already, all ok.")
  exp_output_path <- paste0(output_path,"experiment_data_",experimentModel,"/")
  ifelse(!dir.exists(exp_output_path), dir.create(exp_output_path), "Folder exists already, all ok.")
  
  start.time <- Sys.time()
  set.seed(subjectId*initialization)
  roundList <- 1:ROUNDS_CV #Remove training round and bonus round
  #cross-validation routine
  modelDir <- paste0(exp_output_path,"fittedmodel_",fittingModel)
  modelDir <- ifelse(EXPONENTIAL_ASYMMETRY,paste0(modelDir,"_exp"),paste0(modelDir,"_basic"))
  modelDir <- ifelse(surprise_model,paste0(modelDir,"_movavg"),modelDir)
  modelDir <-paste0(modelDir,'/')
  print(modelDir)
  ifelse(!dir.exists(modelDir), dir.create(modelDir), "Folder exists already, all ok.")
  
  base_name <- paste0(modelDir,"part_",subjectId)
  output <- c()
  
  for (r in roundList){ #loop through rounds in roundList
    if(r>=missing_rounds_from){
      print(r)
      cv <- cvfun(data=data,subject=subjectId, kernelFun=rbf, acquisition = ucb, leaveoutindex=r,model_version=fittingModel,fit_normalGP = FIT_NORMAL_GP,DEFAULT_ERR_VAR=DEFAULT_ERR_VAR,NUM_CLICKS=NUM_CLICKS,exponential_asymmetry=EXPONENTIAL_ASYMMETRY,surprise_model=surprise_model)
      output <- rbind(output, cv)
      final_name=paste0(base_name,'_init_',initialization,".csv")
      write.csv(output,final_name)  
    }
  }
  write.csv(output,final_name)
    
  end.time <- Sys.time()
  elapsed <- end.time - start.time
  print(elapsed)
}





########################################################################################################################
######################################## Run fitting routine based on config ###########################################
########################################################################################################################
# Case 1: fit all models, all conditions
# Case 2: fit all models, 1 condition
# Case 3: fit 1 model, both conditions
# Case 4: fit 1 model, one condition
subjectId <- clusterid

# profvis({
if(fitting_model_id < 5 && experiment_model_id < 3){
  scenarioIndex <- experiment_model_id
  fm <- fitting_model_id
  modelCombIndex <- ifelse(scenarioIndex==1, fm,num_fitting_models+fm)
  
  fitting_routine_single_config(subjectId = subjectId, modelCombIndex = modelCombIndex)
  
} else if(fitting_model_id == 5 && experiment_model_id == 3){
  for(fm in 1:(fitting_model_id-1)){
    for (scenarioIndex in 1:(experiment_model_id-1)) {
      modelCombIndex <- ifelse(scenarioIndex==1, fm,num_fitting_models+fm)
      
      fitting_routine_single_config(subjectId = subjectId, modelCombIndex = modelCombIndex)
    }
  }
  
} else if(fitting_model_id == 5 && experiment_model_id < 3){
  for(fm in 1:(fitting_model_id-1)){
    scenarioIndex <- experiment_model_id
    modelCombIndex <- ifelse(scenarioIndex==1, fm,num_fitting_models+fm)
      
    fitting_routine_single_config(subjectId = subjectId, modelCombIndex = modelCombIndex)
    
  }
} else if(fitting_model_id < 5 && experiment_model_id == 3){
  for(scenarioIndex in 1:(experiment_model_id-1)){
    fm <- fitting_model_id
    modelCombIndex <- ifelse(scenarioIndex==1, fm,num_fitting_models+fm)
    
    fitting_routine_single_config(subjectId = subjectId, modelCombIndex = modelCombIndex)
    
  }
} else if(fitting_model_id == 6 && experiment_model_id < 3){
  for(fm in c(1,2,4)){
    scenarioIndex <- experiment_model_id
    modelCombIndex <- ifelse(scenarioIndex==1, fm,num_fitting_models+fm)
    
    fitting_routine_single_config(subjectId = subjectId, modelCombIndex = modelCombIndex)
    
  }
} else if(fitting_model_id == 6 && experiment_model_id == 3){
  for(fm in c(1,2,4)){
    for (scenarioIndex in 1:(experiment_model_id-1)) {
      modelCombIndex <- ifelse(scenarioIndex==1, fm,num_fitting_models+fm)
      
      fitting_routine_single_config(subjectId = subjectId, modelCombIndex = modelCombIndex)
    }
  }
}
# })

