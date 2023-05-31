rm(list=ls()) #house keeping

#load packages
packages <- c('plyr', 'jsonlite', 'DEoptim', "matrixcalc", "fields")
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
input_model_id <- as.integer(args[3])

missing_rounds_from <- as.integer(args[4])
if(is.na(missing_rounds_from)){
  missing_rounds_from<-1
}

subjectId<- clusterid
homePath= paste0(getwd(),"/")
input_path <- paste0(homePath,"data/data_simulated_11x11/EXPERIMENT_VALUES/")
# input_path <- paste0(homePath,"data/experiment_data_11x11/experiment_data_pilot.R")
output_path <- paste0(homePath,"output/")

############## DEBUG PARAMS ##################
##############################################
MAX_ITER_FITTING=100 # 100 is our default, 200 is general default
ROUNDS_CV=7 #Full fitting is 7
FIT_NORMAL_GP=TRUE #CONST to determine whether GP params should be fit too
DEFAULT_ERR_VAR=0.0001

EXPONENTIAL_ASYMMETRY=TRUE
surprise_model <- 3

# GRIDSIZE=10 #0-10 (11x11)
GRIDSIZE=11 #0-7 (8x8)
NUM_CLICKS=25


nParticipants <- 100
##############################################################################################################
#Cluster configuration: (1 subject x model combination) per CPU
##############################################################################################################

#create list of all fitting models
inputModelList <- c("recency","surprise+","full","null")
fittingModelList<-c("recency","surprise+","full","null") # "surprise+" for asymmetric surprise
modelCombs <- expand.grid(fittingModelList, inputModelList) #fittingModelList

num_fitting_models <- length(fittingModelList)
num_experiment_models <- length(inputModelList)

fitting_routine_single_config <- function(subjectId,modelCombIndex){
  
  print(c("Fitting model, Input model: ",modelCombs[modelCombIndex,]))
  print(c("Participant:",subjectId))
  
  fittingModel <- modelCombs[modelCombIndex,1]
  inputModel <- modelCombs[modelCombIndex,2]
  
  # Seed is subjectID, Same participant should have same random draws across models
  set.seed(subjectId)
  
  ##############################################################################################################
  #Compile Experimental Data
  ##############################################################################################################
  
  # import preprocessed data
  data = read.csv(paste0(input_path,"simulatedData_",inputModel,".csv"))
  #Normalize data to [-.5 , .5]
  data$z <- (data$z -50)/100
  
  uid <- unique(data$id)[subjectId] #convert subjectId to uid
  
  
  
  
  ##############################################################################################################
  #OPTIMIZATION ROUTINE
  ##############################################################################################################
  
  # Extend output with folder and name of input script
  ifelse(!dir.exists(output_path), dir.create(output_path), "Folder exists already, all ok.")
  output_path <- paste0(output_path,"simulatedData_",inputModel,"/")
  ifelse(!dir.exists(output_path), dir.create(output_path), "Folder exists already, all ok.")
  
  
  
  start.time <- Sys.time()
  roundList <- 1:ROUNDS_CV #Remove training round and bonus round
  #cross-validation routine
  
  modelDir <- paste0(output_path,"fittedmodel_",fittingModel,"/")
  ifelse(!dir.exists(modelDir), dir.create(modelDir), "Folder exists already, all ok.")
  
  base_name <- paste0(modelDir,"part_",subjectId)
  output <- c()
  for (r in roundList){ #loop through rounds in roundList
    
    if(r>=missing_rounds_from){
      print(r)
      cv <- cvfun(data=data,subject=subjectId, kernelFun=rbf, acquisition = ucb, leaveoutindex=r,model_version=fittingModel,fit_normalGP = FIT_NORMAL_GP,DEFAULT_ERR_VAR=DEFAULT_ERR_VAR,NUM_CLICKS=NUM_CLICKS,exponential_asymmetry=EXPONENTIAL_ASYMMETRY,surprise_model=surprise_model)
      
      output <- rbind(output, cv)
      final_name=paste0(base_name,".csv")
      write.csv(output,final_name)
    }
  }
  
  #save the vector with kernel-acquisition-pair as name
  final_name=paste0(base_name,".csv")
  write.csv(output,final_name)
  
  
  
  print(output)
  end.time <- Sys.time()
  elapsed <- end.time - start.time
  print(elapsed)
  
  ##############################################################################################################
  #THE END
  ##############################################################################################################
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
fm <- fitting_model_id
if(input_model_id < 5){
  scenarioIndex <- input_model_id
  modelCombIndex <- num_fitting_models*(scenarioIndex-1)+fm
  
  fitting_routine_single_config(subjectId = subjectId, modelCombIndex = modelCombIndex)
  
} else if( input_model_id == 6){ # 6 = R,S
  for (scenarioIndex in c(1,2)) {
    modelCombIndex <- num_fitting_models*(scenarioIndex-1)+fm
    
    fitting_routine_single_config(subjectId = subjectId, modelCombIndex = modelCombIndex)
  }  
} else if( input_model_id == 5){ # 5 = F,N
  for (scenarioIndex in c(3,4)) {
    modelCombIndex <- num_fitting_models*(scenarioIndex-1)+fm
    
    fitting_routine_single_config(subjectId = subjectId, modelCombIndex = modelCombIndex)
  }  
} 
