# house keeping
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr','jsonlite','stringr') #tidyverse
invisible(lapply(packages, require, character.only = TRUE)) 

source("data_munging.R")

homePath= paste0(getwd(),"/")
SIMULATION_DATA_PATH = "data/data_simulated_11x11/"


KERNEL_11= "data/env_generation/kernelSmooth_11x11.json"

#############################################################################################################################
# IMPORT DATA AND ENVIRONMENTS
#############################################################################################################################
modelList<-list("null", "recency","surprise+","full")# Done: 

# constants for the run
NUM_PARTICIPANTS=100
NUM_RUNS=7
GRIDSIZE=11
NUM_CLICKS=25

DEFAULT_ERR_VAR=0.0001

FACTOR_MEMORY=500
SD_FACTOR_MEMORY=100

GP_PARAM_BOUNDS=10


PARAM_SPACE_UNIFORM='UNIFORM'
PARAM_SPACE_EXPERIMENT='EXPERIMENT_VALUES_VALIDATION'


PARAM_GENERATION=PARAM_SPACE_EXPERIMENT

batchIdentifier= PARAM_GENERATION

#Environments
smoothEnvironments_11 <- lapply(fromJSON(paste0(homePath,KERNEL_11), flatten=TRUE), FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,121), c('x1', 'y', 'x2'))))

#Choose 8x8 size
smoothEnvironments<-smoothEnvironments_11
NUM_ENVS = length(smoothEnvironments)

basic_grid=expand.grid(x=0:(GRIDSIZE-1), y=0:(GRIDSIZE-1))
#Grid for calculating distance to previous choice
distance_grid = expand.grid(x1=0:(GRIDSIZE-1), x2=0:(GRIDSIZE-1), y1=0:(GRIDSIZE-1), y2=0:(GRIDSIZE-1))
distance_grid$distance = NA

for(i in 1:dim(distance_grid)[1]){
  distance_grid$distance[i] <- dist(rbind(c(distance_grid$x1[i], distance_grid$x2[i]), c(distance_grid$y1[i], distance_grid$y2[i])), method = "manhattan")
}

d_fitting <- load_fitting_data()
d_params <- d_fitting %>% 
  filter(input_model=='Memory') %>% 
  group_by(fitted_model) %>% 
  summarise(meanL=median(lambda),sdL=sd(lambda),
            meanB=median(beta),sdB=sd(beta),
            meanT=median(tau),sdT=sd(tau),
            meanR=median(recency),sdR=sd(recency),
            meanS=median(surprise_plus),sdS=sd(surprise_plus),
            meanA=median(asymmetry),sdA=sd(asymmetry),
            meanNLL=mean(nLL),sdNLL=sd(nLL)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(meanL=exp(meanL),meanB=exp(meanB),meanT=exp(meanT),meanA=exp(meanA))


generateModelData <- function(homePath, dataPath,batchIdentifier, kernelFun,acquisitionFun,localization,modelConfig,participants=NUM_PARTICIPANTS,rounds=NUM_RUNS,expParams=d_params) {
  
  d_correctParams <- expParams %>% filter(fitted_model==modelConfig)
  simD<-NA
  for(i in 1:participants){
    for(e in sample((0:(NUM_ENVS-1)),rounds,replace=FALSE)){
    # for(e in sample((0:19),rounds,replace=FALSE)){
      temp<-data.frame(
        id=i,
        round=NA,
        kernel=c("Smooth"),
        env=e,
        tau=NA,
        lambda=NA, 
        beta=NA,
        kError=NA,
        recency=NA,
        surprise=NA,
        negsurprisebias=NA,
        step=NA,
        x=c(1:(NUM_CLICKS+1)),
        y=c(1:(NUM_CLICKS+1)),
        z=c(1:(NUM_CLICKS+1)),
        prev_z=c(1:(NUM_CLICKS+1)),
        chosen=c(1:(NUM_CLICKS+1)),
        distance=c(1:(NUM_CLICKS+1))
      )
      if(is.na(simD)){
        simD<-temp
      }
      else{
        simD<-rbind(simD,temp)
      }
    }
  
  }

  #build manhattan blocks and choice matrix
  choices <- manhattan <- expand.grid('x1'=0:(GRIDSIZE-1), 'x2'=0:(GRIDSIZE-1)) 

  #############################################################################################################################
  # Run simulation
  #############################################################################################################################
  for (i in 1:participants){ #loop thorugh participants
    print(i)
    set.seed(i)
    #Old, used for pilot simulations
    # memory_weight=beta(0.06272,1) #mean=10
    # memory_weight=rgamma(1,FACTOR_MEMORY,1) #mean=memory_factor
    
    
    if(PARAM_GENERATION==PARAM_SPACE_UNIFORM){
      #For testing whether whole param range is actually recoverable
      memory_weight=runif(1,-FACTOR_MEMORY,FACTOR_MEMORY) #mean=memory_factor
      neg_surprise_weight=runif(1,-GP_PARAM_BOUNDS,GP_PARAM_BOUNDS)
      
      if(modelConfig=="full"){
        recency <- memory_weight
        surprise <- memory_weight
        neg_surprise_bias <- neg_surprise_weight
      } else if(modelConfig=="recency"){
        recency <- memory_weight
        surprise <- 0
        neg_surprise_bias <- NA
      } else if(modelConfig=="surprise+"){
        recency <- 0
        surprise <- memory_weight
        neg_surprise_bias <- neg_surprise_weight
      } else if(modelConfig=="null"){
        recency <- 0
        surprise <- 0
        neg_surprise_bias <- NA
      }
      
      
      r_lambda=runif(1,exp(-GP_PARAM_BOUNDS),exp(GP_PARAM_BOUNDS))
      r_beta=runif(1,exp(-4*GP_PARAM_BOUNDS),exp(4*GP_PARAM_BOUNDS))
      r_tau=runif(1,exp(-GP_PARAM_BOUNDS),exp(GP_PARAM_BOUNDS))
    }
    
    if(PARAM_GENERATION==PARAM_SPACE_EXPERIMENT){
      # #For testing whether we can simulate behavior at our experiment threshold
      # r_lambda <- rnorm(1,d_correctParams$meanL,d_correctParams$sdL)
      # r_beta <- rnorm(1,d_correctParams$meanB,d_correctParams$sdB)
      # r_tau <- rnorm(1,d_correctParams$meanT,d_correctParams$sdT)
      # 
      # recency <- rnorm(1,d_correctParams$meanR,d_correctParams$sdR)
      # surprise <- rnorm(1,d_correctParams$meanS,d_correctParams$sdS)
      # neg_surprise_bias <- rnorm(1,d_correctParams$meanA,d_correctParams$sdA)
      
      
      r_lambda <- d_correctParams$meanL
      r_beta <- d_correctParams$meanB
      r_tau <- d_correctParams$meanT
      
      recency <- d_correctParams$meanR
      surprise <- d_correctParams$meanS
      neg_surprise_bias <- d_correctParams$meanA
      
    }
    
    if(modelConfig!="null"){
      errVar_vec <-c(recency,surprise,neg_surprise_bias)
    }
    
    
    subjd <- subset(simD, id == i) #subset participant
    # if (subjd$kernel[1]=="Rough"){#which class of environments? 
    if (subjd$kernel=="Rough"){#which class of environments? 
      envClass <-roughEnvironments
    }else{ envClass <-smoothEnvironments }
    envs <- unique(subjd$env) #which specific environments did the subject encounter?
    
    for (round in 1:rounds){ #loop through rounds
      local_lambda=NA
      local_kError=NA
      local_beta=NA
      local_tau=NA
      if(modelConfig!='null'){
        
        local_recency <- as.numeric(errVar_vec[1])
        local_surprise <- as.numeric(errVar_vec[2])
        if(as.numeric(errVar_vec[3]!= 0 )){
          local_negsurprisebias <- as.numeric(errVar_vec[3])
        }
        else{
          local_negsurprisebias<- NA
        } 
      }else{
        local_recency <-0
        local_surprise <-0
        local_negsurprisebias<-0
      }

      envNum <- envs[round] #env number

      horizonLength="Long" # only long horizon
      horizonValue=NUM_CLICKS
      
      local_lambda <- r_lambda
      local_beta <- r_beta
      local_tau <- r_tau

      location <- sample(1:GRIDSIZE^2,1) #first location is random
      #Observations
      x1 <- choices[location,'x1']
      x2<- choices[location,'x2']
      chosen <- c(location)
      distance <- c(-1)
      prev_y <- c(-1)
      step<-1

      #rewards
      reward<-c()
      reward[1] <- y <- envClass[[envNum+1]][location,"y"]*100 #add 1 to envNum to go from range 0-19 to 1-20
      
      posterior_cache <- 0
      for (j in 1:(horizonValue)){
        obs <- data.frame(x1,x2,y)
        parVec_gpr <- c(r_lambda, r_lambda, 1,DEFAULT_ERR_VAR)
        
        if(modelConfig=="null"){
          local_errorVariance <-DEFAULT_ERR_VAR
        } else{
          
          
          errVar<- gp_error_variance_exponential(obs=data.frame(x1,x2,y=((y-50)/100)),theta=errVar_vec,clicks=NUM_CLICKS,exponential_asymmetry=T,surprise_model=3,prev_posterior=posterior_cache)
          test<-data.frame(errVar)
          errVar_index <-1:length(errVar) + 3
          parVec_gpr <- replace(parVec_gpr, errVar_index, errVar)
          
        }
        post <- gpr(X.test = choices, theta = parVec_gpr, X = cbind(obs$x1,obs$x2), y = ((obs$y-50)/100), k = rbf) #scale y observations to zero mean and variance of 1
        
        posterior_cache <- mean(post$mu)
        utilityVec <- ucb(post, pars = c(local_beta))
      
        #scale to max of prevent overflow by subtracting max
        utilityVec <- utilityVec - max(utilityVec)
        #compute softmax choice probabilities
        p <- exp(utilityVec/local_tau)
        p <- p/sum(p)
        
        if (NA %in% p || anyNA(p)){
          print("some value in p is NA")
          print(round)
          print(j)
          print(obs)
          print(local_errorVariance)
          print(post)
          print(utilityVec)
        }
        
        #Sample next choice
        location <- sample(1:GRIDSIZE^2,1, prob=p, replace=TRUE)
        
        #calculate distance to previous choice
        latest_distance <- as.numeric(
          distance_grid %>% 
          filter(x1==tail(x1,1)) %>% 
          filter(y1==tail(x2,1)) %>% 
          filter(x2==choices[location, 'x1']) %>% 
          filter(y2==choices[location, 'x2']) %>% 
          select(distance)
        )
        
        distance <- c(distance, c(latest_distance))
        
        #update reward, x1, x2, and y
        reward[j+1] <- envClass[[envNum+1]][location,"y"] *100
        
        x1 <- c(x1, choices[location, 'x1'])
        x2 <- c(x2, choices[location, 'x2'])
        chosen <- c(chosen, location)
        # print(chosen)
        y <- c(y,  reward[j+1])
        
        prev_y<-c(prev_y, reward[j])
        
        step <- c(step, j+1)
      }

      #}
      #insert data intosimD
      simD[simD$id==i & simD$env==envNum,]$round <- round
      
      simD[simD$id==i & simD$env==envNum,]$step <- step
      simD[simD$id==i & simD$env==envNum,]$x <- x1
      simD[simD$id==i & simD$env==envNum,]$y <- x2
      simD[simD$id==i & simD$env==envNum,]$z <- y
      simD[simD$id==i & simD$env==envNum,]$chosen <- chosen
      simD[simD$id==i & simD$env==envNum,]$distance <- distance
      simD[simD$id==i & simD$env==envNum,]$prev_z <- prev_y
      
      simD[simD$id==i & simD$env==envNum,]$tau <- local_tau
      simD[simD$id==i & simD$env==envNum,]$lambda <- local_lambda
      simD[simD$id==i & simD$env==envNum,]$beta <- local_beta
      simD[simD$id==i & simD$env==envNum,]$recency <- local_recency
      simD[simD$id==i & simD$env==envNum,]$surprise <- local_surprise
      simD[simD$id==i & simD$env==envNum,]$negsurprisebias <- local_negsurprisebias
    }
  }

  folder_name <-paste0(dataPath,batchIdentifier)
  ifelse(!dir.exists(folder_name), dir.create(folder_name), "Folder exists already, overwriting data.")
  filename = paste0(dataPath,batchIdentifier,"/simulatedData_", modelConfig, ".csv")
  print(filename)
  write.csv(simD, filename)

}


#############################################################################################################################
# RUN DATA GENERATION
#############################################################################################################################

for(m in modelList){
    generateModelData(homePath=homePath, dataPath=SIMULATION_DATA_PATH,batchIdentifier=batchIdentifier, kernelFun=rbf,acquisitionFun=ucb,localization=FALSE,
    modelConfig = m)
}




bimodalDistFunc <- function (n,cpct, mu1, mu2, sig1, sig2) {
  y0 <- rlnorm(n,mean=mu1, sd = sig1)
  y1 <- rlnorm(n,mean=mu2, sd = sig2)
  
  flag <- rbinom(n,size=1,prob=cpct)
  y <- y0*(1 - flag) + y1*flag 
}

# mu1 <- 0
# mu2 <- 300
# sig1 <- 50
# sig2 <- 50
# cpct <- 0.6
# 
# bimodalData <- bimodalDistFunc(n=10000,cpct,mu1,mu2, sig1,sig2)
# hist(log(bimodalData),breaks=50)


