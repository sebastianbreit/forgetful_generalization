#Source dependencies
# rm(list=ls()) #house keeping

#load packages
packages <- c('plyr','dplyr', 'jsonlite', 'DEoptim', "matrixcalc", "fields")
lapply(packages, require, character.only = TRUE)

homePath= paste0(getwd(),"/")
modelpath <-paste(homePath,'data_munging.R',sep="")
source(modelpath)
##############################################################################################################
#Model Fitting
##############################################################################################################
chosenGrid <- data.frame(x=expand.grid(0:10,0:10)$Var1,y=expand.grid(0:10,0:10)$Var2,chosen=row(expand.grid(0:10,0:10))) %>% 
  mutate(chosen=chosen.1) %>% 
  select(x,y,chosen)


#Negative Log Likelihood of model prediction
#parameters are distinct for each individual model
#subjD = subset of data for a specific subject
#rounds to be considered
modelFit_alternative<-function(par, subjD, acquisition, k, rounds,model_version,fit_normalGP,DEFAULT_ERR_VAR,NUM_CLICKS,X_map,exponential_asymmetry,surprise_model){
  
  homePath= paste0(getwd(),"/")
  modelpath <-paste(homePath,'data_munging.R',sep="")
  source(modelpath)
  
  asymmetry <- NA

  lambda <- exp(par[1])
  beta <- exp(par[2])
  tau <- exp(par[3])
    
  if(model_version=="full"){
    recency <- (par[4])
    surprise <- (par[5])
    asymmetry <- (par[6])
  } else if(model_version=="recency"){
    recency <- (par[4])
    surprise <- 0
  } else if(model_version=="surprise+"){
    recency <- 0
    surprise <- (par[4])
    asymmetry <- (par[5])
  }

  #Vector to store negative log likelihods
  nLL <- rep(0,length(rounds))
  for (r in rounds){ #Begin looping through each round
    GP_UCB_out <- GP_UCB_pipeline(subjD=subjD,round=r,horizon=NUM_CLICKS+1,model_version=model_version,X_map=X_map, lambda=lambda,beta=beta,tau=tau,recency=recency,surprise=surprise,asymmetry=asymmetry,
                                  exponential_asymmetry=exponential_asymmetry,surprise_model=surprise_model,NUM_CLICKS=NUM_CLICKS,DEFAULT_ERR_VAR=0.0001)
    
    p<-GP_UCB_out[[4]]
    chosen <- GP_UCB_out[[5]]
    print(paste0('p: ',p,' | chosen: ',chosen))
    nLL[which(rounds==r)] <- -sum(log(p[cbind(c(1:NUM_CLICKS),chosen)]))
    if(is.nan(nLL[which(rounds==r)]) | is.na(nLL[which(rounds==r)])){
      #if(TRUE){
      print(c("nLL is NaN or NA, something went wrong."))
      print(paste0("Softmax values:",p))
    }
      
  }
  
  return(sum(nLL))  #Return negative log likelihoods of all observations 
  
}

modelFit<-function(par, subjD, acquisition, k, rounds,model_version,fit_normalGP,DEFAULT_ERR_VAR,NUM_CLICKS,X_map,exponential_asymmetry,surprise_model){
  
  homePath= paste0(getwd(),"/")
  modelpath <-paste(homePath,'data_munging.R',sep="")
  source(modelpath)
  
  neg_surprise_bias <- NA
  
  if(fit_normalGP){
    lambda <- exp(par[1])
    beta <- exp(par[2])
    tau <- exp(par[3])
    
    if(model_version=="full"){
      recency <- (par[4])
      surprise <- (par[5])
      neg_surprise_bias <- (par[6])
    } else if(model_version=="recency"){
      recency <- (par[4])
      surprise <- 0
    } else if(model_version=="surprise"){
      recency <- 0
      surprise <- (par[4])
    }else if(model_version=="surprise+"){
      recency <- 0
      surprise <- (par[4])
      neg_surprise_bias <- (par[5])
    }
  } else{
    lambda <- 0.8
    beta <- 0.5
    tau <- 0.1
    
    
    if(model_version=="full"){
      recency <- (par[1])
      surprise <- (par[2])
      neg_surprise_bias <- (par[3])
    } else if(model_version=="recency"){
      recency <- (par[1])
      surprise <- 0
    } else if(model_version=="surprise"){
      recency <- 0
      surprise <- (par[1])
    }else if(model_version=="surprise+"){
      recency <- 0
      surprise <- (par[1])
      neg_surprise_bias <- (par[2])
    }
  }
  
  
  
  
  
  #Vector to store negative log likelihods
  nLL <- rep(0,length(rounds))
  for (r in rounds){ #Begin looping through each round
    #subset of data for round r
    roundD <- subset(subjD, round==r)
    
    
    parVec_gpr <- c(lambda, lambda, 1,DEFAULT_ERR_VAR)
    
    if(model_version!="null"){
      errVar_vec <- if(is.na(neg_surprise_bias)) c(recency,surprise) else c(recency,surprise,neg_surprise_bias)
    }
    
    # print(roundD)
    horizon <- nrow(roundD)
    #Observations of subject choice behavior
    chosen <- roundD$chosen
    chosen <- chosen[2:length(chosen)] # trim first observation, since it wasn't a choice but a randomly revealed tile
    y  <- roundD$z[0:(horizon-1)] #trim off the last observation, because it was not used to inform a choice (round already over)
    x1 <- roundD$x[0:(horizon-1)]
    x2 <- roundD$y[0:(horizon-1)]
    #create observation matrix
    X<-as.matrix(cbind(x1,x2))
    #make sure X is a matrix
    X<-as.matrix((X))
    Xnew<-as.matrix((X_map))
    #Utilties of each choice
    utilities <- NULL
    prevPost <- NULL #set the previous posterior computation to NULL for the kalman filter
    
    posterior_cache <- 0
    #loop through observations
    for (i in 1:(horizon-1)){ #skip the last observation, because no choice was made based on that information
      parVec_gpr <- c(lambda, lambda, 1,DEFAULT_ERR_VAR)
      
      #new observation
      # print(paste0(r,',',i,': ',X))
      X_t<-matrix((X[1:i,]), ncol=2)
      y1<-matrix((y[1:i]))
      #Which posterior function to use
      if (model_version=="null"){# Default GP #TODO: What to fit here?
        out <- gpr(X.test=Xnew, theta=parVec_gpr, X=X_t, y=y1, k=k) 
      }else {# Heteroscedastic GP - all memory models
        
        
        errVar <- gp_error_variance_exponential(obs=data.frame(x1=X_t[,1],x2=X_t[,2],y=y1),theta=errVar_vec,clicks=NUM_CLICKS,prior_mean=.5,default_noise=DEFAULT_ERR_VAR,exponential_asymmetry=exponential_asymmetry,surprise_model=surprise_model,prev_posterior=posterior_cache)
        errVar_index <-1:length(errVar) + 3
        parVec_gpr <- replace(parVec_gpr, errVar_index, errVar)
        
        out <- gpr(X.test=Xnew, theta=parVec_gpr, X=X_t, y=y1, k=k) 
        
        posterior_cache <- mean(out$mu)
      }
      
      #UCB function
      utilityVec<-acquisition(out, c(beta))
      utilityVec <- utilityVec - max(utilityVec) #avoid overflow
      utilities <- rbind(utilities, t(utilityVec)) # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
    }
    # print(paste0(x1,',',x2,',',posterior_stack))
    #Softmax rule
    p <- exp(utilities/tau)
    #avoid NaN calculation when doing rowsums
    p <- (pmax(p, 0.00001))
    p <- p/rowSums(p)
    #avoid underflow by setting a floor and a ceiling
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    #Calculate Negative log likelihood
    nLL[which(rounds==r)] <- -sum(log(p[cbind(c(1:(horizon-1)),chosen)]))
    if(is.nan(nLL[which(rounds==r)]) | is.na(nLL[which(rounds==r)])){
      #if(TRUE){
      print(c("nLL is NaN or NA, something went wrong."))
      print(c("GPR params:",parVec_gpr))
      print(c("Softmax values:",p))
      print(c("Utilities:",utilities))
    }
  }
  return(sum(nLL))  #Return negative log likelihoods of all observations 
  
}

##############################################################################################################
#CROSS VALIDATION FUNCTION
##############################################################################################################
#function to plug in to the optimaztion routine
#leaveoutindex is [1,2,...,n_rounds]
cvfun<-function(data,subject, kernelFun, acquisition, leaveoutindex,model_version,fit_normalGP,DEFAULT_ERR_VAR,NUM_CLICKS,exponential_asymmetry,surprise_model){
  
  #initialize Xtest
  X_map<-as.matrix(expand.grid(0:(GRIDSIZE-1),0:(GRIDSIZE-1)))
  
  #subselect participant, horizon and rounds not left out
  
  d1<-data %>% filter(id==subject)
  #training set
  rounds <- 1:ROUNDS_CV 
  trainingSet <- rounds[! rounds==leaveoutindex] #remove round specified by leaveoutindex
  #test set
  testSet <- leaveoutindex
  
  #Set upper and lower bounds based on nParams
  if(model_version=="null"){
    nParams_memory=0 # Default GP lambda,beta,tau ? or 0?
    nParams_asymmetry=0
  } else if(model_version=="full"){
    nParams_memory=2 # Full memory model, params recency+surprise
    nParams_asymmetry=1
  } else if(model_version=="recency" | model_version=="surprise"){
    nParams_memory=1 # Full memory model, params recency+surprise
    nParams_asymmetry=0
  } else if(model_version=="surprise+"){
    nParams_memory=1 # Full memory model, params recency+surprise
    nParams_asymmetry=1
  }
  if(fit_normalGP){
    nParams_gp=3
  }else{
    
    nParams_gp=0
  }
  # TODO: exponentiate GP params
  lbound <- c(c(-10,-50,-10),rep(-1000, nParams_memory),rep(-10, nParams_asymmetry))
  ubound <- c(c(10,50,10),rep(1000, nParams_memory),rep(10, nParams_asymmetry))
  
  #TRAINING SET
  fit<-DEoptim(modelFit, lower=lbound, upper=ubound, subjD=d1, k=kernelFun, rounds = trainingSet, acquisition=acquisition, model_version=model_version,fit_normalGP=fit_normalGP,X_map=X_map,DEFAULT_ERR_VAR=DEFAULT_ERR_VAR,NUM_CLICKS=NUM_CLICKS,exponential_asymmetry=exponential_asymmetry,surprise_model=surprise_model,DEoptim.control(itermax=MAX_ITER_FITTING))
  paramEstimates <- fit$optim$bestmem #MODEL DEPENDENT PARAMETER ESTIMATES
  #TEST SET
  predict <- modelFit(par=paramEstimates, subjD=d1, acquisition=acquisition, k=kernelFun, rounds=testSet,model_version=model_version,fit_normalGP=fit_normalGP,X_map=X_map,DEFAULT_ERR_VAR=DEFAULT_ERR_VAR,NUM_CLICKS=NUM_CLICKS,exponential_asymmetry=exponential_asymmetry,surprise_model=surprise_model)
  
  #output <- data.frame(loo=leaveoutindex, params=fit$optim$bestmem, nLL=predict[[1]],predictions=predict[[2]], chosen=predict[[3]]) # leaveoutindex, parameters, nLL,predictions, chosen....
  output <- c(predict,fit$optim$bestmem) # leaveoutindex, parameters, nLL,predictions, chosen....
  
  
  return(output) #return optimized value
}

