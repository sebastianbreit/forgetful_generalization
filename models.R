#Models and sampling strategies
#Charley Wu, Eric Schulz, Sept 2019
# packages <- c("Rfast")
# lapply(packages, require, character.only = TRUE)
##############################################################################################################
#MATRIX INVERSION
##############################################################################################################

#calculate inverse of the cov-function using sigular value decomposition
cov.inverse.svd <- function(X, tol = sqrt(.Machine$double.eps)){
  # Generalized Inverse of a Matrix
  dnx <- dimnames(X)
  if(is.null(dnx)) dnx <- vector("list", 2)
  #singular value decomposition
  s <- svd(X)
  nz <- s$d > tol * s$d[1]
  #inverse
  K.inv <- structure(
    if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else X,
    dimnames = dnx[2:1])
  #logarithm of determinant.
  log.K.det <- sum(log(s$d))
  #return inverse plus log determinant
  return(list(Inv = K.inv,lDet = log.K.det))
}

#calculate inverse of the cov-function using Cholesky
cov.inverse.chol <- function(X){
  #cholseky decomposition
  R <- chol(X)
  #complex conjugate
  Rt <- Conj(t(R))
  #invert
  R.inv <- solve(R)
  #invert
  Rt.inv <- solve(Rt)
  #multiply matrices
  X.inv <- R.inv %*% Rt.inv
  #log determinant
  log.X.det <- 2*sum(log(diag(R))) 
  #return both
  return(list(Inv = X.inv, lDet = log.X.det))
}



##############################################################################################################
#GP KERNEL
##############################################################################################################

#Radial Basis Kernel
rbf <- function(x1,x2,theta){
  
  #check dimensions
  if(ncol(x1) != ncol(x2)){
    stop("x1 and x2 must contain input values of the same dimension.")
  } 
  
  
  #get dimensions
  N1 <- nrow(x1)
  N2 <- nrow(x2)
  d <- ncol(x1)
  #initialize sigma
  sigma <-  matrix(rep(0, N1*N2),nrow=N1)
  
  #observational variance
  sf <- theta[d+1]
  #noise variance
  sn <- theta[d+2]

  #loop through
  for(i in 1:d){
    #length scale
    l <- theta[i] #Note: assumes a unique length scale for each dimension
    #x-diff
    xdiff_1 <- (outer(x1[,i],x2[,i],function(x,y) x - y)/l)^2
    # xdiff <- (Rfast::Outer(x1[,i],x2[,i],'-')/l)^2
    xdiff <- xdiff_1
    
    sigma <- sigma + xdiff
  }
  #RBF function
  sigma.final <- sf*exp(-0.5*sigma)
  #return final covariance matrix
  return(sigma.final)
}
#class(rbf)<- c(class(rbf), "GP") #identify the rbf kernel as a gp model




gp_error_variance_exponential <- function(obs,theta,clicks,prior_mean=0.5,default_noise=0.0001,exponential_asymmetry=TRUE,surprise_model=3,prev_posterior=NA){
  # obs$y has to be scaled to 0-1 already
  # recency 1 == latest obs [-1,1]
  # surprise 0 == mean 50, 1 == max (0 or 100) [0,1]
  
  # Recency [0,1]
  time<- 1:nrow(obs)
  f_recency <- 1-((max(time) - time) /clicks)

  #Surprise [0,1]
  if(surprise_model==1){
    #Default surprise (model so far)
    diff<- (obs$y+.5-prior_mean)
  }else if(surprise_model==2){
    observed_mean <- mean(obs$y)
    #Default surprise (model so far)
    diff<- (obs$y-observed_mean)
  }else if(surprise_model==3){
      diff<- (obs$y-prev_posterior)
  }else {
    break
  }
  #Asymmetric surprise
  neg_surprise_bias <- if(length(theta)==3) as.numeric(theta[3]) else as.numeric(1)

  if(!exponential_asymmetry){
    f_surprise<- ifelse(diff>=0,
                        sqrt(abs(diff)),
                        sqrt(abs(diff))*(neg_surprise_bias))
  }else{
    f_surprise<- ifelse(diff>=0,
                        sqrt(abs(diff)),
                        sqrt(abs(diff))*exp(neg_surprise_bias))
  }
  
  
  # Logarithmic surprise
  # f_surprise<- -log(max(f_surprise,0.0001))
  # feature_mat <- cbind(c(1-f_recency),c(f_surprise))
  # features_x_weights <-feature_mat %*% as.numeric(theta[1:2])

  # High Surprise (1): feat_x_weight=0
  # High Recency (1): feat_x_weight=0
  # feature_mat <- cbind(c(1-f_recency),c(1-f_surprise))
  feature_mat <- cbind(c(max(abs(f_recency))-f_recency)/max(abs(f_recency)),
                       c(max(abs(f_surprise))-f_surprise)/max(abs(f_surprise)))
  
  if(max(abs(f_recency))==0){
    feature_mat <- cbind(c(1-f_recency),
                         c(max(abs(f_surprise))-f_surprise)/max(abs(f_surprise)))
  }
  if(max(abs(f_surprise))==0){
    feature_mat <- cbind(c(max(abs(f_recency))-f_recency)/max(abs(f_recency)),
                             c(1-f_surprise))
  }
  if(max(abs(f_recency))==0 && max(abs(f_surprise))==0){
    feature_mat <- cbind(c(1-f_recency),
                         c(1-f_surprise))
  }
  
  features_x_weights <-feature_mat %*% as.numeric(theta[1:2])
 
  scaling <- default_noise
  error_var <- exp(features_x_weights) *as.numeric(scaling)
  
  # TODO: Seems like too large values run into issues when inverse is calculated-- should we simply cap at some point?
  MAX <- 10^10
  MIN <- 10^(-5)
  error_var <- ifelse(error_var>MAX,MAX,error_var)
  # error_var <- ifelse(error_var<MIN,
  #                     ifelse(error_var>=0,MIN,error_var),error_var)
  
  error_var <- ifelse(error_var<MIN,MIN,error_var)
  return(c(error_var))
}



##############################################################################################################
#GAUSSIAN PROCESS
##############################################################################################################

#Gaussian Process function
#X.test: matrix for predcitions
#theta: vector of hyper-parameters (lambda, Sf, Sn)
#X; matrix of observations
#y: matrix (vector) of observed outcomes
#kernel: used kernel function, can be "rbf", "oru", or "mat"
gpr <- function(X.test, theta, X,y, k,d_default=0.0001){

  Xstar <-X.test
  
  #dimensions
  d <- ncol(X)
  
  #calculate capital K
  K <- k(X,X,theta) 
  
  # Add noise
  num_obs<-nrow(X)
  start <- d+2
  end <-length(theta)
  err_var <-  theta[start:end]
  
  D <- (diag(rep(1,num_obs)) * err_var) #+ diag(rep(1,num_obs))*d_default
  KD <- K+D 
  
  #Check if matrix is positive semi-definite
  if (is.positive.definite(KD)){
    #KK <- cov.inverse.chol(K) #use Cholesky
    KK.inv <- chol2inv(chol(KD)) #MASS implementation of Cholesky
  } else {
    KK.inv <- array(unlist(cov.inverse.svd(KD)[1]) , dim = dim(KD)) #use SVD
    # KK.inv <-(svd.inverse(KD))
  }
  
  #apply the kernel
  result <- apply(Xstar, 1, function(x){
    XX <- matrix(x,nrow=1)
    Kstar <- k(X, XX, theta)
    Kstarstar <- k(XX,XX,theta)
    Kstar_x_KKinv <- t(Kstar) %*% KK.inv
    
    #get mean vector
    mu <- Kstar_x_KKinv %*% y
    
    #get covariance
    cv <- Kstarstar - (Kstar_x_KKinv %*% Kstar) #+ d_default (except at the observations as they already had noise added?) # Kstarstar should be one row, so no mult necessary? diag(rep(1,nrow(as.matrix(Kstarstar))))*
    
    return(c(mu, cv))
  })
  
  #as a data frame with names mu and sig
  prediction <- as.data.frame(t(result))
  prediction[is.na(prediction)] <- d_default #remove NaN items with the noise variance 
  colnames(prediction) <- c("mu", "sig")
  return(prediction)
}



##############################################################################################################
#ACQUISITION FUNCTIONS
##############################################################################################################

#Upper Confidence Bound Sampling
ucb<-function(out, pars, refactor=F){
  if (refactor==TRUE){
    gamma <- pars[1]
    beta_star<-pars[2]
    #calulate all the upper confidence bounds
    outtotal<-(gamma*out$mu)+(beta_star*sqrt(out$sig)) #refactored parameters in combination with softmax tau, where gamma = 1/tau and beta_star = beta/tau
    #avoid borderline cases
    outtotal[outtotal<=0]<-0.0001
    outtotal[outtotal>100]<-100
    outtotal<-matrix(outtotal, ncol=nrow(out)/64, byrow=TRUE)
  }else{
    beta <- pars[1]
    #calulate all the upper confidence bounds
    outtotal<-out$mu+(beta*sqrt(out$sig)) #refactored parameters in combination with softmax tau, where gamma = 1/tau and beta_star = beta/tau
    #avoid borderline cases
    outtotal[outtotal<=0]<-0.0001
    outtotal[outtotal>50]<-50
    outtotal<-matrix(outtotal, ncol=nrow(out)/64, byrow=TRUE)
  }
  #return them
  return(outtotal)
}
#add "UCB" to the class of the ucb function, so that modelFit can recognize that it has a longer parameter array
#class(ucb)<- c(class(ucb), "UCB")

GP_UCB_pipeline <- function(subjD,round,horizon,model_version,X_map, lambda,beta,tau,recency,surprise,asymmetry,
                            exponential_asymmetry=T,surprise_model=T,NUM_CLICKS=25,DEFAULT_ERR_VAR=0.0001){
  
  roundD <- subset(subjD, round==round)
  
  if(model_version!="null"){
    errVar_vec <- if(is.na(asymmetry)) c(recency,surprise) else c(recency,surprise,asymmetry)
  }
  
  # print(roundD)
  # horizon <- nrow(roundD)
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
  
  posterior_stack <- NA
  #loop through observations
  for (i in 1:(horizon-1)){ #skip the last observation, because no choice was made based on that information
    parVec_gpr <- c(lambda, lambda, 1,DEFAULT_ERR_VAR)
    
    #new observation
    # print(paste0(r,',',i,': ',X))
    X_t<-matrix((X[1:i,]), ncol=2)
    y1<-matrix((y[1:i]))
    #Which posterior function to use
    if (model_version=="null"){# Default GP #TODO: What to fit here?
      out <- gpr(X.test=Xnew, theta=parVec_gpr, X=X_t, y=y1, k=rbf) 
    }else {# Heteroscedastic GP - all memory models
      errVar <- gp_error_variance_exponential(obs=data.frame(x1=X_t[,1],x2=X_t[,2],y=y1+.5),theta=errVar_vec,clicks=NUM_CLICKS,prior_mean=.5,default_noise=DEFAULT_ERR_VAR,exponential_asymmetry=exponential_asymmetry,surprise_model=surprise_model,prev_posterior=posterior_stack[chosen[1:i]])
      errVar_index <-1:length(errVar) + 3
      parVec_gpr <- replace(parVec_gpr, errVar_index, errVar)
      
      out <- gpr(X.test=Xnew, theta=parVec_gpr, X=X_t, y=y1, k=rbf) 
      
      posterior_stack <- out$mu
    }
    
    #UCB function
    utilityVec<-ucb(out, c(beta))
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
  
  
  return(c(out$mu,out$sig,utilities,p,chosen))
}