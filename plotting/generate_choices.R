rm(list=ls())

packages <- c('plyr', 'jsonlite', 'reshape', 'DEoptim', "matrixcalc", "fields", 'RColorBrewer', 'ggplot2', 'gridExtra', 'cowplot','useful')
lapply(packages, require, character.only = TRUE)
source("models.R")

set.seed(4) # 4 is good pattern





GRIDSIZE=11
NUM_CLICKS <- 25
PARAMS_8x8_ANNA <- c(1.23,0.24,0.03) #Adult params
PARAMS_11x11_CHARLEY <- c(0.78,0.5,0.09)
PARAMS_USED <- PARAMS_11x11_CHARLEY

BETA <- .5

EVAL_AT_TIME <-10

DEFAULT_ERR <- 0.0001

PLOT_PATH <- paste0(getwd(),"/plots/plots_choices/11x11/eval_at_",EVAL_AT_TIME)



ifelse(!dir.exists(PLOT_PATH), dir.create(PLOT_PATH), "Folder exists already, continuing")


param_combs_exponential <-list(
  c(0,0,DEFAULT_ERR),
  c(10,0,DEFAULT_ERR),c(0,10,DEFAULT_ERR),
  c(20,0,DEFAULT_ERR),c(0,20,DEFAULT_ERR),
  c(30,0,DEFAULT_ERR),c(0,30,DEFAULT_ERR),
  c(40,0,DEFAULT_ERR),c(0,40,DEFAULT_ERR),
  c(50,0,DEFAULT_ERR),c(0,50,DEFAULT_ERR),
  c(100,0,DEFAULT_ERR),c(0,100,DEFAULT_ERR),
  c(200,0,DEFAULT_ERR),c(0,200,DEFAULT_ERR),
  c(500,0,DEFAULT_ERR),c(0,500,DEFAULT_ERR)
  )

##############################################################################################################
# Setup
##############################################################################################################

#Load envs
# envs <- lapply(fromJSON("env_generation/smoothKernel_8x8.json", flatten=TRUE),  FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,(GRIDSIZE*GRIDSIZE)), c('x2', 'y', 'x1'))))
envs <- lapply(fromJSON("env_generation/smoothKernel_11x11.json", flatten=TRUE),  FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,(GRIDSIZE*GRIDSIZE)), c('x1','x2', 'y'))))
choices <- expand.grid('x1'=0:(GRIDSIZE-1), 'x2'=0:(GRIDSIZE-1)) 
#number formating function
numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.0f", val)) }
envNum <- 3 #fix
#Which NUM_CLICKS is currently being predicted?
#Create a set of randomn choices. In the future, replace with either handpicked observations, participant data, or sampled choices under a policy

chosen <- sample(1:(GRIDSIZE*GRIDSIZE), EVAL_AT_TIME) 

x1 <- choices[chosen,'x1']
x2 <- choices[chosen,'x2']
#create observation matrix
X<-as.matrix(cbind(x1,x2))
#initialize Xtest
Xnew<-as.matrix(expand.grid(0:(GRIDSIZE-1),0:(GRIDSIZE-1)))
#make sure X is a matrix
X<-as.matrix(X)
y <- envs[[envNum]][chosen,'y'] + rnorm(EVAL_AT_TIME, 0, sd = sqrt(DEFAULT_ERR)) #TODO: check that this observation variance is consistent with the noise added in the experiment w.r.t. to reward scaling
y[y<.01] <- .01
y[y>1] <- 1
yrescaled <- y*100 #Think of this as canonical scaling, where in the experiment rewards are scaled to randomly sampled max values






##############################################################################################################
# Generating Plots
##############################################################################################################
generate_GP_plots <- function(param_comb){
  
  
  #Model specifications
  kernel=rbf
  acq = ucb
  
  lambda <- PARAMS_USED[1]
  beta <- PARAMS_USED[2]
  tau <- PARAMS_USED[3]
  
  signalVariance <- 1
  errorVariance <- DEFAULT_ERR
  
  
  obs=data.frame(x1,x2,y)
  
  errorVariance<- gp_error_variance_exponential( obs=obs,theta=param_comb,clicks=NUM_CLICKS)
  test = data.frame(errorVariance)
  theta=c(lambda, lambda, signalVariance, errorVariance)
  
  #Compute posterior predictions
  GPpost <- gpr(X.test=Xnew, theta=theta, X=X, y=y-0.5, k=kernel)
  GPpost$mu <- GPpost$mu + 0.5
  
  
  ##############################################################################################################
  # PLOTS
  ##############################################################################################################
  
  
  
  
  #Plot 1. Random choices
  d1 <- data.frame(cbind(Xnew,rep(NA, (GRIDSIZE*GRIDSIZE)))) 
  colnames(d1) <- c("x1", "x2", "y")
  for (row in seq(1:length(y))){ 
    obs <- X[row,]
    d1$y[d1$x1==obs[1] & d1$x2==obs[2]] <- yrescaled[row]
  }
  d1$ytext <- numformat(d1$y)
  d1$ytext[d1$ytext=='NA'] <- ""
  
  #Plot 1a. Choices with reward
  p1 <- ggplot(d1, aes(x = x1+1, y = x2+1, fill = y)) + 
    geom_tile(color='black', width=1, height=1) +
    theme_bw() +
    coord_equal() +
    xlim(0.5,GRIDSIZE + .5) +
    ylim(0.5,GRIDSIZE + .5) + 
    ggtitle(paste0('Revealed (t=',NUM_CLICKS,")")) +
    geom_text(aes(x = x1+1, y = x2+1, label = ytext))+
    scale_fill_distiller(palette = "Spectral",limits=c(-5,105), na.value = 'white', breaks=c(0,25,50,75,100))+
    labs(fill="Payoff")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank())
  
  
  
  
  d_11 <-d1
  for(i in 1:length(chosen)){
    d_11$ytext[chosen[i]]<-numformat(i)
  }
  
  #Plot 1b. Choices with order
  p1_1 <- ggplot(d_11, aes(x = x1+1, y = x2+1, fill = y)) + 
    geom_tile(color='black', width=1, height=1) +
    theme_bw() +
    coord_equal() +
    xlim(0.5,GRIDSIZE + .5) +
    ylim(0.5,GRIDSIZE + .5) + 
    ggtitle(paste0('Revealed (t=',NUM_CLICKS,")")) +
    geom_text(aes(x = x1+1, y = x2+1, label = ytext))+
    #scale_fill_gradientn(name='Payoff', colours = hm.palette(100), values = seq(0, 100, length=9),  rescaler = function(x, ...) x, oob = identity) +
    scale_fill_distiller(palette = "Spectral",limits=c(-5,105), na.value = 'white', breaks=c(0,25,50,75,100))+
    labs(fill="Payoff")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank())
  
  
  
  #Plot 2. Posterior mean
  d2 <- melt(matrix((GPpost[,1]*100), nrow=GRIDSIZE, ncol=GRIDSIZE)) #*100 is to preserve same scaling factor
  names(d2) <- c('X1', 'X2', 'value')
  p2<-ggplot(d2, aes(x = X1, y = X2, fill = value)) + 
    geom_tile(color='black', width=1, height=1) +
    theme_bw() +
    xlim(0.5,GRIDSIZE + .5) +
    ylim(0.5,GRIDSIZE + .5) + 
    coord_equal() +
    ggtitle('Expected Reward') +
    #scale_fill_gradientn(name = "Exp. Payoff", colours = hm.palette(100),values = seq(0, 100, length=9)) +
    scale_fill_distiller(palette = "Spectral",limits=c(-5,105), na.value = 'white',  breaks=c(0,25,50,75,100))+
    labs(fill="E(Payoff)")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank(), 
          plot.margin=unit(c(0,0,0,0),"mm")) 
  #p2
  
  
  
  #Plot 3. Posterior variance
  d3 <- melt(matrix((GPpost[,2]*100), nrow=GRIDSIZE, ncol=GRIDSIZE))  #*100 is to preserve same scaling factor
  names(d3) <- c('X1', 'X2', 'value')
  p3<-ggplot(d3, aes(x = X1, y = X2, fill = value)) + 
    geom_tile(color='black', width=1, height=1) +
    theme_bw() +
    xlim(0.5,GRIDSIZE + .5) +
    ylim(0.5,GRIDSIZE + .5) + 
    coord_equal() +
    ggtitle('Uncertainty') +
    #scale_fill_gradientn(name = "Exp. Payoff", colours = hm.palette(100),values = seq(0, 100, length=9)) +
    scale_fill_distiller(palette = "Spectral", na.value = 'white')+
    labs(fill="V(payoff)")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank()) 
  #p3
  
  #Compute UCB values
  utilityVec <- acq(GPpost, beta) #*100 is to preserve same scaling factor
  
  #Plot 4. UCB value
  d4 <- melt(matrix((utilityVec  * 100), nrow=GRIDSIZE, ncol=GRIDSIZE))  
  names(d4) <- c('X1', 'X2', 'value')
  p4<-ggplot(d4, aes(x = X1, y = X2, fill = value)) + 
    geom_tile(color='black', width=1, height=1) +
    theme_bw() +
    xlim(0.5,GRIDSIZE + .5) +
    ylim(0.5,GRIDSIZE + .5) + 
    coord_equal() +
    ggtitle('UCB') +
    #scale_fill_gradientn(name = "Exp. Payoff", colours = hm.palette(100),values = seq(0, 100, length=9)) +
    scale_fill_distiller(palette = "Spectral", na.value = 'white')+
    labs(fill="UCB")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank()) 
  #p4
  
  p <- utilityVec - max(utilityVec) #subtract max value to prevent overflow
  p <- exp(p/tau)
  #avoid underflow, overflow case sum(p)=0
  p <- (pmax(p, 0.00001))
  p <- p/sum(p)
  #avoid underflow by setting a floor and a ceiling
  p <- (pmax(p, 0.00001))
  p <- (pmin(p, 0.99999))
  #Next choice at t = trial + 1
  #print(sum(p))
  
  
  #Plot 5. Softmax surface of acquisition function
  d5<- melt(matrix(p, nrow=GRIDSIZE, ncol=GRIDSIZE))
  names(d5) <- c('X1', 'X2', 'value')
  d5<- round(d5, 3)
  p5<-ggplot(d5, aes(x = X1, y = X2, fill = value)) + 
    geom_tile(color='black', width=1, height=1) +
    theme_bw() +
    xlim(0.5,GRIDSIZE + .5) +
    ylim(0.5,GRIDSIZE + .5) + 
    coord_equal() +
    ggtitle('Softmax') +
    #scale_fill_gradientn(name='P(choice)',colours = hm.palette(100),values = seq(0, 1, length=9)) +
    scale_fill_distiller(palette = "Spectral", na.value = 'white' )+
    labs(fill="P(Choice)")+
    #annotate("text", x = nextChoice[1] + 1, y = nextChoice[2] + 1, label = "X") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank())
  #p5
  return(list(p1,p1_1,p2,p3,p4,p5))
}
  
for(comb in param_combs_exponential){
    print(comb)
    plots <- generate_GP_plots(comb)
    
    folder_name <-paste0(PLOT_PATH,"/",comb[3])
    ifelse(!dir.exists(folder_name), dir.create(folder_name), "Folder exists already, overwriting data.")
    combination=''
    for(c in comb[1:length(comb)]){
      combination=paste0(combination,"_",c)
    }
    
    plot_name = paste0(folder_name,"/comb", combination, "_plot.pdf")
    chosen_name=paste0(folder_name,"/chosen.csv")
    
    pdf(file=plot_name)
    write.csv(chosen, chosen_name)
    
    for(p in plots){
      plot(p)
    }
    dev.off()
}
