#Behavior plots CogSci
# Plot behavioral results
# house keeping
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'viridis' , 'brms', 'lme4', 'sjPlot', 'cowplot','tidybayes','modelr') #'cowplot',  'entropy', 'withr', 'ggbeeswarm','jsonlite'
invisible(lapply(packages, require, character.only = TRUE)) #loads packages

source('data_munging.R')
source('statistics.R')

homePath= paste0(getwd(),"/")
modelpath = 'brmsModels'

FACTOR=25
GRIDSIZE=11
CLICKS=25
ROUNDS=15

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
scenarioPalette <- c('#ef8a62','#67a9cf')

COLOR_CONDITION_BASELINE = cbPalette[8]
COLOR_CONDITION_MEMORY = cbPalette[7]

COLOR_MODEL_BASIC = cbPalette[1]
COLOR_MODEL_RECENCY = cbPalette[2]
COLOR_MODEL_SURPRISE = cbPalette[3]
COLOR_MODEL_SURPRISE_PLUS = cbPalette[4]


KERNEL_8= "data/env_generation/smoothKernel_8x8.json"
KERNEL_11= "data/env_generation/smoothKernel_11x11.json"

PLOT_PATH="plots/plots_simulation/"


#Environments
smoothEnvironments_8 <- lapply(fromJSON(paste(homePath,KERNEL_8,sep=""), flatten=TRUE), FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64), c( 'x2', 'y','x1'))))
smoothEnvironments_11 <- lapply(fromJSON(paste(homePath,KERNEL_11,sep=""), flatten=TRUE), FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,121), c('x1', 'x2', 'y'))))

#Choose 8x8 size
smoothEnvironments<-smoothEnvironments_11
NUM_ENVS = length(smoothEnvironments)

# extract environments
env <- as.data.frame(smoothEnvironments[[1]])
for (i in 2:(NUM_ENVS-1)){
  env<-rbind(env,as.data.frame(smoothEnvironments[[i]]))
}
env$en<-rep(1:(NUM_ENVS-1), each=GRIDSIZE^2)

# d <- load_exp_data(includeLast = FALSE)
d <- loadSimulatedData()


################################################################################################
# Learning effects
################################################################################################

#Learning over trials
dTrials <- ddply(d, ~step, plyr::summarize, meanReward = mean(z)+.5) 
corTestPretty(dTrials$meanReward, seq(1,26))


################################################################################################
# 2a: Learning curves
################################################################################################
dLearningCurves <- ddply(d, ~id+step+model, plyr::summarize, meanReward = mean(z)+.5) 
pCurveModel <- ggplot(dLearningCurves, aes(x=step, y=meanReward, color=model, fill=model, group=model))+
  stat_summary(fun = mean, geom='line', size=.5)+
  stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha=0.3, color=NA)+
  geom_hline(yintercept=0.5, linetype='dashed', color='red') + # random choice model
  xlab('Trial')+
  ylab('Normalized Reward ± 95% CI')+
  labs(color='Simulating Model') +
  labs(fill='Simulating Model') +
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  # ggtitle('Learning Curves') +
  theme_classic()  +
  # theme(strip.background=element_blank()) #, legend.position='none'
  theme(legend.position = c(0.05,1), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pCurveModel
# ggsave(filename = paste0(PLOT_PATH,"learningCurves_age.png"), plot=pCurveAge, height=4.5, width=8, units = "in")


dInd <- ddply(d, ~id+step+model, plyr::summarize, meanReward = mean(z)+.5) 
dInd <- ddply(d, ~id+model, plyr::summarize, meanReward = mean(z)+.5) 
ttestPretty(subset(dInd, model == 'surprise')$meanReward, subset(dInd, model == 'surprise_plus')$meanReward, paired=T)


################################################################################################
# Calculate mean distance for random choice
################################################################################################
grid = expand.grid(x1=0:10, x2=0:10, y1=0:10, y2=0:10)
grid$distance = NA

for(i in 1:dim(grid)[1]){
  grid$distance[i] <- dist(rbind(c(grid$x1[i], grid$x2[i]), c(grid$y1[i], grid$y2[i])), method = "manhattan")
}

randomMeanDistance = mean(grid$distance)

################################################################################################
# 2c: Distance ~ Step
################################################################################################
#Diffs between null model and other models in distance?
dDistModel<- ddply(d, ~id+model, plyr::summarize, meanDist = mean(distance, na.rm=T)) 
ttestPretty(subset(dDistModel, model=='null')$meanDist, subset(dDistModel, model=='recency')$meanDist, paired=T)
ttestPretty(subset(dDistModel, model=='null')$meanDist, subset(dDistModel, model=='surprise')$meanDist, paired=T)
ttestPretty(subset(dDistModel, model=='null')$meanDist, subset(dDistModel, model=='surprise_plus')$meanDist, paired=T)
ttestPretty(subset(dDistModel, model=='null')$meanDist, subset(dDistModel, model=='full')$meanDist, paired=T)

#Did distance decrease over trials?
dDistStep <- ddply(d, ~step, plyr::summarize, meanDist = mean(distance, na.rm=T)) 
corTestPretty(dDistStep$meanDist, dDistStep$step)

#distance over trials
dDistCurves <- ddply(d, ~id+step+model, plyr::summarize, meanDist = mean(distance, na.rm=T)) 
pDistCurvesTrials <- ggplot(dDistCurves, aes(x=step, y=meanDist, color=model, fill=model))+
  stat_summary(fun = mean, geom='line', size=.5)+
  stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha=0.3, color=NA)+
  geom_hline(yintercept=randomMeanDistance, linetype='dashed', color='red') + # mean distance
  xlab('Trial')+
  ylab('Avg. Search Distance')+
  # ggtitle('Search Distance') +
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  theme_classic()  +
  theme(legend.position = c(0,0.1), legend.justification=c(0,0),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pDistCurvesTrials

################################################################################################
# 2c: Distance ~ Reward
################################################################################################

#Did distance decrease with reward?
dDistReward <- ddply(d, ~prev_z, plyr::summarize, meanDist = mean(distance, na.rm=T)) 
corTestPretty(dDistReward$meanDist, dDistReward$prev_z)

dDistRewardModel <- ddply(d, ~prev_z+model, plyr::summarize, meanDist = mean(distance, na.rm=T)) 
corTestPretty(subset(dDistRewardModel, model=='null')$meanDist, subset(dDistRewardModel, model=='null')$prev_z)
corTestPretty(subset(dDistRewardModel, model=='recency')$meanDist, subset(dDistRewardModel, model=='recency')$prev_z)
corTestPretty(subset(dDistRewardModel, model=='surprise')$meanDist, subset(dDistRewardModel, model=='surprise')$prev_z)
corTestPretty(subset(dDistRewardModel, model=='surprise_plus')$meanDist, subset(dDistRewardModel, model=='surprise_plus')$prev_z)
corTestPretty(subset(dDistRewardModel, model=='full')$meanDist, subset(dDistRewardModel, model=='full')$prev_z)



#distance over reward
dDistCurves <- ddply(d, ~prev_z+model, plyr::summarize, meanDist = mean(distance, na.rm=T)) #id+
pDistCurvesReward <- ggplot(dDistCurves, aes(x=prev_z, y=meanDist, color=model, fill=model))+
  # stat_summary(fun = mean, geom='line', size=.5)+
  # stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha=0.3, color=NA)+
  geom_smooth(alpha=0.2)+
  geom_hline(yintercept=randomMeanDistance, linetype='dashed', color='red') + # mean distance
  xlab('Previous reward')+
  ylab('Avg. Search Distance')+
  # ggtitle('Search Distance') +
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  theme_classic()  +
  theme(legend.position = c(0,0.1), legend.justification=c(0,0),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pDistCurvesReward


###################################################################################################################################
#### Final behavioral plot
###################################################################################################################################

pBehavior <- cowplot::plot_grid(pCurveModel,pDistCurvesTrials, pDistCurvesReward,
                                ncol = 2, labels = 'auto'
)
pBehavior

ggsave('plots/plots_simulated/behavior.pdf', pBehavior, width = 7, height = 7, units ='in')




