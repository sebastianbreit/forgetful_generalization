# Plot behavioral results
# house keeping
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'viridis' , 'brms', 'lme4', 'sjPlot', 'cowplot','tidybayes','modelr','lmerTest','xtable') #'cowplot',  'entropy', 'withr', 'ggbeeswarm','jsonlite'
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
COLOR_MODEL_FULL = cbPalette[4]

modelPalette <- c(COLOR_CONDITION_MEMORY,COLOR_MODEL_BASIC,COLOR_MODEL_RECENCY,COLOR_MODEL_SURPRISE,COLOR_MODEL_FULL)

KERNEL_8= "data/env_generation/smoothKernel_8x8.json"
KERNEL_11= "data/env_generation/smoothKernel_11x11.json"

PLOT_PATH="plots/plots_recovery/"



d_exp <- load_exp_data(includeLast = FALSE,removeSA = T,removeLearning = T) %>% 
  filter(scenario=='Memory Condition') %>% 
  select(UID,round,env,trial,chosen,x,y,z,prev_z,distance,age_bin) %>% 
  mutate(model='experimental') %>% 
  mutate(round =round-4)

d_simulated <- loadSimulatedData() %>% 
  mutate(distance=ifelse(distance==-1,NA,distance)) %>% 
  mutate(UID=id+1000) %>% 
  mutate(trial=step) %>% 
  select(UID,round,env,trial,chosen,x,y,z,prev_z,distance,model) %>% 
  mutate(age_bin='simulated')

d <- rbind(d_exp,d_simulated) %>% 
  mutate(origin=ifelse(model=='experimental','experimental','simulated'))

d <- d %>% 
  mutate(model = factor(model,levels=c('experimental','null','recency','surprise+','full'),
         labels= c('Experimental Memory', 'GP','GP + R','GP + S','GP + RS')))
 #
d_recency<- d %>% filter(model %in% c('Experimental Memory','GP + R'))





################################################################################################
# Learning
################################################################################################

dLearningCurves <- ddply(d, ~UID+trial+model+age_bin, plyr::summarize, meanReward = mean(z)+.5) 
pRewardCurve <- ggplot(dLearningCurves, aes(x=trial, y=meanReward, color=model, fill=model, group=model))+
  stat_summary(fun = mean, geom='line', size=.5)+
  stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha=0.3, color=NA)+
  geom_hline(yintercept=0.5, linetype='dashed', color='red') + # random choice model
  xlab('Trial')+
  ylab('Normalized Reward ± 95% CI')+
  labs(color='Age Group') +
  labs(fill='Age Group') +
  # scale_color_viridis(discrete=TRUE, direction=1) +
  # scale_fill_viridis(discrete=TRUE, direction=1) +
  scale_color_manual(values = modelPalette, name ='Condition')+
  scale_fill_manual(values = modelPalette, name ='Condition')+
  # ggtitle('Learning Curves') +
  theme_classic()  +
  # theme(strip.background=element_blank()) #, legend.position='none'
  theme(legend.position = c(0.05,1), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pRewardCurve

dDistCurves <- ddply(d, ~UID+trial+model, plyr::summarize, meanDist = mean(distance, na.rm=T)) 
pDistCurves <- ggplot(dDistCurves, aes(x=trial, y=meanDist, color=model, fill=model))+
  stat_summary(fun = mean, geom='line', size=.5)+
  stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha=0.3, color=NA)+
  xlab('Trial')+
  ylab('Avg. Search Distance')+
  scale_color_manual(values = modelPalette, name ='Condition')+
  scale_fill_manual(values = modelPalette, name ='Condition')+
  # ggtitle('Search Distance') +
  theme_classic()  +
  theme(legend.position = 'none')
pDistCurves

################################################################################################
# Meta-Learning
################################################################################################
d_noTraining <- d %>% 
  group_by(round,UID,age_bin,origin,model) %>% 
  summarize(z=mean(z+.5),meanDist=mean(distance))

dReward_scenario_x_rounds <- d_noTraining %>% group_by(round,age_bin,model) %>% summarize(z=mean(z))
pRoundLearning <- ggplot(dReward_scenario_x_rounds,aes(x=round,y=z,color=model,fill=model))+
  coord_cartesian(ylim=c(0.5,0.75))+
  # scale_color_manual(values = scenarioPalette, name ='', labels = c('Control', 'Memory'))+
  # scale_fill_manual(values = scenarioPalette, name = '', labels = c('Control', 'Memory'))+
  scale_color_manual(values = modelPalette, name ='Condition')+
  scale_fill_manual(values = modelPalette, name ='Condition')+
  labs(y='Mean reward',x='Experiment round')+
  geom_smooth(alpha=0.2)+
  geom_vline(xintercept = 1, linetype='dashed', color='red')+
  scale_x_continuous(limits = c(1,11),breaks=seq(1,15,2))+
  theme_classic()  +
  theme(legend.position = 'none')
pRoundLearning


################################################################################################
# Unique options
################################################################################################

# variance over options
# don't count first randomly revealed tile as unique option
dUniqueOpts = data.frame()
for (i in unique(d$UID)) {
  for (r in unique(subset(d, UID==i)$round)) {
    for(sc in unique(subset(subset(d,UID==i),round==r)$model)){
      subd = subset(d, UID==i & round==r & model==sc)
      if(!is.na(subd)){
        uniqueOpts = unique(subset(subd, trial!=1)$chosen)
        firstTile = subset(subd, trial==1)$chosen
        nUniqueOpts = ifelse(firstTile %in% uniqueOpts, length(uniqueOpts)-1, length(uniqueOpts))
        
        dat = data.frame(UID=i, round=r, model=sc,
                         age_bin=unique(subd$age_bin),
                         nUniqueOpt=nUniqueOpts)
        dUniqueOpts = rbind(dUniqueOpts, dat)
      }
    }
  }
}

dMeanUniqueOpts = ddply(dUniqueOpts, ~UID+age_bin+model, plyr::summarize,
                        mUniqueOpt=mean(nUniqueOpt))
randomUnique<- mean(sapply(1:10000, FUN=function(i) length(unique(sample(1:121, 25, replace = T)))))


pUniqueOptsDist <- ggplot(dUniqueOpts,aes(x=nUniqueOpt,fill=model))+
  geom_histogram(position = 'dodge')+
  # geom_density(position='stack')+
  # scale_fill_viridis(discrete=TRUE, direction=1) +
  scale_color_manual(values = modelPalette, name ='Condition')+
  scale_fill_manual(values = modelPalette, name ='Condition')+
  coord_cartesian(xlim=c(0,25))+
  labs(x='Unique Options',y='Total Occurrences',fill='Age group')+
  geom_vline(xintercept=25, linetype='dashed', color='red')+
  geom_vline(xintercept=randomUnique, linetype='dotted', color='red')+
  theme_classic() +
  theme(strip.background=element_blank(), axis.line = element_line(),
        axis.ticks = element_line())+
  theme(legend.position = c(0.1,0.9), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())

pUniqueOptsDist

dMeanUniqueOptsRound = ddply(dUniqueOpts, ~model, plyr::summarize,
                             mUniqueOpt=mean(nUniqueOpt))

pUniqueRound <- ggplot(dUniqueOpts,aes(x=model,y=nUniqueOpt,color=model))+
  geom_boxplot(outlier.shape = NA)+  
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_color_manual(values = modelPalette, name ='Condition')+
  scale_fill_manual(values = modelPalette, name ='Condition')+
  geom_hline(yintercept=randomUnique, linetype='dotted', color='red') +
  geom_hline(yintercept=25, linetype='dashed', color='red') +
  labs(x='Model',y='Unique Options',color='Age group')+
  theme_classic()+
  theme(legend.position = 'none')
pUniqueRound

# Repeats by age group
dRepeats <- d %>% 
  filter(distance==0) %>% 
  group_by(UID,model) %>% 
  mutate(repeats= ifelse(model=='Experimental Memory',n()/14,n()/7)) %>% 
  distinct(repeats) 
# %>% 
#   group_by(age_bin) %>% 
# summarise(mean_repeats=mean(repeats),sd_repeats=sd(repeats))
pRepeatsPerRound <- ggplot(data=dRepeats,mapping=aes(x=model,y=repeats, color=model))+theme_classic()+
  # scale_color_viridis(discrete = T)+
  geom_boxplot(outlier.shape = NA)+  
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_color_manual(values = modelPalette, name ='Condition')+
  scale_fill_manual(values = modelPalette, name ='Condition')+
  geom_hline(yintercept=0, linetype='dashed', color='red') +
  labs(x='Model',y='Repeats',color='Age group')+
  theme(legend.position = 'none')
pRepeatsPerRound



################################################################################################
# Search decision types
################################################################################################


dDecisionType <- d %>% 
  mutate(type_choice= ifelse(distance==0,'Repeat',
                             ifelse(distance %in% c(1), 'Near', 'Far'))) %>% 
  
  filter(age_bin!='2018_Data') %>% 
  filter(!is.na(type_choice))

dDecisionProbs = ddply(subset(dDecisionType, !is.na(type_choice)), ~age_bin+UID+model, plyr::summarize,
                       prob=prop.table(table(type_choice)),
                       type_choice=names(table(type_choice)))

dDecisionProbs <- dDecisionProbs %>%
  mutate(type_choice = factor(type_choice,levels = c('Repeat','Near','Far')))



dDecisionCount <- dDecisionType %>% 
  group_by(age_bin,UID,type_choice,model) %>% 
  summarize(countChoice=n(),z=mean(z),prev_z=mean(prev_z)) %>%
  mutate(type_choice = factor(type_choice,levels = c('Repeat','Near','Far')))

dDecision <- merge(dDecisionProbs,dDecisionCount,by=c('UID','age_bin','type_choice','model'))

dDecision_ext <- data.frame()
for(p in unique(dDecision$UID)){
  new_row<-NA
  temp_sum <- dDecision %>% filter(UID==p)
  temp_sum <- data.frame(temp_sum) 
  for(tc in c('Repeat','Near','Far')){
    for(sc in unique(d$model)){
      if(nrow(dDecision %>% filter(UID==p) %>% filter(type_choice==tc) %>% filter(model==sc))==0){
        
        temp_row <- data.frame(age_bin=unique(temp_sum$age_bin), UID=p,type_choice=tc,model=sc,countChoice=0,z=NA,prev_z=NA,prob=0)
        # print(temp_row)
        if(is.na(new_row)){
          new_row <- temp_row
        } else{
          new_row<- rbind(new_row,temp_row)
        }
      }
    }
  }
  if(!is.na(new_row)){
    dDecision_ext<- rbind(dDecision_ext,temp_sum,new_row)
  }else{
    dDecision_ext<- rbind(dDecision_ext,temp_sum)
  }
}

dDecision<- dDecision_ext


randBaseline = data.frame(decision = c('Repeat', 'Near', 'Far'),
                          prob = c(1/121, 4/121, 116/121),
                          group = rep('Random\nChoice\nModel', 3))


pDecisionType = ggplot() +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=prob, color=model, group=model),
               fun.y=mean, geom='line') +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=prob, color=model, group=model),
               fun.data=mean_cl_boot, show.legend=F) +
  geom_point(randBaseline, mapping=aes(x=decision, y=prob, group=group, fill=group), color='red', show.legend=FALSE) +
  geom_line(randBaseline, mapping=aes(x=decision, y=prob, group=group), linetype='dashed', color='red',  show.legend=FALSE) +
  scale_y_continuous() +
  xlab('Search Distance') +
  ylab('P(Search Distance)') +
  # coord_cartesian(ylim=c(0.1,0.5))+
  labs(color='Age Group', fill='') +
  # scale_color_viridis(discrete=TRUE, direction=1) +
  scale_color_manual(values = modelPalette, name ='Condition')+
  scale_fill_manual(values = modelPalette, name ='Condition')+
  theme_classic() +
  theme(strip.background=element_blank())+ #, legend.position='none'+
  theme(legend.position = c(0.1,0.9), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())


pDecisionType



pDecisionType_x_Reward = ggplot() +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=z, color=model, group=model),
               fun.y=mean, geom='line') +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=z, color=model, group=model),
               fun.data=mean_cl_boot, show.legend=F) +
  labs(color='Age Group', fill='') +
  xlab('Search Distance') +
  ylab('Current reward') +
  # scale_color_viridis(discrete=TRUE, direction=1) +
  scale_color_manual(values = modelPalette, name ='Condition')+
  scale_fill_manual(values = modelPalette, name ='Condition')+
  coord_cartesian(ylim=c(0.0,0.5))+
  geom_hline(yintercept=0, linetype='dashed', color='red') +
  theme_classic() +
  theme(strip.background=element_blank())+
  theme(legend.position = 'none')
pDecisionType_x_Reward



pDecisionType_x_prevReward = ggplot() +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=prev_z, color=model, group=model),
               fun.y=mean, geom='line') +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=prev_z, color=model, group=model),
               fun.data=mean_cl_boot, show.legend=F) +
  labs(color='Age Group', fill='') +
  xlab('Search Distance') +
  ylab('Previous reward') +
  # scale_color_viridis(discrete=TRUE, direction=1) +
  scale_color_manual(values = modelPalette, name ='Condition')+
  scale_fill_manual(values = modelPalette, name ='Condition')+
  geom_hline(yintercept=0, linetype='dashed', color='red') +
  coord_cartesian(ylim=c(0.0,0.5))+
  theme_classic() +
  theme(strip.background=element_blank())+
  theme(legend.position = 'none')

pDecisionType_x_prevReward


pValidation <- cowplot::plot_grid(pRewardCurve,pDistCurves,
                                  pUniqueOptsDist,pUniqueRound,
                                  pRepeatsPerRound,pDecisionType,
                                  pDecisionType_x_Reward,pDecisionType_x_prevReward,
                                  ncol=2,labels='auto')
pValidation
ggsave(filename = paste0(PLOT_PATH,"model_validation.png"), plot=pValidation, height=16, width=9, units = "in")
ggsave(filename = paste0(PLOT_PATH,"model_validation.pdf"), plot=pValidation, height=16, width=9, units = "in")
