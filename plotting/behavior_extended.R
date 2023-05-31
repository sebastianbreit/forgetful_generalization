# Plot behavioral results
# house keeping
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'viridis' , 'brms', 'lme4', 'sjPlot', 'cowplot','tidybayes','modelr','lmerTest','xtable','scales') #'cowplot',  'entropy', 'withr', 'ggbeeswarm','jsonlite'
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

COLOR_CONDITION_BASELINE = scenarioPalette[1]
COLOR_CONDITION_MEMORY = scenarioPalette[2]

COLOR_MODEL_BASIC = cbPalette[1]
COLOR_MODEL_RECENCY = cbPalette[2]
COLOR_MODEL_SURPRISE = cbPalette[3]
COLOR_MODEL_SURPRISE_PLUS = cbPalette[4]


KERNEL_8= "data/env_generation/smoothKernel_8x8.json"
KERNEL_11= "data/env_generation/smoothKernel_11x11.json"

PLOT_PATH="plots/plots_behavior/"



modelpath = 'brmsModels'
run_model <- function(expr, modelName, path=modelpath, reuse = TRUE) {
  path <- paste0(path,'/', modelName, ".brm")
  if (reuse) {
    fit <- suppressWarnings(try(readRDS(path), silent = TRUE))
    print("read worked")
  }
  if (is(fit, "try-error")) {
    print("read error")
    fit <- eval(expr)
    saveRDS(fit, file = path)
  }
  fit
}


################################################################################################
# Regression: Exploration efficiency 
################################################################################################
d <- load_exp_data(removeSA = T, removeLearning = T)
mExploreEff <- run_model(brm(z ~ prev_z + age + scenario + prev_z:age + prev_z:scenario + (1|id),
                             data =d, cores=4,
                             iter = 4000, warmup = 1000, 
                             control = list(adapt_delta = 0.99)),
                         modelName=paste0('brm_explorationEfficiency'))
summary(mExploreEff)




################################################################################################
# Meta-Learning
################################################################################################
d_metalearning <- load_exp_data(includeLast = FALSE,removeSA = T,removeLearning = F)


d_withTraining <- d_metalearning %>% 
  group_by(round,UID,age,age_bin,scenario,Country.of.residence) %>% 
  summarize(z=mean(z+.5),meanDist=mean(distance))

dReward_scenario_x_rounds <- d_withTraining %>% group_by(scenario,round,age_bin,UID) %>% summarize(z=mean(z))
pRoundLearning <- ggplot(dReward_scenario_x_rounds,aes(x=round,y=z,fill=scenario,color=scenario))+
  coord_cartesian(ylim=c(0.65,0.75))+
  scale_color_manual(values = scenarioPalette, name ='', labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, name = '', labels = c('Control', 'Memory'))+
  labs(y='Mean reward',x='Experiment round')+
  geom_smooth(alpha=0.2)+
  geom_vline(xintercept = 5, linetype='dashed', color='red')+
  scale_x_continuous(limits = c(1,15),breaks=seq(1,15,2))+
  theme_classic()  +
  theme(legend.position = c(0.7,0.3), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pRoundLearning

# pRoundLearning_Age <- ggplot(dReward_scenario_x_rounds,aes(x=round,y=z,fill=age_bin,color=age_bin))+
#   facet_grid(scenario ~ .)+
#   # coord_cartesian(ylim=c(0.65,0.75))+
#   scale_color_viridis(discrete = T)+
#   scale_fill_viridis(discrete = T)+
#   geom_smooth(alpha=0.1)
# pRoundLearning_Age

anova_roundlearning <- aov(z~round*scenario*age, data=d_withTraining)
summary(anova_roundlearning)
print(xtable((anova_roundlearning), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4
corTestPretty(d_withTraining$z,d_withTraining$round)

# ggplot(data=d_roundlearning,mapping = aes(x=round,y=meanReward,fill=Country.of.residence))+
#   stat_summary(fun = mean, geom='line', size=.5)+
#   stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha=0.3, color=NA)

#### Exclude first 4 rounds from data?
d_noTraining <- d_withTraining %>% filter(round>4) 

anova_round_x_age_filtered <- aov(z ~ age*round*scenario, data = d_noTraining)
summary(anova_round_x_age_filtered)
print(xtable((anova_round_x_age_filtered), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4

corTestPretty(d_noTraining$z,d_noTraining$round)


dReward_scenario_x_rounds_fixed <- d_noTraining %>% group_by(scenario,round,UID) %>% summarize(z=mean(z))
pRoundLearning_fixed <- ggplot(dReward_scenario_x_rounds_fixed,aes(x=round,y=z,fill=scenario,color=scenario))+
  coord_cartesian(ylim=c(0.65,0.75))+
  scale_color_manual(values = scenarioPalette, name ='', labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, name = '', labels = c('Control', 'Memory'))+
  labs(y='Mean reward',x='Experiment round')+
  geom_smooth(alpha=0.2)+
  geom_vline(xintercept = 5, linetype='dashed', color='red')+
  scale_x_continuous(limits = c(1,15),breaks=seq(5,15,2))+
  theme_classic()  +
  theme(legend.position = 'none')
  # theme(legend.position = c(0.1,0.9), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pRoundLearning_fixed

pLearning <- cowplot::plot_grid(pRoundLearning,pRoundLearning_fixed, ncol=2, labels='auto')
pLearning
ggsave(filename = paste0(PLOT_PATH,"learning_rounds.pdf"), plot=pLearning, height=3.5, width=10, units = "in")
ggsave(filename = paste0(PLOT_PATH,"learning_rounds.png"), plot=pLearning, height=3.5, width=10, units = "in")







################################################################################################
# Unique options
################################################################################################

d <- load_exp_data(removeSA = T, removeLearning = T)
# variance over options
# don't count first randomly revealed tile as unique option
dUniqueOpts = data.frame()
for (i in unique(d$id)) {
  for (r in unique(subset(d, id==i)$round)) {
    subd = subset(d, id==i & round==r)
    
    uniqueOpts = unique(subset(subd, trial!=1)$chosen)
    firstTile = subset(subd, trial==1)$chosen
    nUniqueOpts = ifelse(firstTile %in% uniqueOpts, length(uniqueOpts)-1, length(uniqueOpts))
    
    dat = data.frame(id=i, round=r, 
                     age=unique(subd$age),
                     age_bin=unique(subd$age_bin),
                     age_bin_numeric=unique(subd$age_bin_numeric),
                     scenario=unique(subd$scenario),
                     nUniqueOpt=nUniqueOpts)
    dUniqueOpts = rbind(dUniqueOpts, dat)
  }
}

dMeanUniqueOpts = ddply(dUniqueOpts, ~id+age+age_bin+scenario+round, plyr::summarize,
                        mUniqueOpt=mean(nUniqueOpt))
randomUnique<- mean(sapply(1:10000, FUN=function(i) length(unique(sample(1:121, 25, replace = T)))))

# dMeanUniqueOpts_onlyNonUnique <- dMeanUniqueOpts %>% filter(mUniqueOpt<25)

# pUniqueOpts <- ggplot(dMeanUniqueOpts, aes(x=age, y=mUniqueOpt, color = age_bin)) +
#   geom_point(aes(group=id), alpha=0.5, size =0.5) +
#   geom_smooth(color = '#377eb8', fill = '#377eb8', size=.5) +
#   geom_hline(yintercept=randomUnique, linetype='dotted', color='red') +
#   geom_hline(yintercept=25, linetype='dashed', color='red') +
#   coord_cartesian(ylim=c(0,25))+
#   scale_x_continuous(limits = c(18,78),breaks=seq(18,78,10)) +
#   scale_color_viridis(discrete=TRUE, direction=1) +
#   # xlab('Age (Years)') +
#   # ylab('Unique Options') +
#   # ggtitle('Unique Options per Round') +
#   labs(x='Age (Years)',y='Unique Options',color='Age group')+
#   theme_classic() +
#   theme(strip.background=element_blank(), axis.line = element_line(),
#         axis.ticks = element_line())+
#   theme(legend.position = 'none')
# 
# pUniqueOpts
# 
# # No simple linear correlation between age and unique options
# corTestPretty(dMeanUniqueOpts$age,dMeanUniqueOpts$mUniqueOpt)


pUniqueOptsDist <- ggplot(dUniqueOpts,aes(x=nUniqueOpt,fill=age_bin))+
  geom_histogram(position = 'dodge')+
  # geom_density(position='stack')+
  scale_fill_viridis(discrete=TRUE, direction=1) +
  coord_cartesian(xlim=c(0,25))+
  labs(x='Unique Options',y='Total Occurrences',fill='Age group')+
  geom_vline(xintercept=25, linetype='dashed', color='red')+
  geom_vline(xintercept=randomUnique, linetype='dotted', color='red')+
  theme_classic() +
  theme(strip.background=element_blank(), axis.line = element_line(),
        axis.ticks = element_line())+
  theme(legend.position = c(0.1,0.9), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())

pUniqueOptsDist

dMeanUniqueOptsRound = ddply(dUniqueOpts, ~age_bin+round+age_bin_numeric+scenario, plyr::summarize,
                             mUniqueOpt=mean(nUniqueOpt))

pUniqueAgeBoxplot <- ggplot(dUniqueOpts,aes(x=age_bin,y=nUniqueOpt,color=age_bin,fill=age_bin))+
  # geom_smooth(method = 'lm')+
  geom_boxplot(fill=NA,outlier.alpha = 0)+
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  geom_hline(yintercept=randomUnique, linetype='dotted', color='red') +
  geom_hline(yintercept=25, linetype='dashed', color='red') +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  # coord_cartesian(ylim=c(15,25))+
  # scale_x_continuous(limits = c(5, 15), breaks = seq(5, 15, 2))+
  labs(x='Age Group',y='Unique Options',color='Age group',fill='Age group')+
  theme_classic()+
  theme()
pUniqueAgeBoxplot


pUniqueScenario <- ggplot(dUniqueOpts,aes(x=round,y=nUniqueOpt,color=scenario,fill=scenario))+
  geom_smooth(method = 'lm')+
  # scale_color_viridis(discrete=TRUE, direction=1) +
  scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
  geom_hline(yintercept=randomUnique, linetype='dotted', color='red') +
  geom_hline(yintercept=25, linetype='dashed', color='red') +
  coord_cartesian(ylim=c(15,25))+
  scale_x_continuous(limits = c(5, 15), breaks = seq(5, 15, 2))+
  labs(x='Experiment round',y='Unique Options',color='Condition',fill='Condition')+
  theme_classic()+
  theme()
pUniqueScenario


pUniquePerAge <- ggplot(data=dMeanUniqueOpts,mapping=aes(x=age,y=mUniqueOpt,color=scenario,fill=scenario))+theme_classic()+
  geom_hline(yintercept=randomUnique, linetype='dotted', color='red') +
  geom_hline(yintercept=25, linetype='dashed', color='red') +
  scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
  geom_smooth()+
  # scale_x_discrete(guide = guide_axis(n.dodge=2))+
  coord_cartesian(xlim=c(18,68))+
  labs(x='Age group',y='Unique Options',color='Age group',fill='Age group')+
  theme()
pUniquePerAge

# dRepeatsRound <- d %>% 
#   filter(distance==0) %>% 
#   group_by(UID,age_bin,age,scenario,round) %>% 
#   mutate(repeats= n()) %>% 
#   distinct(repeats) 
# 
# dRepeatsCorrected <- data.frame()
# for(rip_id in unique(d$UID)){
#     for(r in unique(d$round)){
#       subd <- d %>%  filter(UID==rip_id) %>% filter(round==r)
#       subRepeats <- dRepeatsRound %>%  filter(UID==rip_id) %>% filter(round==r)
#       repeat_cache <- 0
#       
#       if(nrow(subRepeats)!=0){
#         repeat_cache<- subRepeats$repeats
#       }
#       dat = data.frame(UID=rip_id, round=r, 
#                        age=unique(subd$age),
#                        age_bin=unique(subd$age_bin),
#                        scenario=unique(subd$scenario),
#                        repeats=repeat_cache
#                         )
#       dRepeatsCorrected = rbind(dRepeatsCorrected, dat)
#     
#   }
# }
# 
# 
# pRepeatsPerRoundAge <- ggplot(data=dRepeatsCorrected,mapping=aes(x=age,y=repeats,color=scenario,fill=scenario))+theme_classic()+
#   scale_color_viridis(discrete = T)+
#   # geom_boxplot(outlier.shape = NA)+  
#   scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
#   scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
#   geom_smooth()+
#   # scale_x_discrete(guide = guide_axis(n.dodge=2))+
#   labs(x='Age (years)',y='Repeats',color='Condition',fill='Condition')+
#   coord_cartesian(xlim=c(18,68))+
#   theme()
# pRepeatsPerRoundAge
# 
# pRepeatsPerRoundScenario <- ggplot(data=dRepeatsCorrected,mapping=aes(x=round,y=repeats,color=scenario,fill=scenario))+theme_classic()+
#   scale_color_viridis(discrete = T)+
#   # geom_boxplot(outlier.shape = NA)+  
#   scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
#   scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
#   geom_smooth()+
#   # scale_x_discrete(guide = guide_axis(n.dodge=2))+
#   labs(x='Experiment round',y='Repeats',color='Condition',fill='Condition')+
#   scale_x_continuous(limits = c(5, 15), breaks = seq(5, 15, 2))+
#   theme()
# pRepeatsPerRoundScenario


# test <- aov(repeats~age*scenario*round,data=dRepeatsCorrected)
# summary(test)

# pUniqueBC <- cowplot::plot_grid(pUniqueScenario,pUniquePerAge,ncol=2,labels=c('b','c'))

pUniqueSummary <- cowplot::plot_grid(pUniqueOptsDist,pUniqueAgeBoxplot, pUniqueScenario,pUniquePerAge,
                                     ncol=2, labels='auto')
pUniqueSummary
ggsave(filename = paste0(PLOT_PATH,"uniqueOpts.png"), plot=pUniqueSummary, height=7, width=13, units = "in")
ggsave(filename = paste0(PLOT_PATH,"uniqueOpts.pdf"), plot=pUniqueSummary, height=7, width=13, units = "in")



anova_unique <- aov(mUniqueOpt~age*scenario*round,data=dMeanUniqueOpts)
summary(anova_unique)
print(xtable((anova_unique), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4
# No direct corrlation
corTestPretty(dMeanUniqueOptsRound$age_bin_numeric,dMeanUniqueOptsRound$mUniqueOpt)
corTestPretty(dMeanUniqueOptsRound$round,dMeanUniqueOptsRound$mUniqueOpt)

# Difference between oldest 2 and rest
ttestPretty(subset(dMeanUniqueOptsRound,age_bin %in% c('68-77'))$mUniqueOpt,
            subset(dMeanUniqueOptsRound,!age_bin %in% c('68-77'))$mUniqueOpt,
            bonferroni=6)

ttestPretty(subset(dMeanUniqueOptsRound,age_bin %in% c('58-67'))$mUniqueOpt,
            subset(dMeanUniqueOptsRound,!age_bin %in% c('58-67'))$mUniqueOpt,
            bonferroni=6)

ttestPretty(subset(dMeanUniqueOptsRound,age_bin %in% c('48-57'))$mUniqueOpt,
            subset(dMeanUniqueOptsRound,!age_bin %in% c('48-57'))$mUniqueOpt,
            bonferroni=6)



################################################################################################
# Search decision types
################################################################################################


dDecisionType <- d %>% 
  mutate(type_choice= ifelse(distance==0,'Repeat',
                             ifelse(distance %in% c(1), 'Near', 'Far'))) %>% 
  
  filter(age_bin!='2018_Data') %>% 
  filter(!is.na(type_choice))

dDecisionProbs = ddply(subset(dDecisionType, !is.na(type_choice)), ~age_bin+id+scenario, plyr::summarize,
                       prob=prop.table(table(type_choice)),
                       type_choice=names(table(type_choice)))

dDecisionProbs <- dDecisionProbs %>%
  mutate(type_choice = factor(type_choice,levels = c('Repeat','Near','Far')))



dDecisionCount <- dDecisionType %>% 
  group_by(age_bin,id,type_choice,scenario,age) %>% 
  summarize(countChoice=n(),z=mean(z),prev_z=mean(prev_z)) %>%
  mutate(type_choice = factor(type_choice,levels = c('Repeat','Near','Far')))

dDecision <- merge(dDecisionProbs,dDecisionCount,by=c('id','age_bin','type_choice','scenario'))

dDecision_ext <- data.frame()
for(p in unique(dDecision$id)){
  new_row<-NA
  temp_sum <- dDecision %>% filter(id==p)
  temp_sum <- data.frame(temp_sum) 
  for(tc in c('Repeat','Near','Far')){
    for(sc in c('Baseline Condition','Memory Condition')){
      if(nrow(dDecision %>% filter(id==p) %>% filter(type_choice==tc) %>% filter(scenario==sc))==0){
        
        temp_row <- data.frame(age_bin=unique(temp_sum$age_bin), id=p,type_choice=tc,scenario=sc,age=unique(temp_sum$age),countChoice=0,z=NA,prev_z=NA,prob=0)
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


pDecisionTypeScenario = ggplot() +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=prob, color=scenario, group=scenario),
               fun.y=mean, geom='line') +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=prob, color=scenario, group=scenario),
               fun.data=mean_cl_boot, show.legend=F) +
  geom_point(randBaseline, mapping=aes(x=decision, y=prob, group=group, fill=group), color='red', show.legend=FALSE) +
  geom_line(randBaseline, mapping=aes(x=decision, y=prob, group=group), linetype='dashed', color='red',  show.legend=FALSE) +
  scale_y_continuous() +
  xlab('Search Distance') +
  ylab('P(Search Distance)') +
  # coord_cartesian(ylim=c(0.1,0.5))+
  labs(color='Age Group', fill='') +
  scale_color_manual(values = scenarioPalette, name ='', labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, name = '', labels = c('Control', 'Memory'))+
  theme_classic() +
  theme(strip.background=element_blank())+ #, legend.position='none'+
  theme(legend.position = c(0.1,0.9), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())


pDecisionTypeScenario


dDecisionDiff <- dDecision %>% 
  group_by(id,age_bin,type_choice) %>% summarise(probDiff =-diff(prob)) # -diff, because ordering is Memory, then Baseline

pDecisionTypeDiff = ggplot() +
  stat_summary(dDecisionDiff, mapping=aes(x=type_choice, y=probDiff, color=age_bin, group=age_bin),
               fun.y=mean, geom='line') +
  stat_summary(dDecisionDiff, mapping=aes(x=type_choice, y=probDiff, color=age_bin, group=age_bin),
               fun.data=mean_cl_boot, show.legend=F) +
  # geom_point(randBaseline, mapping=aes(x=decision, y=probDiff, group=group, fill=group), color='red', show.legend=FALSE) +
  # geom_line(randBaseline, mapping=aes(x=decision, y=probDiff, group=group), linetype='dashed', color='red',  show.legend=FALSE) +
  geom_hline(yintercept = 0,, linetype='dashed', color='red')+
  scale_y_continuous() +
  xlab('Search Distance') +
  ylab(expr(Delta*'P (Control - Memory)')) +
  # coord_cartesian(ylim=c(0.1,0.5))+
  labs(color='Age Group', fill='') +
  scale_color_viridis(discrete=TRUE, direction=1) +
  # ggtitle('Delta Control-Memory') +
  theme_classic() +
  theme(strip.background=element_blank(), legend.position='none')

pDecisionTypeDiff
# temp<-dDecision %>% group_by(age_bin,type_choice,scenario) %>% summarise(prob=mean(prob))

# pDecisionTypeScenario = ggplot() +
#   stat_summary(dDecision, mapping=aes(x=type_choice, y=prob, color=scenario, group=scenario),
#                fun.y=mean, geom='line') +
#   stat_summary(dDecision, mapping=aes(x=type_choice, y=prob, color=scenario, group=scenario),
#                fun.data=mean_cl_boot, show.legend=F) +
#   geom_point(randBaseline, mapping=aes(x=decision, y=prob, group=group, fill=group), color='red', show.legend=FALSE) +
#   geom_line(randBaseline, mapping=aes(x=decision, y=prob, group=group), linetype='dashed', color='red',  show.legend=FALSE) +
#   # geom_hline(yintercept = 0,, linetype='dashed', color='red')+
#   scale_y_continuous() +
#   xlab('Search Distance') +
#   ylab('P(Search Distance)') +
#   # coord_cartesian(ylim=c(0.1,0.5))+
#   # labs(color='Age Group', fill='') +
#   # scale_color_viridis(discrete=TRUE, direction=1) +
#   scale_color_manual(values = scenarioPalette, name ='', labels = c('Control', 'Memory'))+
#   scale_fill_manual(values = scenarioPalette, name = '', labels = c('Control', 'Memory'))+
#   # ggtitle('Delta Control-Memory') +
#   theme_classic() +
#   theme(strip.background=element_blank())+
#   theme(legend.position = c(0.1,0.9), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
# 
# 
# pDecisionTypeScenario


dDecision<- dDecision %>% 
  mutate(zDiff = z-prev_z)


pDecisionType_x_RewardDiff_age = ggplot() +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=zDiff, color=age_bin, group=age_bin),
               fun.y=mean, geom='line') +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=zDiff, color=age_bin, group=age_bin),
               fun.data=mean_cl_boot, show.legend=F) +
  labs(color='Age Group', fill='') +
  xlab('Search Distance') +
  ylab(expr(Delta*'Reward (Current - Previous)')) +
  # ggtitle("Control Condition") +
  scale_color_viridis(discrete=TRUE, direction=1) +
  # coord_cartesian(ylim=c(0.1,0.5))+
  geom_hline(yintercept = 0,color='red',linetype='dashed' )+
  theme_classic() +
  theme(strip.background=element_blank())+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        # panel.border=element_rect(colour = COLOR_CONDITION_BASELINE,fill=NA,size=3),C
        plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0.1,0.9), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
  
pDecisionType_x_RewardDiff_age

pDecisionType_x_RewardDiff_scenario = ggplot() +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=zDiff, color=scenario, group=scenario),
               fun.y=mean, geom='line') +
  stat_summary(dDecision, mapping=aes(x=type_choice, y=zDiff, color=scenario, group=scenario),
               fun.data=mean_cl_boot, show.legend=F) +
  labs(color='Age Group', fill='') +
  xlab('Search Distance') +
  ylab(expr(Delta*'Reward (Current - Previous)')) +
  # ggtitle("Control Condition") +
  # scale_color_viridis(discrete=TRUE, direction=1) +
  scale_color_manual(values = scenarioPalette, name ='', labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, name = '', labels = c('Control', 'Memory'))+
  # coord_cartesian(ylim=c(0.1,0.5))+
  geom_hline(yintercept = 0,color='red',linetype='dashed' )+
  theme_classic() +
  theme(strip.background=element_blank())+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        # panel.border=element_rect(colour = COLOR_CONDITION_BASELINE,fill=NA,size=3),
        plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0.1,0.9), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())

pDecisionType_x_RewardDiff_scenario


# pDecisionType_x_Reward = ggplot() +
#   stat_summary(dDecision, mapping=aes(x=type_choice, y=z, color=age_bin, group=age_bin),
#                fun.y=mean, geom='line') +
#   stat_summary(dDecision, mapping=aes(x=type_choice, y=z, color=age_bin, group=age_bin),
#                fun.data=mean_cl_boot, show.legend=F) +
#   labs(color='Age Group', fill='') +
#   xlab('Search Distance') +
#   ylab('Current reward') +
#   scale_color_viridis(discrete=TRUE, direction=1) +
#   coord_cartesian(ylim=c(0.1,0.5))+
#   theme_classic() +
#   theme(strip.background=element_blank())
# pDecisionType_x_Reward
# 
# 
# 
# pDecisionType_x_prevReward = ggplot() +
#   stat_summary(dDecision, mapping=aes(x=type_choice, y=prev_z, color=age_bin, group=age_bin),
#                fun.y=mean, geom='line') +
#   stat_summary(dDecision, mapping=aes(x=type_choice, y=prev_z, color=age_bin, group=age_bin),
#                fun.data=mean_cl_boot, show.legend=F) +
#   labs(color='Age Group', fill='') +
#   xlab('Search Distance') +
#   ylab('Previous reward') +
#   scale_color_viridis(discrete=TRUE, direction=1) +
#   coord_cartesian(ylim=c(0.1,0.5))+
#   theme_classic() +
#   theme(strip.background=element_blank())+
#   theme(legend.position = c(0.9,0.9), legend.justification=c(1,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
# 
# 
# pDecisionType_x_prevReward

dDecisionRewardDiff <- dDecision %>%
  group_by(id,age_bin,type_choice) %>%
  summarise(zDiff =-diff(z),prev_zDiff=-diff(prev_z)) # -diff, because ordering is Memory, then Baseline
#   
#   
# pDecisionDiff_x_prevReward = ggplot() +
#   stat_summary(dDecisionRewardDiff, mapping=aes(x=type_choice, y=prev_zDiff, color=age_bin, group=age_bin),
#                fun.y=mean, geom='line') +
#   stat_summary(dDecisionRewardDiff, mapping=aes(x=type_choice, y=prev_zDiff, color=age_bin, group=age_bin),
#                fun.data=mean_cl_boot, show.legend=F) +
#   labs(color='Age Group', fill='') +
#   xlab('Search Distance') +
#   ylab(expr(Delta*'Previous reward')) +
#   scale_color_viridis(discrete=TRUE, direction=1) +
#   geom_hline(yintercept = 0,color='red',linetype='dashed')+
#   # coord_cartesian(ylim=c(0.1,0.5))+
#   theme_classic() +
#   theme(strip.background=element_blank(), legend.position='none')
# 
# pDecisionDiff_x_prevReward
# ttestPretty(subset(dDecisionRewardDiff,!is.na(prev_zDiff))$prev_zDiff)
# 
# pDecisionDiff_x_Reward = ggplot() +
#   stat_summary(dDecisionRewardDiff, mapping=aes(x=type_choice, y=zDiff, color=age_bin, group=age_bin),
#                fun.y=mean, geom='line') +
#   stat_summary(dDecisionRewardDiff, mapping=aes(x=type_choice, y=zDiff, color=age_bin, group=age_bin),
#                fun.data=mean_cl_boot, show.legend=F) +
#   labs(color='Age Group', fill='') +
#   xlab('Search Distance') +
#   ylab(expr(Delta*'Current reward')) +
#   scale_color_viridis(discrete=TRUE, direction=1) +
#   geom_hline(yintercept = 0,color='red',linetype='dashed')+
#   # coord_cartesian(ylim=c(0.1,0.5))+
#   theme_classic() +
#   theme(strip.background=element_blank(), legend.position='none')
# 
# pDecisionDiff_x_Reward
# 
# 
# pDecisionType_x_Reward_x_Scenario = ggplot() +
#   stat_summary(dDecision, mapping=aes(x=type_choice, y=z, color=scenario, group=scenario),
#                fun.y=mean, geom='line') +
#   stat_summary(dDecision, mapping=aes(x=type_choice, y=z, color=scenario, group=scenario),
#                fun.data=mean_cl_boot, show.legend=F) +
#   labs(color='Age Group', fill='') +
#   xlab('Search Distance') +
#   ylab('Current reward') +
#   scale_color_viridis(discrete=TRUE, direction=1) +
#   coord_cartesian(ylim=c(0.1,0.5))+  
#   scale_color_manual(values = scenarioPalette, name ='', labels = c('Control', 'Memory'))+
#   scale_fill_manual(values = scenarioPalette, name = '', labels = c('Control', 'Memory'))+
#   # ggtitle('Delta Control-Memory') +
#   theme_classic() +
#   theme(strip.background=element_blank(), legend.position='none')
# pDecisionType_x_Reward_x_Scenario
# 
# pDecisionType_x_prevReward_x_Scenario = ggplot() +
#   stat_summary(dDecision, mapping=aes(x=type_choice, y=prev_z, color=scenario, group=scenario),
#                fun.y=mean, geom='line') +
#   stat_summary(dDecision, mapping=aes(x=type_choice, y=prev_z, color=scenario, group=scenario),
#                fun.data=mean_cl_boot, show.legend=F) +
#   labs(color='Age Group', fill='') +
#   xlab('Search Distance') +
#   ylab('Previous reward') +
#   scale_color_manual(values = scenarioPalette, name ='Condition', labels = c('Control', 'Memory'))+
#   scale_fill_manual(values = scenarioPalette, name = 'Condition', labels = c('Control', 'Memory'))+
#   coord_cartesian(ylim=c(0.1,0.5))+
#   theme_classic() +
#   theme(strip.background=element_blank())+
#   theme(legend.position = c(0.9,0.9), legend.justification=c(1,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
# 
# pDecisionType_x_prevReward_x_Scenario

dTypeReward <- dDecision %>% 
  filter(!is.na(z)) %>% 
  mutate(age_bin_numeric= ifelse(age_bin=='18-27',20,
                                 ifelse(age_bin=='28-37',30,
                                        ifelse(age_bin=='38-47',40,
                                               ifelse(age_bin=='48-57',50,
                                                      ifelse(age_bin=='58-67',60,70))))))

dRepeatReward<-dTypeReward %>% filter(type_choice=='Repeat')
# temp_anova <- aov(z~scenario*age_bin_numeric,data=dRepeatReward)
# summary(temp_anova)


dNearReward<-dTypeReward %>% filter(type_choice=='Near')
# ttestPretty(subset(dNearReward,age_bin=='18-27')$z,subset(dNearReward,age_bin=='68-77')$z)
# ttestPretty(subset(dNearReward,scenario=='Baseline Condition')$z,subset(dNearReward,scenario=='Memory Condition')$z)
# corTestPretty(dNearReward$prev_z,dNearReward$age_bin_numeric)
# corTestPretty(dNearReward$prev_z,dNearReward$age)

# temp_anova <- aov(prob~scenario*age_bin_numeric,data=dNearReward)
# summary(temp_anova)


dFarReward<-dTypeReward %>% filter(type_choice=='Far')

# temp_anova <- aov(prob~scenario*age_bin_numeric,data=dFarReward)
# summary(temp_anova)




# Not sure if this one helps much
# anova_decision_x_reward <- aov(z ~ age*type_choice*scenario, data = dTypeReward)
# summary(anova_decision_x_reward)

# A) No probability differences by age
dTypeRewardDiff_prob <- dTypeReward  %>% 
  group_by(id,age_bin_numeric,age,type_choice) %>% summarise(probDiff =-diff(prob)) # -diff, because ordering is Memory, then Baseline
anova_decision_x_probDiff <- aov(probDiff ~ age*type_choice, data = dTypeRewardDiff_prob)
summary(anova_decision_x_probDiff)
print(xtable((anova_decision_x_probDiff), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4


# # NONE) For reward, we have many effects, too unclear how/what to report
# anova_decision_x_z <- aov(z ~ age*type_choice*scenario, data = dTypeReward)
# summary(anova_decision_x_z)
# 
# # For repeat,
# # we have a correlation between age~reward
# # we have a difference between conditions
# corTestPretty(dRepeatReward$z,dRepeatReward$age_bin_numeric)
# corTestPretty(dRepeatReward$z,dRepeatReward$age)
# ttestPretty(subset(dRepeatReward,scenario=='Baseline Condition')$z,subset(dRepeatReward,scenario=='Memory Condition')$z)
# 


anova_decision_x_zDiff <- aov(zDiff ~ age*type_choice*scenario, data = dTypeReward)
summary(anova_decision_x_zDiff)
print(xtable((anova_decision_x_zDiff), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4


# Only Near shows a reward difference with age, which disappears with Bonferroni-correction
# corTestPretty(subset(dTypeReward,type_choice=='Repeat')$zDiff,subset(dTypeReward,type_choice=='Repeat')$age,bonferroni=3)
corTestPretty(subset(dTypeReward,type_choice=='Near')$zDiff,subset(dTypeReward,type_choice=='Near')$age,bonferroni=3)
# corTestPretty(subset(dTypeReward,type_choice=='Far')$zDiff,subset(dTypeReward,type_choice=='Far')$age,bonferroni=3)


anova_decision_x_probs <- aov(prob ~ age*type_choice*scenario, data = dTypeReward)
summary(anova_decision_x_probs)
print(xtable((anova_decision_x_probs), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4

# For repeat,
# we have NO correlation with age~prob

# For near, 
# we have a correlation between age and probability
# Older adults over-explore more
# cor.test(subset(dTypeReward,type_choice=='Near')$prob,subset(dTypeReward,type_choice=='Near')$age_bin_numeric)
corResult <- cor.test(subset(dTypeReward,type_choice=='Near')$prob,subset(dTypeReward,type_choice=='Near')$age)
corResult$p.value<- p.adjust(corResult$p.value,'bonferroni',3)
corResult
# For far, 
# we have a NO correlation between age and probability





# pChoicetype_simple <- cowplot::plot_grid(pDecisionType,pDecisionTypeScenario,pDecisionTypeDiff, ncol=3, labels='auto')
# pChoicetype_extended <- cowplot::plot_grid(pDecisionType_x_prevReward_x_Scenario,pDecisionType_x_prevReward,pDecisionDiff_x_prevReward,pDecisionType_x_Reward_x_Scenario,pDecisionType_x_Reward,pDecisionDiff_x_Reward, ncol=3, labels='auto')
# 
# pChoicetype_simple
# ggsave(filename = paste0(PLOT_PATH,"choicetype_simple.pdf"), plot=pChoicetype_simple, height=4.5, width=16, units = "in")
# ggsave(filename = paste0(PLOT_PATH,"choicetype_simple.png"), plot=pChoicetype_simple, height=4.5, width=16, units = "in")
# 
# pChoicetype_extended
# ggsave(filename = paste0(PLOT_PATH,"choicetype_extended.pdf"), plot=pChoicetype_extended, height=9, width=16, units = "in")
# ggsave(filename = paste0(PLOT_PATH,"choicetype_extended.png"), plot=pChoicetype_extended, height=9, width=16, units = "in")

pChoiceTypeFinal_CD <- cowplot::plot_grid(pDecisionType_x_RewardDiff_scenario,pDecisionType_x_RewardDiff_age,ncol=2,labels=c('c','d'))
pChoiceTypeFinal_AB <- cowplot::plot_grid(pDecisionTypeScenario,pDecisionTypeDiff,ncol=2,labels=c('a','b'))

pChoiceTypeFinal <- cowplot::plot_grid(pChoiceTypeFinal_AB,pChoiceTypeFinal_CD,ncol=1,labels=c(NA,NA))
pChoiceTypeFinal
ggsave(filename = paste0(PLOT_PATH,"choicetype_final.pdf"), plot=pChoiceTypeFinal, height=7, width=13, units = "in")
ggsave(filename = paste0(PLOT_PATH,"choicetype_final.png"), plot=pChoiceTypeFinal, height=7, width=13, units = "in")

################################################################################################
# Exploration efficiency?
################################################################################################
dDistPrevzAgeZ <- d %>% 
  filter(age_bin!='2018_Data') %>% 
  group_by(age_bin,distance,prev_z) %>% 
  summarise(z=mean(z+.5),sdZ=sd(z))

dDistAgeZ <- d %>% 
  filter(age_bin!='2018_Data') %>% 
  group_by(UID,age_bin,distance) %>% 
  summarise(z=mean(z+.5)) %>% 
  filter(!is.na(distance))


d_prevz_clean <- d %>% 
  filter(!is.na(prev_z)) %>% 
  mutate(prev_z = round(prev_z*2,digits = 1)/2) %>% 
  filter(prev_z<0.5) %>% 
  filter(prev_z>-0.5) %>% 
  filter(age_bin!='2018_Data') %>% 
  mutate(z=z+.5) %>% 
  mutate(prev_z=prev_z+.5)

dPrevzScenarioZ <- d_prevz_clean %>% 
  group_by(UID,scenario,prev_z) %>% 
  summarise(z=mean(z)) 

pCurrentPrev <- ggplot(dPrevzScenarioZ,aes(x=prev_z,y=z,fill=scenario,color=scenario))+
  geom_smooth()+
  geom_hline(yintercept = 0.5, color='red',linetype='dashed')+
  scale_color_manual(values = scenarioPalette, name ='Condition', labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, name = 'Condition', labels = c('Control', 'Memory'))+
  geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
  coord_cartesian(xlim=c(0,1))+
  theme_classic()+
  labs(x='Previous reward',y='Current reward')
pCurrentPrev

pCurrentPrevZoomed <- ggplot(dPrevzScenarioZ,aes(x=prev_z,y=z,fill=scenario,color=scenario))+
  geom_smooth()+
  geom_hline(yintercept = 0.5, color='red',linetype='dashed')+
  scale_color_manual(values = scenarioPalette, name ='Condition', labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, name = 'Condition', labels = c('Control', 'Memory'))+
  geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
  theme_classic()+
  labs(x='Previous reward',y='Current reward')+
  coord_cartesian(xlim = c(0.65,0.75),ylim=c(0.65,0.75))
pCurrentPrevZoomed

# dZScore <- d_prevz_clean %>%
#   group_by(UID,scenario,prev_z) %>%
#   mutate(zImprovement=z/prev_z)
# pImprovement <- ggplot(dZScore,aes(x=prev_z,y=zImprovement))+
#   stat_summary(fun = mean, geom='point', size=1)+
#   stat_summary(fun.data = mean_cl_boot,  alpha = 0.2,color=NA,geom = 'crossbar')+
#   # scale_color_viridis(discrete = T)+
#   # scale_fill_viridis(discrete = T)+
#   scale_y_continuous(trans='log10')+
#   scale_color_manual(values = scenarioPalette, name ='Condition', labels = c('Control', 'Memory'))+
#   scale_fill_manual(values = scenarioPalette, name = 'Condition', labels = c('Control', 'Memory'))+
#   geom_hline(yintercept = 1, color = 'black', linetype = 'dashed')
# pImprovement

# dZScoreDiff <- dZScore %>% group_by(UID,age_bin) %>% summarise(zImprovementDiff=-diff(zImprovement))
# pImprovementDiff <- ggplot(dZScoreDiff,aes(x=age_bin,y=zImprovementDiff,color=age_bin,fill=age_bin))+
#   stat_summary(fun = mean, geom='point', size=1)+
#   stat_summary(fun.data = mean_cl_boot,  alpha = 0.2,color=NA,geom = 'crossbar')+
#   scale_color_viridis(discrete = T)+
#   scale_fill_viridis(discrete = T)+
#   geom_hline(yintercept = 0, color = 'red', linetype = 'dashed')
# pImprovementDiff

dZQuotient <- d_prevz_clean %>% 
  group_by(UID,scenario,prev_z,age_bin,age) %>% 
  summarise(z=mean(z))  %>% 
  mutate(zQuotient= z/prev_z) %>% 
  filter(zQuotient>0) 
pZQuotient <- ggplot(dZQuotient,aes(x=prev_z,y=zQuotient,fill=scenario,color=scenario))+
  # stat_summary(fun = mean, geom='line', size=.5)+
  # stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha = 0.2,color=NA)+
  geom_smooth()+
  scale_color_manual(values = scenarioPalette, name ='Condition', labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, name = 'Condition', labels = c('Control', 'Memory'))+
  coord_cartesian(xlim=c(0,1))+
  geom_vline(xintercept = 0.67,linetype='dotted',color='black')+
  scale_y_continuous(trans = 'log10')+
  geom_hline(yintercept = 1, color = 'black', linetype = 'dashed')+
  theme_classic()+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  labs(x='Previous reward',y='Exploration efficiency\n(Current / Previous)')
pZQuotient

pZQuotientPositive <- ggplot(subset(dZQuotient,prev_z>0),aes(x=prev_z,y=zQuotient,fill=scenario,color=scenario))+
  # stat_summary(fun = mean, geom='line', size=.5)+
  # stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha = 0.2,color=NA)+
  # geom_smooth(method='loess')+
  geom_smooth()+
  scale_color_manual(values = scenarioPalette, name ='Condition', labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, name = 'Condition', labels = c('Control', 'Memory'))+
  coord_cartesian(xlim=c(0.55,1),ylim=c(0.9,1.2))+
  geom_vline(xintercept = 0.67,linetype='dotted',color='black')+
  scale_y_continuous(trans = 'log10')+
  geom_hline(yintercept = 1, color = 'black', linetype = 'dashed')+
  theme_classic()+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  labs(x='Previous reward',y='Exploration efficiency\n(Current / Previous)')
pZQuotientPositive


dZDiff <- ddply(d_prevz_clean, ~UID+age_bin+scenario+age+prev_z, plyr::summarize, zQuotient = mean(z)/prev_z) %>% 
  group_by(UID, age_bin,age,prev_z) %>% summarize(zDiff = -diff(zQuotient)) 

pZDiff <- ggplot(dZDiff, aes(x = age_bin, y = zDiff,color=age_bin))+
  stat_summary(fun = mean, geom='point', size=.5)+
  stat_summary(fun.data = mean_cl_boot, geom='errorbar', width = 0.2)+
  geom_hline(yintercept=0, linetype='dashed', color='red') + # random choice model
  scale_color_viridis(discrete=TRUE, direction=1) +
  theme_classic()  +
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.position = 'none')+
labs(x='Age group',y='Control - Memory')
pZDiff


insettedZQuotient<-ggdraw(pZQuotient+theme(legend.position='none')) +
  draw_plot(pZDiff+
              theme(text = element_text(size = 10),
                    plot.background = element_rect(fill = "transparent",colour = NA),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)), x=.3, y=.45, width = .65, height = .4)
insettedZQuotient





# maxDist<-20
# dDistQuotient <- d_prevz_clean %>% 
#   filter(!is.na(distance)) %>% 
#   group_by(UID,scenario,prev_z,age_bin) %>% 
#   summarise(distance=mean(distance)) %>% 
#   mutate(distance =distance/maxDist) %>%
#   # mutate(prev_z=prev_z-0.5) %>% 
#   mutate(distQuotient= distance*prev_z) 
#   # filter(distQuotient>0) 
# pDistPrevz <- ggplot()+
#   # stat_summary(data= dDistQuotient,mapping=aes(x=prev_z,y=distQuotient,color=scenario),fun = mean, geom='line', size=.5)+
#   # stat_summary(data= dDistQuotient,mapping=aes(x=prev_z,y=distQuotient,fill=scenario,color=scenario),fun.data = mean_cl_boot, geom='ribbon', alpha = 0.2,color=NA)+
#   stat_summary(data= dDistQuotient,mapping=aes(x=prev_z,y=distance,color=scenario),fun = mean, geom='line', size=.5)+
#   stat_summary(data= dDistQuotient,mapping=aes(x=prev_z,y=distance,fill=scenario,color=scenario),fun.data = mean_cl_boot, geom='ribbon', alpha = 0.2,color=NA)+
#   
#   scale_color_manual(values = scenarioPalette, name ='Condition', labels = c('Control', 'Memory'))+
#   scale_fill_manual(values = scenarioPalette, name = 'Condition', labels = c('Control', 'Memory'))+
#   geom_abline(slope = -9, intercept=9, color = 'black', linetype = 'dashed')+
#   # coord_cartesian(ylim=c(0,5))+
#   # scale_y_continuous(trans = 'log10')+
#   # geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')+
#   # geom_vline(xintercept = 0.5, color = 'black', linetype = 'dashed')+
#   theme_classic()+
#   theme(plot.background = element_rect(fill = "transparent",colour = NA))
# pDistPrevz
# 
# pDistQuotient <- ggplot()+
#   stat_summary(data= dDistQuotient,mapping=aes(x=prev_z,y=distQuotient,color=scenario),fun = mean, geom='line', size=.5)+
#   stat_summary(data= dDistQuotient,mapping=aes(x=prev_z,y=distQuotient,fill=scenario,color=scenario),fun.data = mean_cl_boot, geom='ribbon', alpha = 0.2,color=NA)+
#   # stat_summary(data= dDistQuotient,mapping=aes(x=prev_z,y=distance,color=scenario),fun = mean, geom='line', size=.5)+
#   # stat_summary(data= dDistQuotient,mapping=aes(x=prev_z,y=distance,fill=scenario,color=scenario),fun.data = mean_cl_boot, geom='ribbon', alpha = 0.2,color=NA)+
#   
#   scale_color_manual(values = scenarioPalette, name ='Condition', labels = c('Control', 'Memory'))+
#   scale_fill_manual(values = scenarioPalette, name = 'Condition', labels = c('Control', 'Memory'))+
#   # geom_abline(slope = -9, intercept=9, color = 'black', linetype = 'dashed')+
#   # coord_cartesian(ylim=c(0,5))+
#   # scale_y_continuous(trans = 'log10')+
#   # geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')+
#   geom_vline(xintercept = 0.5, color = 'black', linetype = 'dashed')+
#   theme_classic()+
#   theme(plot.background = element_rect(fill = "transparent",colour = NA))
# pDistQuotient
# 
# dDistDiff <- ddply(d_prevz_clean, ~UID+scenario+age_bin+prev_z, plyr::summarize, distQuotient = mean(distance)/prev_z)  %>% group_by(UID, age_bin) %>% summarize(distDiff = -diff(distQuotient))
# pDistDiff <- ggplot(dDistDiff, aes(x = age_bin, y = distDiff,color=age_bin))+
#   stat_summary(fun = mean, geom='point', size=.5)+
#   stat_summary(fun.data = mean_cl_boot, geom='errorbar', width = 0.2)+
#   geom_hline(yintercept=0, linetype='dashed', color='red') + # random choice model
#   xlab('Age')+
#   ylab('Control - Memory')+
#   scale_color_viridis(discrete=TRUE, direction=1) +
#   theme_classic()  +
#   theme(plot.background = element_rect(fill = "transparent",colour = NA))+
#   theme(legend.position = 'none')
# pDistDiff
# 
# 
# insettedDistQuotient<-ggdraw(pDistQuotient+theme(legend.position='none')) +
#   draw_plot(pDistDiff+
#               theme(text = element_text(size = 10),
#                     plot.background = element_rect(fill = "transparent",colour = NA),
#                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)), x=.2, y=0.1, width = .65, height = .4)
# insettedDistQuotient


# dDistance_simplified <- d_prevz_clean %>% 
#   mutate(dist_simple= ifelse(distance<5,distance,5))
# pEfficiency <- ggplot()+
#   stat_summary(data=dDistance_simplified,mapping=aes(x=dist_simple, y= z,color=age_bin), fun = mean, geom='line', size=.5)+
#   stat_summary(data=dDistance_simplified,mapping=aes(x=dist_simple, y= z,fill=age_bin),fun.data = mean_cl_boot, geom='ribbon', alpha = 0.2,color=NA)+
#   theme_classic()+
#   scale_color_viridis(discrete = T)+
#   scale_fill_viridis(discrete = T)+
#   coord_cartesian(ylim = c(0.5,1))
# pEfficiency
# 
# pReactivity <- ggplot()+
#   stat_summary(data=dDistance_simplified,mapping=aes(x=dist_simple, y= prev_z,color=age_bin), fun = mean, geom='line', size=.5)+
#   stat_summary(data=dDistance_simplified,mapping=aes(x=dist_simple, y= prev_z,fill=age_bin),fun.data = mean_cl_boot, geom='ribbon', alpha = 0.2,color=NA)+
#   theme_classic()+
#   scale_color_viridis(discrete = T)+
#   scale_fill_viridis(discrete = T)+
#   coord_cartesian(ylim = c(0.5,1))
# pReactivity
# 
# 
# pDE <- cowplot::plot_grid(pReactivity,pEfficiency,ncol = 2,labels = c('d','e'))
# pDE

#pExplorationEfficiencyZoomed
pExplorationEfficiencyBC <- cowplot::plot_grid(insettedZQuotient,pZQuotientPositive,ncol=2,labels=c('b','c'))
pExplorationEfficiency <- cowplot::plot_grid(pCurrentPrev,
                                             pExplorationEfficiencyBC,
                                             ncol=1,labels=c('a',NA,NA))
pExplorationEfficiency

ggsave(filename = paste0(PLOT_PATH,"exploration_efficiency.png"), plot=pExplorationEfficiency, height=7, width=10, units = "in")
ggsave(filename = paste0(PLOT_PATH,"exploration_efficiency.pdf"), plot=pExplorationEfficiency, height=7, width=10, units = "in")


anova_expl_eff <- aov(formula= zQuotient ~ prev_z*age*scenario,data=dZQuotient)
summary(anova_expl_eff)
print(xtable((anova_expl_eff), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4

anova_expl_eff_diff <- aov(formula= zDiff ~ prev_z*age,data=dZDiff)
summary(anova_expl_eff_diff)
print(xtable((anova_expl_eff_diff), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4
corTestPretty(dZDiff$zDiff,dZDiff$age)

dIndZQuot <- dZDiff %>% group_by(UID,age_bin,age) %>% summarize(zDiff=mean(zDiff))
corTestPretty(dIndZQuot$zDiff,dIndZQuot$age)






lmExplEff <- lmer(formula=zQuotient~prev_z*scenario*age+(1|UID),data=dZQuotient)
summary(lmExplEff)
print(xtable(coef(summary(lmExplEff)), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4

pExplEffPred <- plot_model(lmExplEff,type = 'pred',terms = c('age','scenario'))+
  ggtitle('')+
  theme_classic()+
  scale_color_manual(values = scenarioPalette, name ='Condition', labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, name = 'Condition', labels = c('Control', 'Memory'))+
  labs(x='Age', y='Exploration efficiency')
pExplEffPred



VIRIDIS_PALETTE <- c('#fde725','#7ad151','#22a884','#2a788e','#414487','#440154')
lmExplEffDiff <- lmer(formula=zDiff~prev_z*age+(1|UID),data=dZDiff)
summary(lmExplEffDiff)
print(xtable(coef(summary(lmExplEffDiff)), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4

pExplEffDiffPred<- plot_model(lmExplEffDiff,type = 'pred',terms = c('prev_z','age[20,30,40,50,60,70]'))+
  ggtitle('')+
  theme_classic()+
  scale_color_manual(labels = c("18-27","28-37","38-47","48-57","58-67","68-77"),values = rev(VIRIDIS_PALETTE),name='Age group')+
  scale_fill_manual(labels = c("18-27","28-37","38-47","48-57","58-67","68-77"),values = rev(VIRIDIS_PALETTE),name='Age group')+
  geom_hline(yintercept=0, linetype='dashed', color='red')+
  labs(x='Previous reward', y=expr(Delta*'(Control - Memory)'))+
  theme(legend.position = c(0.7,0.1), legend.justification=c(0,0),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pExplEffDiffPred

pExplEffPreds <- cowplot::plot_grid(pExplEffPred,pExplEffDiffPred,ncol=2,labels='auto')
pExplEffPreds

ggsave(filename = paste0(PLOT_PATH,"exploration_efficiency_lme.png"), plot=pExplEffPreds, height=5, width=12, units = "in")
ggsave(filename = paste0(PLOT_PATH,"exploration_efficiency_lme.pdf"), plot=pExplEffPreds, height=5, width=12, units = "in")
