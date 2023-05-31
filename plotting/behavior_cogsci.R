#Behavior plots CogSci
# Plot behavioral results
# house keeping
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'viridis' , 'brms', 'lme4', 'sjPlot', 'cowplot','tidybayes','modelr','lmerTest') #'cowplot',  'entropy', 'withr', 'ggbeeswarm','jsonlite'
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

PLOT_PATH="plots/plots_cogsci/"


#Excluding sub-sample of South African participants due to large behavior differences
d <- load_exp_data(includeLast = FALSE,removeSA = T,removeLearning = T)





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
# Learning effects
################################################################################################

#Learning over trials
dTrials <- ddply(d, ~step, plyr::summarize, meanReward = mean(z)+.5) 
corTestPretty(dTrials$meanReward, seq(1,26))

#Learning over rounds
dRound <- ddply(d, ~round, plyr::summarize, meanReward = mean(z)+.5) 
corTestPretty(dRound$meanReward, seq(5,14))

# First three rounds seem sub-par to the rest, maybe exclude those
plot(dRound$round, dRound$meanReward)

#age-related differences
dAge <- ddply(d, ~id + age+scenario, plyr::summarize, meanReward = mean(z)+.5) 

corTestPretty(dAge$meanReward, dAge$age)
corTestPretty(subset(dAge,scenario=='Baseline Condition')$meanReward, subset(dAge,scenario=='Baseline Condition')$age)
corTestPretty(subset(dAge,scenario=='Memory Condition')$meanReward, subset(dAge,scenario=='Memory Condition')$age)
# p.adjust(c(.119,.007),'bonferroni',2)

################################################################################################
# 2a: Learning curves
################################################################################################
dLearningCurves <- ddply(d, ~id+step+scenario+age_bin, plyr::summarize, meanReward = mean(z)+.5) 
pCurveAge <- ggplot(dLearningCurves, aes(x=step, y=meanReward, color=age_bin, fill=age_bin, group=age_bin))+
  stat_summary(fun = mean, geom='line', size=.5)+
  stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha=0.3, color=NA)+
  geom_hline(yintercept=0.5, linetype='dashed', color='red') + # random choice model
  xlab('Trial')+
  ylab('Normalized Reward ± 95% CI')+
  labs(color='Age Group') +
  labs(fill='Age Group') +
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  # ggtitle('Learning Curves') +
  theme_classic()  +
  # theme(strip.background=element_blank()) #, legend.position='none'
  theme(legend.position = c(0.05,1), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pCurveAge
# ggsave(filename = paste0(PLOT_PATH,"learningCurves_age.png"), plot=pCurveAge, height=4.5, width=8, units = "in")


#Learning curves by condition
dLearningCurvesCond <- ddply(d, ~id+step+scenario, plyr::summarize, meanReward = mean(z)+.5) 
pCurveCond <- ggplot(dLearningCurvesCond, aes(x=step, y=meanReward, color=scenario, fill=scenario))+
  stat_summary(fun = mean, geom='line', size=.5)+
  stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha=0.3, color=NA)+
  geom_hline(yintercept=0.5, linetype='dashed', color='red') + # random choice model
  xlab('Trial')+
  ylab('Avg. Reward (normalized)')+
  scale_color_manual(values = scenarioPalette, name  = 'Condition', labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette,name  = 'Condition', labels = c('Control', 'Memory'))+
  # ggtitle('Learning Curves') +
  theme_classic()  +
  # theme(strip.background=element_blank()) #, legend.position='none'
  theme(legend.position = c(0.05,1), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pCurveCond

dInd <- ddply(d, ~id+scenario+age_bin, plyr::summarize, meanReward = mean(z)+.5) 
ttestPretty(subset(dInd, scenario == 'Baseline Condition')$meanReward, subset(dInd, scenario == 'Memory Condition')$meanReward, paired=T)


dRewardDiff <- d %>% 
  group_by(id,scenario,age_bin) %>% summarize(meanReward = mean(z)) %>% 
  group_by(id,age_bin) %>% summarise(reward_diff =-diff(meanReward)) # -diff, because ordering is Memory, then Baseline

pDiff <- ggplot(dRewardDiff, aes(x=age_bin, y=reward_diff,color=age_bin))+
  scale_color_viridis(discrete=TRUE, direction=1) +
  stat_summary(fun = mean, geom='point', size=.5)+
  stat_summary(fun.data = mean_cl_boot, geom='errorbar', width = 0.2)+
  geom_hline(yintercept=0, linetype='dashed', color='red') + # random choice model
  xlab('Age')+
  ylab('Control - Memory')+
  theme_classic()  +
  theme( legend.position='none')
pDiff
# ggsave(filename = paste0(PLOT_PATH,"differenceCurves_overview.png"), plot=pCurveDiff, height=4.5, width=8, units = "in")



insettedReward<-ggdraw(pCurveCond) +
  draw_plot(pDiff+
              theme(text = element_text(size = 10),
                    plot.background = element_rect(fill = "transparent",colour = NA),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
                    ), x=.3, y=.2, width = .65, height = .4)
insettedReward




################################################################################################
# 2b: Reward ~ Age
################################################################################################
dAgeBinned <- d %>% group_by(id, age_bin, scenario) %>% summarize(z = mean(z)+.5) %>% 
  group_by(age_bin,scenario) %>% summarize(n = n(), meanReward = mean(z), se = sd(z)/sqrt(n)) %>%
  mutate(lower = meanReward - qt(1 - (0.05 / 2), n - 1) * se ,
         upper = meanReward + qt(1 - (0.05 / 2), n - 1) * se )
dAgeBinned$age <- as.numeric(factor(dAgeBinned$age_bin)) #converts to the ones digit of the age bin
dAgeBinned$age  <- (dAgeBinned$age *10) + 12.5 #converts to the median age in the bin

pAgeContinuous <- ggplot(dAge, aes(x = age, y = meanReward, color = scenario, fill = scenario))+
  #geom_point(alpha =0.6)+
  #geom_hline(yintercept=0.5, linetype='dashed', color='red') + # random choice model
  geom_smooth(alpha = 0.2)+
  geom_point(data = dAgeBinned, position=position_dodge(width = 2))+
  geom_errorbar(data = dAgeBinned, aes(ymin = lower, ymax = upper ), width = 2, position=position_dodge(width = 2))+
  ylab('Avg. Reward (normalized)')+
  xlab('Age')+
  labs(color='Condition') +
  labs(fill='Condition') +
  scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
  coord_cartesian(ylim=c(0.65,0.8))+
  theme_classic()  +
  # theme(strip.background=element_blank()) #, legend.position='none'
  theme(legend.position = c(0.0,0), legend.justification=c(0,0),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pAgeContinuous


#Age + scenario
dAgeDiff <- ddply(d, ~id+ age+scenario, plyr::summarize, meanReward = mean(z)+.5) %>% group_by(id,age) %>% summarize(rewDiff = -diff(meanReward))
corTestPretty(dAgeDiff$rewDiff, dAgeDiff$age)




################################################################################################
# 2c: Distance ~ Step
################################################################################################
#Did distance decrease over trials?
dDistStep <- ddply(d, ~step, plyr::summarize, meanDist = mean(distance, na.rm=T)) 
corTestPretty(dDistStep$meanDist, dDistStep$step)

dDistCond<- ddply(d, ~id+scenario, plyr::summarize, meanDist = mean(distance, na.rm=T)) 
ttestPretty(subset(dDistCond, scenario=='Memory Condition')$meanDist, subset(dDistCond, scenario=='Baseline Condition')$meanDist, paired=T)


#distance by condition
dDistCurves <- ddply(d, ~id+step+scenario, plyr::summarize, meanDist = mean(distance, na.rm=T)) 
pDistCurves <- ggplot(dDistCurves, aes(x=step, y=meanDist, color=scenario, fill=scenario))+
  stat_summary(fun = mean, geom='line', size=.5)+
  stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha=0.3, color=NA)+
  xlab('Trial')+
  ylab('Avg. Search Distance')+
  scale_color_manual(values = scenarioPalette, name ='Condition', labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, name = 'Condition', labels = c('Control', 'Memory'))+
  # ggtitle('Search Distance') +
  theme_classic()  +
  theme(legend.position = c(0,0.1), legend.justification=c(0,0),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pDistCurves

#distance by age
dDistCurvesAge <- ddply(d, ~id+step+age_bin, plyr::summarize, meanDist = mean(distance, na.rm=T)) 
pDistCurvesAge<- ggplot(dDistCurvesAge, aes(x=step, y=meanDist, color=age_bin, fill=age_bin))+
  stat_summary(fun = mean, geom='line', size=.5)+
  stat_summary(fun.data = mean_cl_boot, geom='ribbon', alpha=0.3, color=NA)+
  geom_hline(yintercept=0.5, linetype='dashed', color='red') + # random choice model
  xlab('Trial')+
  ylab('Avg. Distance ± 95% CI')+
  labs(color='') +
  labs(fill='') +
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  # ggtitle('Search Distance') +
  theme_classic()  +
  # theme(strip.background=element_blank()) #, legend.position='none'
  theme(legend.position = c(0,0.1), legend.justification=c(0,0),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pDistCurvesAge

dDistAgeCont <- ddply(d, ~id+scenario+age, plyr::summarize, meanDist = mean(distance, na.rm=T))
corTestPretty(dDistAgeCont$age, dDistAgeCont$meanDist)

corTestPretty(subset(dDistAgeCont,scenario=='Memory Condition')$age, subset(dDistAgeCont,scenario=='Memory Condition')$meanDist)
corTestPretty(subset(dDistAgeCont,scenario=='Baseline Condition')$age, subset(dDistAgeCont,scenario=='Baseline Condition')$meanDist)
p.adjust(c(.319,.914),'bonferroni',2)


dDistDiff <- ddply(d, ~id+scenario+age_bin, plyr::summarize, meanDist = mean(distance, na.rm=T)) %>% group_by(id, age_bin) %>% summarize(distDiff = -diff(meanDist))
pDistDiff <- ggplot(dDistDiff, aes(x = age_bin, y = distDiff,color=age_bin))+
  stat_summary(fun = mean, geom='point', size=.5)+
  stat_summary(fun.data = mean_cl_boot, geom='errorbar', width = 0.2)+
  geom_hline(yintercept=0, linetype='dashed', color='red') + # random choice model
  xlab('Age')+
  ylab('Control - Memory')+
  scale_color_viridis(discrete=TRUE, direction=1) +
  theme_classic()  +
  theme(legend.position = 'none')
pDistDiff


dDistDiffAgeCont <- ddply(d, ~id+scenario+age, plyr::summarize, meanDist = mean(distance, na.rm=T)) %>% group_by(id, age) %>% summarize(distDiff = -diff(meanDist))
corTestPretty(dDistDiffAgeCont$age, dDistDiffAgeCont$distDiff)

insettedDistance<-ggdraw(pDistCurves+theme(legend.position='none')) +
  draw_plot(pDistDiff+
              theme(text = element_text(size = 10),
                    plot.background = element_rect(fill = "transparent",colour = NA),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)), x=.3, y=.45, width = .65, height = .4)
insettedDistance




################################################################################################
# 2d: Distance ~ Age (Old)
################################################################################################
#Distance Continuous
dDistAge <- ddply(d, ~id + age+scenario, plyr::summarize, meanDist = mean(distance, na.rm=T)) 

dDistAgeBinned <- d %>% group_by(id, age_bin, scenario) %>% summarize(distance =mean(distance, na.rm=T)) %>% 
  group_by(age_bin,scenario) %>% summarize(n = n(), meanDist = mean(distance), se = sd(distance)/sqrt(n)) %>%
  mutate(lower = meanDist - qt(1 - (0.05 / 2), n - 1) * se ,
         upper = meanDist + qt(1 - (0.05 / 2), n - 1) * se )
dDistAgeBinned$age <- as.numeric(factor(dDistAgeBinned$age_bin)) #converts to the ones digit of the age bin
dDistAgeBinned$age  <- (dDistAgeBinned$age *10) + 12.5 #converts to the median age in the bin

pDistContinuous <- ggplot(dDistAge, aes(x = age, y = meanDist, color = scenario, fill = scenario))+
  geom_smooth(alpha = 0.2)+
  geom_point(data = dDistAgeBinned, position=position_dodge(width = 2))+
  geom_errorbar(data = dDistAgeBinned, aes(ymin = lower, ymax = upper ), width = 2, position=position_dodge(width = 2))+
  ylab('Avg. Search Distance')+
  xlab('Age')+
  labs(color='Condition') +
  labs(fill='Condition') +
  scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
  scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory'))+
  coord_cartesian(ylim=c(1.5,3))+
  theme_classic()  +
  # ggtitle('Avg. Distance')+
  theme(legend.position = 'none')
  # theme(legend.position = c(0.0,0), legend.justification=c(0,0),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
pDistContinuous



#Age + scenario
dDistCont <- ddply(d, ~id+ age+scenario, plyr::summarize, meanDist = mean(distance,na.rm=T))
dDistCont_mem <- dDistCont %>% filter(scenario=='Memory Condition')
dDistCont_nomem <- dDistCont %>% filter(scenario=='Baseline Condition')
corTestPretty(dDistCont_mem$meanDist, dDistCont_mem$age)
corTestPretty(dDistCont_nomem$meanDist, dDistCont_nomem$age)




###################################################################################################################################
#### Final behavioral plot
###################################################################################################################################

pBehavior <- cowplot::plot_grid(insettedReward, pAgeContinuous+theme(legend.position='none'), #+ggtitle('Performance')
                                insettedDistance,pDistContinuous,#pDistRegression,
                                # pDistRewardRegression_nomemory +theme(legend.position='none'), pDistRewardRegression_memory,  
                                ncol = 2, labels = 'auto'
                                # , align = 'vh'
                                )
pBehavior

ggsave('plots/plots_cogsci/behavior.pdf', pBehavior, width = 7, height = 7, units ='in')







###################################################################################################################################
##################################################### REGRESSIONS ###################################################################
###################################################################################################################################


# ################################################################################################
# # 2d: Distance ~ Poly(age) * scenario (Regression with previous reward accounted for)
# ################################################################################################
# 
# 
# d_brms <- load_exp_data(includeLast = FALSE)
# # d_brms$prev_z <- scale(d_brms$prev_z)
# # d_brms$age <- scale(d_brms$age)
# # d_brms$step <- scale(d_brms$step)
# # d_brms$z <- scale(d_brms$z)
# 
# d_brms <- d_brms %>% filter(!is.na(prev_z)) %>% 
#   filter(UID %in% d$UID)
# 
# mDistPolyAge <- run_model(brm(distance ~ poly(age,2)* poly(prev_z,2)*scenario + (1+poly(age,2) +poly(prev_z,2) +scenario  |id),
#                               data = d_brms, cores = 4, 
#                               iter = 4000, warmup = 2000,
#                               # control = list(adapt_delta = 0.99) #Default =0.95 - only increase when warning with divergent transitions
# ), 
# modelName = paste0('brm_unscaled_distance_age_x_prevz_x_condition_4kiter_excl_SA'))
# 
# plot_model(mDistPolyAge,type='pred',terms=c('age','scenario'))
# plot_model(mDistPolyAge,type='pred',terms = c('prev_z','scenario','age'))
# summary(mDistPolyAge)
# 
# mDistPolyAge_lmer <- run_model(lmer(formula = distance ~ poly(age,2)* poly(prev_z,2)*scenario + (1 +poly(prev_z,2) + scenario  |id),
#                                     data    = d_brms),
#                                modelName=paste0('lmer_unscaled_distance_age_x_prevz_x_condition_4kiter_excl_SA'))#to run the model
# summary(mDistPolyAge_lmer)
# ranova(mDistPolyAge_lmer)
# 
# 
# mDistLinearAge_lmer <- run_model(lmer(formula = distance ~ age* prev_z*scenario + (1+prev_z + scenario  |id),
#                                       data    = d_brms),
#                                  modelName=paste0('lmer_unscaled_distance_age_x_prevz_x_condition_4kiter_excl_SA_linear'))#to run the model
# summary(mDistLinearAge_lmer)
# ranova(mDistLinearAge_lmer)
# 
# # Significant effects
# # Population-Level Effects: 
# #                                                 Estimate Est.Error  l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # polyprev_z21                                    -610.96     11.09   -632.96  -589.26 1.00     1852     3631
# # polyprev_z22                                      78.43      6.12     66.40    90.47 1.00     6014     5559
# # 
# # scenarioMemoryCondition                           -0.19      0.02     -0.23    -0.16 1.00     5888     6690
# # 
# # polyage21:scenarioMemoryCondition                -16.34      7.07    -30.11    -2.39 1.00     6073     5955
# # polyage22:scenarioMemoryCondition                 15.89      7.08      1.97    29.96 1.00     5590     6328
# # 
# # polyprev_z21:scenarioMemoryCondition              49.75      4.70     40.68    59.17 1.00    15146     5279
# # polyage21:polyprev_z21:scenarioMemoryCondition  6887.52   1658.51   3668.46 10123.13 1.00    18725     5806
# 
# 
# # d_exp <- load_exp_data(includeLast=FALSE)
# d_exp <- d
# 
# d_ages<- d_exp %>% group_by(UID) %>% summarise(ages=age) %>% distinct(UID,ages)
# ages<- as.numeric(d_ages$ages)
# 
# d_prev_z<- d_exp %>% select(prev_z) %>% filter(!is.na(prev_z))
# prev_zs<- as.numeric(d_prev_z$prev_z)
# 
# # Alternative 1: results in too large distances...
# age = unique(scale(ages))
# scenario = levels(d_exp$scenario)
# prev_z = unique(scale(prev_zs))
# newdat = expand.grid(prev_z=prev_z,age=age,scenario=scenario)
# 
# 
# 
# 
# # # Alternative 2: Same ... Issue might be the different distributions of prev_z & distances
# # age = unique(d_brms$age)
# # scenario = levels(d_brms$scenario)
# # prev_z = unique(d_brms$prev_z)
# # newdat = expand.grid(prev_z=prev_z,age=age,scenario=scenario)
# 
# 
# 
# preds = fitted(mDistPolyAge, re_formula=NA, newdata=newdat, probs=c(.025, .975))
# predsDF = data.frame(prev_z=rep(prev_z, length(scenario)*length(age)),
#                      age=rep(age, each=length(prev_z)),
#                      scenario=rep(scenario, each=length(prev_z)*length(age)),
#                      distance=preds[,1],
#                      lower=preds[,3],
#                      upper=preds[,4])
# 
# predsDF$age <- predsDF$age * attr(scale(ages), 'scaled:scale') + attr(scale(ages), 'scaled:center')
# predsDF$prev_z <- predsDF$prev_z * attr(scale(prev_zs), 'scaled:scale') + attr(scale(prev_zs), 'scaled:center')
# 
# 
# d_exp_scaled <- d_exp %>%
#   mutate(prev_z=scale(prev_z)) %>%
#   mutate(age=scale(age))
# 
# d_exp_scaled_summary <- d_exp_scaled %>%
#   filter(!is.na(distance)) %>%
#   group_by(age,scenario,age_bin) %>%
#   summarize(distance=mean(distance))
# 
# 
# predsDF_summary <- predsDF %>% 
#   group_by(age,scenario) %>% 
#   summarize(distance=median(distance),lower=median(lower),upper=median(upper))
# 
# 
# unscale_age_trans <- trans_new(
#   name="unscale_age", 
#   transform=function(x) x * attr(scale(ages), 'scaled:scale') + attr(scale(ages), 'scaled:center'), 
#   inverse=function(x) scale(x),
#   breaks=breaks_extended()
# )
# 
# plot_model(mDistPolyAge,type='pred',terms=c('age','scenario'))
# # plot_model(mDistPolyAge,type='pred',terms=c('age','scenario')) + 
# #   scale_x_continuous(trans=unscale_age_trans)
# 
# 
# 
# 
# dDistAgeBinned <- d_exp_scaled %>%
#   filter(!is.na(distance)) %>%
#   group_by(UID,age_bin, scenario) %>% 
#   summarize(distance =mean(distance, na.rm=T)) %>% 
#   group_by(age_bin,scenario) %>% 
#   summarize(n = n(), meanDist = mean(distance), se = sd(distance)/sqrt(n)) %>%
#   mutate(lower = meanDist - qt(1 - (0.05 / 2), n - 1) * se ,
#          upper = meanDist + qt(1 - (0.05 / 2), n - 1) * se )
# 
# dDistAgeBinned$age <- as.numeric(factor(dDistAgeBinned$age_bin)) #converts to the ones digit of the age bin
# dDistAgeBinned$age  <- (dDistAgeBinned$age *10) + 12.5 #converts to the median age in the bin
# 
# 
# pDistRegression <- ggplot()+
#   # geom_point(data=d_exp_scaled_summary, mapping=aes(x=age, y=distance, color=scenario, fill=scenario), alpha=0.7, size=.5)+
#   geom_line(predsDF_summary, mapping=aes(x=age, y=distance, color=scenario), size=.3) +
#   geom_ribbon(predsDF_summary, mapping=aes(x=age, y=distance, ymin=lower, ymax=upper, fill=scenario), alpha=.3) +
#   # geom_point(data = dDistAgeBinned,mapping = aes(x=age,y=meanDist, color=scenario), position=position_dodge(width = 2))+
#   # geom_errorbar(data = dDistAgeBinned, aes(x=age,y=meanDist,ymin = lower, ymax = upper, color=scenario ), width = 2, position=position_dodge(width = 2))+
#   xlab('Age')+
#   ylab('Distance to Next Option')+
#   labs(color='Condition',fill='Condition')+
#   scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
#   scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
#   # ggtitle('Search Distance ~ Age') +title
#   theme_classic() +
#   theme(text = element_text(size=10))+ #,  family="sans"
#   theme(legend.position = c(0,0), legend.justification=c(0,0),legend.key.size = unit(0.35,'cm'),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
# 
# pDistRegression
# 
# 
# 
# ###################################################################################################################################
# #### Polynomial Regression models
# ###################################################################################################################################
# 
# # #Age + scenario Regression
# # #mAgeFreq <- lmer(meanReward~poly(age,2)*scenario + (1|id), data =dAge) #Frequentist model for testing
# # mAge <- run_model(brm(meanReward~poly(age,2)*scenario + (1+poly(age,2)+scenario|id),
# #                       data =dAge, iter = 4000, warmup = 1000, control = list(adapt_delta = 0.99)), #increase iterations later
# #                   'ageScenario')
# # 
# # summary(mAge)
# # plot_model(mAge,type='pred',terms=c('age','scenario'))
# 
# #distance ~ Age + scenario Regression
# dAge <- ddply(d, ~id + age + scenario, plyr::summarize, meanDist = mean(distance, na.rm=T) )
# #mAgeFreq <- lmer(meanDist~poly(age,2)*scenario + (1|id), data =dAge) #Frequentist model for testing
# mAgeDist <- run_model(brm(meanDist~poly(age,2)*scenario + (1+poly(age,2)+scenario|id),
#                           data =dAge, iter = 4000, warmup = 1000, control = list(adapt_delta = 0.99)), #increase iterations later
#                       'brm_unscaled_ageScenarioDist_excl_SA')
# 
# summary(mAgeDist)
# plot_model(mAgeDist,type='pred',terms=c('age','scenario'))
# plot_model(mAgeDist,type='est')
# 
# 
