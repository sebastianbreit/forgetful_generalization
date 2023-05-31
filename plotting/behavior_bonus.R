# Plot behavioral results
# house keeping
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'viridis' , 'brms', 'lme4', 'sjPlot', 'cowplot','tidybayes','modelr','lmerTest','xtable','ggbeeswarm') #'cowplot',  'entropy', 'withr','jsonlite'
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

VIRIDIS_PALETTE_INCL_BLACK <- c("gray",'#fde725','#7ad151','#22a884','#2a788e','#414487','#440154')

COLOR_MODEL_BASIC = cbPalette[1]
COLOR_MODEL_SURPRISE = cbPalette[3]
COLOR_MODEL_FULL = cbPalette[4]
COLOR_MODEL_RECENCY = cbPalette[2]

COLORS_MODELS_ALL = c(COLOR_MODEL_RECENCY,COLOR_MODEL_SURPRISE,COLOR_MODEL_FULL,COLOR_MODEL_BASIC)
COLORS_MODELS_ALL_REORDERED = c(COLOR_MODEL_BASIC,COLOR_MODEL_FULL,COLOR_MODEL_SURPRISE,COLOR_MODEL_RECENCY)

PLOT_PATH="plots/plots_bonus/"

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


d_bonus_judegements <- load_bonus_preds_combined() %>% 
  mutate(h_est=h_est/100) %>% 
  mutate(gp_est=gp_est+.5) %>% 
  mutate(gp_conf=gp_conf) %>% 
  mutate(z_obs=z_obs+.5)

#TODO: dist_nearest, dist_max, dist_mostrecenct
d_bonus_hist <- load_bonus_history(inclAFC = F)

d_dist <- data.frame()
for(id in unique(d_bonus_judegements$UID)){
  sub_judgement <- d_bonus_judegements %>% filter(UID==id)
  sub_hist <- d_bonus_hist %>% filter(UID==id)
  
  unique_judgements <- sub_judgement %>% 
    filter(!duplicated(paste0(x, y))) %>% 
    select(x,y)
  
  for (r in 1:nrow(unique_judgements) ){
    temp <- unique_judgements[r,]
    xdiff<- abs(sub_hist$x-temp$x)
    ydiff<- abs(sub_hist$y-temp$y)
    
    dist_nearest <- min(xdiff)+min(ydiff)
    dist_recent <- xdiff[length(xdiff)]+ydiff[length(ydiff)]
    
    max_obs <- sub_hist %>% filter(z==max(z))
    dist_max<- min(abs(max_obs$x-temp$x)+abs(max_obs$y-temp$y))
   
    d_dist_row <- data.frame(UID=id,x=temp$x,y=temp$y, dist_nearest=dist_nearest,dist_recent=dist_recent,dist_max=dist_max)
    d_dist <- rbind(d_dist,d_dist_row)
  }
}

d_bonus <- merge(d_bonus_judegements, d_dist, by=c('UID','x','y'))


###################################################################################################################### 
###################################### Bonus plots beeswarm ########################################################## 
###################################################################################################################### 

indBonusDF <- d_bonus %>% 
  group_by(UID,scenario,gp_model) %>% 
  summarize(MAE_H = mean(abs(z-h_est)),MAE_GP=mean(abs(z-(gp_est))))
randomError <- mean(abs(sample(d_bonus$z, size = 10000, replace=T) - runif(10000,0.01,1))) #Simulate random error
contextLabels <- c('Baseline Condition' = 'Control Condition', 'Memory Condition' = 'Memory Condition')

indBonusDF_human_unique <-indBonusDF %>% group_by(UID,scenario) %>% summarise(MAE_H=mean(MAE_H))
p1 <- ggplot(indBonusDF_human_unique, aes(x = scenario, y = MAE_H, color = scenario))+
  geom_hline(yintercept = randomError, linetype = 'dashed')+
  geom_line(aes(group=UID), color = 'black', alpha = 0.1)+
  geom_quasirandom(alpha = 0.7)+
  geom_boxplot(outlier.shape=NA, fill=NA, color = 'black', width = .2)+
  stat_summary(fun.y=mean, geom='point', color = 'black', shape = 5, size = 2)+
  scale_color_manual(values=c(COLOR_CONDITION_BASELINE,COLOR_CONDITION_MEMORY),breaks = c('Baseline Condition','Memory Condition'), 
                     name='Condition',labels = c('Control', 'Memory'))+
  xlab('')+
  # coord_cartesian(ylim=c(0,70))+
  # geom_signif(comparison=list(c('Conceptual', 'Spatial')), color = 'black', annotations = c('BF=0.10'))+
  ylab('Participant Error (MAE)')+
  labs(color='Condition')+
  scale_x_discrete(label=c('Control\nCondition', 'Memory\nCondition'))+
  theme_classic()+
  theme( strip.background=element_blank(), legend.key=element_rect(color=NA))+
  theme(legend.position = c(0.05,1), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
p1


p1_gp <- ggplot(indBonusDF, aes(x = gp_model, y = MAE_GP, color = gp_model))+
  geom_hline(yintercept = randomError, linetype = 'dashed')+
  geom_line(aes(group=UID), color = 'black', alpha = 0.1)+
  geom_quasirandom(alpha = 0.7)+
  facet_grid(scenario ~ .)+
  geom_boxplot(outlier.shape=NA, fill=NA, color = 'black', width = .2)+
  stat_summary(fun.y=mean, geom='point', color = 'black', shape = 5, size = 2)+
  # scale_color_brewer(palette = 'Dark2')+
  xlab('')+
  # coord_cartesian(ylim=c(0,70))+
  # geom_signif(comparison=list(c('Conceptual', 'Spatial')), color = 'black', annotations = c('BF=0.10'))+
  ylab('Participant Error (MAE)')+
  # scale_x_discrete(label=c('Control\nCondition', 'Memory\nCondition'))+
  theme_classic()+
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))
p1_gp


test_anova <- aov(MAE_GP ~ scenario*gp_model, data=indBonusDF)
summary(test_anova)

indConfDF <- d_bonus %>% 
  group_by(UID,scenario,gp_model) %>% 
  summarize(conf_h=mean(h_conf),conf_gp=mean(gp_conf))

indConfDF_human_unique <-indConfDF %>% group_by(UID,scenario) %>% summarise(conf_h=mean(conf_h))
p2 <- ggplot(indConfDF_human_unique, aes(x=scenario, y = conf_h,  color = scenario))+
  geom_line(aes(group=UID), color = 'black', alpha = 0.1)+
  geom_quasirandom(alpha = 0.7)+
  geom_boxplot(fill='NA', color='black', width = .2, outlier.shape=NA) +
  #geom_dotplot(binaxis='y', stackdir='center', shape=16, color='black', alpha = 0.5, dotsize = 1.5 )+
  stat_summary(fun.y=mean, geom='point', color = 'black', shape = 5, size = 2)+
  #geom_line(aes(group=participant), color = 'black', alpha = 0.2)+
  scale_color_manual(values=c(COLOR_CONDITION_BASELINE,COLOR_CONDITION_MEMORY),breaks = c('Baseline Condition','Memory Condition'), 
                     name='Condition',labels = c('Control', 'Memory'))+
  ylab('Confidence')+
  xlab('')+
  scale_y_continuous(limits = c(1,12), breaks=c(3,6,9))+
  # geom_signif(comparison=list(c('Conceptual', 'Spatial')), color = 'black', annotations = c('BF=0.13'))+
  scale_x_discrete(label=c('Control\nCondition', 'Memory\nCondition'))+
  theme_classic()+
  theme(legend.position="top")+
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))
p2



# 
d_bonus_null <- d_bonus %>% filter(gp_model=='null')
d_bonus_full <- d_bonus %>% filter(gp_model=='full')
d_bonus_recency <- d_bonus %>% filter(gp_model=='recency')
d_bonus_surprise <- d_bonus %>% filter(gp_model=='surprise')

predictionDF <- data.frame(id= rep(d_bonus_null$UID, 4), humanPrediction = rep(d_bonus_null$h_est, 4),
                           humanConfidence = rep(d_bonus_null$h_conf, 4), scenario = rep(d_bonus_null$scenario, 4),
                           environment = rep(d_bonus_null$env, 4),
                           modelPrediction = c(d_bonus_null$gp_est, d_bonus_full$gp_est, d_bonus_recency$gp_est, d_bonus_surprise$gp_est),
                           modelUncertainty = c(d_bonus_null$gp_conf, d_bonus_full$gp_conf, d_bonus_recency$gp_conf, d_bonus_surprise$gp_conf),
                           model = rep(c('GP', 'GP + RS','GP + R','GP + S'),each = nrow(d_bonus_null) ))
predictionDF$model <- factor(predictionDF$model, level <- c('GP', 'GP + RS','GP + R','GP + S'))
#Mixed effects models
prior <- c(set_prior("normal(0,1)", class = "b"),set_prior("normal(0,1)", class = "sd"))
judgments_GP <- run_model(brm(modelPrediction ~ 0+ intercept+ humanPrediction+scenario+humanPrediction*scenario +(1+humanPrediction*scenario|id), data=subset(predictionDF, model=='GP'),
                              prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_','GP'))
judgments_GPRS <- run_model(brm(modelPrediction ~ 0+ intercept+ humanPrediction+scenario+humanPrediction*scenario +(1+humanPrediction*scenario|id), data=subset(predictionDF, model=='GP + RS'),
                                prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_','GP+RS'))
judgments_GPR <- run_model(brm(modelPrediction ~ 0+ intercept+ humanPrediction+scenario+humanPrediction*scenario +(1+humanPrediction*scenario|id), data=subset(predictionDF, model=='GP + R'),
                               prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_','GP+R'))
judgments_GPS <- run_model(brm(modelPrediction ~ 0+ intercept+ humanPrediction+scenario+humanPrediction*scenario +(1+humanPrediction*scenario|id), data=subset(predictionDF, model=='GP + S'),
                               prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_','GP+S'))


bayes_R2(GPjudgments) #R2
fixedTerms <- fixef(judgments_GP)#Look at fixed termsz 
#Now generate predictions, removing id as a random effect
xseq <- seq(0.01,1,0.01)
newdat <-data.frame(scenario = rep(c("Memory Condition","Baseline Condition"), each=100), humanPrediction = rep(xseq,2))

fixedDF <- data.frame()

predsGP <- fitted(judgments_GP, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
fixedDF <- rbind(fixedDF,data.frame(gp_model='GP', scenario = rep(c("Memory Condition","Baseline Condition"), each=100), h_est = rep(xseq,2),
                      gp_est = predsGP[,1], lower = predsGP[,3], upper = predsGP[,4] ))

predsGPR <- fitted(judgments_GPR, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
fixedDF <- rbind(fixedDF,data.frame(gp_model='GP + R', scenario = rep(c("Memory Condition","Baseline Condition"), each=100), h_est = rep(xseq,2),
                                    gp_est = predsGPR[,1], lower = predsGPR[,3], upper = predsGPR[,4] ))

predsGPS <- fitted(judgments_GPS, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
fixedDF <- rbind(fixedDF,data.frame(gp_model='GP + S', scenario = rep(c("Memory Condition","Baseline Condition"), each=100), h_est = rep(xseq,2),
                                    gp_est = predsGPS[,1], lower = predsGPS[,3], upper = predsGPS[,4] ))

predsGPRS <- fitted(judgments_GPRS, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
fixedDF <- rbind(fixedDF,data.frame(gp_model='GP + RS', scenario = rep(c("Memory Condition","Baseline Condition"), each=100), h_est = rep(xseq,2),
                                    gp_est = predsGPRS[,1], lower = predsGPRS[,3], upper = predsGPRS[,4] ))



p_density_absolute <- ggplot(d_bonus,aes(x=h_est,color=scenario)) +
  geom_density()
p_density_absolute

p_density_predictionDF <- ggplot(fixedDF,aes(x=h_est,color=scenario)) +
  geom_density()
p_density_predictionDF


#TODO: Replot p3 & p4 with predictions
p3 <- ggplot(d_bonus, aes(h_est, gp_est, color = gp_model, fill  = gp_model)) +
  geom_point(alpha =.05, color = 'black', fill=NA) +
  geom_line(data = fixedDF,  size = 1)+ #GP is
  geom_ribbon(data = fixedDF, aes(ymin=lower, ymax = upper), color = NA, alpha = 0.2 )+
  scale_color_manual(values = COLORS_MODELS_ALL_REORDERED)+
  scale_fill_manual(values = COLORS_MODELS_ALL_REORDERED)+
  theme_classic()+
  facet_grid(~scenario, labeller = as_labeller(contextLabels) )+
  xlab("Participant Estimate")+
  ylab("Model Estimate")+
  labs(color='Model',fill='Model')+
  theme(legend.position=c(0, 0.9), legend.justification=c(0,1), strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())+
  theme(legend.position = 'none')
p3



#Compute rank-ordered confidence for plot
confidenceDF <- data.frame()
for (id in unique(d_bonus$UID)){
  for (sc in unique(d_bonus$scenario)){
    for (m in unique(d_bonus$gp_model)){
      dsub <- subset(d_bonus, UID == id & scenario ==  sc & gp_model==m)
      dummy <- data.frame(gp_model = rep(m,nrow(dsub)),UID=rep(id,nrow(dsub)), rankParticipantConfidence= rank(dsub$h_conf), scenario = rep(sc,nrow(dsub)), rankModelUncertainty = rank(dsub$gp_conf) )
      confidenceDF <- rbind(dummy,confidenceDF)
    }  
  }
}
confidenceDF<-confidenceDF %>% 
  mutate(gp_model=ifelse(gp_model=='null','GP',
                         ifelse(gp_model=='full','GP + RS',
                                ifelse(gp_model=='recency','GP + R','GP + S'))))

p4<- ggplot(confidenceDF, aes(x=rankParticipantConfidence, y = rankModelUncertainty))+
  stat_summary(fun.y = mean, geom = "point", color = 'black') + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", color = 'black', width = 0)+
  # geom_smooth( method = 'lm',formula=y~x,  fullrange=TRUE)+ #fill = NA,
  geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.2,
              aes(color= NULL,fill=gp_model)) +
  geom_line(stat='smooth', method = "lm", alpha=1,
            aes(color= gp_model,fill=NULL))+
  facet_grid(~scenario, labeller = as_labeller(contextLabels))+
  scale_color_manual(values = (COLORS_MODELS_ALL_REORDERED),breaks=c('GP','GP + R','GP + S','GP + RS'),
                     name='Model')+
  scale_fill_manual(values = (COLORS_MODELS_ALL_REORDERED),breaks=c('GP','GP + R','GP + S','GP + RS'),
                    name='Model')+
  
  ylab(expression(paste("GP Uncertainty (rank order)")))+
  xlab('Participant Confidence (rank order)')+
  theme_classic() + 
  theme(legend.position = c(0.0,0.65), legend.justification=c(0,1),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
p4



# ###################################################################################################################### 
# ###################################### Bonus plots reward corr ####################################################### 
# ###################################################################################################################### 
# 
# 
# 
# 
# p_dist_max_conf <- ggplot(d_bonus,aes(x=dist_max,y=h_conf,color=scenario))+
#   geom_smooth()+
#   coord_cartesian(ylim=c(3,7))
# p_dist_max_conf
# 
# p_dist_recent_conf <- ggplot(d_bonus,aes(x=dist_recent,y=h_conf,color=scenario))+
#   geom_smooth()+
#   coord_cartesian(ylim=c(3,7))
# p_dist_recent_conf
# 
# 
# p_dist_max_est <- ggplot(d_bonus,aes(x=dist_max,y=h_est,color=scenario))+
#   geom_smooth()+
#   coord_cartesian(ylim=c(0.3,0.7))
# p_dist_max_est
# 
# p_dist_recent_est <- ggplot(d_bonus,aes(x=dist_recent,y=h_est,color=scenario))+
#   geom_smooth()+
#   coord_cartesian(ylim=c(0.3,0.7))
# p_dist_recent_est
# 
# # Recent ~ Max
# corTestPretty(d_bonus$dist_recent,d_bonus$dist_max,method='kendall')
# p_corr_recent_max <- ggplot(d_bonus,aes(x=dist_recent,y=dist_max))+
#   geom_smooth()+
#   geom_abline(slope=1,intercept = 0,linetype='dashed')+
#   coord_cartesian(xlim=c(0,20),ylim=c(0,20))
# p_corr_recent_max
# 
# p_dist_reward <- ggplot(subset(d_bonus,!is.na(z_obs)),aes(x=dist_max,y=z_obs))+
#   geom_smooth()
# p_dist_reward
# 
# 
# corTestPretty(d_bonus$dist_nearest,d_bonus$dist_max,method='kendall')
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# corTestPretty(x=d_bonus$h_conf,y=d_bonus$z)
# corTestPretty(x=d_bonus$h_est,y=d_bonus$z)
# 
# p_z_est_scenario_H<- ggplot(d_bonus)+
#   # geom_smooth(method='lm',alpha=.1,aes(x=z,y=gp_est,color=scenario,fill=scenario))+
#   geom_smooth(method='lm',aes(x=z,y=h_est,color=scenario,fill=scenario))+
#   geom_abline(slope=1,intercept = 0,linetype='dashed')+
#   coord_cartesian(xlim = c(0,1),ylim=c(0.3,0.6))
# p_z_est_scenario_H
# 
# d_bonus_grouped<-d_bonus %>% mutate(gp_model=ifelse(gp_model %in% c('null','surprise'),'GP / GP + S','GP + R / GP + RS' ))
# p_z_est_scenario_GP<- ggplot(d_bonus_grouped)+
#   geom_smooth(method='lm',alpha=.1,aes(x=z,y=gp_est,color=interaction( gp_model,scenario, sep=':'),fill=interaction( gp_model,scenario, sep=':')))+
#   geom_abline(slope=1,intercept = 0,linetype='dashed')+
#   coord_cartesian(xlim = c(0,1),ylim=c(0.3,0.6))
# p_z_est_scenario_GP
# 
# 
# 
# p_z_conf_scenario_H<- ggplot(d_bonus)+
#   geom_smooth(method='lm',aes(x=z,y=h_conf/10,color=scenario,fill=scenario))+
#   geom_abline(slope=1,intercept = 0,linetype='dashed')+
#   coord_cartesian(xlim = c(0,1),ylim=c(0,0.6))
# p_z_conf_scenario_H
# 
# p_z_conf_scenario_GP<- ggplot(d_bonus_grouped)+
#   geom_smooth(method='lm',alpha=.1,aes(x=z,y=gp_conf,color=interaction( gp_model,scenario, sep=':'),fill=interaction( gp_model,scenario, sep=':')))+
#   geom_abline(slope=1,intercept = 0,linetype='dashed')+
#   coord_cartesian(xlim = c(0,1),ylim=c(0,0.6))
# p_z_conf_scenario_GP

###################################################################################################################### 
###################################### Bonus plots -- cowplot ######################################################## 
###################################################################################################################### 

pBonus <- cowplot::plot_grid(p1,p2,p3,p4,ncol=2,labels='auto')
pBonus

ggsave(filename = paste0(PLOT_PATH,"bonus.png"), plot=pBonus, height=6, width=12, units = "in")
ggsave(filename = paste0(PLOT_PATH,"bonus.pdf"), plot=pBonus, height=6, width=12, units = "in")

ttestPretty(subset(indBonusDF_human_unique,scenario=='Baseline Condition')$MAE_H,subset(indBonusDF_human_unique,scenario=='Memory Condition')$MAE_H)
ttestPretty(subset(indConfDF_human_unique,scenario=='Baseline Condition')$conf_h,subset(indConfDF_human_unique,scenario=='Memory Condition')$conf_h)

corTestPretty(d_bonus$h_est,d_bonus$gp_est)
corTestPretty(d_bonus$h_conf,d_bonus$gp_conf)

anova_model_ests <- aov(MAE_GP~gp_model*scenario*MAE_H,data=indBonusDF)
summary(anova_model_ests)
print(xtable(((anova_model_ests)), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4


dBonusNS<- subset(indBonusDF,gp_model%in%c('null','surprise'))
dBonusRF<-subset(indBonusDF,gp_model%in%c('recency','full'))

ttestPretty(dBonusNS$MAE_GP, dBonusRF$MAE_GP)
ttestPretty(subset(dBonusRF,gp_model=='recency')$MAE_GP, subset(dBonusRF,gp_model=='full')$MAE_GP,bonferroni = 6)
ttestPretty(subset(dBonusNS,gp_model=='null')$MAE_GP, subset(dBonusNS,gp_model=='surprise')$MAE_GP,bonferroni = 6)

anova_model_confs <- aov(gp_conf~gp_model*scenario*h_conf,data=indConfDF)
summary(anova_model_confs)
print(xtable(((anova_model_confs)), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4


dConfNS<- subset(indConfDF,gp_model%in%c('null','surprise'))
dConfRF<-subset(indConfDF,gp_model%in%c('recency','full'))

ttestPretty(dConfNS$conf_gp, dConfRF$conf_gp)
ttestPretty(subset(indConfDF,gp_model=='recency')$conf_gp, subset(indConfDF,gp_model=='full')$conf_gp,bonferroni = 6)
ttestPretty(subset(indConfDF,gp_model=='null')$conf_gp, subset(indConfDF,gp_model=='surprise')$conf_gp,bonferroni = 6)
