# modeling_cogsci

#Plotting fitting results
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot', 'viridis', 'entropy', 'lme4', 'sjPlot', 'brms', 'withr', 'tidyr', 'ggbeeswarm','jsonlite','tools','corrplot', "ggExtra", 'gridExtra')
invisible(lapply(packages, require, character.only = TRUE)) #loads packages

source('data_munging.R')
source('statistics.R')
set.seed(1)
################################################################################################
# Data import
################################################################################################
models <- c("recency","surprise+","full","null")#,"null" # exclude null model from analysis since some of them stopped
exp_models <- c("Memory", "NoMemory") #
model_descriptions_sim <-c("GP + R","GP + S","GP + RS","GP")#
model_descriptions_exp <-c("Memory","Baseline")

PLOT_PATH="plots/plots_cogsci/"

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
scenarioPalette <- c('#ef8a62','#67a9cf')

COLOR_CONDITION_BASELINE = scenarioPalette[1]
COLOR_CONDITION_MEMORY = scenarioPalette[2]

COLOR_MODEL_BASIC = cbPalette[1]
COLOR_MODEL_SURPRISE = cbPalette[3]
COLOR_MODEL_FULL = cbPalette[4]
COLOR_MODEL_RECENCY = cbPalette[2]

COLORS_MODELS_ALL = c(COLOR_MODEL_RECENCY,COLOR_MODEL_SURPRISE,COLOR_MODEL_FULL,COLOR_MODEL_BASIC)
COLORS_MODELS_ALL_REORDERED = c(COLOR_MODEL_BASIC,COLOR_MODEL_FULL,COLOR_MODEL_SURPRISE,COLOR_MODEL_RECENCY)

apply_modelNames_sim <- function(df_col){
  return(factor(df_col,
                levels=rev(models),
                labels=rev(model_descriptions_sim)))
}

apply_modelNames_exp <- function(df_col){
  return(factor(df_col,
                levels=rev(exp_models),
                labels=rev(model_descriptions_exp)))
}

NUM_PARTICIPANTS <- 346

# ###############################################################################
# ############################ Code for identifying which combs to rerun ########
# ###############################################################################
d <- load_fitting_data()
d$input_model <-apply_modelNames_exp(d$input_model)
d$fitted_model <- apply_modelNames_sim(d$fitted_model)

d <- d %>% filter(age_bin=='38-47')





################################################################################################
# Final Parameter plots
################################################################################################

d_best <- d %>% filter(fitted_model=='GP + R')
# Either show log params (see limits easier), or actual params
# d_actualparams <- d_best %>%
#   mutate(lambda=exp(lambda)) %>%
#   mutate(beta=exp(beta)) %>%
#   mutate(tau=exp(tau))
# d_best <- d_actualparams


d_best_mem <- d_best %>% filter(input_model=='Memory')
d_best_nomem <- d_best %>% filter(input_model=='Baseline')

memoryParams <- ddply(d_best_mem, ~participant, plyr::summarize, memory_lambda=mean(lambda), memory_beta = mean(beta), memory_tau = mean(tau))
baselineParams <- ddply(d_best_nomem, ~participant, plyr::summarize, baseline_lambda=mean(lambda), baseline_beta = mean(beta), baseline_tau = mean(tau))
corDF <- merge(memoryParams, baselineParams, by="participant")


dLambda <- d_best %>% 
  mutate(baseline_lambda= ifelse(input_model=='Baseline',lambda,NA)) %>% 
  mutate(memory_lambda= ifelse(input_model=='Memory',lambda,NA)) %>% 
  select(UID,baseline_lambda,memory_lambda,fitted_model,age_bin) %>%
  group_by(UID,fitted_model,age_bin) %>%
  summarise(baseline_lambda=median(baseline_lambda,na.rm=TRUE),memory_lambda=median(memory_lambda,na.rm=TRUE))

dBeta <- d_best %>% 
  mutate(baseline_beta= ifelse(input_model=='Baseline',beta,NA)) %>% 
  mutate(memory_beta= ifelse(input_model=='Memory',beta,NA)) %>% 
  select(UID,baseline_beta,memory_beta,fitted_model,age_bin) %>% 
  group_by(UID,fitted_model,age_bin) %>% 
  summarise(baseline_beta=median(baseline_beta,na.rm=TRUE),memory_beta=median(memory_beta,na.rm=TRUE))

dTau <- d_best %>% 
  mutate(baseline_tau= ifelse(input_model=='Baseline',tau,NA)) %>% 
  mutate(memory_tau= ifelse(input_model=='Memory',tau,NA)) %>% 
  select(UID,baseline_tau,memory_tau,fitted_model,age_bin) %>% 
  group_by(UID,fitted_model,age_bin) %>% 
  summarise(baseline_tau=median(baseline_tau,na.rm=TRUE),memory_tau=median(memory_tau,na.rm=TRUE))


dRecency <- d_best %>% 
  filter(fitted_model %in% c('GP + R','GP + RS')) %>% 
  mutate(recency=ifelse(fitted_model=='GP + R',recency,full_r)) %>% 
  mutate(baseline_recency= ifelse(input_model=='Baseline',recency,NA)) %>% 
  mutate(memory_recency= ifelse(input_model=='Memory',recency,NA)) %>% 
  select(UID,baseline_recency,memory_recency,fitted_model,age_bin) %>% 
  group_by(UID,fitted_model,age_bin) %>% 
  summarise(baseline_recency=max(baseline_recency,na.rm=TRUE),memory_recency=max(memory_recency,na.rm=TRUE))

dSurprise <- d %>% 
  filter(fitted_model %in% c('GP + S','GP + RS')) %>% 
  mutate(surprise=ifelse(fitted_model=='GP + S',surprise_plus,full_s)) %>% 
  mutate(baseline_surprise= ifelse(input_model=='Baseline',surprise,NA)) %>% 
  mutate(memory_surprise= ifelse(input_model=='Memory',surprise,NA)) %>% 
  select(UID,baseline_surprise,memory_surprise,fitted_model,age_bin) %>% 
  group_by(UID,fitted_model,age_bin) %>% 
  summarise(baseline_surprise=max(baseline_surprise,na.rm=TRUE),memory_surprise=max(memory_surprise,na.rm=TRUE))

dAsymmetry <- d %>% 
  filter(fitted_model %in% c('GP + S','GP + RS')) %>% 
  mutate(asymmetry=ifelse(fitted_model=='GP + S',asymmetry,asymmetry)) %>% 
  mutate(baseline_asymmetry= ifelse(input_model=='Baseline',asymmetry,NA)) %>% 
  mutate(memory_asymmetry= ifelse(input_model=='Memory',asymmetry,NA)) %>% 
  select(UID,baseline_asymmetry,memory_asymmetry,fitted_model,age_bin) %>% 
  group_by(UID,fitted_model,age_bin) %>% 
  summarise(baseline_asymmetry=max(baseline_asymmetry,na.rm=TRUE),memory_asymmetry=max(memory_asymmetry,na.rm=TRUE))



################################################################################################
# Lambda
################################################################################################
uppertquartiles <- c(quantile(dLambda$memory_lambda, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dLambda$baseline_lambda, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dLambda$memory_lambda, na.rm = T), 1.5 * IQR(dLambda$baseline_lambda, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
#Density for plotting
corDF$lambdaDensity <- fields::interp.surface(MASS::kde2d(memoryParams$memory_lambda, baselineParams$baseline_lambda), corDF[,c("memory_lambda", "baseline_lambda")]) #bivariate point density

dLambdaAge <- ddply(dLambda, ~age_bin, plyr::summarize, baseline_lambda=mean(baseline_lambda), memory_lambda=mean(memory_lambda))
p4a <- ggplot(subset(dLambda,memory_lambda <= upperLimit &  baseline_lambda<= upperLimit), aes(y=memory_lambda, x=baseline_lambda),group=age_bin) +
  geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
  geom_point(alpha=0.5)+
  geom_point(data=dLambdaAge,mapping = aes(x=baseline_lambda,y=memory_lambda,color=age_bin,fill=age_bin),shape=23,size=4.)+
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  labs(color='Age Group',fill='Age Group')+
  expand_limits(x = -1, y = -1) +
  # coord_cartesian(xlim=c(0,1),ylim=c(0,1))+
  
  xlab(expression("Control "*lambda))+
  ylab(expression("Memory "*lambda)) +
  guides(shape = guide_legend(override.aes = list(size = 0.5)))+
  guides(fill = guide_legend(reverse=TRUE),color= guide_legend(reverse=TRUE))+
  theme_classic()+
  theme(text = element_text(family="sans"))+
  theme(text = element_text(size=12,  family="sans"))+
  theme(legend.key.size = unit(0.5,'cm'),
        legend.background = element_blank(), legend.position='right')
leg_params <- get_legend(p4a) #extract legend
p4a <- p4a + theme(legend.position = 'none')


ttestPretty(dLambda$baseline_lambda, dLambda$memory_lambda, paired = T)
corTestPretty(dLambda$baseline_lambda, dLambda$memory_lambda,method = 'kendall')
p4a <- p4a + annotation_custom(grid::textGrob(label = (expression(bold(tau*" = 0.28"))),
                                              x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                              gp = grid::gpar(cex = 0.9)))

# p4a <- ggExtra::ggMarginal(p4a, type = "boxplot", alpha=1, fill='transparent', size = 10, outlier.shape = NA) #fill='transparent',
p4a



# correlation test (kendall’s tau for rank correlation) to see if they remain correlated

################################################################################################
# Beta
################################################################################################
uppertquartiles <- c(quantile(dBeta$memory_beta, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dBeta$baseline_beta, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dBeta$memory_beta, na.rm = T), 1.5 * IQR(dBeta$baseline_beta, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
#Density for plotting
corDF$betaDensity <- fields::interp.surface(MASS::kde2d(memoryParams$memory_beta, baselineParams$baseline_beta), corDF[,c("memory_beta", "baseline_beta")]) #bivariate point density

dBetaAge <- ddply(dBeta, ~age_bin, plyr::summarize, baseline_beta=mean(baseline_beta), memory_beta=mean(memory_beta))
p4b <- ggplot(subset(dBeta,memory_beta <= upperLimit &  baseline_beta<= upperLimit), aes(y=memory_beta, x=baseline_beta)) +
  geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
  geom_point(alpha=0.5)+
  geom_point(data=dBetaAge,mapping = aes(x=baseline_beta,y=memory_beta,color=age_bin,fill=age_bin),shape=23,size=4.)+
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  labs(color='Age Group',fill='Age Group')+
  # coord_cartesian(xlim=c(-7,-4),ylim = c(-7,-4))+
  expand_limits(x = 0, y = 0) +
  xlab(expression("Control "*beta))+
  ylab(expression("Memory "*beta)) +
  theme_classic()+
  theme(text = element_text(size=12,  family="sans"))+
  theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
p4b <- p4b + theme(legend.position = 'none')
ttestPretty(dBeta$baseline_beta, dBeta$memory_beta, paired = T)
corTestPretty(dBeta$baseline_beta, dBeta$memory_beta, method = 'kendall')
p4b <- p4b + annotation_custom(grid::textGrob(label = (expression(bold(tau*" = 0.35"))),
                                              x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                              gp = grid::gpar(cex = 0.9)))


# p4b <- ggExtra::ggMarginal(p4b, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
p4b




################################################################################################
# Tau
################################################################################################
uppertquartiles <- c(quantile(dTau$memory_tau, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dTau$baseline_tau, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dTau$memory_tau, na.rm = T), 1.5 * IQR(dTau$baseline_tau, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
#Density for plotting
corDF$tauDensity <- fields::interp.surface(MASS::kde2d(memoryParams$memory_tau, baselineParams$baseline_tau), corDF[,c("memory_tau", "baseline_tau")]) #bivariate point density

dTauAge <- ddply(dTau, ~age_bin, plyr::summarize, baseline_tau=mean(baseline_tau), memory_tau=mean(memory_tau))
p4c <- ggplot(subset(dTau,memory_tau <= upperLimit &  baseline_tau<= upperLimit), aes(y=memory_tau, x=baseline_tau)) +
  geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
  # geom_point()+
  geom_point(alpha=0.5)+
  geom_point(data=dTauAge,mapping = aes(x=baseline_tau,y=memory_tau,color=age_bin,fill=age_bin),shape=23,size=4.)+
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  labs(color='Age Group',fill='Age Group')+
  # coord_cartesian(xlim=c(-5,-3),ylim = c(-5,-3))+
  xlab(expression("Control "*tau))+
  ylab(expression("Memory "*tau)) +
  theme_classic()+
  scale_color_viridis(discrete=TRUE, direction=1) +
  theme(text = element_text(size=12,  family="sans"))+
  theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
p4c <- p4c + theme(legend.position = 'none')
ttestPretty(dTau$baseline_tau, dTau$memory_tau, paired = T)
corTestPretty(dTau$baseline_tau, dTau$memory_tau, method = 'kendall')
p4c <- p4c + annotation_custom(grid::textGrob(label = (expression(bold(tau*" = 0.44"))),
                                              x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                              gp = grid::gpar(cex = 0.9)))

# p4c <- ggExtra::ggMarginal(p4c, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
p4c


################################################################################################
# Recency
################################################################################################
uppertquartiles <- c(quantile(dRecency$memory_recency, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dRecency$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dRecency$memory_recency, na.rm = T), 1.5 * IQR(dRecency$baseline_recency, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
#Density for plotting
corDF$recencyDensity <- fields::interp.surface(MASS::kde2d(memoryParams$memory_recency, baselineParams$baseline_recency), corDF[,c("memory_recency", "baseline_recency")]) #bivariate point density

dRecencyAge <- ddply(dRecency, ~age_bin, plyr::summarize, baseline_recency=mean(baseline_recency), memory_recency=mean(memory_recency))
p4d <- ggplot(subset(dRecency,memory_recency <= upperLimit &  baseline_recency<= upperLimit), aes(y=memory_recency,x= baseline_recency)) +
  geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
  geom_point(alpha=0.5)+
  geom_point(data=dRecencyAge,mapping = aes(x=baseline_recency,y=memory_recency,color=age_bin,fill=age_bin),shape=23,size=4.)+
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  labs(color='Age Group',fill='Age Group')+
  # coord_cartesian(xlim=c(100,300),ylim = c(100,300))+
  # expand_limits(x = 0, y = 0) +
  xlab(expression("Control "*Recency))+
  ylab(expression("Memory "*Recency)) +
  # coord_cartesian(xlim = c(-100,400),ylim = c(-100,400))
  xlim(c(-50,400))+
  ylim(c(-50,400))+
  theme_classic()+
  theme(text = element_text(size=12,  family="sans"))+
  theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
p4d <- p4d + theme(legend.position = 'none')
ttestPretty(dRecency$baseline_recency, dRecency$memory_recency, paired = T)
corTestPretty(dRecency$baseline_recency, dRecency$memory_recency, method = 'kendall')
p4d <- p4d + annotation_custom(grid::textGrob(label = (expression(bold(tau*" = 0.34"))),
                                              x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                              gp = grid::gpar(cex = 0.9)))

# p4d <- ggExtra::ggMarginal(p4d, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
p4d



pParams <- cowplot::plot_grid(p4a,p4b, p4c,p4d, nrow = 2,align = 'vh',labels=c('b','c','d','e'), label_size = 20)
pParams <- cowplot::plot_grid(pParams,leg_params, rel_widths =  c(1,.15), nrow = 1)
pParams

d_exp <- load_exp_data()
d_exp_okay <-  d_exp %>% 
  filter(age_bin!='38-47') %>% 
  group_by(UID,age_bin) %>% 
  summarise(meanReward=mean(z),sdReward=sd(z),medianTime=median(time)) %>% 
  group_by(age_bin) %>% 
  summarise(meanGroupReward=mean(meanReward),sdGroupReward=sd(meanReward),medianGroupTime=median(medianTime)) 
  
d_exp_outliers <- d_exp %>% 
  filter(age_bin=='38-47') %>% 
  group_by(UID,age_bin) %>% 
  summarise(meanReward=mean(z),sdReward=sd(z),medianTime=median(time)) %>% 
  group_by(age_bin) %>% 
  summarise(meanGroupReward=mean(meanReward),sdGroupReward=sd(meanReward),medianGroupTime=median(medianTime)) 

d_exp_outliers <- d_exp %>% 
  filter(age_bin!='38-47') %>% 
  group_by(UID,age_bin) %>% 
  summarise(meanReward=mean(z),sdReward=sd(z),meanTime=mean(time))
