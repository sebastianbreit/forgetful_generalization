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
model_descriptions_sim <-c("GP + R","GP + S+","GP + RS","GP")#
model_descriptions_exp <-c("Memory","Baseline")

PLOT_PATH="plots/plots_cogsci/"

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
scenarioPalette <- c('#ef8a62','#67a9cf')

COLOR_CONDITION_BASELINE = cbPalette[8]
COLOR_CONDITION_MEMORY = cbPalette[7]

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
 
d <- load_fitting_data()



d$input_model <-apply_modelNames_exp(d$input_model)
d$fitted_model <- apply_modelNames_sim(d$fitted_model)
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
  filter(fitted_model %in% c('GP + S+','GP + RS')) %>% 
  mutate(surprise=ifelse(fitted_model=='GP + S+',surprise_plus,full_s)) %>% 
  mutate(baseline_surprise= ifelse(input_model=='Baseline',surprise,NA)) %>% 
  mutate(memory_surprise= ifelse(input_model=='Memory',surprise,NA)) %>% 
  select(UID,baseline_surprise,memory_surprise,fitted_model,age_bin) %>% 
  group_by(UID,fitted_model,age_bin) %>% 
  summarise(baseline_surprise=max(baseline_surprise,na.rm=TRUE),memory_surprise=max(memory_surprise,na.rm=TRUE))

dAsymmetry <- d %>% 
  filter(fitted_model %in% c('GP + S+','GP + RS')) %>% 
  mutate(asymmetry=ifelse(fitted_model=='GP + S+',asymmetry,asymmetry)) %>% 
  mutate(baseline_asymmetry= ifelse(input_model=='Baseline',asymmetry,NA)) %>% 
  mutate(memory_asymmetry= ifelse(input_model=='Memory',asymmetry,NA)) %>% 
  select(UID,baseline_asymmetry,memory_asymmetry,fitted_model,age_bin) %>% 
  group_by(UID,fitted_model,age_bin) %>% 
  summarise(baseline_asymmetry=max(baseline_asymmetry,na.rm=TRUE),memory_asymmetry=max(memory_asymmetry,na.rm=TRUE))


# ################################################################################################
# # Beta ~ Recency
# ################################################################################################
uppertquartiles <- c(quantile(dRecency$memory_recency, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dRecency$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dRecency$memory_recency, na.rm = T), 1.5 * IQR(dRecency$baseline_recency, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dRecencySub <- subset(dRecency,memory_recency <= upperLimit &  baseline_recency<= upperLimit)

uppertquartiles <- c(quantile(dBeta$memory_beta, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dBeta$baseline_beta, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dBeta$memory_beta, na.rm = T), 1.5 * IQR(dBeta$baseline_beta, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dBetaSub <- subset(dBeta,memory_beta <= upperLimit &  baseline_beta<= upperLimit)

dBetaRecencySub <- inner_join(dBetaSub,dRecencySub,by=c('UID','age_bin','fitted_model'))

p4e <- ggplot(dBetaRecencySub, aes(y=memory_recency,x= memory_beta)) +
  geom_point(alpha=0.5)+
  geom_smooth(method='lm')+
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  labs(color='Age Group',fill='Age Group')+
  xlab(expression("Memory "*beta))+
  ylab(expression("Memory "*Recency)) +
  # coord_cartesian(xlim = c(-100,400),ylim = c(-100,400))
  # xlim(c(-50,400))+
  ylim(c(-50,400))+
  theme_classic()+
  theme(text = element_text(size=16,  family="sans"))+
  theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
p4e <- p4e + theme(legend.position = 'none')
corTestPretty(dBetaRecencySub$memory_beta, dBetaRecencySub$memory_recency, method = 'pearson')

p4e <- p4e + annotation_custom(grid::textGrob(label = (expression(bold(r*" = -0.33"))),
                                              x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                              gp = grid::gpar(cex = 0.9)))

p4e <- ggExtra::ggMarginal(p4e, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
p4e


# ################################################################################################
# # Lambda ~ Recency
# ################################################################################################
uppertquartiles <- c(quantile(dRecency$memory_recency, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dRecency$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dRecency$memory_recency, na.rm = T), 1.5 * IQR(dRecency$baseline_recency, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dRecencySub <- subset(dRecency,memory_recency <= upperLimit &  baseline_recency<= upperLimit)

uppertquartiles <- c(quantile(dLambda$memory_lambda, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dLambda$baseline_lambda, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dLambda$memory_lambda, na.rm = T), 1.5 * IQR(dLambda$baseline_lambda, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dLambdaSub <- subset(dLambda,memory_lambda <= upperLimit &  baseline_lambda<= upperLimit)

dLambdaRecencySub <- inner_join(dLambdaSub,dRecencySub,by=c('UID','age_bin','fitted_model'))

p4f <- ggplot(dLambdaRecencySub, aes(y=memory_recency,x= memory_lambda)) +
  geom_point(alpha=0.5)+
  geom_smooth(method='lm')+
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  labs(color='Age Group',fill='Age Group')+
  xlab(expression("Memory "*lambda))+
  ylab(expression("Memory "*Recency)) +
  # coord_cartesian(xlim = c(-100,400),ylim = c(-100,400))
  # xlim(c(-50,400))+
  ylim(c(-50,400))+
  theme_classic()+
  theme(text = element_text(size=16,  family="sans"))+
  theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
p4f <- p4f + theme(legend.position = 'none')
corTestPretty(dLambdaRecencySub$memory_lambda, dLambdaRecencySub$memory_recency, method = 'pearson')

p4f <- p4f + annotation_custom(grid::textGrob(label = (expression(bold(r*" = 0.42"))),
                                              x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                              gp = grid::gpar(cex = 0.9)))

p4f <- ggExtra::ggMarginal(p4f, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
p4f

# ################################################################################################
# # Tau ~ Recency
# ################################################################################################
uppertquartiles <- c(quantile(dRecency$memory_recency, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dRecency$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dRecency$memory_recency, na.rm = T), 1.5 * IQR(dRecency$baseline_recency, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dRecencySub <- subset(dRecency,memory_recency <= upperLimit &  baseline_recency<= upperLimit)

uppertquartiles <- c(quantile(dTau$memory_tau, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dTau$baseline_tau, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dTau$memory_tau, na.rm = T), 1.5 * IQR(dTau$baseline_tau, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dTauSub <- subset(dTau,memory_tau <= upperLimit &  baseline_tau<= upperLimit)

dTauRecencySub <- inner_join(dTauSub,dRecencySub,by=c('UID','age_bin','fitted_model'))

p4g <- ggplot(dTauRecencySub, aes(y=memory_recency,x= memory_tau)) +
  geom_point(alpha=0.5)+
  # geom_smooth(method='lm')+
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  labs(color='Age Group',fill='Age Group')+
  xlab(expression("Memory "*tau))+
  ylab(expression("Memory "*Recency)) +
  # coord_cartesian(xlim = c(-100,400),ylim = c(-100,400))
  # xlim(c(-50,400))+
  ylim(c(-50,400))+
  theme_classic()+
  theme(text = element_text(size=16,  family="sans"))+
  theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
p4g <- p4g + theme(legend.position = 'none')
corTestPretty(dTauRecencySub$memory_tau, dTauRecencySub$memory_recency, method = 'pearson')

p4g <- p4g + annotation_custom(grid::textGrob(label = (expression(bold(r*" = None"))),
                                              x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                              gp = grid::gpar(cex = 0.9)))

p4g <- ggExtra::ggMarginal(p4g, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
p4g


# ################################################################################################
# # Beta ~ Recency
# ################################################################################################
uppertquartiles <- c(quantile(dRecency$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dRecency$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dRecency$baseline_recency, na.rm = T), 1.5 * IQR(dRecency$baseline_recency, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dRecencySub <- subset(dRecency,baseline_recency <= upperLimit &  baseline_recency<= upperLimit)

uppertquartiles <- c(quantile(dBeta$baseline_beta, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dBeta$baseline_beta, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dBeta$baseline_beta, na.rm = T), 1.5 * IQR(dBeta$baseline_beta, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dBetaSub <- subset(dBeta,baseline_beta <= upperLimit &  baseline_beta<= upperLimit)

dBetaRecencySub <- inner_join(dBetaSub,dRecencySub,by=c('UID','age_bin','fitted_model'))

p4a <- ggplot(dBetaRecencySub, aes(y=baseline_recency,x= baseline_beta)) +
  geom_point(alpha=0.5)+
  geom_smooth(method='lm')+
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  labs(color='Age Group',fill='Age Group')+
  xlab(expression("Baseline "*beta))+
  ylab(expression("Baseline "*Recency)) +
  # coord_cartesian(xlim = c(-100,400),ylim = c(-100,400))
  # xlim(c(-50,400))+
  ylim(c(-50,400))+
  theme_classic()+
  theme(text = element_text(size=16,  family="sans"))+
  theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
p4a <- p4a + theme(legend.position = 'none')
corTestPretty(dBetaRecencySub$baseline_beta, dBetaRecencySub$baseline_recency, method = 'pearson')

p4a <- p4a + annotation_custom(grid::textGrob(label = (expression(bold(r*" = -0.38"))),
                                              x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                              gp = grid::gpar(cex = 0.9)))

p4a <- ggExtra::ggMarginal(p4a, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
p4a


# ################################################################################################
# # Lambda ~ Recency
# ################################################################################################
uppertquartiles <- c(quantile(dRecency$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dRecency$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dRecency$baseline_recency, na.rm = T), 1.5 * IQR(dRecency$baseline_recency, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dRecencySub <- subset(dRecency,baseline_recency <= upperLimit &  baseline_recency<= upperLimit)

uppertquartiles <- c(quantile(dLambda$baseline_lambda, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dLambda$baseline_lambda, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dLambda$baseline_lambda, na.rm = T), 1.5 * IQR(dLambda$baseline_lambda, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dLambdaSub <- subset(dLambda,baseline_lambda <= upperLimit &  baseline_lambda<= upperLimit)

dLambdaRecencySub <- inner_join(dLambdaSub,dRecencySub,by=c('UID','age_bin','fitted_model'))

p4b <- ggplot(dLambdaRecencySub, aes(y=baseline_recency,x= baseline_lambda)) +
  geom_point(alpha=0.5)+
  geom_smooth(method='lm')+
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  labs(color='Age Group',fill='Age Group')+
  xlab(expression("Baseline "*lambda))+
  ylab(expression("Baseline "*Recency)) +
  # coord_cartesian(xlim = c(-100,400),ylim = c(-100,400))
  # xlim(c(-50,400))+
  ylim(c(-50,400))+
  theme_classic()+
  theme(text = element_text(size=16,  family="sans"))+
  theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
p4b <- p4b + theme(legend.position = 'none')
corTestPretty(dLambdaRecencySub$baseline_lambda, dLambdaRecencySub$baseline_recency, method = 'pearson')

p4b <- p4b + annotation_custom(grid::textGrob(label = (expression(bold(r*" = 0.36"))),
                                              x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                              gp = grid::gpar(cex = 0.9)))

p4b <- ggExtra::ggMarginal(p4b, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
p4b

# ################################################################################################
# # Tau ~ Recency
# ################################################################################################
uppertquartiles <- c(quantile(dRecency$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dRecency$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dRecency$baseline_recency, na.rm = T), 1.5 * IQR(dRecency$baseline_recency, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dRecencySub <- subset(dRecency,baseline_recency <= upperLimit &  baseline_recency<= upperLimit)

uppertquartiles <- c(quantile(dTau$baseline_tau, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dTau$baseline_tau, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
H <- c(1.5 * IQR(dTau$baseline_tau, na.rm = T), 1.5 * IQR(dTau$baseline_tau, na.rm = T))
upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
dTauSub <- subset(dTau,baseline_tau <= upperLimit &  baseline_tau<= upperLimit)

dTauRecencySub <- inner_join(dTauSub,dRecencySub,by=c('UID','age_bin','fitted_model'))

p4c <- ggplot(dTauRecencySub, aes(y=baseline_recency,x= baseline_tau)) +
  geom_point(alpha=0.5)+
  # geom_smooth(method='lm')+
  scale_color_viridis(discrete=TRUE, direction=1) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  labs(color='Age Group',fill='Age Group')+
  xlab(expression("Baseline "*tau))+
  ylab(expression("Baseline "*Recency)) +
  # coord_cartesian(xlim = c(-100,400),ylim = c(-100,400))
  # xlim(c(-50,400))+
  ylim(c(-50,400))+
  theme_classic()+
  theme(text = element_text(size=16,  family="sans"))+
  theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
p4c <- p4c + theme(legend.position = 'none')
corTestPretty(dTauRecencySub$baseline_tau, dTauRecencySub$baseline_recency, method = 'pearson')

p4c <- p4c + annotation_custom(grid::textGrob(label = (expression(bold(r*" = None"))),
                                              x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                              gp = grid::gpar(cex = 0.9)))

p4c <- ggExtra::ggMarginal(p4c, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
p4c


pParamCrossCorr <- cowplot::plot_grid(p4a,p4b,p4c,p4e,p4f,p4g,
                                ncol = 3 ,
                                labels = 'auto', label_size = 20,
                                align='vh')
pParamCrossCorr


ggsave('plots/param_crosscorr.pdf', pParamCrossCorr, width = 7, height = 10, units ='in')
