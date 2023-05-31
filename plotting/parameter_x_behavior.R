# parameter_x_behavior

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



################################################################################################
# Recency ~ Reward
################################################################################################
d_exp <- load_exp_data(includeLast = FALSE)
dDist <- ddply(d_exp, ~UID+scenario, plyr::summarize, meanDist = mean(z+.5, na.rm=T))

dDistRecency <- merge(dDist,dRecency,by='UID') %>%
  mutate(recency=ifelse(scenario=='Memory Condition',memory_recency,baseline_recency)) %>%
  select(-memory_recency,-baseline_recency)

pDistRecency <-ggplot(dDistRecency,aes(x=meanDist,y=recency,fill=scenario,color=scenario))+
  geom_point(alpha=0.3)+
  geom_smooth(method='lm')+
  scale_color_manual(values=scenarioPalette)
pDistRecency

corTestPretty(subset(dDistRecency,scenario=='Baseline Condition')$meanDist, subset(dDistRecency,scenario=='Baseline Condition')$recency)
corTestPretty(subset(dDistRecency,scenario=='Memory Condition')$meanDist, subset(dDistRecency,scenario=='Memory Condition')$recency)
################################################################################################
# beta ~ Distance
################################################################################################

dDist <- ddply(d_exp, ~UID+scenario, plyr::summarize, meanDist = mean(distance, na.rm=T))
dDistBeta <- merge(dDist,dBeta,by='UID') %>%
  mutate(beta=ifelse(scenario=='Memory Condition',memory_beta,baseline_beta)) %>%
  select(-memory_beta,-baseline_beta)

pDistBeta <-ggplot(dDistBeta,aes(x=meanDist,y=beta,fill=scenario,color=scenario))+
  geom_point(alpha=0.3)+
  geom_smooth(method='lm')+
  # ylim(c(0,2))+
  scale_color_manual(values=scenarioPalette)
pDistBeta

corTestPretty(subset(dDistBeta,scenario=='Baseline Condition')$meanDist, subset(dDistBeta,scenario=='Baseline Condition')$beta)
corTestPretty(subset(dDistBeta,scenario=='Memory Condition')$meanDist, subset(dDistBeta,scenario=='Memory Condition')$beta)

