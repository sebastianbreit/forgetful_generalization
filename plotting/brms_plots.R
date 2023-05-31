rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'brms','lme4','lmerTest')
# packages <- cbind(c('tidyverse','ggmcmc','ggthemes','ggridges','ggbeeswarm',  'viridis', 'ggplot2',  'sjPlot'))
invisible(lapply(packages, require, character.only = TRUE)) #loads packages

#


source('data_munging.R')
set.seed(1)

homePath= paste0(getwd(),"/")
modelpath = 'brmsModels'
PLOT_PATH="plots/plots_brms"
PLOT_PATH_CLUSTER <- 'output/plots'
PLOT_PATH <-PLOT_PATH_CLUSTER

ifelse(!dir.exists(PLOT_PATH), dir.create(PLOT_PATH), "Folder exists already, overwriting data.")

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

d_demo <- load_anonymous_demographics()

d_demo_simplified <- d_demo %>% 
  mutate(Country.of.residence=ifelse((!Country.of.residence %in% c('South Africa','United Kingdom')),'Rest/Europe',
                                     ifelse(Country.of.residence=='South Africa','South Africa',
                                            ifelse(Country.of.residence=='United Kingdom','United Kingdom',NA)))) %>% 
  select(-age,-sex,-time,-education)

d_exp <- load_exp_data(includeLast = FALSE)
d <- merge(d_exp,d_demo_simplified,by='UID')
#Excluding sub-sample of South African participants due to large behavior differences
d <- d %>% filter(Country.of.residence!='South Africa')


producePlots <- 0
scaleParams <- 0
modelNo<-0
scalingDesc <- "unscaled"

# Input arguments for the script
args = commandArgs(TRUE)
modelNo <- as.integer(args[1])

#If 1: scale all params, if 0: scale none
# scaleParams <-1 # Scaled provides better readable results

#Scale all to 0 mean to have comparable effects
if(scaleParams==1){
  d$prev_z <- scale(d$prev_z)
  d$age <- scale(d$age)
  d$step <- scale(d$step)
  d$z <- scale(d$z)
  
  scalingDesc <- "scaled"
}

# d$distance <- scale(d$distance)
d <- d %>% filter(!is.na(prev_z))

if(modelNo==1){
  mRewardLinear <- run_model(brm(z ~ poly(age,2)* poly(prev_z,2)*scenario + (1+poly(age,2) +poly(prev_z,2)+scenario |id),                                 data = d, cores = 4, 
                                 iter = 4000, warmup = 2000,
                                 # control = list(adapt_delta = 0.99) #Default =0.95 - only increase when warning with divergent transitions
  ), 
  modelName = paste0('brm_',scalingDesc,'_reward_age_x_prevz_x_condition_4kiter'))
  
} else if(modelNo==2){
  mRewardLinearTime <- run_model(brm(distance ~ poly(age,2)* poly(prev_z,2)*scenario + (1+poly(age,2) +poly(prev_z,2) +scenario  |id),
                                     data = d, cores = 4, 
                                     iter = 4000, warmup = 2000,
                                     # control = list(adapt_delta = 0.99) #Default =0.95 - only increase when warning with divergent transitions
  ), 
  modelName = paste0('brm_',scalingDesc,'_distance_age_x_prevz_x_condition_4kiter'))
  
} else if(modelNo==3){
  
  mDistancePoly <- run_model(brm(distance ~ prev_z*scenario*age_bin + (1+prev_z+scenario+age_bin |id),                                 
                                 data = d, cores = 4, 
                                 # iter = 4000, warmup = 2000
                                 iter=4000, warmup=1000
  ), 
  modelName = paste0('brm_',scalingDesc,'_distance_agebin_x_prevz_x_condition_4kiter'))
}else if(modelNo==4){
  mDistPolyAge <- run_model(brm(distance ~ poly(age,2)* poly(prev_z,2)*scenario + (1+poly(age,2) +poly(prev_z,2) +scenario  |id),
                                data = d, cores = 4, 
                                iter = 4000, warmup = 1000,
                                control = list(adapt_delta = 0.99) #Default =0.95 - only increase when warning with divergent transitions
  ), 
  modelName = paste0('brm_',scalingDesc,'_distance_age_x_prevz_x_condition_4kiter_excl_SA'))
  
  # plot_model(mDistPolyAge,type='pred',terms=c('age','scenario'))
  # summary(mDistPolyAge)
}else if(modelNo==5){
  dAge <- ddply(d, ~id + age + scenario, plyr::summarize, meanDist = mean(distance, na.rm=T) )
  mAgeDist <- run_model(brm(meanDist~poly(age,2)*scenario + (1+poly(age,2)+scenario|id),
                            data =dAge, cores=4,
                            iter = 4000, warmup = 1000, 
                            control = list(adapt_delta = 0.99)),
                        modelName=paste0('brm_',scalingDesc,'_ageScenarioDist_excl_SA'))
  
  # summary(mAgeDist)
  # plot_model(mAgeDist,type='pred',terms=c('age','scenario'))
} else if(modelNo==6){
  mDistPolyAge_lmer <- run_model(lmer(formula = distance ~ poly(age,2)* poly(prev_z,2)*scenario + (1+poly(age,2) +poly(prev_z,2) + scenario  |id),
                               data    = d),
                               modelName=paste0('lmer_',scalingDesc,'_distance_age_x_prevz_x_condition_4kiter_excl_SA'))#to run the model
  # ranova(age_residency_reward)
}
