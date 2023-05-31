#Plotting fitting results
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot', 'viridis', 'entropy', 'lme4', 'sjPlot', 'brms', 'withr', 'tidyr', 'ggbeeswarm','jsonlite','tools')
invisible(lapply(packages, require, character.only = TRUE)) #loads packages

source('data_munging.R')
################################################################################################
# Data import
################################################################################################
models <- c("recency","surprise+","full","null")#,"null" # exclude null model from analysis since some of them stopped
exp_models <- c("Memory", "NoMemory") #
model_descriptions_sim <-c("GP + R","GP + S+","GP + RS","GP")#
model_descriptions_exp <-c("Memory","Baseline")

PATH_HOME <- paste0(getwd(),'/')

d_full_fitting <- load_fitting_data()
d_partial_fitting <- load_fitting_data_without_bonus()
d_full_exp <- load_exp_data(includeLast = TRUE)

d_exp_bonus <- d_full_exp %>% 
  group_by(participant, scenario) %>% 
  summarise(hasBonus = ifelse(max(round)==15,TRUE,FALSE)) %>% 
  distinct() %>% 
  filter(hasBonus==TRUE) %>% 
  select(participant,scenario)

d_bonus_in_memory <- d_exp_bonus %>% 
  filter(scenario=='Memory Condition') %>% 
  select(participant)
d_bonus_in_baseline <- d_exp_bonus %>% 
  filter(scenario=='Baseline Condition') %>% 
  select(participant)

PATH_FAILED_FITTING <- paste0(PATH_HOME,'cluster-scripts/failed_fitting_combinations/')
write.table(d_bonus_in_memory,paste0(PATH_FAILED_FITTING,'rerun_baseline.csv'),row.names = FALSE,col.names = FALSE,eol=',')
write.table(d_bonus_in_baseline,paste0(PATH_FAILED_FITTING,'rerun_memory.csv'),row.names = FALSE,col.names = FALSE,eol=',')




d_incomplete <- d_full_fitting %>% 
  select(participant,input_model,fitted_model,nLL) %>% 
  group_by(participant,input_model,fitted_model) %>% 
  summarise(cv_rounds=n()) %>% 
  filter(cv_rounds <7)

d_incomplete_full <- d_incomplete %>% filter(fitted_model=='full')

d_incomplete <- d_incomplete_full # Only for full model rounds are missing
d_incomplete_mem <- d_incomplete %>% 
  filter(input_model=='Memory')
d_incomplete_nomem <- d_incomplete %>% 
  filter(input_model=='NoMemory')

d_incomplete_mismatch_mem <- d_incomplete_mem %>% filter(!participant %in% d_incomplete_nomem$participant)
d_incomplete_mismatch_nomem <- d_incomplete_nomem %>% filter(!participant %in% d_incomplete_mem$participant)
d_incomplete_mismatch <- rbind(d_incomplete_mismatch_mem,d_incomplete_mismatch_nomem)

# 3 participants actually had one condition finish but not the other
# However, for simplicity we should just fit all incomplete participants with the last 3 rounds again
d_refitting <- d_incomplete %>% ungroup() %>% distinct(participant)
write.table(d_refitting,paste0(PATH_FAILED_FITTING,'cv_timeout.csv'),row.names = FALSE,col.names = FALSE,eol=',')

d_refitting_mem <- d_refitting %>% filter(participant %in% d_bonus_in_baseline$participant)
d_refitting_nomem <- d_refitting %>% filter(participant %in% d_bonus_in_memory$participant)
write.table(d_refitting_mem,paste0(PATH_FAILED_FITTING,'rerun_memory_cv_timeout.csv'),row.names = FALSE,col.names = FALSE,eol=',')
write.table(d_refitting_nomem,paste0(PATH_FAILED_FITTING,'rerun_baseline_cv_timeout.csv'),row.names = FALSE,col.names = FALSE,eol=',')


