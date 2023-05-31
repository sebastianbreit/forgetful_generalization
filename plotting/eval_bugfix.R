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

COLOR_CONDITION_BASELINE = cbPalette[8]
COLOR_CONDITION_MEMORY = cbPalette[7]

COLOR_MODEL_BASIC = cbPalette[1]
COLOR_MODEL_RECENCY = cbPalette[2]
COLOR_MODEL_SURPRISE = cbPalette[3]
COLOR_MODEL_SURPRISE_PLUS = cbPalette[4]

PLOT_PATH="plots/plots_modeling/"

data_path <- paste0(homePath,"fitting_results/output/test_bugfix/experiment_data_Memory/")
df_fitting_bugged <- load_fitting_data() %>% 
  filter(participant %in% 1:5) %>% 
  filter(input_model=='Memory') %>% 
  filter(!fitted_model %in% c('recency','surprise+_static'))

df_fitting<- data.frame()
for(p in 1:5){
  for(fm in c('null','surprise+_exp_posterior','full_exp_posterior')){
    fitting_output_file<- paste0(data_path,'/fittedmodel_',fm,'/part_',p,'_init_1.csv')
    partial_entry <- read.csv(fitting_output_file)
    
    df_fitting <- rbind(df_fitting,data.frame(input_model='Memory',fmodel=fm,participant=p,
                            nLL=partial_entry[,2],lambda=partial_entry[,3],beta=partial_entry[,4],tau=partial_entry[,5],
                            recency= if(dim(partial_entry)[2]>=6 && fm%in%c('recency_exp','full_exp','full_exp_posterior')) partial_entry[,6] else NA,
                            surprise_plus= ifelse(dim(partial_entry)[2]>=6 && fm %in% c('surprise+_exp','surprise+_exp_movavg','surprise+_exp_posterior'), 
                                                  partial_entry[,6], ifelse(dim(partial_entry)[2]>=6 && fm %in% c('full_exp','full_exp_posterior'),partial_entry[,7],NA)),
                            asymmetry= ifelse( fm %in% c('surprise+_exp','surprise+_exp_movavg','surprise+_exp_posterior','full_exp','full_exp_posterior') ,ifelse(dim(partial_entry)[2]==7, partial_entry[,7],partial_entry[,8]), NA)))
    
    
  }
}
df_fitting<- df_fitting %>% mutate(fmodel=ifelse(fmodel=='surprise+_exp_posterior','surprise+',
                                    ifelse(fmodel== 'full_exp_posterior','full', 'null')))

df_fitting_bugged_new <- data.frame()
for(p in 1:5){
  temp <- df_fitting_bugged %>% filter(participant==p)
  for(fm in unique(df_fitting_bugged$fitted_model)){
    temp_model <- temp %>% filter(fitted_model==fm)
    df_fitting_bugged_new<- rbind(df_fitting_bugged_new,temp_model[5:7,])
  }
}
df_fitting_bugged<- df_fitting_bugged_new %>% mutate(fmodel=fitted_model) %>% select(-id,-UID,-education,-age_bin,-fitted_model) 

df_merged <- rbind(data.frame(df_fitting,type='fixed'),data.frame(df_fitting_bugged,type='bugged'))

ggplot(df_merged,aes(x=fmodel,y=nLL,fill=type))+
  geom_boxplot()
