#Plotting fitting results
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot', 'viridis', 'entropy', 'lme4', 'sjPlot', 'brms', 'withr', 'tidyr', 'ggbeeswarm','jsonlite','tools','corrplot', "ggExtra", 'gridExtra','caret')
invisible(lapply(packages, require, character.only = TRUE)) #loads packages

source('data_munging.R')
source('statistics.R')
set.seed(1)

load_recovery_data_and_save_to_csvs()
d <- load_recovery_data()

d %>% 
  group_by(input_model,fitted_model,participant) %>% 
  summarise(rows=n()) %>% 
  group_by(input_model,fitted_model) %>% 
  summarize(rowsum=sum(rows))

d_participant_medians <- d %>% 
  group_by(input_model,fitted_model,participant) %>% 
  summarise(mean_nLL=mean(nLL),median_recency=median(recency),median_lambda=median(lambda),median_beta=median(beta),median_tau=median(tau))

d_winning <- data.frame()
  
for (gm in c('recency','surprise+','full','null')) {
  d_temp <- d_participant_medians %>% 
    filter(input_model==gm)
  for (p in 1:30) {
    d_temp_temp <- d_temp %>% 
      filter(participant==p)
    winning <- (d_temp_temp[which.min(d_temp_temp$mean_nLL),]$fitted_model)
    d_winning<- rbind(d_winning, data.frame(part=p,generating=gm,winning=winning))
  }
}

d_confusion <- d_winning %>% 
  group_by(generating,winning) %>% 
  summarise(count=n()) %>% 
  mutate(generating=factor(generating, levels=c('null','recency','surprise+','full'),labels = c('GP','GP + R','GP + S', 'GP + RS'))) %>% 
  mutate(winning=factor(winning, levels=c('null','recency','surprise+','full'),labels = c('GP','GP + R','GP + S', 'GP + RS')))

p_conf <- ggplot(d_confusion, aes(x=generating, y=winning, fill=count/30)) + 
  geom_tile() +
  scale_fill_viridis(begin=min(d_inversion$p_inv),end=max(d_inversion$p_inv))+
  labs(x='Generating model',y='Fitted model',title = 'P( fitted model | generating model )',fill='p')+
  geom_text(aes(label=round(count/30,digits=2)))+

  theme_classic()
p_conf


leg_conf <- get_legend(p_conf) #extract legend
p_conf <- p_conf + theme(legend.position = 'none')

d_inversion_wins <- d_winning %>% 
  group_by(generating,winning) %>% 
  summarise(count=n()) %>% 
  mutate(generating=factor(generating, levels=c('null','recency','surprise+','full'),labels = c('GP','GP + R','GP + S', 'GP + RS'))) %>% 
  mutate(winning=factor(winning, levels=c('null','recency','surprise+','full'),labels = c('GP','GP + R','GP + S', 'GP + RS'))) %>% 
  group_by(winning) %>% 
  summarize(num_model_wins= sum(count))

d_inversion <- merge(d_confusion,d_inversion_wins,by='winning') %>% 
  mutate(p_inv=count/num_model_wins)

p_inv <- ggplot(d_inversion, aes(x=generating, y=winning, fill=p_inv)) + 
  geom_tile() +
  scale_fill_viridis(begin=min(d_inversion$p_inv),end=max(d_inversion$p_inv))+
  labs(x='Generating model',y='Fitted model',title = 'P( generating model | fitted model )')+
  geom_text(aes(label=round(p_inv,digits=2)))+
  
  theme_classic()+
  theme(legend.position = 'none')
p_inv


p_recovery <- cowplot::plot_grid(p_conf,p_inv,leg_conf,ncol=3, rel_widths = c(0.45,0.45,0.1),labels=c('a','b',NA))
p_recovery

ggsave('plots/plots_recovery/recovery.pdf', p_recovery, width = 10, height = 4, units ='in')
ggsave('plots/plots_recovery/recovery.png', p_recovery, width = 10, height = 4, units ='in')
