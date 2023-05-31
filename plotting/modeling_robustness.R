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

PLOT_PATH <- "plots/plots_modeling/experiment_full_11x11/extreme_participants/"
WORST_PATH <- 'fitting_results/output/verifying_initialization/experiment_data_Memory/'
BUGGED_MODELS <- 'bugged_models/'
CORRECT_MODELS <- 'corrected_models/'

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

COLOR_CONDITION_BASELINE = cbPalette[8]
COLOR_CONDITION_MEMORY = cbPalette[7]

COLOR_MODEL_BASIC = cbPalette[1]
COLOR_MODEL_SURPRISE = cbPalette[3]
COLOR_MODEL_FULL = cbPalette[4]
COLOR_MODEL_RECENCY = cbPalette[2]

COLORS_MODELS_ALL = c(COLOR_MODEL_FULL,COLOR_MODEL_BASIC,COLOR_MODEL_RECENCY,COLOR_MODEL_SURPRISE)

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


d<- load_fitting_data()



# ###############################################################################
# ############################ Asymmetry analysis ###############################
# ###############################################################################
# d_nn <- d %>% filter(surprise_plus<0) %>% filter(asymmetry<0) %>% summarise(meanNLL=mean(nLL),sd_NLL=sd(nLL),count=n())
# d_pp <- d %>% filter(surprise_plus>0) %>% filter(asymmetry>0) %>% summarise(meanNLL=mean(nLL),sd_NLL=sd(nLL),count=n())
# d_pn <- d %>% filter(surprise_plus>0) %>% filter(asymmetry<0) %>% summarise(meanNLL=mean(nLL),sd_NLL=sd(nLL),count=n())
# d_np <- d %>% filter(surprise_plus<0) %>% filter(asymmetry>0) %>% summarise(meanNLL=mean(nLL),sd_NLL=sd(nLL),count=n())
# 
# d_n <- d %>% filter(surprise_plus<0)
# d_p <- d %>% filter(surprise_plus>0)
# 
# d_consitent_participants <- d %>% 
#   # filter(surprise_plus<0) %>% filter(asymmetry<0) %>%
#   filter(fitted_model=='surprise+') %>% 
#   # filter(input_model=="Memory") %>% 
#   group_by(participant,fitted_model) %>% 
#   summarize(meanNLL = mean(nLL),sd_NLL=sd(nLL),sum_asymmetry=sum(ifelse(asymmetry> (1),1,0)),sum_surprise=sum(ifelse(surprise_plus> (1),1,0))) %>% 
#   mutate(both_neg= sum_asymmetry+sum_surprise)
# 
# pSurpriseCorr <- ggplot(d_consitent_participants, aes(x=both_neg, y=meanNLL)) +
#     # geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
#     geom_point(aes(color=fitted_model, shape=fitted_model), size =3, alpha=0.9)+
#     geom_smooth(method='lm')+
#     xlab(expression("Amount of similar signed cv rounds"))+
#     ylab(expression("Mean nLL"))
# pSurpriseCorr
# 
# 
# p_density <- ggplot(d, aes(x=surprise_plus)) + 
#   geom_histogram(aes(y=..density..),      
#                  binwidth=.5,
#                  colour="black", fill="white") +
#   geom_density(alpha=.2, fill="#FF6666")
# p_density
# 
# pSurpriseCorr <- ggplot(d_p, aes(x=surprise_plus, y=asymmetry)) +
#   # geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
#   geom_point(aes(color=fitted_model, shape=fitted_model), size =3, alpha=0.9)+
#   geom_smooth(method='lm')+
#   xlab(expression("Surprise weight"))+
#   ylab(expression("Asymmetry weight"))+
#   xlim(0,50)
# pSurpriseCorr
#####################################################################################################################
####################################### Plots #######################################################################
#####################################################################################################################

# d_diffNLL <- d %>% 
#   group_by(participant,input_model,fitted_model) %>% summarize(sumNLL = sum(nLL)) %>% 
#   group_by(participant,fitted_model) %>% summarise(nLL_diff =diff(sumNLL))
# 
# d_diffNLL$fitted_model <- apply_modelNames_sim(d_diffNLL$fitted_model)
# pNLLdiff = ggplot(d_diffNLL, aes(x=fitted_model, y=nLL_diff, fill=fitted_model,color=fitted_model)) +
#   geom_boxplot(color='black', outlier.shape = NA, width=.5) +
#   geom_hline(yintercept=0, linetype='dashed', color='red') + # random choice model
#   stat_summary(aes(x=fitted_model, y=nLL_diff), 
#                position = position_dodge(width = 0.5),fun=mean, geom="point", shape=23, fill="white", color='black', size=1.) +
#   # coord_cartesian(ylim=c(0, 800))+
#   scale_x_discrete(guide = guide_axis(n.dodge=2))+
#   xlab('Fitted Model') +
#   ylab('nLL Delta ') +
#   scale_fill_manual(breaks = unique(d_diffNLL$fitted_model),
#                     values = COLORS_MODELS_ALL)+
#   ggtitle('nLL Delta (Baseline-Memory)(data not fully complete)') +
#   # labs(fill = "Fitted Model") + 
#   theme_classic() +
#   theme(strip.background=element_blank())
# 
# pNLLdiff
# ggsave(filename = paste0(PLOT_PATH,"nLL_delta.png"), plot=pNLLdiff, height=2.25, width=4, units = "in")
# 
# 
# d_sumNLL = ddply(d, ~participant+input_model+fitted_model, plyr::summarize, sum_nLL=sum(nLL)) # TODO: switch back to sum when data is full
# d_sumNLL$input_model <-apply_modelNames_exp(d_sumNLL$input_model)
# d_sumNLL$fitted_model <- apply_modelNames_sim(d_sumNLL$fitted_model)
# pNLL = ggplot(d_sumNLL, aes(x=input_model, y=sum_nLL, fill=factor(fitted_model, levels=levels(fitted_model))), color=factor(fitted_model, levels=levels(fitted_model))) +
#   geom_boxplot(color='black', outlier.shape = NA, width=.5) +
#   # geom_hline(yintercept=(-25*7*log(1/121)), linetype='dashed', color='red') + # random choice model
#   geom_hline(yintercept=(-25*log(1/121)), linetype='dashed', color='red') + # random choice model
#   stat_summary(aes(x=input_model, y=sum_nLL, group=fitted_model), 
#                position = position_dodge(width = 0.5),fun=mean, geom="point", shape=23, fill="white", color='black', size=1.) +
#   # coord_cartesian(ylim=c(0, 800))+
#   scale_x_discrete(guide = guide_axis(n.dodge=2))+
#   xlab('Experiment Condition') +
#   ylab('Negative Log-Likelihood') +
#   scale_fill_manual(breaks = factor(d_sumNLL$fitted_model, levels=levels(d_sumNLL$fitted_model)),
#                     values = COLORS_MODELS_ALL)+
#   ggtitle('Model Performance: Mean nLL (data not fully complete)') +
#   labs(fill = "Fitted Model") + 
#   theme_classic() +
#   theme(strip.background=element_blank())
# 
# pNLL
# ggsave(filename = paste0(PLOT_PATH,"nLL.png"), plot=pNLL, height=2.25, width=4, units = "in")
# 
# # nLL_null <- (-25*7*log(1/121))
# nLL_null <- (-25*7*log(1/121))
# d_Rsquared <-d_sumNLL %>% 
#   mutate(Rsquared=1-(sum_nLL/nLL_null)) %>% 
#   dplyr::select(-sum_nLL)
# pRSquared = ggplot(d_Rsquared, aes(x=input_model, y=Rsquared, fill=fitted_model), color=fitted_model) +
#   geom_boxplot(color='black', outlier.shape = NA, width=.5) +
#   geom_hline(yintercept=0, linetype='dashed', color='red') + # random choice model
#   stat_summary(aes(x=input_model, y=Rsquared, group=fitted_model), 
#                position = position_dodge(width = 0.5),fun=mean, geom="point", shape=23, fill="white", color='black', size=1.) +
#   coord_cartesian(ylim=c(0,1))+
#   scale_x_discrete(guide = guide_axis(n.dodge=2))+
#   xlab('Experiment Condition') +
#   ylab('Pseudo R-Squared') +
#   scale_fill_manual(breaks = unique(d_sumNLL$fitted_model),
#                     values =COLORS_MODELS_ALL)+
#   ggtitle('Variance explained by the model') +
#   labs(fill = "Fitted Model") + 
#   theme_classic() +
#   theme(strip.background=element_blank())
# 
# pRSquared
# ggsave(filename = paste0(PLOT_PATH,"Rsquared.png"), plot=pRSquared, height=2.25, width=4, units = "in")
# 
# 
# d_eval_starting_locations <- d_sumNLL %>% 
#   filter(fitted_model == 'GP + RS') %>% 
#   filter(input_model=='Memory')
# 
# d_lowest <- d_eval_starting_locations %>% 
#   select(-fitted_model,input_model) %>% 
#   arrange(participant,desc(sum_nLL)) %>% 
#   top_n(n = -2) %>% 
#   dplyr::mutate(category = "low")
# 
# 
# d_highest <- d_eval_starting_locations %>% 
#   select(-fitted_model,input_model) %>% 
#   arrange(participant,(sum_nLL)) %>% 
#   top_n(n = 2) %>% 
#   dplyr::mutate(category = "high")
# 
# d_eval_starting_locations <- rbind(d_lowest,d_highest)
# 
# 
# 
# 
# d_sumlowest <-  d_sumNLL %>% 
#   filter(fitted_model %in% c('GP + RS','GP','GP + R')) %>% 
#   filter(input_model=='Memory') %>% 
#   group_by(participant) %>% 
#   summarise(nll=sum(sum_nLL))



# d_worst_111 <- data.frame()
# for (init in 1:5) {
#   d_worst_111<- rbind(d_worst_111,
#                   read.csv(
#                     paste0(WORST_PATH,'/fittedmodel_full/part_111_init_',init,'.csv'),
#                     col.names = c('rowname','nll','par1','par2','par3','par4','par5','par6')
#                   ) %>%
#                     select(-rowname) %>%
#                     summarise(sumNLL=sum(nll),par1=median(par1),par2=median(par2),par3=median(par3),par4=median(par4),par5=median(par5),par6=median(par6)) %>%
#                     mutate(participant=111)
#   )
# }
# d_111 <- d %>% filter(participant==111) %>% filter(fitted_model=='full') %>% filter(input_model=='Memory') %>% 
#   summarise(nll=sum(nLL),lambda=median(lambda),beta=median(beta),tau=median(tau),recency=median(full_r),surprise=median(full_s),asymmetry=median(asymmetry))


d_worst_244 <- data.frame()
for (init in 1:5) {
  for (fm in c('recency','null')) {
    d_worst_244<- rbind(d_worst_244,
                        read.csv(
                          paste0(WORST_PATH,'fittedmodel_',fm,'/part_244_init_',init,'.csv'),
                          col.names = c('rowname','nll','par1','par2','par3','par4','par5','par6')
                        ) %>%
                          select(-rowname) %>%
                          summarise(sumNLL=sum(nll),par1=median(par1),par2=median(par2),par3=median(par3),par4=median(par4),par5=median(par5),par6=median(par6)) %>%
                          mutate(participant=244) %>% 
                          mutate(model=fm)
    )
  }
  for(mv in c(BUGGED_MODELS,CORRECT_MODELS)){
    for (fm in c('full_basic','full_exp','surprise+_basic','surprise+_exp','surprise+_exp_movavg')) {
      temp_file <- paste0(WORST_PATH,mv,'fittedmodel_',fm,'/part_244_init_',init,'.csv')
      if(file.exists(temp_file)){
        d_worst_244<- rbind(d_worst_244,
                            read.csv(
                              temp_file,
                              col.names = c('rowname','nll','par1','par2','par3','par4','par5','par6')
                            ) %>%
                              select(-rowname) %>%
                              summarise(sumNLL=sum(nll),par1=median(par1),par2=median(par2),par3=median(par3),par4=median(par4),par5=median(par5),par6=median(par6)) %>%
                              mutate(participant=244) %>% 
                              mutate(model=paste0(mv,fm))
        )
      }
    }
  }
  
  
}

# d_244 <- d %>% filter(participant==244) %>% filter(fitted_model=='full') %>% filter(input_model=='Memory') %>% 
#   summarise(nll=sum(nLL),lambda=median(lambda),beta=median(beta),tau=median(tau),recency=median(full_r),surprise=median(full_s),asymmetry=median(asymmetry))


d_min <-d_worst_244 %>% 
  group_by(model) %>% 
  summarise(min_nll=min(sumNLL))
d_iterations <- data.frame()
for(fm in c('recency','null')){
  d_temp <- d_worst_244 %>% 
    filter(model==fm) %>% 
    arrange(desc(sumNLL)) %>% 
    mutate(iteration=row_number()) %>% 
    mutate(min_nll=min(sumNLL)) %>% 
    mutate(nll_diff= sumNLL-min_nll)
    
  d_iterations<-rbind(d_iterations,d_temp)
}

for(fm in c('full_basic','full_exp','surprise+_basic','surprise+_exp','surprise+_exp_movavg')){
  for(mv in c(BUGGED_MODELS,CORRECT_MODELS)){
    d_temp <- d_worst_244 %>% 
      filter(model==paste0(mv,fm)) %>% 
      arrange(desc(sumNLL)) %>% 
      mutate(iteration=row_number()) %>% 
      mutate(min_nll=min(sumNLL)) %>% 
      mutate(nll_diff= sumNLL-min_nll)
    
    d_iterations<-rbind(d_iterations,d_temp)
  }
}

p_initialization <- ggplot(data=d_iterations,mapping = aes(x=iteration, y=nll_diff,color=model,fill=model))+
  geom_point()+
  geom_line()+
  scale_color_viridis(discrete = T)
p_initialization
ggsave(paste0(PLOT_PATH,'/sensitivity.png'), p_initialization, width = 7, height = 3, units ='in')



nll_best <- d_iterations %>% mutate(abs_min=min(sumNLL)) %>% distinct(abs_min) %>% pull
# d_iterations_abs <- d_iterations %>% mutate(nll_diff = sumNLL-nll_best)
p_initialization_abs <- ggplot(data=d_iterations,mapping = aes(x=iteration, y=sumNLL,color=model,fill=model))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=nll_best, linetype='dashed', color='red')+
  scale_color_viridis(discrete = T)
p_initialization_abs
ggsave(paste0(PLOT_PATH,'/best_model_sensitivity.png'), p_initialization_abs, width = 7, height = 3, units ='in')

nll_best <- d_iterations %>% mutate(abs_min=min(sumNLL)) %>% distinct(abs_min) %>% pull
# d_iterations_abs <- d_iterations %>% mutate(nll_diff = sumNLL-nll_best)
p_initialization_abs <- ggplot(data=d_iterations,mapping = aes(x=iteration, y=sumNLL,color=model,fill=model))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=nll_best, linetype='dashed', color='red')+
  scale_color_viridis(discrete = T)
p_initialization_abs
ggsave(paste0(PLOT_PATH,'/best_model_sensitivity.png'), p_initialization_abs, width = 7, height = 3, units ='in')



d_iterations_highNLL<-data.frame()
for (mv in c(BUGGED_MODELS,CORRECT_MODELS)) {
  d_temp<- d_iterations %>% 
    filter(model %in% c('null',paste0(mv,'surprise+_basic'),paste0(mv,'surprise+_exp'),paste0(mv,'surprise+_exp_movavg')))
  d_iterations_highNLL <- rbind(d_iterations_highNLL,d_temp)
}
d_iterations_lowNLL <- d_iterations %>% 
  filter(!model %in% unique(d_iterations_highNLL$model))

nll_best_high <- d_iterations_highNLL %>% mutate(abs_min=min(sumNLL)) %>% distinct(abs_min) %>% pull
p_initialization_abs_high <- ggplot(data=d_iterations_highNLL,mapping = aes(x=iteration, y=sumNLL,color=model,fill=model))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=nll_best_high, linetype='dashed', color='red')+
  scale_color_viridis(discrete = T)
p_initialization_abs_high
ggsave(paste0(PLOT_PATH,'/best_model_sensitivity_highNLL.png'), p_initialization_abs_high, width = 7, height = 3, units ='in')

nll_best_low <- d_iterations_lowNLL %>% mutate(abs_min=min(sumNLL)) %>% distinct(abs_min) %>% pull
p_initialization_abs_low <- ggplot(data=d_iterations_lowNLL,mapping = aes(x=iteration, y=sumNLL,color=model,fill=model))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=nll_best_low, linetype='dashed', color='red')+
  scale_color_viridis(discrete = T)
p_initialization_abs_low
ggsave(paste0(PLOT_PATH,'/best_model_sensitivity_lowNLL.png'), p_initialization_abs_low, width = 7, height = 3, units ='in')





d_best_217 <- data.frame()
for (fm in c('recency','full','null')) {
  for (init in 1:10) {
    d_best_217<- rbind(d_best_217,
                                 read.csv(
                                   paste0(WORST_PATH,'fittedmodel_',fm,'/part_217_init_',init,'.csv'),
                                   col.names = c('rowname','nll','par1','par2','par3','par4','par5','par6')
                                 ) %>%
                                   select(-rowname) %>%
                                   summarise(sumNLL=sum(nll),par1=median(par1),par2=median(par2),par3=median(par3),par4=median(par4),par5=median(par5),par6=median(par6)) %>%
                                   mutate(participant=217) %>% 
                                   mutate(model=fm)
    )
  }
}
d_iterations_best <- data.frame()
for(fm in c('recency','full','null')){
  d_temp <- d_best_217 %>% 
    filter(model==fm) %>% 
    arrange(desc(sumNLL)) %>% 
    mutate(iteration=row_number()) 
  
  # if(fm=='full'){
  #   d_temp <- d_temp %>% filter(!row_number() %in% c(7:10))
  # }
  d_temp <- d_temp %>% 
    mutate(min_nll=min(sumNLL)) %>% 
    mutate(nll_diff= sumNLL-min_nll)
  
  d_iterations_best<-rbind(d_iterations_best,d_temp)
}


p_initialization_best <- ggplot(data=d_iterations_best,mapping = aes(x=iteration, y=nll_diff,color=model,fill=model))+
  geom_point()+
  geom_line()
p_initialization_best
ggsave(paste0(PLOT_PATH,'/sensitivity_best.png'), p_initialization_best, width = 7, height = 3, units ='in')



nll_best <- d_iterations_best %>% mutate(abs_min=min(sumNLL)) %>% distinct(abs_min) %>% pull
# d_iterations_abs_best <- d_iterations_best %>% mutate(nll_diff = sumNLL-nll_best)
p_initialization_abs_best <- ggplot(data=d_iterations_best,mapping = aes(x=iteration, y=sumNLL,color=model,fill=model))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=nll_best, linetype='dashed', color='red')
p_initialization_abs_best
ggsave(paste0(PLOT_PATH,'/best_model_sensitivity_best.png'), p_initialization_abs_best, width = 7, height = 3, units ='in')



d_plotting <- d_iterations %>% 
  mutate(par6=ifelse(grepl("surprise+", model, fixed=TRUE),par5,par6)) %>% 
  mutate(par5=ifelse(grepl("surprise+", model, fixed=TRUE),par4,par5)) %>% 
  mutate(par4=ifelse(grepl("surprise+", model, fixed=TRUE),NA,par4)) %>% 
  mutate(par6=ifelse(grepl("_exp", model, fixed=TRUE),exp(par6),par6)) 


p_lambda <- ggplot(d_plotting,aes(model,par1,fill=model))+
  geom_boxplot(outlier.color = NA,outlier.size = 0,outlier.shape = NA)+
  coord_cartesian(ylim = quantile(d_plotting$par1, c(0.1, 0.9)))+
  theme(axis.text.x=element_blank())
model_leg <- get_legend(p_lambda)
p_lambda<- p_lambda + theme(legend.position = 'none')
# p_lambda

p_beta <- ggplot(d_plotting,aes(model,par2,fill=model))+
  geom_boxplot(outlier.color = NA,outlier.size = 0,outlier.shape = NA)+
  coord_cartesian(ylim = quantile(d_plotting$par2, c(0.05, 0.95)))+
  theme(axis.text.x=element_blank(),legend.position = 'none')
# p_beta

p_tau <- ggplot(d_plotting,aes(model,par3,fill=model))+
  geom_boxplot(outlier.color = NA,outlier.size = 0,outlier.shape = NA)+
  coord_cartesian(ylim = quantile(d_plotting$par3, c(0.05, 0.95)))+
  theme(axis.text.x=element_blank(),legend.position = 'none')
# p_tau

p_recency <- ggplot(d_plotting,aes(model,par4,fill=model))+
  geom_boxplot(outlier.color = NA,outlier.size = 0,outlier.shape = NA)+
  coord_cartesian(ylim = quantile(d_plotting$par4, c(0.01, 0.99),na.rm = T))+
  theme(axis.text.x=element_blank(),legend.position = 'none')
# p_recency


p_surprise_pos <- ggplot(d_plotting,aes(model,par5,fill=model))+
  geom_boxplot(outlier.color = NA,outlier.size = 0,outlier.shape = NA)+
  coord_cartesian(ylim = quantile(d_plotting$par5, c(0.01, 0.99),na.rm = T))+
  theme(axis.text.x=element_blank(),legend.position = 'none')
# p_surprise_pos


p_surprise_neg <- ggplot(d_plotting,aes(model,par5*par6,fill=model))+
  geom_boxplot(outlier.color = NA,outlier.size = 0,outlier.shape = NA)+
  coord_cartesian(ylim = quantile(d_plotting$par5*d_plotting$par6, c(0.01, 0.99),na.rm = T))+
  theme(axis.text.x=element_blank(),legend.position = 'none')
# p_surprise_neg


p_asym<- ggplot(d_plotting,aes(model,par6,fill=model))+
  geom_boxplot(outlier.color = NA,outlier.size = 0,outlier.shape = NA)+
  coord_cartesian(ylim = quantile(d_plotting$par6, c(0.01, 0.99),na.rm = T))+
  theme(axis.text.x=element_blank(),legend.position = 'none')
# p_asym


pPlots_legendless <- cowplot::plot_grid(p_lambda,
                                p_beta,
                                p_tau,
                                p_recency,
                                p_surprise_pos,
                                p_surprise_neg,  
                                ncol = 3
)

pPlots <- cowplot::plot_grid(pPlots_legendless,model_leg,rel_widths = c(0.8,0.2),ncol = 2)
pPlots
ggsave(paste0(PLOT_PATH,'parameters.png'), pPlots, width = 16, height = 9, units ='in')
ggsave(paste0(PLOT_PATH,'parameters.pdf'), pPlots, width = 16, height = 9, units ='in')


