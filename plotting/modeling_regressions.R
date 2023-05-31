# modeling_cogsci

#Plotting fitting results
rm(list=ls())

# package <- c('report')
# invisible(lapply(package, require, character.only = TRUE)) #loads packages

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot', 'viridis', 'entropy', 'lme4', 'sjPlot', 'brms', 'withr', 'tidyr', 'ggbeeswarm','jsonlite','tools','corrplot', "ggExtra", 'gridExtra','xtable')
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

VIRIDIS_PALETTE_INCL_BLACK <- c("gray",'#fde725','#7ad151','#22a884','#2a788e','#414487','#440154')

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

modelpath = 'brmsModels'
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

# ###############################################################################
# ############################ Code for identifying which combs to rerun ########
# ###############################################################################
d <- load_fitting_data()


d$input_model <-apply_modelNames_exp(d$input_model)
d$fitted_model <- apply_modelNames_sim(d$fitted_model)




################################################################################################
# Median Log-Parameter regressions by age
################################################################################################

d_demographics <- load_exp_data() %>% distinct(participant,age,education,education_levels)
d_merged <- merge(d,d_demographics)

nLL_null <- (-25*log(1/121)*7)
d_fitting_medians <-d_merged %>% 
  group_by(participant,age,input_model,fitted_model,age_bin) %>% 
  summarise(sum_nLL=sum(nLL),median_recency=median(recency),median_lambda=median(lambda),median_beta=median(beta),median_tau=median(tau)) %>% 
  mutate(Rsquared=1-(sum_nLL/nLL_null))


dRecency<- d_fitting_medians %>% filter(fitted_model=='GP + R')


mLambda <- run_model(brm(median_lambda ~ age*input_model + (1+age+input_model  |participant),
                         data = dRecency, cores=4,
                         iter = 4000, warmup = 1000,
                         control = list(adapt_delta = 0.99)
),
modelName = paste0('brm_median_lambda'))

mbeta <- run_model(brm(median_beta ~ age*input_model + (1+age+input_model  |participant),
                       data = dRecency, cores=4,
                       iter = 4000, warmup = 1000,
                       control = list(adapt_delta = 0.99)
),
modelName = paste0('brm_median_beta'))

mtau <- run_model(brm(median_tau ~ age*input_model + (1+age+input_model  |participant),
                      data = dRecency, cores=4,
                      iter = 4000, warmup = 1000,
                      control = list(adapt_delta = 0.99) #Default =0.95 - only increase when warning with divergent transitions
),
modelName = paste0('brm_median_tau'))

mrecency <- run_model(brm(median_recency ~ age*input_model + (1+age+input_model  |participant),
                          data = dRecency, cores=4,
                          iter = 4000, warmup = 1000,
                          control = list(adapt_delta = 0.99) #Default =0.95 - only increase when warning with divergent transitions
),
modelName = paste0('brm_median_recency'))


# ################################################################################################
# # Median Parameter regressions by age
# ################################################################################################
# d_fitting_medians<-d_fitting_medians %>% 
#   mutate(median_lambda=exp(median_lambda))%>% 
#   mutate(median_beta=exp(median_beta))%>% 
#   mutate(median_beta=exp(median_tau))
# dRecency<- d_fitting_medians %>% filter(fitted_model=='GP + R')
# 
# 
# mLambda <- run_model(brm(median_lambda ~ age*input_model + (1+age+input_model  |participant),
#                          data = dRecency, cores=4, 
#                          iter = 4000, warmup = 1000,
#                          control = list(adapt_delta = 0.99)
# ), 
# modelName = paste0('brm_median_lambda_exp'))
# 
# mbeta <- run_model(brm(median_beta ~ age*input_model + (1+age+input_model  |participant),
#                        data = dRecency, cores=4, 
#                        iter = 4000, warmup = 1000,
#                        control = list(adapt_delta = 0.99) 
# ), 
# modelName = paste0('brm_median_beta_exp'))
# 
# mtau <- run_model(brm(median_tau ~ age*input_model + (1+age+input_model  |participant),
#                       data = dRecency, cores=4, 
#                       iter = 4000, warmup = 1000,
#                       control = list(adapt_delta = 0.99) #Default =0.95 - only increase when warning with divergent transitions
# ), 
# modelName = paste0('brm_median_tau_exp'))
# 
# mrecency <- run_model(brm(median_recency ~ age*input_model + (1+age+input_model  |participant),
#                           data = dRecency, cores=4, 
#                           iter = 4000, warmup = 1000,
#                           control = list(adapt_delta = 0.99) #Default =0.95 - only increase when warning with divergent transitions
# ), 
# modelName = paste0('brm_median_recency'))




# ################################################################################################
# # Absolute Parameter regressions by age
# ################################################################################################
# 
# d_demographics <- load_exp_data() %>% distinct(participant,age,education,education_levels)
# d_merged <- merge(d,d_demographics)
# 
# 
# dRecency<- d_merged %>% filter(fitted_model=='GP + R')
# 
# 
# mLambda <- run_model(brm(lambda ~ age*input_model + (1+age+input_model  |participant),
#                          data = dRecency, cores=4, 
#                          iter = 4000, warmup = 1000,
#                          control = list(adapt_delta = 0.99)
# ), 
# modelName = paste0('brm_lambda'))
# 
# mbeta <- run_model(brm(beta ~ age*input_model + (1+age+input_model  |participant),
#                        data = dRecency, cores=4, 
#                        iter = 4000, warmup = 1000,
#                        control = list(adapt_delta = 0.99) 
# ), 
# modelName = paste0('brm_beta'))
# 
# mtau <- run_model(brm(tau ~ age*input_model + (1+age+input_model  |participant),
#                       data = dRecency, cores=4, 
#                       iter = 4000, warmup = 1000,
#                       control = list(adapt_delta = 0.99) #Default =0.95 - only increase when warning with divergent transitions
# ), 
# modelName = paste0('brm_tau'))
# 
# mrecency <- run_model(brm(recency ~ age*input_model + (1+age+input_model  |participant),
#                           data = dRecency, cores=4, 
#                           iter = 4000, warmup = 1000,
#                           control = list(adapt_delta = 0.99) #Default =0.95 - only increase when warning with divergent transitions
# ), 
# modelName = paste0('brm_recency'))
# 
# 
# 
# 
# # plot_model(mLambda,type='pred',terms = c('age','input_model'))



################################################################################################
# Parameter regression Plots by age
################################################################################################
model_lambda=readRDS('brmsModels/brm_median_lambda.brm') 
plot_model(model_lambda,type='pred',terms=c('age','input_model'))


age = seq(18,77) 
input_model = levels(d$input_model)
newdat = expand.grid(age=age,input_model=input_model)

preds = fitted(model_lambda, re_formula=NA, newdata=newdat, probs=c(.025, .975))
predsDF = data.frame(age=rep(age, 2),
                     input_model=rep(input_model, each=length(age)),
                     estimate=preds[,1],
                     lower=preds[,3],
                     upper=preds[,4])


dLambdaAgeBinned <- d_merged %>%
  filter(fitted_model=='GP + R') %>% 
  filter(!is.na(lambda)) %>%
  group_by(participant,age_bin, input_model) %>%
  summarize(lambda =mean(lambda, na.rm=T)) %>%
  group_by(age_bin,input_model) %>%
  summarize(n = n(), meanLambda = mean(lambda), se = sd(lambda)/sqrt(n)) %>%
  mutate(lower = meanLambda - qt(1 - (0.05 / 2), n - 1) * se ,
         upper = meanLambda + qt(1 - (0.05 / 2), n - 1) * se )

dLambdaAgeBinned$age <- as.numeric(factor(dLambdaAgeBinned$age_bin)) #converts to the ones digit of the age bin
dLambdaAgeBinned$age  <- (dLambdaAgeBinned$age *10) + 12.5 #converts to the median age in the bin


dLambdaAgeBinned <- dLambdaAgeBinned %>% 
  mutate(meanLambda=exp(meanLambda)) %>% 
  mutate(lower=exp(lower)) %>% 
  mutate(upper=exp(upper))
predsDF <- predsDF %>% 
  mutate(estimate=exp(estimate)) %>% 
  mutate(lower=exp(lower)) %>% 
  mutate(upper=exp(upper))


pLambda <- ggplot()+
  geom_point(data = dLambdaAgeBinned,mapping = aes(x=age,y=meanLambda, color=input_model), position=position_dodge(width = 2))+
  geom_errorbar(data = dLambdaAgeBinned, aes(x=age,y=meanLambda,ymin = lower, ymax = upper, color=input_model ), width = 2, position=position_dodge(width = 2))+
  geom_line(predsDF, mapping=aes(x=age, y=estimate, color=input_model), size=.3) +
  geom_ribbon(predsDF, mapping=aes(x=age, y=estimate, ymin=lower, ymax=upper, fill=input_model), alpha=.3) +
  xlab('Age')+
  ylab(expression(lambda))+
  labs(color = 'Condition',fill='Condition')+
  # coord_cartesian(xlim=c(-0.5,0.5),ylim=c(0,15))+
  scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
  scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
  # ggtitle('Lambda ~ Age') +
  theme_classic() +
  theme(text = element_text(size=14,  family="sans"))+
  theme(legend.position = c(0,0), legend.justification=c(0,0),legend.key.size = unit(0.35,'cm'),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())

pLambda

#t-Test shows difference between conditions
ttestPretty(subset(dRecency, input_model == 'Baseline')$median_lambda, subset(dRecency, input_model == 'Memory')$median_lambda, paired=T)
corTestPretty(subset(dRecency, input_model == 'Baseline')$median_lambda,subset(dRecency, input_model == 'Baseline')$age,bonferroni = 2)
corTestPretty(subset(dRecency, input_model == 'Memory')$median_lambda,subset(dRecency, input_model == 'Memory')$age,bonferroni = 2)
corTestPretty(dRecency$median_lambda,dRecency$age,bonferroni = 1)
report_table(model_lambda)
#Summary shows no effects at all
summary(model_lambda)
plot_model(model_lambda)
# print(xtable(coef((model_lambda)), digits = 3,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4
# tab_model(model_lambda)

model_beta=readRDS('brmsModels/brm_median_beta.brm') 
plot_model(model_beta,type='pred',terms=c('age','input_model'))

age = seq(18,77) 
input_model = levels(d$input_model)
newdat = expand.grid(age=age,input_model=input_model)

preds = fitted(model_beta, re_formula=NA, newdata=newdat, probs=c(.025, .975))
predsDF = data.frame(age=rep(age, 2),
                     input_model=rep(input_model, each=length(age)),
                     estimate=preds[,1],
                     lower=preds[,3],
                     upper=preds[,4])

dBetaAgeBinned <- d_merged %>%
  filter(fitted_model=='GP + R') %>% 
  filter(!is.na(beta)) %>%
  group_by(participant,age_bin, input_model) %>%
  summarize(beta =mean(beta, na.rm=T)) %>%
  group_by(age_bin,input_model) %>%
  summarize(n = n(), meanBeta = mean(beta), se = sd(beta)/sqrt(n)) %>%
  mutate(lower = meanBeta - qt(1 - (0.05 / 2), n - 1) * se ,
         upper = meanBeta + qt(1 - (0.05 / 2), n - 1) * se )

dBetaAgeBinned$age <- as.numeric(factor(dBetaAgeBinned$age_bin)) #converts to the ones digit of the age bin
dBetaAgeBinned$age  <- (dBetaAgeBinned$age *10) + 12.5 #converts to the median age in the bin

dBetaAgeBinned <- dBetaAgeBinned %>% 
  mutate(meanBeta=exp(meanBeta)) %>% 
  mutate(lower=exp(lower)) %>% 
  mutate(upper=exp(upper))
predsDF <- predsDF %>% 
  mutate(estimate=exp(estimate)) %>% 
  mutate(lower=exp(lower)) %>% 
  mutate(upper=exp(upper))

pBeta <- ggplot()+
  geom_point(data = dBetaAgeBinned,mapping = aes(x=age,y=(meanBeta), color=input_model), position=position_dodge(width = 2))+
  geom_errorbar(data = dBetaAgeBinned, aes(x=age,y=(meanBeta),ymin = lower, ymax = upper, color=input_model ), width = 2, position=position_dodge(width = 2))+
  geom_line(predsDF, mapping=aes(x=age, y=(estimate), color=input_model), size=.3) +
  geom_ribbon(predsDF, mapping=aes(x=age, y=(estimate), ymin=lower, ymax=upper, fill=input_model), alpha=.3) +
  # geom_hline(yintercept=0, linetype='dashed', color='red') + # mean distance
  # facet_grid(cols = vars(input_model))+
  xlab('Age')+
  ylab(expression(beta*''))+
  labs(color = 'Condition',fill='Condition')+
  scale_y_continuous(trans='log10')+
  # coord_cartesian(xlim=c(-0.5,0.5),ylim=c(0,15))+
  scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
  scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
  # ggtitle('Beta ~ Age') +
  theme_classic() +
  theme(text = element_text(size=14,  family="sans"))+
  theme(legend.position = 'none')
  # theme(legend.position = c(0,0), legend.justification=c(0,0),legend.key.size = unit(0.35,'cm'),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())

pBeta
#t-Test shows difference between conditions
ttestPretty(subset(dRecency, input_model == 'Baseline')$median_beta, subset(dRecency, input_model == 'Memory')$median_beta, paired=T)
corTestPretty(subset(dRecency, input_model == 'Baseline')$median_beta,subset(dRecency, input_model == 'Baseline')$age,bonferroni = 2)
corTestPretty(subset(dRecency, input_model == 'Memory')$median_beta,subset(dRecency, input_model == 'Memory')$age,bonferroni = 2)
corTestPretty(dRecency$median_beta,dRecency$age,bonferroni = 1)

#Only age effect of beta
summary(model_beta)


model_tau=readRDS('brmsModels/brm_median_tau.brm') 
plot_model(model_tau,type='pred',terms=c('age','input_model'))

age = seq(18,77) 
input_model = levels(d$input_model)
newdat = expand.grid(age=age,input_model=input_model)

preds = fitted(model_tau, re_formula=NA, newdata=newdat, probs=c(.025, .975))
predsDF = data.frame(age=rep(age, 2),
                     input_model=rep(input_model, each=length(age)),
                     estimate=preds[,1],
                     lower=preds[,3],
                     upper=preds[,4])


dTauAgeBinned <- d_merged %>%
  filter(fitted_model=='GP + R') %>% 
  filter(!is.na(tau)) %>%
  group_by(participant,age_bin, input_model) %>%
  summarize(tau =mean(tau, na.rm=T)) %>%
  group_by(age_bin,input_model) %>%
  summarize(n = n(), meanTau = mean(tau), se = sd(tau)/sqrt(n)) %>%
  mutate(lower = meanTau - qt(1 - (0.05 / 2), n - 1) * se ,
         upper = meanTau + qt(1 - (0.05 / 2), n - 1) * se )

dTauAgeBinned$age <- as.numeric(factor(dTauAgeBinned$age_bin)) #converts to the ones digit of the age bin
dTauAgeBinned$age  <- (dTauAgeBinned$age *10) + 12.5 #converts to the median age in the bin

dTauAgeBinned <- dTauAgeBinned %>% 
  mutate(meanTau=exp(meanTau)) %>% 
  mutate(lower=exp(lower)) %>% 
  mutate(upper=exp(upper))
predsDF <- predsDF %>% 
  mutate(estimate=exp(estimate)) %>% 
  mutate(lower=exp(lower)) %>% 
  mutate(upper=exp(upper))

pTau <- ggplot()+
  geom_point(data = dTauAgeBinned,mapping = aes(x=age,y=(meanTau), color=input_model), position=position_dodge(width = 2))+
  geom_errorbar(data = dTauAgeBinned, aes(x=age,y=(meanTau),ymin = lower, ymax = upper, color=input_model ), width = 2, position=position_dodge(width = 2))+
  geom_line(predsDF, mapping=aes(x=age, y=(estimate), color=input_model), size=.3) +
  geom_ribbon(predsDF, mapping=aes(x=age, y=(estimate), ymin=lower, ymax=upper, fill=input_model), alpha=.3) +
  # geom_hline(yintercept=meanDist, linetype='dashed', color='red') + # mean distance
  # facet_grid(cols = vars(input_model))+
  xlab('Age')+
  ylab(expression(tau))+
  labs(color = 'Condition',fill='Condition')+
  # coord_cartesian(xlim=c(-0.5,0.5),ylim=c(0,15))+
  scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
  scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
  # ggtitle('Tau ~ Age') +
  theme_classic() +
  theme(text = element_text(size=14,  family="sans"))+
  theme(legend.position = 'none')
  # theme(legend.position = c(0,0), legend.justification=c(0,0),legend.key.size = unit(0.35,'cm'),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())

pTau

#t-Test shows no effects
ttestPretty(subset(dRecency, input_model == 'Baseline')$median_tau, subset(dRecency, input_model == 'Memory')$median_tau, paired=T)
corTestPretty(subset(dRecency, input_model == 'Baseline')$median_tau,subset(dRecency, input_model == 'Baseline')$age)
corTestPretty(subset(dRecency, input_model == 'Memory')$median_tau,subset(dRecency, input_model == 'Memory')$age)
corTestPretty(dRecency$median_tau,dRecency$age,bonferroni = 1)

#Summary shows no effects at all
summary(model_tau)


model_recency=readRDS('brmsModels/brm_median_recency.brm') 
plot_model(model_recency,type='pred',terms=c('age','input_model'))

age = seq(18,77) 
input_model = levels(d$input_model)
newdat = expand.grid(age=age,input_model=input_model)

preds = fitted(model_recency, re_formula=NA, newdata=newdat, probs=c(.025, .975))
predsDF = data.frame(age=rep(age, 2),
                     input_model=rep(input_model, each=length(age)),
                     estimate=preds[,1],
                     lower=preds[,3],
                     upper=preds[,4])


dRecencyAgeBinned <- d_merged %>%
  filter(fitted_model=='GP + R') %>% 
  filter(!is.na(recency)) %>%
  group_by(participant,age_bin, input_model) %>%
  summarize(recency =mean(recency, na.rm=T)) %>%
  group_by(age_bin,input_model) %>%
  summarize(n = n(), meanRecency = mean(recency), se = sd(recency)/sqrt(n)) %>%
  mutate(lower = meanRecency - qt(1 - (0.05 / 2), n - 1) * se ,
         upper = meanRecency + qt(1 - (0.05 / 2), n - 1) * se )

dRecencyAgeBinned$age <- as.numeric(factor(dRecencyAgeBinned$age_bin)) #converts to the ones digit of the age bin
dRecencyAgeBinned$age  <- (dRecencyAgeBinned$age *10) + 12.5 #converts to the median age in the bin


pRecency <- ggplot()+
  geom_point(data = dRecencyAgeBinned,mapping = aes(x=age,y=meanRecency, color=input_model), position=position_dodge(width = 2))+
  geom_errorbar(data = dRecencyAgeBinned, aes(x=age,y=meanRecency,ymin = lower, ymax = upper, color=input_model ), width = 2, position=position_dodge(width = 2))+
  geom_line(predsDF, mapping=aes(x=age, y=estimate, color=input_model), size=.3) +
  geom_ribbon(predsDF, mapping=aes(x=age, y=estimate, ymin=lower, ymax=upper, fill=input_model), alpha=.3) +
  geom_hline(yintercept=0, linetype='dashed', color='black') + 
  # facet_grid(cols = vars(input_model))+
  xlab('Age')+
  ylab(expression(w[r]))+
  labs(color = 'Condition',fill='Condition')+
  # coord_cartesian(xlim=c(-0.5,0.5),ylim=c(0,15))+
  scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
  scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
  # ggtitle('Recency ~ Age') +
  theme_classic() +
  theme(text = element_text(size=14,  family="sans"))+
  theme(legend.position = 'none')
  # theme(legend.position = c(0,0), legend.justification=c(0,0),legend.key.size = unit(0.35,'cm'),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())

pRecency

#t-Test shows difference between conditions
ttestPretty(subset(dRecency, input_model == 'Baseline')$median_recency, subset(dRecency, input_model == 'Memory')$median_recency, paired=T)
corTestPretty(subset(dRecency, input_model == 'Baseline')$median_recency,subset(dRecency, input_model == 'Baseline')$age)
corTestPretty(subset(dRecency, input_model == 'Memory')$median_recency,subset(dRecency, input_model == 'Memory')$age)
corTestPretty(dRecency$median_recency,dRecency$age,bonferroni = 1)
#Summary shows no effects at all
summary(model_recency)




pParams <- cowplot::plot_grid(pLambda,pBeta, pTau,pRecency, nrow = 2,align = 'vh',labels='auto', label_size = 20)
# pParams <- cowplot::plot_grid(pParams,leg_params, rel_widths =  c(1,.15), nrow = 1)
pParams
ggsave('plots/plots_modeling/params_regressions.pdf', pParams, width = 7, height = 5, units ='in')

pModeling3 <- cowplot::plot_grid(pxpPlots,
                                 pParams,
                                 nrow = 2, rel_heights = c(0.4,0.6),
                                 # labels = 'auto', label_size = 20,
                                 align='vh')
pModeling3
ggsave('plots/modeling_3b.pdf', pModeling3, width = 7, height = 10, units ='in')




d_Recency_medians <- d_fitting_medians %>% filter(fitted_model=='GP + R')
d_Recency_medians <- d_Recency_medians %>% 
  mutate(median_lambda=exp(median_lambda)) %>% 
  mutate(median_beta=exp(median_beta)) %>% 
  mutate(median_tau=exp(median_tau))

ttestPretty(subset(d_Recency_medians, input_model=='Memory')$median_lambda, subset(d_Recency_medians, input_model=='Baseline')$median_lambda, paired=T)
ttestPretty(subset(d_Recency_medians, input_model=='Memory')$median_beta, subset(d_Recency_medians, input_model=='Baseline')$median_beta, paired=T)
ttestPretty(subset(d_Recency_medians, input_model=='Memory')$median_tau, subset(d_Recency_medians, input_model=='Baseline')$median_tau, paired=T)
ttestPretty(subset(d_Recency_medians, input_model=='Memory')$median_recency, subset(d_Recency_medians, input_model=='Baseline')$median_recency, paired=T)


corTestPretty(d_Recency_medians$median_lambda, d_Recency_medians$age)
corTestPretty(d_Recency_medians$median_beta, d_Recency_medians$age)
corTestPretty(d_Recency_medians$median_tau, d_Recency_medians$age)
corTestPretty(d_Recency_medians$median_recency, d_Recency_medians$age)

# d_Recency_abs <- d_merged %>% filter(fitted_model=='GP + R')
# ttestPretty(subset(d_Recency_abs, input_model=='Memory')$lambda, subset(d_Recency_abs, input_model=='Baseline')$lambda, paired=T)
# ttestPretty(subset(d_Recency_abs, input_model=='Memory')$beta, subset(d_Recency_abs, input_model=='Baseline')$beta, paired=T)
# ttestPretty(subset(d_Recency_abs, input_model=='Memory')$tau, subset(d_Recency_abs, input_model=='Baseline')$tau, paired=T)
# ttestPretty(subset(d_Recency_abs, input_model=='Memory')$recency, subset(d_Recency_abs, input_model=='Baseline')$recency, paired=T)


# ################################################################################################
# # R-squared ~ params
# ################################################################################################
# # No diff in recency
# d_Recency_medians <- d_fitting_medians %>% filter(fitted_model=='GP + R')
# ttestPretty(subset(d_Recency_medians, input_model=='Memory')$Rsquared, subset(d_Recency_medians, input_model=='Baseline')$Rsquared, paired=T)
# 
# # Surprise fits control better
# d_Surprise_medians <- d_fitting_medians %>% filter(fitted_model=='GP + S')
# ttestPretty(subset(d_Surprise_medians, input_model=='Memory')$Rsquared, subset(d_Surprise_medians, input_model=='Baseline')$Rsquared, paired=T)
# 
# # Basic GP fits Memory better
# d_Null_medians <- d_fitting_medians %>% filter(fitted_model=='GP')
# ttestPretty(subset(d_Null_medians, input_model=='Memory')$Rsquared, subset(d_Null_medians, input_model=='Baseline')$Rsquared, paired=T)
# 
# 
# 
# ttestPretty(subset(d_Recency_medians, input_model=='Memory')$median_lambda, subset(d_Recency_medians, input_model=='Baseline')$median_lambda, paired=T)
# ttestPretty(subset(d_Recency_medians, input_model=='Memory')$median_recency, subset(d_Recency_medians, input_model=='Baseline')$median_recency, paired=T)
# ttestPretty(subset(d_Recency_medians, input_model=='Memory')$median_beta, subset(d_Recency_medians, input_model=='Baseline')$median_beta, paired=T)
# ttestPretty(subset(d_Recency_medians, input_model=='Memory')$median_tau, subset(d_Recency_medians, input_model=='Baseline')$median_tau, paired=T)
# 
# 
# 
# plambdaR = ggplot(d_Recency_medians, aes(x=median_lambda, y=Rsquared, fill=input_model, color=input_model)) +
#   geom_point(alpha=0.1)+
#   geom_smooth(alpha=0.2, method='lm')+
#   geom_hline(yintercept=0, linetype='dashed', color='red') + 
#   xlab(expression(lambda)) +
#   ylab('Pseudo R-Squared') +
#   labs(color='Condition',fill = "Condition") + 
#   scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
#   scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
#   theme_classic() +
#   theme(text = element_text(size=14,  family="sans"))+
#   theme(legend.position = c(0,1), legend.justification=c(0,1),legend.key.size = unit(0.35,'cm'),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
# 
# 
# plambdaR
# 
# pbetaR = ggplot(d_Recency_medians, aes(x=median_beta, y=Rsquared, fill=input_model, color=input_model)) +
#   geom_point(alpha=0.1)+
#   geom_smooth(alpha=0.2, method='lm')+
#   geom_hline(yintercept=0, linetype='dashed', color='red') + 
#   xlab(expression(beta)) +
#   ylab('Pseudo R-Squared') +
#   labs(color='Condition',fill = "Condition") + 
#   scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
#   scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
#   theme_classic() +
#   theme(text = element_text(size=14,  family="sans"))+
#   theme(legend.position = c(0,1), legend.justification=c(0,1),legend.key.size = unit(0.35,'cm'),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
# 
# 
# pbetaR
# 
# ptauR = ggplot(d_Recency_medians, aes(x=median_tau, y=Rsquared, fill=input_model, color=input_model)) +
#   geom_point(alpha=0.1)+
#   geom_smooth(alpha=0.2, method='lm')+
#   geom_hline(yintercept=0, linetype='dashed', color='red') + 
#   xlab(expression(tau)) +
#   ylab('Pseudo R-Squared') +
#   labs(color='Condition',fill = "Condition") + 
#   scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
#   scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
#   theme_classic() +
#   theme(text = element_text(size=14,  family="sans"))+
#   theme(legend.position = c(0,1), legend.justification=c(0,1),legend.key.size = unit(0.35,'cm'),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
# 
# 
# ptauR
# 
# 
# precencyR = ggplot(d_Recency_medians, aes(x=median_recency, y=Rsquared, fill=input_model, color=input_model)) +
#   geom_point(alpha=0.1)+
#   geom_smooth(alpha=0.2, method='lm')+
#   geom_hline(yintercept=0, linetype='dashed', color='red') + 
#   xlab(expression(recency)) +
#   ylab('Pseudo R-Squared') +
#   labs(color='Condition',fill = "Condition") + 
#   scale_color_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
#   scale_fill_manual(values = scenarioPalette, labels = c('Control', 'Memory')) +
#   theme_classic() +
#   theme(text = element_text(size=14,  family="sans"))+
#   theme(legend.position = c(0,1), legend.justification=c(0,1),legend.key.size = unit(0.35,'cm'),strip.background=element_blank(), legend.key=element_blank(), legend.background=element_blank())
# 
# 
# precencyR


