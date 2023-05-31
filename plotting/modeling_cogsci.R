# modeling_cogsci

#Plotting fitting results
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot', 'viridis', 'entropy', 'lme4', 'sjPlot', 'brms', 'withr', 'tidyr', 'ggbeeswarm','jsonlite','tools','corrplot', "ggExtra", 'gridExtra','rlang')
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


for(demo in c('_exclSA')){#,'_full','_onlySA'
  d <- load_fitting_data()
  
  
  d$input_model <-apply_modelNames_exp(d$input_model)
  d$fitted_model <- apply_modelNames_sim(d$fitted_model)
  
  # d <- d %>% 
  #   mutate(cv_round=rep(c(1:4),2*4*346)) 
  
  d_demo <- load_anonymous_demographics()
  
  d_demo_simplified <- d_demo %>% 
    mutate(Country.of.residence=ifelse((!Country.of.residence %in% c('South Africa','United Kingdom')),'Rest/Europe',
                                       ifelse(Country.of.residence=='South Africa','South Africa',
                                              ifelse(Country.of.residence=='United Kingdom','United Kingdom',NA)))) %>% 
    select(-sex,-time,-education)
  
  
  d <- merge(d,d_demo_simplified,by='UID')
  
  if(demo=='_exclSA'){
    d <- d %>% filter(Country.of.residence!='South Africa')
    PXP_PATH <- 'fitting_results/output/corrected_models/United KingdomOther/pxp/'
    
  }else if(demo=='_onlySA'){
    d <- d %>% filter(Country.of.residence=='South Africa')
    PXP_PATH <- 'fitting_results/output/corrected_models/South Africa/pxp/'
  }else{
    d <- d
    PXP_PATH <- 'fitting_results/output/corrected_models/full_demographics/pxp/'
  }
  
  
  ################################################################################################
  # Correlation Plot
  ################################################################################################
  
  d_corr_prep <- d %>% 
    mutate(surprise=surprise_plus) %>% 
    # mutate(recency=ifelse(fitted_model=='GP + R',recency,ifelse(fitted_model=='GP + RS',full_r,0))) %>% 
    # mutate(surprise=ifelse(fitted_model=='GP + S',surprise_plus,ifelse(fitted_model=='GP + RS',full_s,0))) %>%
    # mutate(asymmetry=ifelse(fitted_model=='GP + S',asymmetry,ifelse(fitted_model=='GP + RS',asymmetry,0))) %>% 
    select(participant,input_model,fitted_model,lambda,beta,tau, recency, surprise,asymmetry) 
  
  d_corr_temp = ddply(d_corr_prep, ~participant+input_model+fitted_model, 
                      plyr::summarize,mean_lambda=mean(lambda),mean_beta=mean(beta),mean_tau=mean(tau),
                      mean_recency=mean(recency),mean_surprise=mean(surprise),mean_asymmetry=mean(asymmetry))
  
  
  d_corr <- d_corr_temp %>% 
    # filter(input_model=='Memory') %>% 
    filter(fitted_model=='GP + RS') %>%
    select(-participant,-input_model,-fitted_model)
  
  
  # png(filename = paste0(PLOT_PATH,"correlation_fitted_plot.png"))
  M = cor(d_corr)
  p_corr <- corrplot.mixed(M)
  print(p_corr)
  dev.off()
  
  
  ################################################################################################
  # Pxp Plots
  ################################################################################################
  pxp_mem = read.csv(paste0(PXP_PATH,'PXP_memory.csv'), header=F)
  pxp_nomem = read.csv(paste0(PXP_PATH,'PXP_nomemory.csv'), header=F)
  pxp_all <- rbind(pxp_mem,pxp_nomem)
  for(ab in unique(levels(d$age_bin))){
    pxp_temp_mem <-  read.csv(paste0(PXP_PATH,'by_age/PXP_memory_',as.character(ab),'.csv'), header=F)
    pxp_temp_nomem <-  read.csv(paste0(PXP_PATH,'by_age/PXP_nomemory_',as.character(ab),'.csv'), header=F)
    pxp_all <-rbind(pxp_all,pxp_temp_mem,pxp_temp_nomem)
  }
  
  pxp_noOverall_no40s <- rbind(pxp_all[3:6,],pxp_all[9:14,])
  pxp_nomem_noOverall_no40s <- pxp_noOverall_no40s[seq(2,10,2),]
  pxp_mem_noOverall_no40s <- pxp_noOverall_no40s[seq(1,9,2),]
  
  # combine
  colnames(pxp_all) = c('GP', 'GP + R', 'GP + S', 'GP + RS')
  age_groups <- rev(c(rep('68-77',2),rep('58-67',2),rep('48-57',2),rep('38-47',2),rep('28-37',2),rep('18-27',2),rep('Overall',2)))
  pxp_all$agegroup = age_groups
  # pxp_all$agegroup = factor(pxp_all$agegroup, 
  #                           levels=age_groups)
  pxp_all$condition = rev(c(rep(c('Baseline','Memory'),7)))
  
  
  pxp = gather(pxp_all, key=ModelName, value=pxp, 1:4)
  pxp$ModelName <- factor(pxp$ModelName, levels =  c('GP', 'GP + R', 'GP + S', 'GP + RS'))
  
  xlabels <- expression('GP-UCB', lambda * " lesion", beta * " lesion", tau * " lesion")
  pxp_mem <- pxp %>% filter(condition=='Memory')
  pxp_nomem <- pxp %>% filter(condition=='Baseline')
  
  pxp_noOverall <- pxp %>% filter(agegroup!='Overall')
  pxp_Overall <- pxp %>% filter(agegroup=='Overall')
  
  pxp_mem_noOverall <- pxp_noOverall %>% filter(condition=='Memory')
  pxp_mem_Overall <- pxp_Overall %>% filter(condition=='Memory')
  
  
  pxp_nomem_noOverall <- pxp_noOverall %>% filter(condition!='Memory')
  pxp_nomem_Overall <- pxp_Overall %>% filter(condition!='Memory')
  
  p_pxp_mem = ggplot(pxp_mem, aes(x=pxp, y=ModelName, fill=agegroup)) +
    geom_bar(stat='identity', position="dodge2", color = 'black') +
    # geom_bar(data=pxp_mem_Overall,mapping=aes(x=pxp, y=ModelName), stat='identity', position="dodge2", color = 'black',fill='gray') + #
    xlab(expression(italic(pxp))) +
    ylab('') +
    scale_y_discrete(limits=rev)+
    expand_limits(x=c(0,1))+
    scale_fill_manual(values = rev(VIRIDIS_PALETTE_INCL_BLACK) )+
    labs(fill='Age Group')+
    guides(fill = guide_legend(reverse=TRUE))+
    ggtitle("Memory Condition") +
    theme_classic() +
    theme(text = element_text(size=12,  family="sans"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          panel.border=element_rect(colour = COLOR_CONDITION_MEMORY,fill=NA,size=3))+
    theme(legend.position= 'right',strip.background=element_blank(),plot.title = element_text(hjust = 0.5))
  p_pxp_mem
  
  p_pxp_nomem = ggplot(pxp_nomem, aes(x=pxp, y=ModelName, fill=agegroup)) +
    geom_bar(stat='identity', position="dodge2", color = 'black') +
    # geom_bar(data=pxp_nomem_Overall,mapping=aes(x=pxp, y=ModelName), stat='identity', position="dodge2", color = 'black',fill='gray') + #
    xlab(expression(italic(pxp))) +
    ylab('') +
    scale_y_discrete(limits=rev)+
    expand_limits(x=c(0,1))+
    scale_fill_manual(values = rev(VIRIDIS_PALETTE_INCL_BLACK) )+
    labs(fill='Age Group')+
    guides(fill = guide_legend(reverse=TRUE))+
    ggtitle("Control Condition") +
    theme_classic() +
    theme(text = element_text(size=12,  family="sans"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          panel.border=element_rect(colour = COLOR_CONDITION_BASELINE,fill=NA,size=3))+
    theme(legend.position= 'right',strip.background=element_blank(),plot.title = element_text(hjust = 0.5))
  p_pxp_nomem
  
  
  leg_pxp <- get_legend(p_pxp_mem) #extract legend
  p_pxp_mem <- p_pxp_mem + theme(legend.position = 'none')
  p_pxp_nomem <- p_pxp_nomem + theme(legend.position = 'none')
  
  pxpPlots <- cowplot::plot_grid(p_pxp_nomem, p_pxp_mem,leg_pxp, nrow = 1, rel_widths = c(1,1,.3))
  pxpPlots
  
  # ggsave('plots/pxp.pdf', pxpPlots, width = 7, height = 3, units ='in')
  
  
  
  
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
    select(UID,baseline_lambda,memory_lambda,fitted_model,age_bin,age) %>%
    group_by(UID,fitted_model,age_bin,age) %>%
    summarise(baseline_lambda=median(baseline_lambda,na.rm=TRUE),memory_lambda=median(memory_lambda,na.rm=TRUE))
  
  dBeta <- d_best %>% 
    mutate(baseline_beta= ifelse(input_model=='Baseline',beta,NA)) %>% 
    mutate(memory_beta= ifelse(input_model=='Memory',beta,NA)) %>% 
    select(UID,baseline_beta,memory_beta,fitted_model,age_bin,age) %>% 
    group_by(UID,fitted_model,age_bin,age) %>% 
    summarise(baseline_beta=median(baseline_beta,na.rm=TRUE),memory_beta=median(memory_beta,na.rm=TRUE))
  
  dTau <- d_best %>% 
    mutate(baseline_tau= ifelse(input_model=='Baseline',tau,NA)) %>% 
    mutate(memory_tau= ifelse(input_model=='Memory',tau,NA)) %>% 
    select(UID,baseline_tau,memory_tau,fitted_model,age_bin,age) %>% 
    group_by(UID,fitted_model,age_bin,age) %>% 
    summarise(baseline_tau=median(baseline_tau,na.rm=TRUE),memory_tau=median(memory_tau,na.rm=TRUE))
  
  
  dRecency <- d_best %>% 
    # filter(fitted_model %in% c('GP + R','GP + RS')) %>% 
    # mutate(recency=ifelse(fitted_model=='GP + R',recency,full_r)) %>% 
    mutate(baseline_recency= ifelse(input_model=='Baseline',recency,NA)) %>% 
    mutate(memory_recency= ifelse(input_model=='Memory',recency,NA)) %>% 
    select(UID,baseline_recency,memory_recency,fitted_model,age_bin,age) %>% 
    group_by(UID,fitted_model,age_bin,age) %>% 
    summarise(baseline_recency=max(baseline_recency,na.rm=TRUE),memory_recency=max(memory_recency,na.rm=TRUE))
  
  dSurprise <- d %>% 
    filter(fitted_model %in% c('GP + RS')) %>% 
    mutate(surprise=surprise_plus) %>% 
    # mutate(surprise=ifelse(fitted_model=='GP + S',surprise_plus,full_s)) %>% 
    mutate(baseline_surprise= ifelse(input_model=='Baseline',surprise,NA)) %>% 
    mutate(memory_surprise= ifelse(input_model=='Memory',surprise,NA)) %>% 
    select(UID,baseline_surprise,memory_surprise,fitted_model,age_bin,age) %>% 
    group_by(UID,fitted_model,age_bin,age) %>% 
    summarise(baseline_surprise=max(baseline_surprise,na.rm=TRUE),memory_surprise=max(memory_surprise,na.rm=TRUE))
  
  dAsymmetry <- d %>% 
    filter(fitted_model %in% c('GP + RS')) %>% 
    # mutate(asymmetry=ifelse(fitted_model=='GP + S',asymmetry,asymmetry)) %>% 
    mutate(baseline_asymmetry= ifelse(input_model=='Baseline',asymmetry,NA)) %>% 
    mutate(memory_asymmetry= ifelse(input_model=='Memory',asymmetry,NA)) %>% 
    select(UID,baseline_asymmetry,memory_asymmetry,fitted_model,age_bin,age) %>% 
    group_by(UID,fitted_model,age_bin,age) %>% 
    summarise(baseline_asymmetry=max(baseline_asymmetry,na.rm=TRUE),memory_asymmetry=max(memory_asymmetry,na.rm=TRUE))
  
  
  
  ################################################################################################
  # Lambda
  ################################################################################################
  uppertquartiles <- c(quantile(dLambda$memory_lambda, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dLambda$baseline_lambda, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
  H <- c(1.5 * IQR(dLambda$memory_lambda, na.rm = T), 1.5 * IQR(dLambda$baseline_lambda, na.rm = T))
  upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
  #Density for plotting
  # corDF$lambdaDensity <- fields::interp.surface(MASS::kde2d(memoryParams$memory_lambda, baselineParams$baseline_lambda), corDF[,c("memory_lambda", "baseline_lambda")]) #bivariate point density
  
  dLambdaAge <- ddply(dLambda, ~age_bin, plyr::summarize, baseline_lambda=mean(baseline_lambda), memory_lambda=mean(memory_lambda))
  p4a <- ggplot(subset(dLambda,memory_lambda <= upperLimit &  baseline_lambda<= upperLimit), aes(y=memory_lambda, x=baseline_lambda),group=age_bin) +
    geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
    geom_point(alpha=0.2)+
    geom_point(data=dLambdaAge,mapping = aes(x=baseline_lambda,y=memory_lambda,color=age_bin,fill=age_bin),shape=23,size=4.)+
    scale_color_viridis(discrete=TRUE, direction=1) +
    scale_fill_viridis(discrete=TRUE, direction=1) +
    labs(color='Age Group',fill='Age Group')+
    expand_limits(x = -1, y = -1) +
    # coord_cartesian(xlim=c(0.5,1.5),ylim=c(0.5,1.5))+
    xlab(expression("Control log("*lambda*")"))+
    ylab(expression("Memory log("*lambda*")")) +
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
  corrTestResult <- corTestPretty(dLambda$baseline_lambda, dLambda$memory_lambda,method = 'kendall')
  corrTestResult
  corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
  corrText  <- (expr("r"[tau]*" = "*!!corr* ' *'))
  
  # Control lambda correlated with age, Memory lambda not correlated
  corTestPretty(dLambda$baseline_lambda, dLambda$age)
  # corTestPretty(dLambda$memory_lambda, dLambda$age)
  
  p4a <- p4a + annotation_custom(grid::textGrob(label = corrText,
                                                x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                gp = grid::gpar(cex = 0.9)))
  p4a <- ggExtra::ggMarginal(p4a, type = "boxplot", alpha=1, fill='transparent', size = 10, outlier.shape = NA) #fill='transparent',
  p4a
  
  
  
  # correlation test (kendall’s tau for rank correlation) to see if they remain correlated
  
  ################################################################################################
  # Beta
  ################################################################################################
  uppertquartiles <- c(quantile(dBeta$memory_beta, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dBeta$baseline_beta, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
  H <- c(1.5 * IQR(dBeta$memory_beta, na.rm = T), 1.5 * IQR(dBeta$baseline_beta, na.rm = T))
  upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
  #Density for plotting
  # corDF$betaDensity <- fields::interp.surface(MASS::kde2d(memoryParams$memory_beta, baselineParams$baseline_beta), corDF[,c("memory_beta", "baseline_beta")]) #bivariate point density
  
  dBetaAge <- ddply(dBeta, ~age_bin, plyr::summarize, baseline_beta=mean(baseline_beta), memory_beta=mean(memory_beta))
  p4b <- ggplot(subset(dBeta,memory_beta <= upperLimit &  baseline_beta<= upperLimit), aes(y=memory_beta, x=baseline_beta)) +
    geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
    geom_point(alpha=0.2)+
    geom_point(data=dBetaAge,mapping = aes(x=baseline_beta,y=memory_beta,color=age_bin,fill=age_bin),shape=23,size=4.)+
    scale_color_viridis(discrete=TRUE, direction=1) +
    scale_fill_viridis(discrete=TRUE, direction=1) +
    labs(color='Age Group',fill='Age Group')+
    # coord_cartesian(xlim=c(-17,-5),ylim = c(-20,-10))+
    expand_limits(x = 0, y = 0) +
    xlab(expression("Control log("*beta*")"))+
    ylab(expression("Memory log("*beta*")")) +
    theme_classic()+
    theme(text = element_text(size=12,  family="sans"))+
    theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
  p4b <- p4b + theme(legend.position = 'none')
  ttestPretty(dBeta$baseline_beta, dBeta$memory_beta, paired = T)
  corrTestResult <- corTestPretty(dBeta$baseline_beta, dBeta$memory_beta, method = 'kendall')
  corrTestResult
  corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
  corrText  <- (expr("r"[tau]*" = "*!!corr* ' *'))
  
  # Only control beta is correlated with age
  corTestPretty(dBeta$baseline_beta, dBeta$age)
  # corTestPretty(dBeta$memory_beta, dBeta$age)
  p4b <- p4b + annotation_custom(grid::textGrob(label = corrText,
                                                x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                gp = grid::gpar(cex = 0.9)))
  
  
  p4b <- ggExtra::ggMarginal(p4b, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
  p4b
  
  
  
  
  ################################################################################################
  # Tau
  ################################################################################################
  uppertquartiles <- c(quantile(dTau$memory_tau, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dTau$baseline_tau, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
  H <- c(1.5 * IQR(dTau$memory_tau, na.rm = T), 1.5 * IQR(dTau$baseline_tau, na.rm = T))
  upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
  #Density for plotting
  # corDF$tauDensity <- fields::interp.surface(MASS::kde2d(memoryParams$memory_tau, baselineParams$baseline_tau), corDF[,c("memory_tau", "baseline_tau")]) #bivariate point density
  
  dTauAge <- ddply(dTau, ~age_bin, plyr::summarize, baseline_tau=mean(baseline_tau), memory_tau=mean(memory_tau))
  p4c <- ggplot(subset(dTau,memory_tau <= upperLimit &  baseline_tau<= upperLimit), aes(y=memory_tau, x=baseline_tau)) +
    geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
    # geom_point(alpha=0.5)+
    geom_point(alpha=0.2)+
    geom_point(data=dTauAge,mapping = aes(x=baseline_tau,y=memory_tau,color=age_bin,fill=age_bin),shape=23,size=4.)+
    scale_color_viridis(discrete=TRUE, direction=1) +
    scale_fill_viridis(discrete=TRUE, direction=1) +
    labs(color='Age Group',fill='Age Group')+
    # coord_cartesian(xlim=c(-5,-3),ylim = c(-5,-3))+
    xlab(expression("Control log("*tau*")"))+
    ylab(expression("Memory log("*tau*")")) +
    theme_classic()+
    scale_color_viridis(discrete=TRUE, direction=1) +
    theme(text = element_text(size=12,  family="sans"))+
    theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
  p4c <- p4c + theme(legend.position = 'none')
  ttestPretty(dTau$baseline_tau, dTau$memory_tau, paired = T)
  
  corrTestResult <- corTestPretty(dTau$baseline_tau, dTau$memory_tau, method = 'kendall')
  corrTestResult
  corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
  corrText  <- (expr("r"[tau]*" = "*!!corr* ' *'))
  
  # No correlation with age
  # corTestPretty(dTau$baseline_tau, dTau$age)
  # corTestPretty(dTau$memory_tau, dTau$age)
  p4c <- p4c + annotation_custom(grid::textGrob(label =corrText,
                                                x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                gp = grid::gpar(cex = 0.9)))
  
  p4c <- ggExtra::ggMarginal(p4c, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
  p4c
  
  
  ################################################################################################
  # Recency
  ################################################################################################
  uppertquartiles <- c(quantile(dRecency$memory_recency, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dRecency$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
  H <- c(1.5 * IQR(dRecency$memory_recency, na.rm = T), 1.5 * IQR(dRecency$baseline_recency, na.rm = T))
  upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
  #Density for plotting
  # corDF$recencyDensity <- fields::interp.surface(MASS::kde2d(memoryParams$memory_recency, baselineParams$baseline_recency), corDF[,c("memory_recency", "baseline_recency")]) #bivariate point density
  
  dRecencyAge <- ddply(dRecency, ~age_bin, plyr::summarize, baseline_recency=mean(baseline_recency), memory_recency=mean(memory_recency))
  p4d <- ggplot(subset(dRecency,memory_recency <= upperLimit &  baseline_recency<= upperLimit), aes(y=memory_recency,x= baseline_recency)) +
    geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
    geom_point(alpha=0.2)+
    geom_point(data=dRecencyAge,mapping = aes(x=baseline_recency,y=memory_recency,color=age_bin,fill=age_bin),shape=23,size=4.)+
    scale_color_viridis(discrete=TRUE, direction=1) +
    scale_fill_viridis(discrete=TRUE, direction=1) +
    labs(color='Age Group',fill='Age Group')+
    # coord_cartesian(xlim=c(0,300),ylim = c(0,300))+
    # expand_limits(x = 0, y = 0) +
    xlab(expression("Control "*Recency))+
    ylab(expression("Memory "*Recency)) +
    # coord_cartesian(xlim = c(-100,400),ylim = c(-100,400))
    # xlim(c(-50,400))+
    # ylim(c(-50,400))+
    theme_classic()+
    theme(text = element_text(size=12,  family="sans"))+
    theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
  p4d <- p4d + theme(legend.position = 'none')
  ttestPretty(dRecency$baseline_recency, dRecency$memory_recency, paired = T)
  corrTestResult <- corTestPretty(dRecency$baseline_recency, dRecency$memory_recency, method = 'kendall')
  corrTestResult
  corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
  corrText  <- (expr("r"[tau]*" = "*!!corr* ' *'))
  
  # Neither is correlated with age
  # corTestPretty(dRecency$baseline_recency, dRecency$age)
  # corTestPretty(dRecency$memory_recency, dRecency$age)
  p4d <- p4d + annotation_custom(grid::textGrob(label = corrText,
                                                x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                gp = grid::gpar(cex = 0.9)))
  
  p4d <- ggExtra::ggMarginal(p4d, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
  p4d
  
  
  ################################################################################################
  # Surprise
  ################################################################################################
  uppertquartiles <- c(quantile(dSurprise$memory_surprise, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dSurprise$baseline_surprise, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
  H <- c(1.5 * IQR(dSurprise$memory_surprise, na.rm = T), 1.5 * IQR(dSurprise$baseline_surprise, na.rm = T))
  upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
  #Density for plotting
  # corDF$surpriseDensity <- fields::interp.surface(MASS::kde2d(memoryParams$memory_surprise, baselineParams$baseline_surprise), corDF[,c("memory_surprise", "baseline_surprise")]) #bivariate point density
  
  dSurpriseAge <- ddply(dSurprise, ~age_bin, plyr::summarize, baseline_surprise=mean(baseline_surprise), memory_surprise=mean(memory_surprise))
  p4e <- ggplot(subset(dSurprise,memory_surprise <= upperLimit &  baseline_surprise<= upperLimit), aes(y=memory_surprise,x= baseline_surprise)) +
    geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
    geom_point(alpha=0.2)+
    geom_point(data=dSurpriseAge,mapping = aes(x=baseline_surprise,y=memory_surprise,color=age_bin,fill=age_bin),shape=23,size=4.)+
    scale_color_viridis(discrete=TRUE, direction=1) +
    scale_fill_viridis(discrete=TRUE, direction=1) +
    labs(color='Age Group',fill='Age Group')+
    # coord_cartesian(xlim=c(0,300),ylim = c(0,300))+
    # expand_limits(x = 0, y = 0) +
    xlab(expression("Control "*Surprise))+
    ylab(expression("Memory "*Surprise)) +
    # coord_cartesian(xlim = c(-100,400),ylim = c(-100,400))
    # xlim(c(-50,400))+
    # ylim(c(-50,400))+
    theme_classic()+
    theme(text = element_text(size=12,  family="sans"))+
    theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
  p4e <- p4e + theme(legend.position = 'none')
  # ttestPretty(dSurprise$baseline_surprise, dSurprise$memory_surprise, paired = T)
  corrTestResult <- corTestPretty(dSurprise$baseline_surprise, dSurprise$memory_surprise, method = 'kendall')
  corrTestResult
  corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
  corrText  <- (expr("r"[tau]*" = "*!!corr* ' *'))
  
  # Neither is correlated with age
  # corTestPretty(dSurprise$baseline_surprise, dSurprise$age)
  # corTestPretty(dSurprise$memory_surprise, dSurprise$age)
  p4e <- p4e + annotation_custom(grid::textGrob(label = corrText,
                                                x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                gp = grid::gpar(cex = 0.9)))
  
  p4e <- ggExtra::ggMarginal(p4e, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
  p4e
  
  
  ################################################################################################
  # Asymmetry
  ################################################################################################
  uppertquartiles <- c(quantile(dAsymmetry$memory_asymmetry, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dAsymmetry$baseline_asymmetry, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
  H <- c(1.5 * IQR(dAsymmetry$memory_asymmetry, na.rm = T), 1.5 * IQR(dAsymmetry$baseline_asymmetry, na.rm = T))
  upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
  #Density for plotting
  # corDF$asymmetryDensity <- fields::interp.surface(MASS::kde2d(memoryParams$memory_asymmetry, baselineParams$baseline_asymmetry), corDF[,c("memory_asymmetry", "baseline_asymmetry")]) #bivariate point density
  
  dAsymmetryAge <- ddply(dAsymmetry, ~age_bin, plyr::summarize, baseline_asymmetry=mean(baseline_asymmetry), memory_asymmetry=mean(memory_asymmetry))
  p4f <- ggplot(subset(dAsymmetry,memory_asymmetry <= upperLimit &  baseline_asymmetry<= upperLimit), aes(y=memory_asymmetry,x= baseline_asymmetry)) +
    geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
    geom_point(alpha=0.2)+
    geom_point(data=dAsymmetryAge,mapping = aes(x=baseline_asymmetry,y=memory_asymmetry,color=age_bin,fill=age_bin),shape=23,size=4.)+
    scale_color_viridis(discrete=TRUE, direction=1) +
    scale_fill_viridis(discrete=TRUE, direction=1) +
    labs(color='Age Group',fill='Age Group')+
    # coord_cartesian(xlim=c(0,300),ylim = c(0,300))+
    # expand_limits(x = 0, y = 0) +
    xlab(expression("Control log("*Asymmetry*")"))+
    ylab(expression("Memory log("*Asymmetry*")")) +
    # coord_cartesian(xlim = c(-100,400),ylim = c(-100,400))
    # xlim(c(-50,400))+
    # ylim(c(-50,400))+
    theme_classic()+
    theme(text = element_text(size=12,  family="sans"))+
    theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
  p4f <- p4f + theme(legend.position = 'none')
  ttestPretty(dAsymmetry$baseline_asymmetry, dAsymmetry$memory_asymmetry, paired = T)
  corrTestResult <- corTestPretty(dAsymmetry$baseline_asymmetry, dAsymmetry$memory_asymmetry, method = 'kendall')
  corrTestResult
  corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
  corrText  <- (expr("r"[tau]*" = "*!!corr* ' *'))
  
  # Neither is correlated with age
  # corTestPretty(dAsymmetry$baseline_asymmetry, dAsymmetry$age)
  # corTestPretty(dAsymmetry$memory_asymmetry, dAsymmetry$age)
  p4f <- p4f + annotation_custom(grid::textGrob(label = corrText,
                                                x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                gp = grid::gpar(cex = 0.9)))
  
  p4f <- ggExtra::ggMarginal(p4f, type = "boxplot", fill='transparent', size = 10, outlier.shape = NA) #add marginal boxplots
  p4f
  
  pxpPlots <- cowplot::plot_grid(p_pxp_nomem, p_pxp_mem,leg_pxp, nrow = 1, rel_widths = c(1,1,.3),labels = 'a', label_size = 20)
  pxpPlots
  
  pxpPlots_only <- cowplot::plot_grid(p_pxp_nomem, p_pxp_mem,leg_pxp, nrow = 1, rel_widths = c(1,1,.3),labels = NA, label_size = 20)
  ggsave(paste0('plots/plots_cogsci/pxp',demo,'.pdf'), pxpPlots_only, width = 7, height = 3, units ='in')
  
  
  pParams <- cowplot::plot_grid(p4a,p4b, p4c,p4d, nrow = 2,align = 'vh',labels=c('b','c','d','e'), label_size = 20)
  pParams <- cowplot::plot_grid(pParams,leg_params, rel_widths =  c(1,.15), nrow = 1)
  pParams
  
  ggsave(paste0('plots/plots_cogsci/params',demo,'.pdf'), pParams, width = 7, height = 5, units ='in')
  
  pModeling <- cowplot::plot_grid(pxpPlots,
                                  pParams,
                                  nrow = 2, rel_heights = c(0.4,0.6),
                                  # labels = 'auto', label_size = 20,
                                  align='vh')
  pModeling
  ggsave(paste0('plots/plots_cogsci/modeling',demo,'.pdf'), pModeling, width = 7, height = 10, units ='in')
  
}






