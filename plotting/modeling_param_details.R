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
d <- d %>% filter(fitted_model!='surprise+_static')

d <- d %>% 
  mutate(age_bin_numeric= ifelse(age_bin=='18-27',20,
                                 ifelse(age_bin=='28-37',30,
                                        ifelse(age_bin=='38-47',40,
                                               ifelse(age_bin=='48-57',50,
                                                      ifelse(age_bin=='58-67',60,70))))))

# d$input_model <-apply_modelNames_exp(d$input_model)
# d$fitted_model <- apply_modelNames_sim(d$fitted_model)


d_demo <- load_anonymous_demographics()

d_demo_simplified <- d_demo %>% 
  mutate(Country.of.residence=ifelse((!Country.of.residence %in% c('South Africa','United Kingdom')),'Rest/Europe',
                                     ifelse(Country.of.residence=='South Africa','South Africa',
                                            ifelse(Country.of.residence=='United Kingdom','United Kingdom',NA)))) %>% 
  select(-sex,-time,-education)


d <- merge(d,d_demo_simplified,by='UID')

d <- d %>% filter(Country.of.residence!='South Africa')



################################################################################################
# Final Parameter plots
################################################################################################
models <- c('null','recency','surprise+','full') #
scenarios <- c('Memory','NoMemory')
params<- c('lambda','beta','tau','recency','surprise_plus','asymmetry')

for(m in models){
  d_model <- d %>% 
    filter(fitted_model==m) %>%
    mutate(lambda=exp(lambda)) %>%
    mutate(beta=exp(beta)) %>%
    mutate(tau=exp(tau)) %>% 
    mutate(asymmetry=exp(asymmetry)) %>% 
    mutate(input_model=ifelse(input_model=='NoMemory','Baseline','Memory'))
  
  
    dParam <- d_model %>% 
      mutate(baseline_lambda= ifelse(input_model=='Baseline',lambda,NA)) %>% 
      mutate(memory_lambda= ifelse(input_model=='Memory',lambda,NA)) %>% 
      mutate(baseline_beta= ifelse(input_model=='Baseline',beta,NA)) %>% 
      mutate(memory_beta= ifelse(input_model=='Memory',beta,NA)) %>% 
      mutate(baseline_tau= ifelse(input_model=='Baseline',tau,NA)) %>% 
      mutate(memory_tau= ifelse(input_model=='Memory',tau,NA)) %>% 
      mutate(baseline_recency= ifelse(input_model=='Baseline',recency,NA)) %>% 
      mutate(memory_recency= ifelse(input_model=='Memory',recency,NA)) %>% 
      mutate(baseline_surprise= ifelse(input_model=='Baseline',surprise_plus,NA)) %>% 
      mutate(memory_surprise= ifelse(input_model=='Memory',surprise_plus,NA)) %>% 
      mutate(baseline_asymmetry= ifelse(input_model=='Baseline',asymmetry,NA)) %>% 
      mutate(memory_asymmetry= ifelse(input_model=='Memory',asymmetry,NA)) %>% 
      
      # select(UID,baseline_lambda,memory_lambda,fitted_model,age_bin,age,age_bin_numeric) %>%
      group_by(UID,fitted_model,age_bin,age,age_bin_numeric) %>%
      
      summarise(
        baseline_lambda=median(baseline_lambda,na.rm=TRUE),memory_lambda=median(memory_lambda,na.rm=TRUE),
        baseline_beta=median(baseline_beta,na.rm=TRUE),memory_beta=median(memory_beta,na.rm=TRUE),
        baseline_tau=median(baseline_tau,na.rm=TRUE),memory_tau=median(memory_tau,na.rm=TRUE), 
        baseline_recency=median(baseline_recency,na.rm=TRUE),memory_recency=median(memory_recency,na.rm=TRUE),
        baseline_surprise=median(baseline_surprise,na.rm=TRUE),memory_surprise=median(memory_surprise,na.rm=TRUE), 
        baseline_asymmetry=median(baseline_asymmetry,na.rm=TRUE),memory_asymmetry=median(memory_asymmetry,na.rm=TRUE)
      )
    
    
    
    ################################################################################################
    # Lambda
    ################################################################################################
    uppertquartiles <- c(quantile(dParam$memory_lambda, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dParam$baseline_lambda, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
    H <- c(1.5 * IQR(dParam$memory_lambda, na.rm = T), 1.5 * IQR(dParam$baseline_lambda, na.rm = T))
    upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
    
    dParamAge <- ddply(dParam, ~age_bin, plyr::summarize, baseline_lambda=median(baseline_lambda), memory_lambda=median(memory_lambda))
    p4a <- ggplot(subset(dParam,memory_lambda <= upperLimit &  baseline_lambda<= upperLimit), aes(y=memory_lambda, x=baseline_lambda),group=age_bin) +
      geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
      # geom_point(alpha=0.2)+
      geom_point(data=dParamAge,mapping = aes(x=baseline_lambda,y=memory_lambda,color=age_bin,fill=age_bin),shape=23,size=4.)+
      scale_color_viridis(discrete=TRUE, direction=1) +
      scale_fill_viridis(discrete=TRUE, direction=1) +
      labs(color='Age Group',fill='Age Group')+
      # coord_trans(y ='log10', x='log10')+
      # expand_limits(x = -1, y = -1) +
      # coord_cartesian(xlim=c(0.25,1.5),ylim=c(0.5,1.5))+
      
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
    
    corrTestResult <- corTestPretty(dParam$baseline_lambda, dParam$memory_lambda,method = 'kendall')
    corrTestResult
    corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
    corrText  <- (expr("r"[lambda]*" = "*!!corr* ' *'))
    
    p4a <- p4a + annotation_custom(grid::textGrob(label = corrText,
                                                  x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                  gp = grid::gpar(cex = 0.9)))
    
    
    
    ################################################################################################
    # Beta
    ################################################################################################
    uppertquartiles <- c(quantile(dParam$memory_beta, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dParam$baseline_beta, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
    H <- c(1.5 * IQR(dParam$memory_beta, na.rm = T), 1.5 * IQR(dParam$baseline_beta, na.rm = T))
    upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
    
    dParamAge <- ddply(dParam, ~age_bin, plyr::summarize, baseline_beta=median(baseline_beta), memory_beta=median(memory_beta))
    p4b <- ggplot(subset(dParam,memory_beta <= upperLimit &  baseline_beta<= upperLimit), aes(y=memory_beta, x=baseline_beta)) +
      geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
      # geom_point(alpha=0.2)+
      geom_point(data=dParamAge,mapping = aes(x=baseline_beta,y=memory_beta,color=age_bin,fill=age_bin),shape=23,size=4.)+
      scale_color_viridis(discrete=TRUE, direction=1) +
      scale_fill_viridis(discrete=TRUE, direction=1) +
      labs(color='Age Group',fill='Age Group')+
      # coord_trans(y ='log10', x='log10')+
      # coord_cartesian(xlim=c(-17,-5),ylim = c(-20,-10))+
      # expand_limits(x = 0, y = 0) +
      xlab(expression("Control "*beta))+
      ylab(expression("Memory "*beta)) +
      theme_classic()+
      theme(text = element_text(size=12,  family="sans"))+
      theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
    p4b <- p4b + theme(legend.position = 'none')
    
    corrTestResult <- corTestPretty(dParam$baseline_beta, dParam$memory_beta,method = 'kendall')
    corrTestResult
    corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
    corrText  <- (expr("r"[beta]*" = "*!!corr* ' *'))
    
    corTestPretty(dParam$baseline_beta, dParam$age)
    # corTestPretty(dParam$memory_beta, dParam$age)
    p4b <- p4b + annotation_custom(grid::textGrob(label = corrText,
                                                  x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                  gp = grid::gpar(cex = 0.9)))
    
    
    
    ################################################################################################
    # Tau
    ################################################################################################
    uppertquartiles <- c(quantile(dParam$memory_tau, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dParam$baseline_tau, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
    H <- c(1.5 * IQR(dParam$memory_tau, na.rm = T), 1.5 * IQR(dParam$baseline_tau, na.rm = T))
    upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
    
    dParamAge <- ddply(dParam, ~age_bin, plyr::summarize, baseline_tau=median(baseline_tau), memory_tau=median(memory_tau))
    p4c <- ggplot(subset(dParam,memory_tau <= upperLimit &  baseline_tau<= upperLimit), aes(y=memory_tau, x=baseline_tau)) +
      geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
      # geom_point(alpha=0.2)+
      geom_point(data=dParamAge,mapping = aes(x=baseline_tau,y=memory_tau,color=age_bin,fill=age_bin),shape=23,size=4.)+
      scale_color_viridis(discrete=TRUE, direction=1) +
      scale_fill_viridis(discrete=TRUE, direction=1) +
      labs(color='Age Group',fill='Age Group')+
      # coord_trans(y ='log10', x='log10')+
      # coord_cartesian(xlim=c(-5,-3),ylim = c(-5,-3))+
      xlab(expression("Control "*tau))+
      ylab(expression("Memory "*tau)) +
      theme_classic()+
      scale_color_viridis(discrete=TRUE, direction=1) +
      theme(text = element_text(size=12,  family="sans"))+
      theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
    p4c <- p4c + theme(legend.position = 'none')
    
    corrTestResult <- corTestPretty(dParam$baseline_tau, dParam$memory_tau,method = 'kendall')
    corrTestResult
    corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
    corrText  <- (expr("r"[tau]*" = "*!!corr* ' *'))
    
    
    p4c <- p4c + annotation_custom(grid::textGrob(label = corrText,
                                                  x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                  gp = grid::gpar(cex = 0.9)))
    
    
    
    
    ################################################################################################
    # Recency
    ################################################################################################
    uppertquartiles <- c(quantile(dParam$memory_recency, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dParam$baseline_recency, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
    H <- c(1.5 * IQR(dParam$memory_recency, na.rm = T), 1.5 * IQR(dParam$baseline_recency, na.rm = T))
    upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
    
    dParamAge <- ddply(dParam, ~age_bin, plyr::summarize, baseline_recency=median(baseline_recency), memory_recency=median(memory_recency))
    p4d <- ggplot(subset(dParam,memory_recency <= upperLimit &  baseline_recency<= upperLimit), aes(y=memory_recency,x= baseline_recency)) +
      geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
      # geom_point(alpha=0.2)+
      geom_point(data=dParamAge,mapping = aes(x=baseline_recency,y=memory_recency,color=age_bin,fill=age_bin),shape=23,size=4.)+
      scale_color_viridis(discrete=TRUE, direction=1) +
      scale_fill_viridis(discrete=TRUE, direction=1) +
      labs(color='Age Group',fill='Age Group')+
      # coord_cartesian(xlim=c(100,300),ylim = c(100,300))+
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
    
    recency_test <-dParam %>% head(1)
    if(!is.na(recency_test$baseline_recency)){
      corrTestResult <- corTestPretty(dParam$baseline_recency, dParam$memory_recency,method = 'kendall')
      corrTestResult
      corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
      corrText  <- (expr("r"[recency]*" = "*!!corr* ' *'))
      
      p4d <- p4d + annotation_custom(grid::textGrob(label = corrText,
                                                    x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                    gp = grid::gpar(cex = 0.9)))
      
    }
    
    
    ################################################################################################
    # Surprise
    ################################################################################################
    uppertquartiles <- c(quantile(dParam$memory_surprise, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dParam$baseline_surprise, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
    H <- c(1.5 * IQR(dParam$memory_surprise, na.rm = T), 1.5 * IQR(dParam$baseline_surprise, na.rm = T))
    upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
    
    dParamAge <- ddply(dParam, ~age_bin, plyr::summarize, baseline_surprise=median(baseline_surprise), memory_surprise=median(memory_surprise))
    p4e <- ggplot(subset(dParam,memory_surprise <= upperLimit &  baseline_surprise<= upperLimit), aes(y=memory_surprise,x= baseline_surprise)) +
      geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
      geom_hline(yintercept=0, linetype='dashed', color='red') + # random choice model
      geom_vline(xintercept=0, linetype='dashed', color='red') + # random choice model
      # geom_point(alpha=0.2)+
      geom_point(data=dParamAge,mapping = aes(x=baseline_surprise,y=memory_surprise,color=age_bin,fill=age_bin),shape=23,size=4.)+
      scale_color_viridis(discrete=TRUE, direction=1) +
      scale_fill_viridis(discrete=TRUE, direction=1) +
      labs(color='Age Group',fill='Age Group')+
      # coord_cartesian(xlim=c(-250,0),ylim = c(-250,0))+
      # expand_limits(x = 0, y = 0) +
      xlab(expression("Control "*Surprise))+
      ylab(expression("Memory "*Surprise)) +
      # coord_cartesian(xlim = c(-300,300),ylim = c(-300,300))+
      # xlim(c(-50,400))+
      # ylim(c(-50,400))+
      theme_classic()+
      theme(text = element_text(size=12,  family="sans"))+
      theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
    p4e <- p4e + theme(legend.position = 'none')
    
    
    surprise_test <-dParam %>% head(1)
    if(!is.na(surprise_test$baseline_surprise)){
      corrTestResult <- corTestPretty(dParam$baseline_surprise, dParam$memory_surprise,method = 'kendall')
      corrTestResult
      corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
      corrText  <- (expr("r"[surprise]*" = "*!!corr* ' *'))
      
      p4e <- p4e + annotation_custom(grid::textGrob(label =corrText,
                                                    x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                    gp = grid::gpar(cex = 0.9)))
      
      
    }
    
    ################################################################################################
    # Asymmetry
    ################################################################################################
    uppertquartiles <- c(quantile(dParam$memory_asymmetry, probs=c(.25, .75),na.rm = TRUE)[2], quantile(dParam$baseline_asymmetry, probs=c(.25, .75),na.rm = TRUE)[2]) #upper quartiles
    H <- c(1.5 * IQR(dParam$memory_asymmetry, na.rm = T), 1.5 * IQR(dParam$baseline_asymmetry, na.rm = T))
    upperLimit <- max(uppertquartiles + H) #use the larger 1.5 * IQR from the upper quartile to set axis limits
    
    dParamAge <- ddply(dParam, ~age_bin, plyr::summarize, baseline_asymmetry=median(baseline_asymmetry), memory_asymmetry=median(memory_asymmetry))
    p4f <- ggplot(subset(dParam,memory_asymmetry <= upperLimit &  baseline_asymmetry<= upperLimit), aes(y=memory_asymmetry,x= baseline_asymmetry)) +
      geom_abline(slope = 1, intercept=0, color = 'black', linetype = 'dashed')+
      geom_hline(yintercept=0, linetype='dashed', color='red') + # random choice model
      geom_vline(xintercept=0, linetype='dashed', color='red') + # random choice model
      # geom_point(alpha=0.2)+
      geom_point(data=dParamAge,mapping = aes(x=baseline_asymmetry,y=memory_asymmetry,color=age_bin,fill=age_bin),shape=23,size=4.)+
      scale_color_viridis(discrete=TRUE, direction=1) +
      scale_fill_viridis(discrete=TRUE, direction=1) +
      labs(color='Age Group',fill='Age Group')+
      # coord_trans(y ='log10', x='log10')+
      # coord_cartesian(xlim=c(-250,0),ylim = c(-250,0))+
      # expand_limits(x = 0, y = 0) +
      xlab(expression("Control "*Asymmetry))+
      ylab(expression("Memory "*Asymmetry)) +
      # coord_cartesian(xlim = c(-2.5,2.5),ylim = c(-2.5,2.5))+
      # xlim(c(-50,400))+
      # ylim(c(-50,400))+
      theme_classic()+
      theme(text = element_text(size=12,  family="sans"))+
      theme(title = element_text(family = 'sans'),   legend.background = element_blank(), legend.position='right')
    p4f <- p4f + theme(legend.position = 'none')
    
    
    asymmetry_test <-dParam %>% head(1)
    if(!is.na(asymmetry_test$baseline_asymmetry)){
      corrTestResult <- corTestPretty(dParam$baseline_asymmetry, dParam$memory_asymmetry,method = 'kendall')
      corrTestResult
      corr <- as.numeric(substring(corrTestResult, regexpr("=", corrTestResult) +1, regexpr(",", corrTestResult) -2))
      corrText  <- (expr("r"[asymmetry]*" = "*!!corr* ''))
      
      p4f <- p4f + annotation_custom(grid::textGrob(label =corrText,
                                                    x = unit(0.25, "npc"), y = unit(0.95, "npc"),
                                                    gp = grid::gpar(cex = 0.9)))
      
    }
    
    
    testL <- is.na(mean(dParam$baseline_lambda,na.rm=T))
    testR <- is.na(mean(dParam$baseline_recency,na.rm=T))
    if(is.na(mean(dParam$baseline_recency,na.rm=T))){
       p4d <- NA
    }
    if(is.na(mean(dParam$baseline_surprise,na.rm=T))){
      p4e <- NA
    }
    if(is.na(mean(dParam$baseline_asymmetry,na.rm=T))){
      p4f <- NA
    }
    
    
    
    pParams_abc <- cowplot::plot_grid(p4a,p4b,p4c, ncol = 3,align = 'vh',labels='auto', label_size = 20)
    pParams_def <- cowplot::plot_grid(p4d,p4e,p4f,
                                      ncol = 3,align = 'vh',
                                      labels=c(ifelse(!is.na(mean(dParam$baseline_recency,na.rm=T)),'d',NA),
                                               ifelse(!is.na(mean(dParam$baseline_surprise,na.rm=T)),'e',NA),
                                               ifelse(!is.na(mean(dParam$baseline_asymmetry,na.rm=T)),'f',NA)), 
                                      label_size = 20)
    if(m=='null'){
      pParams <- pParams_abc
    } else{
      pParams<- cowplot::plot_grid(pParams_abc,pParams_def, nrow = 2,align = 'vh')
    }
    pParams <- cowplot::plot_grid(pParams,leg_params, rel_widths =  c(1,.15), nrow = 1)
   
    PLOT_PATH <- 'plots/plots_modeling/plots_corrected/param_plots/'
    ifelse(!dir.exists(PLOT_PATH), dir.create(PLOT_PATH), "Folder exists already, overwriting data.")
    
    model_path <- paste0(PLOT_PATH,'model_',m)
    if(m=='null'){
      ggsave(paste0(model_path,'.pdf'), pParams, width = 12, height = 3, units ='in')
      ggsave(paste0(model_path,'.png'), pParams, width = 12, height = 3, units ='in')
    } else{
      ggsave(paste0(model_path,'.pdf'), pParams, width = 12, height = 6, units ='in')
      ggsave(paste0(model_path,'.png'), pParams, width = 12, height = 6, units ='in')
    }
    
     
  
}






d_model <- d %>% 
  filter(fitted_model=='recency') %>%
  mutate(lambda=exp(lambda)) %>%
  mutate(beta=exp(beta)) %>%
  mutate(tau=exp(tau)) %>% 
  mutate(asymmetry=exp(asymmetry)) %>% 
  mutate(input_model=ifelse(input_model=='NoMemory','Baseline','Memory'))


dParam <- d_model %>% 
  mutate(baseline_lambda= ifelse(input_model=='Baseline',lambda,NA)) %>% 
  mutate(memory_lambda= ifelse(input_model=='Memory',lambda,NA)) %>% 
  mutate(baseline_beta= ifelse(input_model=='Baseline',beta,NA)) %>% 
  mutate(memory_beta= ifelse(input_model=='Memory',beta,NA)) %>% 
  mutate(baseline_tau= ifelse(input_model=='Baseline',tau,NA)) %>% 
  mutate(memory_tau= ifelse(input_model=='Memory',tau,NA)) %>% 
  mutate(baseline_recency= ifelse(input_model=='Baseline',recency,NA)) %>% 
  mutate(memory_recency= ifelse(input_model=='Memory',recency,NA)) %>% 
  mutate(baseline_surprise= ifelse(input_model=='Baseline',surprise_plus,NA)) %>% 
  mutate(memory_surprise= ifelse(input_model=='Memory',surprise_plus,NA)) %>% 
  mutate(baseline_asymmetry= ifelse(input_model=='Baseline',asymmetry,NA)) %>% 
  mutate(memory_asymmetry= ifelse(input_model=='Memory',asymmetry,NA)) %>% 
  
  # select(UID,baseline_lambda,memory_lambda,fitted_model,age_bin,age,age_bin_numeric) %>%
  group_by(UID,fitted_model,age_bin,age,age_bin_numeric) %>%
  
  summarise(
    baseline_lambda=median(baseline_lambda,na.rm=TRUE),memory_lambda=median(memory_lambda,na.rm=TRUE),
    baseline_beta=median(baseline_beta,na.rm=TRUE),memory_beta=median(memory_beta,na.rm=TRUE),
    baseline_tau=median(baseline_tau,na.rm=TRUE),memory_tau=median(memory_tau,na.rm=TRUE), 
    baseline_recency=median(baseline_recency,na.rm=TRUE),memory_recency=median(memory_recency,na.rm=TRUE),
    baseline_surprise=median(baseline_surprise,na.rm=TRUE),memory_surprise=median(memory_surprise,na.rm=TRUE), 
    baseline_asymmetry=median(baseline_asymmetry,na.rm=TRUE),memory_asymmetry=median(memory_asymmetry,na.rm=TRUE)
  )



################################################################################################
# Lambda
################################################################################################
p4a
ttestPretty(dParam$baseline_lambda, dParam$memory_lambda, paired = T)
# No direct correlations with age sadly
# corTestPretty(dParam$baseline_lambda, dParam$age,method = 'pearson')
# corTestPretty(dParam$memory_lambda, dParam$age,method = 'pearson')

################################################################################################
# Beta
################################################################################################
ttestPretty(dParam$baseline_beta, dParam$memory_beta, paired = T)
p4b

################################################################################################
# Tau
################################################################################################
p4c

################################################################################################
# Recency
################################################################################################
p4d

corTestPretty(dParam$baseline_recency, dParam$age)
corTestPretty(dParam$memory_recency, dParam$age)
ttestPretty(dParam$baseline_recency, dParam$memory_recency, paired = T)

pParams

################################################################################################
# Surprise
################################################################################################
p4e
ttestPretty(dParam$baseline_surprise, dParam$memory_surprise, paired = T)


################################################################################################
# Asymmetry
################################################################################################
p4f





