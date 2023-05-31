# Plotting 1D effects of model params
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot','viridis')#, 'viridis', 'entropy', 'lme4', 'sjPlot', 'brms', 'withr', 'tidyr', 'ggbeeswarm','jsonlite','tools')
invisible(lapply(packages, require, character.only = TRUE)) #loads packages

source('data_munging.R')

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

COLOR_MODEL_BASIC = cbPalette[1]
COLOR_MODEL_SURPRISE = cbPalette[3]
COLOR_MODEL_FULL = cbPalette[4]
COLOR_MODEL_RECENCY = cbPalette[2]


PLOT_PATH="plots/plots_simulated/"


# X<-rep(c(0,10,5),3)
# Y <- rep(c(0,100,50),3)
X<-c(5,0,10,5,2,8)
Y <- c(50,0,100,50,20,80)


choices <- manhattan <- expand.grid('x1'=0:(GRIDSIZE-1), 'x2'=0:(GRIDSIZE-1)) %>% mutate(choice=row_number())

draw_label_theme <- function(label, theme = NULL, element = "text", ...) {
  if (is.null(theme)) {
    theme <- ggplot2::theme_get()
  }
  if (!element %in% names(theme)) {
    stop("Element must be a valid ggplot theme element name")
  }
  
  elements <- ggplot2::calc_element(element, theme)
  
  cowplot::draw_label(label, 
                      fontfamily = elements$family,
                      fontface = elements$face,
                      colour = elements$color,
                      size = elements$size,
                      ...
  )
}
calculate_2D_case <- function(grid_layout,x1,x2,y,model,default_noise=0.0001,theta_test=NA,normalized_noise=FALSE,positive_asymmetry=TRUE,surprise_model=3,posterior_stack=NA){
  NUM_CLICKS=25
  choices<- grid_layout %>% select(-choice)
  
  
  if(is.na(theta_test)){
    if(model=="full"){
      # Values from our own experiment:
      r_lambda <- 1.47
      r_beta <- 0.026
      r_tau <- 0.03
      
      recency <- 700
      surprise <- -30
      neg_surprise_bias <- -3
      
    } else if(model=="recency"){
      
      # Values from our own experiment:
      r_lambda <- 2.87
      r_beta <- 0.25
      r_tau <- 0.03
      
      recency <- 250
      surprise <- 0
      neg_surprise_bias <- NA
    }else if(model=="surprise+"){
      # Values from our own experiment:
      # r_lambda <- 0.82
      r_lambda <- 1.5
      r_beta <- 0.267
      r_tau <- 0.03
      
      recency <- 0
      surprise <- 30
      neg_surprise_bias <- log(1)
    }
    
    if(model=="null"){
      # Values from our own experiment:
      r_lambda <- 0.85
      r_beta <- 0.224
      r_tau <- 0.03
      
      recency <- 0
      surprise <- 0
      neg_surprise_bias <- NA
    }
    
  }else{
    r_lambda=theta_test[1]
    r_beta=theta_test[2]
    r_tau=theta_test[3]
    recency=theta_test[4]
    surprise=theta_test[5]
    neg_surprise_bias=theta_test[6]
  }
  
  
  
  if(model!="null"){
    errVar_vec <- if(is.na(neg_surprise_bias)) c(recency,surprise) else c(recency,surprise,neg_surprise_bias)
  }
  
  # 1D case
  if(x1==x2){
    r_lambda <- 2*r_lambda
  }
  
  obs <- data.frame(x1,x2,y)
  parVec_gpr <- c(r_lambda, r_lambda, 1,default_noise)
  
  if(model!="null"){
    #TODO: Test if this is still correct
    errVar<- gp_error_variance_exponential(obs=data.frame(x1,x2,y/100),theta=errVar_vec,clicks=NUM_CLICKS,exponential_asymmetry=positive_asymmetry,surprise_model = surprise_model,default_noise = default_noise,prev_posterior=posterior_stack)
    
    errVar_index <-1:length(errVar) + 3
    parVec_gpr <- replace(parVec_gpr, errVar_index, errVar)
    
    if(normalized_noise){
      MIN_ERRVAR<-default_noise
      sum_errVar_basic <- length(errVar)*MIN_ERRVAR
      errVar_scaled <- errVar/sum(errVar)*sum_errVar_basic
      sum_errVar_scaled<- sum(errVar_scaled)
      parVec_gpr <- replace(parVec_gpr, errVar_index, errVar_scaled)
    }
  }
  post <- gpr(X.test = choices, theta = parVec_gpr, X = cbind(obs$x1,obs$x2), y = ((obs$y-50)/100), k = rbf) #scale y observations to zero mean and variance of 1
  utilityVec <- ucb(post, pars = c(r_beta))
  
  #scale to max of prevent overflow by subtracting max
  utilityVec <- utilityVec - max(utilityVec)
  #compute softmax choice probabilities
  p <- exp(utilityVec/r_tau)
  p <- p/sum(p)
  out <- data.frame(gp_estimates=post,ucb=c(utilityVec),softmax=c(p)) %>% mutate(choice=row_number())
  
  diag_choices <- grid_layout %>% 
    filter(x1==x2) 
  
  diag_choices_num <- diag_choices %>% pull
  
  temp_diagonal <- out %>% 
    filter(choice %in% diag_choices_num)
  
  output <- merge(temp_diagonal,diag_choices) %>% 
    select(-choice) %>% 
    mutate(model=model) %>% 
    mutate(lambda=r_lambda,beta=r_beta,tau=r_tau,recency=recency,surprise=surprise,asymmetry=neg_surprise_bias)
  
  return(output)
}


generate_plots<-function(models,normalized_noise,theta_test=NA,positive_asymmetry=FALSE){
  
  d_simulated<- data.frame()
  for(m in models){#,'recency','surprise+','full'
    posterior_stack<- NA
    for (trial in 2:length(X)) {
      d_temp <- data.frame(calculate_2D_case(grid_layout=choices,x1=X[1:trial],x2=X[1:trial],y=Y[1:trial],model=m,theta_test=theta_test,normalized_noise=normalized_noise,positive_asymmetry=positive_asymmetry,posterior_stack = posterior_stack),trial=trial)
      d_simulated<- rbind(d_simulated,d_temp)
      posterior_stack<- d_temp$gp_estimates.mu
    }
  }
  d_simulated<- d_simulated %>% 
    mutate(lower =  gp_estimates.mu- gp_estimates.sig* 0.5, upper =  gp_estimates.mu+ gp_estimates.sig* 0.5)
  
  plotlist_gridplots <- list()
  
  plotlist_mu<- list()
  plotlist_ucb<- list()
  plotlist_softmax<- list()
  for (t in 2:length(X)) {
    d_temp <- d_simulated %>% 
      filter(trial==t)
    current_obs <- d_temp %>% filter(x1==ifelse(t==1,5,
                                                ifelse(t==2,0,
                                                       ifelse(t==3,10,ifelse(t==4,5,
                                                                             ifelse(t==5,2,8))))))
    
    p_temp_mu <- ggplot(d_temp,aes(x1,gp_estimates.mu,group=model)) +
      geom_point(data=current_obs,mapping=aes(x1,gp_estimates.mu,color=model)) +
      geom_line(d_temp, mapping=aes(x=x1, y=gp_estimates.mu,color=model), size=.3) +
      geom_ribbon(d_temp, mapping=aes(x=x1, y=gp_estimates.mu,ymin=lower,ymax=upper ,fill=model), alpha=.3)+
      coord_cartesian(ylim=c(-0.5,0.5))+
      scale_x_continuous(breaks = seq(0, 10, by = 1))+
      # scale_color_manual(values = c(COLOR_MODEL_FULL,COLOR_MODEL_BASIC,COLOR_MODEL_RECENCY,COLOR_MODEL_SURPRISE), name  = 'Model')+#, labels = c('Control', 'Memory')
      # scale_fill_manual(values = c(COLOR_MODEL_FULL,COLOR_MODEL_BASIC,COLOR_MODEL_RECENCY,COLOR_MODEL_SURPRISE), name  = 'Model')#, labels = c('Control', 'Memory')
      theme()
    p_leg_mu <- get_legend(p_temp_mu) #extract legend
    p_temp_mu <- p_temp_mu + theme(legend.position = 'none')
    plotlist_mu[[t]]=p_temp_mu
    
    
    
    p_temp_ucb <- ggplot(d_temp,aes(x1,ucb,group=model)) +
      geom_point(data=current_obs,mapping=aes(x1,ucb,color=model)) +
      geom_line(d_temp, mapping=aes(x=x1, y=ucb,color=model), size=.3)+
      scale_x_continuous(breaks = seq(0, 10, by = 1))+
      scale_color_manual(values = c(COLOR_MODEL_FULL,COLOR_MODEL_BASIC,COLOR_MODEL_RECENCY,COLOR_MODEL_SURPRISE), name  = 'Model')#, labels = c('Control', 'Memory')
    
    p_leg_ucb <- get_legend(p_temp_ucb) #extract legend
    p_temp_ucb <- p_temp_ucb + theme(legend.position = 'none')
    plotlist_ucb[[t]]=p_temp_ucb
    
    
    
    p_temp_softmax <- ggplot(d_temp,aes(x1,softmax,group=model)) +
      geom_point(data=current_obs,mapping=aes(x1,softmax,color=model)) +
      geom_line(d_temp, mapping=aes(x=x1, y=softmax,color=model), size=.3) +
      scale_x_continuous(breaks = seq(0, 10, by = 1))+
      scale_color_manual(values = c(COLOR_MODEL_FULL,COLOR_MODEL_BASIC,COLOR_MODEL_RECENCY,COLOR_MODEL_SURPRISE), name  = 'Model')#, labels = c('Control', 'Memory')
    
    p_leg_softmax <- get_legend(p_temp_softmax) #extract legend
    p_temp_softmax <- p_temp_softmax + theme(legend.position = 'none')
    plotlist_softmax[[t]]=p_temp_softmax
  }
  
  pPlots_mu <- cowplot::plot_grid(
                                  # plotlist_mu[[1]],
                                  plotlist_mu[[2]],
                                  plotlist_mu[[3]],
                                  plotlist_mu[[4]],
                                  plotlist_mu[[5]],
                                  plotlist_mu[[6]],
                                  # plotlist_mu[[7]],
                                  # plotlist_mu[[8]],
                                  # plotlist_mu[[9]],
                                  ncol = 3
  )
  
  pFinal_mu <- cowplot::plot_grid(pPlots_mu,p_leg_mu,rel_widths = c(0.8,0.2),ncol = 2)
  pFinal_mu
  plotlist_gridplots[[1]]<-pFinal_mu
  ggsave(paste0(PLOT_PATH,'mean_est.pdf'), pFinal_mu, width = 8, height = 6, units ='in')
  
  
  
  pPlots_ucb <- cowplot::plot_grid(
                                   # plotlist_ucb[[1]],
                                   plotlist_ucb[[2]],
                                   plotlist_ucb[[3]],
                                   plotlist_ucb[[4]],
                                   plotlist_ucb[[5]],
                                   plotlist_ucb[[6]],
                                   # plotlist_ucb[[7]],
                                   # plotlist_ucb[[8]],
                                   # plotlist_ucb[[9]],
                                   ncol = 3
  )
  
  pFinal_ucb <- cowplot::plot_grid(pPlots_ucb,p_leg_ucb,rel_widths = c(0.8,0.2),ncol = 2)
  pFinal_ucb
  plotlist_gridplots[[2]]<-pFinal_ucb
  ggsave(paste0(PLOT_PATH,'ucb.pdf'), pFinal_ucb, width = 8, height = 6, units ='in')
  
  pPlots_softmax <- cowplot::plot_grid(
                                      # plotlist_softmax[[1]],
                                       plotlist_softmax[[2]],
                                       plotlist_softmax[[3]],
                                       plotlist_softmax[[4]],
                                       plotlist_softmax[[5]],
                                       plotlist_softmax[[6]],
                                       # plotlist_softmax[[7]],
                                       # plotlist_softmax[[8]],
                                       # plotlist_softmax[[9]],
                                       ncol = 3
  )
  
  pFinal_softmax <- cowplot::plot_grid(pPlots_softmax,p_leg_softmax,rel_widths = c(0.8,0.2),ncol = 2)
  pFinal_softmax
  plotlist_gridplots[[3]]<-pFinal_softmax
  ggsave(paste0(PLOT_PATH,'softmax.pdf'), pFinal_softmax, width = 8, height = 6, units ='in')
  
  
  d_max_noise <- d_simulated %>% 
    group_by(model,beta,recency,surprise) %>% 
    summarise(max_noise= max(gp_estimates.sig),mean_noise= mean(gp_estimates.sig))
  
  
  return(plotlist_gridplots)
  
}

cur_models <-c('recency','surprise+')
plots <- generate_plots(cur_models,F,theta_test=c(1.5,0.27,0.03,0,30,log(1)))
plots[[1]]

################################ BETA should not change since noise remains in the same bounds as before, just assignment of noise differs #####################################



generate_surprise_model_plots <- function(){
  
  
  X<-c(10,5,8,2,0)
  Y <- c(100,50,80,20,0)
  ASYM_VEC <- c(-3,-0.33,0.33,3)
  
  for(lambda_test in c(0.75,1.5,3)){
    for(asym_model in c('exponential','basic')){
      if(asym_model =='exponential'){
        pos_asym=TRUE
      }else{
        pos_asym=FALSE
      }
      
      d_asymmetry <- data.frame()
      for (surp in c(0,-10,10,20,50)) {
        for (asym in ASYM_VEC) {
          for (trial in 1:length(X)) {
            d_temp <- data.frame(calculate_2D_case(grid_layout=choices,x1=X[1:trial],x2=X[1:trial],y=Y[1:trial],model='surprise+',theta_test=c(lambda_test,0.27,0.03,0,surp,asym),positive_asymmetry = pos_asym),trial=trial)
            d_asymmetry<- rbind(d_asymmetry,d_temp)
          }
        }
      }
      d_asymmetry<- d_asymmetry %>% 
        mutate(lower =  gp_estimates.mu- gp_estimates.sig* 0.5, upper =  gp_estimates.mu+ gp_estimates.sig* 0.5)
      
      
      
      plotlist_mu<- list()
      plotlist_ucb<- list()
      plotlist_softmax<- list()
      index<-0
      for (s in unique(d_asymmetry$surprise)) {
        for(asym in unique(d_asymmetry$asymmetry)){
          
          t<-5
          index <- index+1
          d_temp <- d_asymmetry %>% 
            filter(trial==t) %>% 
            filter(surprise==s) %>% 
            filter(asymmetry==asym)
          # current_obs <- d_temp %>% filter(x1==ifelse(t%in%c(1,4,7),0,
          #                                             ifelse(t%in%c(2,5,8),10,5)))
          current_obs <- data.frame(x=X,y=(Y-50)/100)
          
          p_temp_mu <- ggplot(d_temp,aes(x1,gp_estimates.mu)) +
            geom_point(data=current_obs,mapping=aes(x=x,y=y)) +
            geom_line(d_temp, mapping=aes(x=x1, y=gp_estimates.mu), size=.3) +
            geom_ribbon(d_temp, mapping=aes(x=x1, y=gp_estimates.mu,ymin=lower,ymax=upper), alpha=.3)+
            coord_cartesian(ylim=c(-0.5,0.5))+
            scale_x_continuous(breaks = seq(0, 10, by = 1))+
            ggtitle(paste0('surprise: ',s,' | asymmetry: ',(asym)))
          
          # p_temp_mu <- ggplot(d_temp,aes(x1,gp_estimates.mu,group=interaction(surprise,asymmetry))) +
          #   geom_point(data=current_obs,mapping=aes(x,y)) +
          #   geom_line(d_temp, mapping=aes(x=x1, y=gp_estimates.mu,color=interaction(surprise,asymmetry)), size=.3) +
          #   coord_cartesian(ylim=c(-0.5,0.5))+
          #   scale_x_continuous(breaks = seq(0, 10, by = 1))+
          #   ggtitle(paste0('surprise: ',s,' | asymmetry: ',(asym)))
          # 
          p_leg_mu <- get_legend(p_temp_mu) #extract legend
          p_temp_mu <- p_temp_mu + theme(legend.position = 'none')
          plotlist_mu[[index]]=p_temp_mu
          # 
          # p_temp_ucb <- ggplot(d_temp,aes(x1,ucb)) +
          #   # geom_point(data=current_obs,mapping=aes(x=x,y=y)) +
          #   geom_line(d_temp, mapping=aes(x=x1, y=ucb), size=.3) +
          #   # coord_cartesian(ylim=c(-0.5,0.5))+
          #   scale_x_continuous(breaks = seq(0, 10, by = 1))+
          #   ggtitle(paste0('surprise: ',s,' | asymmetry: ',(asym)))
          # 
          # p_leg_ucb <- get_legend(p_temp_ucb) #extract legend
          # p_temp_ucb <- p_temp_ucb + theme(legend.position = 'none')
          # plotlist_ucb[[index]]=p_temp_ucb
          # 
          # 
          # p_temp_softmax <- ggplot(d_temp,aes(x1,softmax)) +
          #   # geom_point(data=current_obs,mapping=aes(x=x,y=y)) +
          #   geom_line(d_temp, mapping=aes(x=x1, y=softmax), size=.3) +
          #   # coord_cartesian(ylim=c(-0.5,0.5))+
          #   scale_x_continuous(breaks = seq(0, 10, by = 1))+
          #   ggtitle(paste0('surprise: ',s,' | asymmetry: ',(asym)))
          # 
          # p_leg_softmax <- get_legend(p_temp_softmax) #extract legend
          # p_temp_softmax <- p_temp_softmax + theme(legend.position = 'none')
          # plotlist_softmax[[index]]=p_temp_softmax
        }
      }
      
      pPlots_mu <- cowplot::plot_grid(plotlist_mu[[1]],
                                      plotlist_mu[[2]],
                                      plotlist_mu[[3]],
                                      plotlist_mu[[4]],
                                      plotlist_mu[[5]],
                                      plotlist_mu[[6]],
                                      plotlist_mu[[7]],
                                      plotlist_mu[[8]],
                                      plotlist_mu[[9]],
                                      plotlist_mu[[10]],
                                      plotlist_mu[[11]],
                                      plotlist_mu[[12]],
                                      plotlist_mu[[13]],
                                      plotlist_mu[[14]],
                                      plotlist_mu[[15]],
                                      plotlist_mu[[16]],
                                      plotlist_mu[[17]],
                                      plotlist_mu[[18]],
                                      plotlist_mu[[19]],
                                      plotlist_mu[[20]],
                                      ncol = 4
      )
      
      pPlots_mu
      
      ifelse(!dir.exists(paste0(PLOT_PATH,'lambda_',lambda_test,'/')), dir.create(paste0(PLOT_PATH,'lambda_',lambda_test,'/')), "Folder exists already, overwriting data.")
      ggsave(paste0(PLOT_PATH,'lambda_',lambda_test,'/asymmetry_mu_',asym_model,'_movavg.pdf'), pPlots_mu, width = 16, height = 9, units ='in')
      ggsave(paste0(PLOT_PATH,'lambda_',lambda_test,'/asymmetry_mu_',asym_model,'_movavg.png'), pPlots_mu, width = 16, height = 9, units ='in')
      
      # pPlots_ucb <- cowplot::plot_grid(plotlist_ucb[[1]],
      #                                  plotlist_ucb[[2]],
      #                                  plotlist_ucb[[3]],
      #                                  plotlist_ucb[[4]],
      #                                  plotlist_ucb[[5]],
      #                                  plotlist_ucb[[6]],
      #                                  plotlist_ucb[[7]],
      #                                  plotlist_ucb[[8]],
      #                                  plotlist_ucb[[9]],
      #                                  plotlist_ucb[[10]],
      #                                  plotlist_ucb[[11]],
      #                                  plotlist_ucb[[12]],
      #                                  plotlist_ucb[[13]],
      #                                  plotlist_ucb[[14]],
      #                                  plotlist_ucb[[15]],
      #                                  plotlist_ucb[[16]],
      #                                  plotlist_ucb[[17]],
      #                                  plotlist_ucb[[18]],
      #                                  plotlist_ucb[[19]],
      #                                  plotlist_ucb[[20]],
      #                                  ncol = 4
      # )
      # 
      # pPlots_ucb
      # ggsave(paste0(PLOT_PATH,'asymmetry_ucb_',asym_model,'_movavg.pdf'), pPlots_ucb, width = 16, height = 9, units ='in')
      # ggsave(paste0(PLOT_PATH,'asymmetry_ucb_',asym_model,'_movavg.png'), pPlots_ucb, width = 16, height = 9, units ='in')
      # 
      # pPlots_softmax <- cowplot::plot_grid(plotlist_softmax[[1]],
      #                                      plotlist_softmax[[2]],
      #                                      plotlist_softmax[[3]],
      #                                      plotlist_softmax[[4]],
      #                                      plotlist_softmax[[5]],
      #                                      plotlist_softmax[[6]],
      #                                      plotlist_softmax[[7]],
      #                                      plotlist_softmax[[8]],
      #                                      plotlist_softmax[[9]],
      #                                      plotlist_softmax[[10]],
      #                                      plotlist_softmax[[11]],
      #                                      plotlist_softmax[[12]],
      #                                      plotlist_softmax[[13]],
      #                                      plotlist_softmax[[14]],
      #                                      plotlist_softmax[[15]],
      #                                      plotlist_softmax[[16]],
      #                                      plotlist_softmax[[17]],
      #                                      plotlist_softmax[[18]],
      #                                      plotlist_softmax[[19]],
      #                                      plotlist_softmax[[20]],
      #                                      ncol = 4
      # )
      # 
      # pPlots_softmax
      # ggsave(paste0(PLOT_PATH,'asymmetry_softmax_',asym_model,'_movavg.pdf'), pPlots_softmax, width = 16, height = 9, units ='in')
      # ggsave(paste0(PLOT_PATH,'asymmetry_softmax_',asym_model,'_movavg.png'), pPlots_softmax, width = 16, height = 9, units ='in')
      
      
      
    }
    
  }
}

# generate_surprise_model_plots()


generate_null_model_plots <- function(){
  
  X<-c(10,5,8,2,0)
  Y <- c(100,50,80,20,0)
  
  
  plotlist_mu<- list()
  plotlist_ucb<- list()
  plotlist_softmax<- list()
  
  index<-0
  for(lambda_test in c(0.75,1.5,3)){
    
    d_asymmetry <- data.frame()
    for (trial in 1:length(X)) {
      d_temp <- data.frame(calculate_2D_case(grid_layout=choices,x1=X[1:trial],x2=X[1:trial],y=Y[1:trial],model='null',theta_test=c(lambda_test,0.27,0.03,0,0,NA),positive_asymmetry = pos_asym),trial=trial)
      d_asymmetry<- rbind(d_asymmetry,d_temp)
    }
    d_asymmetry<- d_asymmetry %>% 
      mutate(lower =  gp_estimates.mu- gp_estimates.sig* 0.5, upper =  gp_estimates.mu+ gp_estimates.sig* 0.5)
    
    
    
    
    t<-5
    index <- index+1
    d_temp <- d_asymmetry %>%
      filter(trial==t)
    #%>% 
    #   filter(surprise==s) %>% 
    #   filter(asymmetry==asym)
    # # current_obs <- d_temp %>% filter(x1==ifelse(t%in%c(1,4,7),0,
    #                                             ifelse(t%in%c(2,5,8),10,5)))
    current_obs <- data.frame(x=X,y=(Y-50)/100)
    
    p_temp_mu <- ggplot(d_temp,aes(x1,gp_estimates.mu)) +
      geom_point(data=current_obs,mapping=aes(x=x,y=y)) +
      geom_line(d_temp, mapping=aes(x=x1, y=gp_estimates.mu), size=.3) +
      geom_ribbon(d_temp, mapping=aes(x=x1, y=gp_estimates.mu,ymin=lower,ymax=upper), alpha=.3)+
      coord_cartesian(ylim=c(-0.5,0.5))+
      scale_x_continuous(breaks = seq(0, 10, by = 1))+
      ggtitle(paste0('lambda: ',lambda_test)) + 
      theme(legend.position = 'none')
    plotlist_mu[[index]]=p_temp_mu
    
    
    
    
  }
  pPlots_mu <- cowplot::plot_grid(plotlist_mu[[1]],
                                  plotlist_mu[[2]],
                                  plotlist_mu[[3]],
                                  ncol = 1
  )
  
  pPlots_mu
  
  ggsave(paste0(PLOT_PATH,'/GP.pdf'), pPlots_mu, width = 16, height = 9, units ='in')
  ggsave(paste0(PLOT_PATH,'/GP.png'), pPlots_mu, width = 16, height = 9, units ='in')
  
  
  
}

# generate_null_model_plots()


comparison_movavg_x_static <- function(){

  
  X<-c(10,5,8,2,0,9,9,9,9,9)
  Y <- c(100,50,80,20,0,90,90,90,90,90)
  ASYM_VEC <- c(-3,-0.33,0.33,3)
  
  # lambda_test <- 1.5
  
  for (lambda_test in c(0.75,1.5,3.0)) {
    
    d_asymmetry <- data.frame()
    for(asym_model in c('movavg','basic')){
      if(asym_model =='movavg'){
        averaging_surprise=TRUE
      }else{
        averaging_surprise=FALSE
      }
      
      for (surp in c(0,-10,10,20,50)) {
        for (asym in ASYM_VEC) {
          for (trial in 1:length(X)) {
            d_temp <- data.frame(calculate_2D_case(grid_layout=choices,x1=X[1:trial],x2=X[1:trial],y=Y[1:trial],model='surprise+_',theta_test=c(lambda_test,0.27,0.03,0,surp,asym),positive_asymmetry = TRUE, averaging_surprise = averaging_surprise),trial=trial,modelName=paste0('surprise+_',asym_model))
            d_asymmetry<- rbind(d_asymmetry,d_temp)
          }
        }
      }
      
    }
    d_asymmetry<- d_asymmetry %>% 
      mutate(lower =  gp_estimates.mu- gp_estimates.sig* 0.5, upper =  gp_estimates.mu+ gp_estimates.sig* 0.5)
    
    
    plotlist_mu<- list()
    plotlist_ucb<- list()
    plotlist_softmax<- list()
    index<-0
    surprise_index<-0
    for (s in unique(d_asymmetry$surprise)) {
      current_obs <- data.frame(x=X,y=(Y-50)/100,trial=1:10)
      for (t in unique(d_asymmetry$trial)) {
        for(asym in unique(d_asymmetry$asymmetry)){
          index <- index+1
          d_temp <- d_asymmetry %>% 
            filter(trial==t) %>% 
            filter(surprise==s) %>% 
            filter(asymmetry==asym)
          
          temp_obs<-current_obs %>% filter(trial==t)
          
          p_temp_mu <- ggplot(d_temp,aes(x1,gp_estimates.mu),group=modelName) +
            geom_point(data=temp_obs,mapping=aes(x=x,y=y)) +
            geom_line(d_temp, mapping=aes(x=x1, y=gp_estimates.mu,color=modelName), size=.3) +
            geom_ribbon(d_temp, mapping=aes(x=x1, y=gp_estimates.mu,ymin=lower,ymax=upper,fill=modelName), alpha=.3)+
            coord_cartesian(ylim=c(-0.5,0.5))+
            scale_x_continuous(breaks = seq(0, 10, by = 1))+
            # theme(plot.subtitle = )+
            labs(x='x',y='y est.',color='Surprise prior',fill='Surprise prior',subtitle = paste0('asym.: ',(asym),' | trial: ',t))
          
          leg_mu <- get_legend(p_temp_mu)
          p_temp_mu <- p_temp_mu +
            theme(legend.position = 'none')
          
          plotlist_mu[[index]]=p_temp_mu
          
        }
      }
      
      
      pPlots_mu <- cowplot::plot_grid(plotlist_mu[[surprise_index*40+0+1]],
                                      plotlist_mu[[surprise_index*40+0+2]],
                                      plotlist_mu[[surprise_index*40+0+3]],
                                      plotlist_mu[[surprise_index*40+0+4]],
                                      plotlist_mu[[surprise_index*40+4+1]],
                                      plotlist_mu[[surprise_index*40+4+2]],
                                      plotlist_mu[[surprise_index*40+4+3]],
                                      plotlist_mu[[surprise_index*40+4+4]],
                                      plotlist_mu[[surprise_index*40+8+1]],
                                      plotlist_mu[[surprise_index*40+8+2]],
                                      plotlist_mu[[surprise_index*40+8+3]],
                                      plotlist_mu[[surprise_index*40+8+4]],
                                      plotlist_mu[[surprise_index*40+12+1]],
                                      plotlist_mu[[surprise_index*40+12+2]],
                                      plotlist_mu[[surprise_index*40+12+3]],
                                      plotlist_mu[[surprise_index*40+12+4]],
                                      plotlist_mu[[surprise_index*40+16+1]],
                                      plotlist_mu[[surprise_index*40+16+2]],
                                      plotlist_mu[[surprise_index*40+16+3]],
                                      plotlist_mu[[surprise_index*40+16+4]],
                                      plotlist_mu[[surprise_index*40+20+16+1]],
                                      plotlist_mu[[surprise_index*40+20+16+2]],
                                      plotlist_mu[[surprise_index*40+20+16+3]],
                                      plotlist_mu[[surprise_index*40+20+16+4]],
                                      
                                      ncol = 4
      )
      
      
      pPlots_mu <- cowplot::plot_grid(pPlots_mu,leg_mu,ncol=2,rel_widths = c(0.9,0.1))
      pPlots_mu
      
      title <- ggdraw() +
        labs(title = paste0("Surprise: ",s))+
        theme(plot.title = element_text(hjust = 0.5))
      pPlots_mu <- cowplot::plot_grid(title,pPlots_mu,ncol=1,rel_heights =  c(0.1,0.9))
      pPlots_mu
      
      lambda_path <- paste0(PLOT_PATH,'lambda_',lambda_test,'/')
      ifelse(!dir.exists(lambda_path), dir.create(lambda_path), "Folder exists already, overwriting data.")
      
      surprise_path <- paste0(lambda_path,'surprise_',s,'/')
      ifelse(!dir.exists(surprise_path), dir.create(surprise_path), "Folder exists already, overwriting data.")
      
      ggsave(paste0(surprise_path,'asymmetry_mu_comparison_movavg_x_static.pdf'), pPlots_mu, width = 16, height = 9, units ='in')
      ggsave(paste0(surprise_path,'asymmetry_mu_comparison_movavg_x_static.png'), pPlots_mu, width = 16, height = 9, units ='in')
      
      
      
      
      surprise_index<-surprise_index+1
    }
    
    
    
  }
  
  
  
}
comparison_movavg_x_static()


generate_surprise_model_plots_noisenormalization <- function(){
  
  
  X<-c(10,5,8,2,0)
  Y <- c(100,50,80,20,0)
  ASYM_VEC <- c(-3,-0.33,0.33,3)
  
  lambda_test <- 1.5
  d_asymmetry <- data.frame()
  for(asym_model in c('exponential_static','normalized_noise','null')){
    if(asym_model =='normalized_noise'){
      noise_normalization=TRUE
    }else{
      noise_normalization=FALSE
    }
    
    for (surp in c(50,20)) {
      for(NOISE_CONSTANT in rev(c(0.0001,0.01,0.1,1))){
        for (asym in ASYM_VEC) {
          for (trial in 1:length(X)) {
            d_temp <- data.frame(calculate_2D_case(grid_layout=choices,x1=X[1:trial],x2=X[1:trial],y=Y[1:trial],model=asym_model,theta_test=c(lambda_test,0.27,0.03,0,surp,asym),normalized_noise = noise_normalization ,positive_asymmetry = TRUE, averaging_surprise = FALSE,default_noise = NOISE_CONSTANT),trial=trial,modelName=paste0('surprise+_',asym_model),noise_term=NOISE_CONSTANT)
            d_asymmetry<- rbind(d_asymmetry,d_temp)
          }
        }
      }
    }
  }
  
  d_asymmetry<- d_asymmetry %>% 
    mutate(lower =  gp_estimates.mu- gp_estimates.sig* 0.5, upper =  gp_estimates.mu+ gp_estimates.sig* 0.5)
  
  d_example <- d_asymmetry %>% 
    group_by(modelName,noise_term,surprise) %>% 
    summarize(meanSig= mean(gp_estimates.sig))
  
  
  for(nt in unique(d_asymmetry$noise_term)){
    
    plotlist_mu<- list()
    plotlist_ucb<- list()
    plotlist_softmax<- list()
    index<-0
    for (s in unique(d_asymmetry$surprise)) {
      current_obs <- data.frame(x=X,y=(Y-50)/100,trial=1:5)
      for (t in unique(d_asymmetry$trial)) {
        for(asym in unique(d_asymmetry$asymmetry)){
          index <- index+1
          d_temp <- d_asymmetry %>% 
            filter(trial==t) %>% 
            filter(surprise==s) %>% 
            filter(asymmetry==asym) %>% 
            filter(noise_term==nt)
          
          temp_obs<-current_obs %>% filter(trial==t)
          
          p_temp_mu <- ggplot(d_temp,aes(x1,gp_estimates.mu),group=modelName) +
            geom_point(data=temp_obs,mapping=aes(x=x,y=y)) +
            geom_line(d_temp, mapping=aes(x=x1, y=gp_estimates.mu,color=modelName), size=.3) +
            geom_ribbon(d_temp, mapping=aes(x=x1, y=gp_estimates.mu,ymin=lower,ymax=upper,fill=modelName), alpha=.3)+
            coord_cartesian(ylim=c(-0.5,0.5))+
            scale_x_continuous(breaks = seq(0, 10, by = 1))+
            ggtitle(paste0('surprise: ',s,' | asymmetry: ',(asym),'| trial: ',t))
          
          leg <- get_legend(p_temp_mu)
          
          p_temp_mu <- p_temp_mu +
            theme(legend.position = 'none')
          
          plotlist_mu[[index]]=p_temp_mu
          
        }
      }
    }
    
    pPlots_mu <- cowplot::plot_grid(plotlist_mu[[0+1]],
                                    plotlist_mu[[0+2]],
                                    plotlist_mu[[0+3]],
                                    plotlist_mu[[0+4]],
                                    plotlist_mu[[4+1]],
                                    plotlist_mu[[4+2]],
                                    plotlist_mu[[4+3]],
                                    plotlist_mu[[4+4]],
                                    plotlist_mu[[8+1]],
                                    plotlist_mu[[8+2]],
                                    plotlist_mu[[8+3]],
                                    plotlist_mu[[8+4]],
                                    plotlist_mu[[12+1]],
                                    plotlist_mu[[12+2]],
                                    plotlist_mu[[12+3]],
                                    plotlist_mu[[12+4]],
                                    plotlist_mu[[16+1]],
                                    plotlist_mu[[16+2]],
                                    plotlist_mu[[16+3]],
                                    plotlist_mu[[16+4]],
                                    
                                    ncol = 4
    )
    
    pPlots_legend <- cowplot::plot_grid(pPlots_mu,leg, nrow = 2,rel_heights = c(0.9,0.1))
    pPlots_legend
    
    ifelse(!dir.exists(paste0(PLOT_PATH,'lambda_',lambda_test,'/')), dir.create(paste0(PLOT_PATH,'lambda_',lambda_test,'/')), "Folder exists already, overwriting data.")
    ggsave(paste0(PLOT_PATH,'lambda_',lambda_test,'/asymmetry_mu_comparison_noisenormal_x_static_',nt,'.pdf'), pPlots_legend, width = 16, height = 9, units ='in')
    ggsave(paste0(PLOT_PATH,'lambda_',lambda_test,'/asymmetry_mu_comparison_noisenormal_x_static_',nt,'.png'), pPlots_legend, width = 16, height = 9, units ='in')
    
    
    
  }
  
  
  
}
# generate_surprise_model_plots_noisenormalization()
