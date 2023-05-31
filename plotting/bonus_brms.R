rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'brms','matrixcalc') 
invisible(lapply(packages, require, character.only = TRUE)) #loads packages

source('data_munging.R')
source('statistics.R')

homePath= paste0(getwd(),"/")
modelpath = 'brmsModels'

# Input arguments for the script
args = commandArgs(TRUE)
# clusterid currently is participant
brm_model <- as.integer(args[1])

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


d_bonus_judegements <- load_bonus_preds_combined() %>% 
  mutate(h_est=h_est/100) %>% 
  mutate(gp_est=gp_est+.5) %>% 
  mutate(gp_conf=1-gp_conf) %>% 
  mutate(z_obs=z_obs+.5)

#TODO: dist_nearest, dist_max, dist_mostrecenct
d_bonus_hist <- load_bonus_history(inclAFC = F)

d_dist <- data.frame()
for(id in unique(d_bonus_judegements$UID)){
  sub_judgement <- d_bonus_judegements %>% filter(UID==id)
  sub_hist <- d_bonus_hist %>% filter(UID==id)
  
  unique_judgements <- sub_judgement %>% 
    filter(!duplicated(paste0(x, y))) %>% 
    select(x,y)
  
  for (r in 1:nrow(unique_judgements) ){
    temp <- unique_judgements[r,]
    xdiff<- abs(sub_hist$x-temp$x)
    ydiff<- abs(sub_hist$y-temp$y)
    
    dist_nearest <- min(xdiff)+min(ydiff)
    dist_recent <- xdiff[length(xdiff)]+ydiff[length(ydiff)]
    
    max_obs <- sub_hist %>% filter(z==max(z))
    dist_max<- min(abs(max_obs$x-temp$x)+abs(max_obs$y-temp$y))
    
    d_dist_row <- data.frame(UID=id,x=temp$x,y=temp$y, dist_nearest=dist_nearest,dist_recent=dist_recent,dist_max=dist_max)
    d_dist <- rbind(d_dist,d_dist_row)
  }
}

d_bonus <- merge(d_bonus_judegements, d_dist, by=c('UID','x','y'))

d_bonus_null <- d_bonus %>% filter(gp_model=='null')
d_bonus_full <- d_bonus %>% filter(gp_model=='full')
d_bonus_recency <- d_bonus %>% filter(gp_model=='recency')
d_bonus_surprise <- d_bonus %>% filter(gp_model=='surprise')

predictionDF <- data.frame(id= rep(d_bonus_null$UID, 4), humanPrediction = rep(d_bonus_null$h_est, 4), 
                           humanConfidence = rep(d_bonus_null$h_conf, 4), scenario = rep(d_bonus_null$scenario, 4),
                           environment = rep(d_bonus_null$env, 4), 
                           modelPrediction = c(d_bonus_null$gp_est, d_bonus_full$gp_est, d_bonus_recency$gp_est, d_bonus_surprise$gp_est), 
                           modelUncertainty = c(d_bonus_null$gp_conf, d_bonus_full$gp_conf, d_bonus_recency$gp_conf, d_bonus_surprise$gp_conf),
                           model = rep(c('GP', 'GP + RS','GP + R','GP + S'),each = nrow(d_bonus_null) ))
predictionDF$model <- factor(predictionDF$model, level <- c('GP', 'GP + RS','GP + R','GP + S')) 
#Mixed effects models
prior <- c(set_prior("normal(0,1)", class = "b"),set_prior("normal(0,1)", class = "sd"))

if(brm_model==1){
  judgments_GP <- run_model(brm(modelPrediction ~ 0+ intercept+ humanPrediction+scenario+humanPrediction*scenario +(1+humanPrediction*scenario|id), data=subset(predictionDF, model=='GP'), 
                                prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_','GP'))
  summary(judgments_GP)
}
if(brm_model==2){
  judgments_GPRS <- run_model(brm(modelPrediction ~ 0+ intercept+ humanPrediction+scenario+humanPrediction*scenario +(1+humanPrediction*scenario|id), data=subset(predictionDF, model=='GP + RS'), 
                                  prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_','GP+RS'))
  summary(judgments_GPRS)
}
if(brm_model==3){
  judgments_GPR <- run_model(brm(modelPrediction ~ 0+ intercept+ humanPrediction+scenario+humanPrediction*scenario +(1+humanPrediction*scenario|id), data=subset(predictionDF, model=='GP + R'), 
                                 prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_','GP+R'))
  summary(judgments_GPR)
}
if(brm_model==4){
  judgments_GPS <- run_model(brm(modelPrediction ~ 0+ intercept+ humanPrediction+scenario+humanPrediction*scenario +(1+humanPrediction*scenario|id), data=subset(predictionDF, model=='GP + S'), 
                                 prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_','GP+S'))
  summary(judgments_GPS)
}


if(brm_model==5){
  judgments_GP <- run_model(brm(modelUncertainty ~ 0+ intercept+ humanConfidence+scenario+humanConfidence*scenario +(1+humanConfidence*scenario|id), data=subset(predictionDF, model=='GP'), 
                                prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_conf_','GP'))
  summary(judgments_GP)
}
if(brm_model==6){
  judgments_GPRS <- run_model(brm(modelUncertainty ~ 0+ intercept+ humanConfidence+scenario+humanConfidence*scenario +(1+humanConfidence*scenario|id), data=subset(predictionDF, model=='GP + RS'), 
                                  prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_conf_','GP+RS'))
  summary(judgments_GPRS)
}
if(brm_model==7){
  judgments_GPR <- run_model(brm(modelUncertainty ~ 0+ intercept+ humanConfidence+scenario+humanConfidence*scenario +(1+humanConfidence*scenario|id), data=subset(predictionDF, model=='GP + R'), 
                                 prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_conf_','GP+R'))
  summary(judgments_GPR)
}
if(brm_model==8){
  judgments_GPS <- run_model(brm(modelUncertainty ~ 0+ intercept+ humanConfidence+scenario+humanConfidence*scenario +(1+humanConfidence*scenario|id), data=subset(predictionDF, model=='GP + S'), 
                                 prior = prior, cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = paste0('GPbonusJudgments_conf_','GP+S'))
  summary(judgments_GPS)
}
