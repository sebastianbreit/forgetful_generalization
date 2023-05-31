#Data preparation of experiment data for evaluations
# Plot behavioral results

# house keeping
# rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr','jsonlite','stringr','matrixcalc','jsonlite') #tidyverse
#invisible(lapply(packages, install.packages, character.only = TRUE))
invisible(lapply(packages, require, character.only = TRUE)) #loads packages

source('models.R')

homePath= paste0(getwd(),"/")

FACTOR=25
GRIDSIZE=11
CLICKS=25
ROUNDS=15

COND_BASELINE=1
COND_MEMORY=2


BASE_PATH_DATA = "data/data_experiment_11x11/"
SIMULATED_DATA_PATH = "data/data_simulated_11x11/"
DATA_PATH= paste0(BASE_PATH_DATA,"experiment_data_raw.csv")
DATA_PATH_2018= paste0('data/data_experiment_2018/',"experiment_data_raw.csv")
# RAW_DATA= 'experiment_11x11_full'
# RAW_DATA= 'experiment_nobonus' # This is bugged model full data
RAW_DATA= 'corrected_models'


DEMOGRAPHICS_PATH="~/Documents/ML-Nextcloud/experiment-output/full/demographics"
BONUS_PATH="~/Documents/ML-Nextcloud/experiment-output/full/bonus"
DEMOGRAPHICS_MERGE_PATH="~/Documents/ML-Nextcloud/experiment-output/full/mapping_session_id.csv"
OUTPUT_PATH_DEMOGRAPHICS= paste0(BASE_PATH_DATA,"demographics_data.csv")
OUTPUT_PATH_BONUS_x_AGE = paste0(BONUS_PATH,"/age_bin")


#Main function
dataImport <- function(normalize=TRUE,bonusRound=FALSE,denormalize=FALSE,apply_agefactor=TRUE,apply_edufactor=TRUE){
  
  #read in data
  
  d_columns= c("UID","scenario","kernel","scale","envOrder","searchHistory","reward","start","end","age","gender","education","processDescription","assigned","completed") 
  d_raw = read.csv(DATA_PATH,sep=";",header = FALSE)
  colnames(d_raw) = d_columns
  
  d_raw<- d_raw %>% 
    filter(!end=="NULL") %>% 
    mutate(age_bin=NA)
  
  dat <- d_raw
  
  #dummy data frame
  data<-data.frame(UID=numeric(), trial=numeric(), x=numeric(), y=numeric(), 
                   z=numeric(),  kernel=numeric(), scenario=numeric(), 
                   round=numeric(), env=numeric(), delta_x=numeric(), 
                   maxFound=numeric(),maxFoundAtTime=numeric(),
                   veryLargeFoundAtTime=numeric(),largeFoundAtTime=numeric())
  
  for(col in 1:ncol(dat)){
    dat[,col]<-as.character(dat[,col])
  }
  
  df_bonus <- data.frame()
  df_bonus_history <- data.frame()
  
  #Compile experiment  data
  for (i in 1:nrow(dat)){
    #parse JSON
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'xcollect:','"xcollect":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'ycollect:','"ycollect":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'zcollect:','"zcollect":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'zcollectScaled:','"zcollectScaled":')
    
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'bonusCollect:','"bonusCollect":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'bonusSelectionCollect:','"bonusSelectionCollect":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'x:','"x":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'y:','"y":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'z:','"z":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'est:','"est":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'conf:','"conf":')
    
    searchHistory <- fromJSON(as.character(dat$searchHistory[i])) # default is ,flatten=FALSE
    
    #sampled x value
    x<-unlist(searchHistory$xcollect)
    #sampled y value
    y<-unlist(searchHistory$ycollect)
    #sampled z value (UNSCALED!)
    z<-unlist(searchHistory$zcollect)
    z_scaled<-unlist(searchHistory$zcollectScaled)
    zmax<-t(apply(z,1, maxton))
    if (normalize==TRUE){
      #normalize z and zmax
      z <- (z-50)/100
      zmax <- (zmax-50)/100
    }
    
    bonusCollect <- NA
    bonusSelectionCollect <- NA
    
    if(bonusRound){
      bonusCollect <- unlist(searchHistory$bonusCollect)
      bonusSelectionCollect <- unlist(searchHistory$bonusSelectionCollect)
      
      df_bonus_temp <- data.frame(x=bonusCollect[1:10],y=bonusCollect[11:20],est=bonusCollect[21:30],conf=bonusCollect[31:40])
    }
    
    # length of round
    len<-CLICKS+1
    trial<-rep(1:len,ROUNDS)
    round <- c()
    for(r in 1:ROUNDS){
      round <- rbind(round,rep(r,len))
    }
    #env number
    env_vec <-fromJSON(as.character(dat$envOrder[i]))
    env <- c()
    for(r in 1:len){
      env <- rbind(env,env_vec)
    } 
    #payoff structure: memory vs no memory
    scenario <-c()
    scenario_list <-as.numeric(strsplit(dat$scenario[i],split='', fixed=TRUE)[[1]])
    if(length(scenario_list)<ROUNDS){
      scenario_list <- append(scenario_list,rep(COND_BASELINE,(ROUNDS-length(scenario_list))))
    }
    for(s in scenario_list){
      scenario <- rbind(scenario, rep(s,len))
    }
    #smoothness: 0 = rough (length scale = 1); 1 = smooth (length scale = 2)
    kernel<-1
    #UID number for each subject
    UID<-c()
    for(r in 1:ROUNDS){
      UID<-rbind(UID,rep(dat$UID[i], len))
    }
    
    
    # env <- t(env)
    round <- t(round)
    scenario <- t(scenario)
    x <- t(x)
    y <- t(y)
    z <- t(z)
    
    #dummy frame
    dim(UID)<-c(ROUNDS*len,1)
    dim(trial)<-c(ROUNDS*len,1)
    dim(x)<-c(ROUNDS*len,1)
    dim(y)<-c(ROUNDS*len,1)
    dim(z)<-c(ROUNDS*len,1)
    dim(zmax)<-c(ROUNDS*len,1)
    # dim(kernel)<-c(ROUNDS*len,1)
    dim(scenario)<-c(ROUNDS*len,1)
    dim(round)<-c(ROUNDS*len,1)
    dim(env)<-c(ROUNDS*len,1)
    
    
    dummy<-data.frame(UID, trial, x, y, z,zmax, kernel, scenario, round, env)
    
    #create a unique listing for each space in the grid
    allopts<-expand.grid(0:(GRIDSIZE-1), 0:(GRIDSIZE-1))
    dummy$chosen<- -99
    for (i in 1:nrow(dummy)){ #loop through data and assign the unique listing from allopts to data$chosen based on the chosen x and y values
      dummy$chosen[i]<-which(dummy$x[i]==allopts$Var1 & dummy$y[i]==allopts$Var2)
    }
    
    bonus_config <- dummy %>% 
      filter(round==15) %>% 
      distinct(UID,env,scenario)
    df_bonus_history_temp <- dummy %>% 
      filter(round==15) %>% 
      filter(trial<15) %>% 
      select(UID,env,scenario,trial,x,y,z,chosen)
    bonus_config$scenario<-ifelse(bonus_config$scenario==1, "Baseline Condition", "Memory Condition")
    
    bonus_config$UID <- as.integer(levels(bonus_config$UID))
    
    temp_UID <- bonus_config %>% distinct(UID) %>% as.numeric()
    
    if(bonusRound){
      df_bonus_temp$UID <- temp_UID
      
      df_bonus_temp <- full_join(bonus_config,df_bonus_temp,by='UID')
    }
    
    #calculate manhattan distance between clicks
    #calculate distance between clicks
    dummy <- dummy %>%
      group_by(round) %>%
      mutate(delta_x = abs(x - lag(x, default = NA)) + abs(y - lag(y, default = NA)) ) 
    
    
    
    dummy$delta_x[dummy$trial==1]<-NA #set as NA for all first clicks, since it was randomly selected
    
    # Add the max of each round
    dummy <- dummy %>%
      group_by(round) %>%
      mutate(maxFound=max(z)) %>% 
      mutate(maxFoundAtTime=which.min(z<0.49)) %>% 
      mutate(largeFoundAtTime=which.min(z<0.3)) %>% 
      mutate(veryLargeFoundAtTime=which.min(z<0.4))
    
    #bind them together
    dummy <- as.data.frame(dummy)
    data<-rbind(data, dummy)
    if(bonusRound){
      df_bonus<-rbind(df_bonus, df_bonus_temp)
      df_bonus_history<- rbind(df_bonus_history,df_bonus_history_temp)
    }
    
  }
  
  
  data$round <- factor(data$round) #convert round into factor
  #Rename variables
  data$kernel<-ifelse(data$kernel==0, "Rough", "Smooth")
  #recode reward function
  data$scenario<-ifelse(data$scenario==1, "Baseline Condition", "Memory Condition")
  
  #Add demographics data
  data_demographics <- read.csv(OUTPUT_PATH_DEMOGRAPHICS)
  
  data_merged <- merge(data,data_demographics)
  data <- data_merged
  
  #Age bins
  age_limits <- c(18,28,38,48,58,68,78)
  for(i in 1:nrow(data)){
    test_age <- data$age[i]
    for(bin in 1:(length(age_limits)-1)){
      if(between(test_age,age_limits[bin],age_limits[bin+1]-1)){
        data$age_bin[i] <-bin
      }
    }
  }
  
  data <- data %>% 
    mutate(step=trial) %>% 
    mutate(distance=delta_x) %>% 
    mutate(prev_z=lag(z, default = NA)) %>%
    mutate(id=UID)
  
  if(denormalize){
    data <- data %>% 
      mutate(z=z+0.5)
  }
  
  
  data$scenario = factor(data$scenario)
  
  if(apply_agefactor){
    data$age_bin = factor(data$age_bin,labels = c("18-27","28-37","38-47","48-57","58-67","68-77"))
  }
  
  if(apply_edufactor){
    unique_edu_labels <- levels(unique(data$education))
    unique_edu_labels<-c("High school diploma/A-levels",
                         "Technical/community college",
                         "Undergraduate degree (BA/BSc/other)",
                         "Graduate degree (MA/MSc/MPhil/other)",
                         "Doctorate degree (PhD/other)"
    )
    education_levels=c(1:5)  
    apply_eduscale <- function(df_col){
      return(factor(df_col,
                    levels=rev(unique_edu_labels),
                    labels=rev(education_levels)))
    }
    
    
    data$education_levels <- apply_eduscale(data$education)
  }
  
  if(!bonusRound){
    return(data) 
  }else if(bonusRound){
    out <- list()
    out$bonus_judegements <- df_bonus
    out$bonus_history <-df_bonus_history
    return(out)
  }
}

#Maximum reward up until trial x
maxton<-function(x,na.rm=FALSE){
  if(!na.rm){
    maxn<-rep(0,length(x))
    maxn[1]<-x[1]
    for (i in 2:length(x)){
      if (x[i]>maxn[i-1]){maxn[i]<-x[i]}
      else{maxn[i]<-maxn[i-1]}
    }
    return(maxn)  
  }else{
    prev_len <- length(x)
    x <- x[!is.na(x)]
    maxn<-rep(0,length(x))
    maxn[1]<-x[1]
    for (i in 2:length(x)){
      if (x[i]>maxn[i-1]){maxn[i]<-x[i]}
      else{maxn[i]<-maxn[i-1]}
    }
    
    return(c(maxn,rep(NA,(prev_len-length(x)))))
  }
  
}


save_anonymous_demographics <-function(){
  data_frame_names <- list.files(path=DEMOGRAPHICS_PATH,pattern = "*.csv")       
  data_frame_list <- lapply(paste0(DEMOGRAPHICS_PATH,"/",data_frame_names), read.csv) 
  
  df_demographics <- data.frame()
  for(row in 1:length(data_frame_list)){
    temp_df <- data_frame_list[[row]]
    df_demographics<- rbind(df_demographics,temp_df)
  }
  
  df_demographics <- df_demographics %>% 
    mutate(session_ID=Submission.id) %>% 
    select(-Participant.id,-Submission.id) 
  # %>% filter(Reviewed.at != "") 
  
  df_merge_internal_ID <- read.csv(DEMOGRAPHICS_MERGE_PATH,header=FALSE) %>% 
    mutate(UID=V1,session_ID=V2) %>% 
    select(-V1,-V2)
  
  # df_demographics_anonymous <- merge(df_demographics,df_merge_internal_ID) %>% 
  #   mutate(time=Time.taken,education=Highest.education.level.completed,age=Age,sex=Sex) %>% 
  #   select(UID,time,education,age,sex)
  
  df_demographics_anonymous <- full_join(df_demographics,df_merge_internal_ID,by='session_ID') %>% 
    mutate(time=Time.taken,education=Highest.education.level.completed,age=Age,sex=Sex) %>% 
    filter(!is.na(UID)) %>% 
    select(-Status,-Started.at,-Completed.at,-Reviewed.at,-Archived.at,-Time.taken,-session_ID,-Age,-Sex) %>% 
    relocate(c('UID','age','sex','education'))
  write.table(df_demographics_anonymous, file=OUTPUT_PATH_DEMOGRAPHICS,sep=',',row.names = F)
  # return(df_demographics)
}

load_anonymous_demographics <- function(){
  return(read.csv( OUTPUT_PATH_DEMOGRAPHICS))
}

get_demographics <- function(){
  data_frame_names <- list.files(path=DEMOGRAPHICS_PATH,pattern = "*.csv")       
  data_frame_list <- lapply(paste0(DEMOGRAPHICS_PATH,"/",data_frame_names), read.csv) 
  
  df_demographics <- data.frame()
  for(row in 1:length(data_frame_list)){
    temp_df <- data_frame_list[[row]]
    df_demographics<- rbind(df_demographics,temp_df)
  }
  
  return(df_demographics)
}



remove_random_performers <- function(df_plain){
  
  d_participant_means <- df_plain %>% group_by(UID) %>% summarise(mean_z=mean(z),mean_time=mean(time)) 
  d_summary <- df_plain %>%  
    summarise(mean_z=mean(z),sd_z=sd(z),mean_time=mean(time),sd_time=sd(time)) 
  d_outliers <- d_participant_means %>% 
    filter(mean_z<(d_summary$mean_z-3*d_summary$sd_z)) %>% 
    filter(mean_z<(d_summary$mean_time-3*d_summary$sd_time))
  
  
  # d_outliers <- df_plain %>% filter()
  # test <- data.frame(mean_time=mean(df_plain$time),median_time=median(df_plain$time), min_time=min(df_plain$time))
  # d_veryyyy_fast <- df_plain %>% filter(time<500) %>% group_by(UID) %>% summarise(mean(z))
  # 
  # d_normal <- df_plain %>% group_by(UID) %>%  summarise(mean_z=mean(z))
  # d_normal_stats <- data.frame(mean_time=mean(d_normal$mean_z),median_time=median(d_normal$mean_z), min_time=min(d_normal$mean_z))
  # 
  # d_veryyyy_low <- d_normal %>% filter(mean_z<0.1)
  # 
  # return(df_plain %>% filter((UID != d_veryyyy_fast$UID) & (UID !=d_veryyyy_low$UID )))
  return(df_plain %>% filter(!(UID %in% d_outliers)))
  
}

separate_bonus_payments_by_age <- function(df_imported){
  data_frame_names <- list.files(path=BONUS_PATH,pattern = "*.csv")       
  data_frame_list <- lapply(paste0(BONUS_PATH,"/",data_frame_names), read.csv,header=FALSE) 
  
  df_bonus <- data.frame()
  for(row in 1:length(data_frame_list)){
    temp_df <- data_frame_list[[row]]
    df_bonus<- rbind(df_bonus,temp_df)
  }
  
  d_columns= c("PID","session_ID","bonus")
  colnames(df_bonus) = d_columns
  
  d_data <- df_imported %>% 
    mutate(UID=as.integer(as.character(UID)))
  
  df_merge_internal_ID <- read.csv(DEMOGRAPHICS_MERGE_PATH,header=FALSE) %>% 
    mutate(UID=V1,session_ID=V2) %>% 
    select(-V1,-V2)
  
  df_data_w_session <- full_join(d_data,df_merge_internal_ID,by='UID') %>% 
    # mutate(time=Time.taken,education=Highest.education.level.completed,age=Age,sex=Sex) %>% 
    select(UID,session_ID,age,age_bin) %>%
    filter(!is.na(UID))
  df_uniquedata_w_session <-unique(df_data_w_session)
  df_bonus_age <- full_join(df_uniquedata_w_session,df_bonus,by='session_ID') %>% 
    select(PID,session_ID,age,age_bin,bonus,UID) %>% 
    filter(!is.na(bonus)) %>% 
    filter(!is.na(age)) %>% 
    filter(bonus!=0)
  
  for(bin in unique(df_bonus_age$age_bin)){
    df_temp <- df_bonus_age %>% filter(age_bin==bin) %>% select(PID,bonus)
    write.table(df_temp, paste0(OUTPUT_PATH_BONUS_x_AGE,"/",bin,"o6.csv"),sep=",",row.names=FALSE,col.names=FALSE,quote=FALSE)
  }
  summary <- df_bonus_age %>% summarize(mean_earned=3+mean(bonus),sd_earned=sd(bonus))
  return(summary)
}

create_pxp_csv <- function(included_demographics=c('United Kingdom','Other','South Africa')){
  homePath= paste0(getwd(),"/")
  fitting_path <- paste0(homePath,"fitting_results/output/",RAW_DATA,"/")
  # 
  experimentModelList <- c("Memory","NoMemory")
  fittingModelList<-c("recency","surprise+","full","null") # "surprise+" for asymmetric surprise
  # 
  NUM_PARTICIPANTS <- 346
  
  d <- load_exp_data()
  d_fitting_without_bonus <- load_fitting_data()
  
  d_demo <- load_anonymous_demographics()
  
  d_demo_simplified <- d_demo %>% 
    mutate(Country.of.residence=ifelse((!Country.of.residence %in% c('South Africa','United Kingdom')),'Other',
                                       ifelse(Country.of.residence=='South Africa','South Africa',
                                              ifelse(Country.of.residence=='United Kingdom','United Kingdom',NA)))) %>% 
    select(-age,-sex,-time,-education)
  
  d_included_UIDs <- d_demo_simplified %>% 
    filter(Country.of.residence %in% included_demographics)
  d <- d %>% filter(UID %in% d_included_UIDs$UID)
  d_fitting_without_bonus <- d_fitting_without_bonus %>% filter(UID %in% d_included_UIDs$UID)
  
  fitting_output_summary <- data.frame(row.names = c('subject','scenario','fitted_model', 'summed_nLL','rounds'))
  for (par in unique(d_fitting_without_bonus$participant)) {
    temp <- d_fitting_without_bonus %>% filter(participant==par)
    
    # temp_sum_nLL <- temp %>% summarise(sum_nLL=sum(X.1))
    temp_sum_nLL <- temp %>% group_by(input_model,fitted_model)%>% summarise(sum_nLL=sum(nLL))
    temp_count_nLL <- temp %>% group_by(input_model,fitted_model)%>% count(participant)
    
    temp_nLL <- merge(temp_sum_nLL,temp_count_nLL)
    
    fitting_output_summary <- rbind(fitting_output_summary, data.frame(subject=temp_nLL$participant, scenario=temp_nLL$input_model,fitted_model=temp_nLL$fitted_model, summed_nLL=temp_nLL$sum_nLL,rounds=temp_nLL$n ))
    fitting_output_summary <- fitting_output_summary %>% mutate(fitted_model=ifelse(fitted_model=='null','GP',as.character(fitted_model)))
  }
  
  d_demographics_temp <- d %>% select(participant, age, age_bin, education, sex)
  d_demographics_temp <- unique(d_demographics_temp)
  
  fitting_output_summary <- fitting_output_summary %>% mutate(participant=subject)
  fitting_output_summary <- right_join(d_demographics_temp, fitting_output_summary, by = 'participant')
  
  
  pxp_matrix <- matrix(nrow=0,ncol=(length(experimentModelList)*length(fittingModelList)))
  for(s in unique(fitting_output_summary$subject)){
    temp <- fitting_output_summary %>% 
      filter(subject==s)
    
    temp_scenario <- temp %>% 
      filter(scenario=='Memory')
    
    gp <- temp_scenario %>% filter(fitted_model=='GP') %>% select(summed_nLL)
    recency <- temp_scenario %>% filter(fitted_model=='recency') %>% select(summed_nLL)
    surprise_plus <- temp_scenario %>% filter(fitted_model=='surprise+') %>% select(summed_nLL)
    full <- temp_scenario %>% filter(fitted_model=='full') %>% select(summed_nLL)
    
    pxp_row_mem <- c(ifelse(nrow(gp)!=0,as.numeric(gp$summed_nLL),NA),
                     ifelse(nrow(recency)!=0,as.numeric(recency$summed_nLL),NA),
                     ifelse(nrow(surprise_plus)!=0,as.numeric(surprise_plus$summed_nLL),NA),
                     ifelse(nrow(full)!=0,as.numeric(full$summed_nLL),NA))
    
    temp_scenario <- temp %>% 
      filter(scenario=='NoMemory')
    
    gp <- temp_scenario %>% filter(fitted_model=='GP') %>% select(summed_nLL)
    recency <- temp_scenario %>% filter(fitted_model=='recency') %>% select(summed_nLL)
    surprise_plus <- temp_scenario %>% filter(fitted_model=='surprise+') %>% select(summed_nLL)
    full <- temp_scenario %>% filter(fitted_model=='full') %>% select(summed_nLL)
    
    
    pxp_row_nomem <- c(ifelse(nrow(gp)!=0,as.numeric(gp$summed_nLL),NA),
                       ifelse(nrow(recency)!=0,as.numeric(recency$summed_nLL),NA),
                       ifelse(nrow(surprise_plus)!=0,as.numeric(surprise_plus$summed_nLL),NA),
                       ifelse(nrow(full)!=0,as.numeric(full$summed_nLL),NA))
    pxp_row_full <- cbind(t(pxp_row_nomem),t(pxp_row_mem))
    
    # if(rowSums(is.na(pxp_row_full))>0){
    #   # print('row has na, removing row from final matrix')
    #   next
    # }
    pxp_matrix <- rbind(pxp_matrix,pxp_row_full) 
    
  }
  folder_name<- paste0(fitting_path,'pxp/')
  if(identical(included_demographics,c('United Kingdom','Other','South Africa'))){
    folder_name <- paste0(fitting_path,'full_demographics','/')
    ifelse(!dir.exists(folder_name), dir.create(folder_name), "Folder exists already, overwriting data.")
    folder_name <- paste0(folder_name,'pxp/')
    ifelse(!dir.exists(folder_name), dir.create(folder_name), "Folder exists already, overwriting data.")
  }else{
    folder_name <- paste0(fitting_path,str_flatten(included_demographics),'/')
    ifelse(!dir.exists(folder_name), dir.create(folder_name), "Folder exists already, overwriting data.")
    folder_name <- paste0(folder_name,'pxp/')
    ifelse(!dir.exists(folder_name), dir.create(folder_name), "Folder exists already, overwriting data.")
  }
  write.table(fitting_output_summary,file=paste0(folder_name,'summary_df_fitting_results.csv'),row.names = FALSE,col.names = TRUE, sep = ',')
  write.table(pxp_matrix,file=paste0(folder_name,'summary_matrix_fitting_results.csv'),row.names = FALSE,col.names = FALSE, sep = ',')
  
  pxp_matrix_memory <- pxp_matrix[,5:8]
  pxp_matrix_nomemory<- pxp_matrix[,1:4]
  
  write.table(pxp_matrix_memory,file=paste0(folder_name,'summary_matrix_fitting_results_memory.csv'),row.names = FALSE,col.names = FALSE, sep = ',')
  write.table(pxp_matrix_nomemory,file=paste0(folder_name,'summary_matrix_fitting_results_nomemory.csv'),row.names = FALSE,col.names = FALSE, sep = ',')
  
  
  
  
  
  for (a_b in unique(fitting_output_summary$age_bin)) {
    pxp_matrix_age <- matrix(nrow=0,ncol=(length(experimentModelList)*length(fittingModelList)))
    temp_by_age <- fitting_output_summary %>% filter(age_bin==a_b)
    
    # print(a_b)
    # print(unique(temp_by_age$subject))
    for(s in unique(temp_by_age$subject)){
      temp <- temp_by_age %>% filter(subject==s)
      
      temp_by_age_scenario <- temp %>% 
        filter(scenario=='Memory')
      
      gp <- temp_by_age_scenario %>% filter(fitted_model=='GP') %>% select(summed_nLL)
      recency <- temp_by_age_scenario %>% filter(fitted_model=='recency') %>% select(summed_nLL)
      surprise_plus <- temp_by_age_scenario %>% filter(fitted_model=='surprise+') %>% select(summed_nLL)
      full <- temp_by_age_scenario %>% filter(fitted_model=='full') %>% select(summed_nLL)
      
      pxp_row_mem_age <- c(ifelse(nrow(gp)!=0,as.numeric(gp$summed_nLL),NA),
                           ifelse(nrow(recency)!=0,as.numeric(recency$summed_nLL),NA),
                           ifelse(nrow(surprise_plus)!=0,as.numeric(surprise_plus$summed_nLL),NA),
                           ifelse(nrow(full)!=0,as.numeric(full$summed_nLL),NA))
      
      temp_by_age_scenario <- temp %>% 
        filter(scenario=='NoMemory')
      
      gp <- temp_by_age_scenario %>% filter(fitted_model=='GP') %>% select(summed_nLL)
      recency <- temp_by_age_scenario %>% filter(fitted_model=='recency') %>% select(summed_nLL)
      surprise_plus <- temp_by_age_scenario %>% filter(fitted_model=='surprise+') %>% select(summed_nLL)
      full <- temp_by_age_scenario %>% filter(fitted_model=='full') %>% select(summed_nLL)
      
      
      pxp_row_nomem_age <- c(ifelse(nrow(gp)!=0,as.numeric(gp$summed_nLL),NA),
                             ifelse(nrow(recency)!=0,as.numeric(recency$summed_nLL),NA),
                             ifelse(nrow(surprise_plus)!=0,as.numeric(surprise_plus$summed_nLL),NA),
                             ifelse(nrow(full)!=0,as.numeric(full$summed_nLL),NA))
      pxp_row_full_age <- cbind(t(pxp_row_nomem_age),t(pxp_row_mem_age))
      # if(rowSums(is.na(pxp_row_full_age))>0){
      #   # print('row has na, removing row from final matrix')
      #   next
      # }
      pxp_matrix_age <- rbind(pxp_matrix_age,pxp_row_full_age) 
      
      
    }
    
    folder_name<- paste0(fitting_path,'pxp/by_age/')
    if(identical(included_demographics,c('United Kingdom','Other','South Africa'))){
      folder_name <- paste0(fitting_path,'full_demographics','/pxp/by_age/')
      ifelse(!dir.exists(folder_name), dir.create(folder_name), "Folder exists already, overwriting data.")
    }else{
      folder_name <- paste0(fitting_path,str_flatten(included_demographics),'/pxp/by_age/')
      ifelse(!dir.exists(folder_name), dir.create(folder_name), "Folder exists already, overwriting data.")
    }
    
    write.table(pxp_matrix_age,file=paste0(folder_name,'summary_matrix_fitting_results_',a_b,'.csv'),row.names = FALSE,col.names = FALSE, sep = ',')
    
    
    pxp_matrix_memory_age <- pxp_matrix_age[,5:8]
    pxp_matrix_nomemory_age<- pxp_matrix_age[,1:4]
    
    write.table(pxp_matrix_memory_age,file=paste0(folder_name,'summary_matrix_fitting_results_memory_',a_b,'.csv'),row.names = FALSE,col.names = FALSE, sep = ',')
    write.table(pxp_matrix_nomemory_age,file=paste0(folder_name,'summary_matrix_fitting_results_nomemory_',a_b,'.csv'),row.names = FALSE,col.names = FALSE, sep = ',')
    
  }
  
  
  
  
  
}

load_raw_data_and_save_to_csvs <- function(){
  ################################################################################################
  # Data import
  ################################################################################################
  models <- c("recency","surprise+","full","null")#,"null" # exclude null model from analysis since some of them stopped
  exp_models <- c("Memory", "NoMemory") #
  model_descriptions_sim <-c("GP + R","GP + S+","GP + RS","GP")#
  model_descriptions_exp <-c("Memory","Baseline")
  
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
  
  d<-data.frame()
  d <- data.frame(row.names = c('input_model','fitted_model','participant','nLL','lambda','beta','tau','recency','surprise_plus','full_r','full_s','asymmetry'))
  
  missing_files <- data.frame(row.names = c('input_model','fitted_model','participant'))
  
  for(input_m in exp_models){
    for(fitted_m in models){
      for(p in 1:NUM_PARTICIPANTS){
        
        fitting_output_file <- paste0('fitting_results/output/',RAW_DATA,'/experiment_data_',input_m,'/fittedmodel_',fitted_m,'/part_',p,'.csv')
        if(!file.exists(fitting_output_file)) {
          missing_files <- rbind(missing_files, data.frame(input_model=input_m,fitted_model=fitted_m,participant=p))
          print(paste0('Missing participant ',p,' with model ',fitted_m,' ,condition ',input_m))
          next
        }
        
        temp <- read.csv(fitting_output_file)
        d <- rbind(d,data.frame(input_model=input_m,fitted_model=fitted_m,participant=p,
                                nLL=temp[,2],lambda=temp[,3],beta=temp[,4],tau=temp[,5],
                                recency= if(dim(temp)[2]>=6 && fitted_m=='recency') temp[,6] else NA, 
                                surprise_plus= if(dim(temp)[2]>=6 && fitted_m=='surprise+') temp[,6] else NA, 
                                full_r= if(dim(temp)[2]>=6 && fitted_m=='full') temp[,6] else NA, 
                                full_s= if(dim(temp)[2]>=6 && fitted_m=='full') temp[,7] else NA, 
                                asymmetry= ifelse( fitted_m=='surprise+' ||  fitted_m=='full' ,ifelse(dim(temp)[2]==7, temp[,7],temp[,8]), NA)))
      }
    }
  }
  
  # ###############################################################################
  # ############################ Code for identifying which combs to rerun ########
  # ###############################################################################
  d_temp <- d %>%
    # filter(fitted_model =='null') %>%
    select(participant,input_model,fitted_model,nLL)
  
  incomplete_runs <- data.frame(row.names = c('input_model','fitted_model','participant'))
  for(p in unique(d_temp$participant)){
    for(in_m in unique(d_temp$input_model)){
      inner_temp <- d_temp %>% filter(participant==p) %>% filter(input_model==in_m)
      for(f_m in unique(d_temp$fitted_model)){
        inner_inner_temp <- inner_temp %>% filter(fitted_model==f_m)
        rows_cv <- inner_inner_temp %>% count(nLL) %>% summarize(cv_rounds=sum(n)) %>% as.integer()
        
        if(rows_cv<7){
          incomplete_run <- unique(data.frame(input_model=in_m,fitted_model=f_m, participant=p))
          # print(incomplete_run)
          incomplete_runs <- rbind(incomplete_runs,incomplete_run) #no fitted_model=inner_temp$fitted_model, since we just do this for null here
        }
      }
    }
  }
  # d_test <- incomplete_runs %>% filter(fitted_model!='full')
  # d_test <- d_temp %>% filter(fitted_model=='full',participant==186,input_model=='Memory')
  
  d_test <- d_temp %>% filter(participant %in% unique(incomplete_runs$participant))
  
  d_test_rounds <- d_test %>% group_by(participant, input_model, fitted_model) %>% count(nLL) %>% summarize(cv_rounds=sum(n))
  minimum_rounds <- min(d_test_rounds$cv_rounds)
  
  d_reduced_cvrounds <- data.frame()
  for(p in unique(d$participant)){
    for(in_m in unique(d$input_model)){
      for(f_m in unique(d$fitted_model)){
        d_temp_cvrounds <- d %>% filter(participant==p) %>% filter(input_model==in_m) %>% filter(fitted_model==f_m) %>% head(minimum_rounds)
        
        d_reduced_cvrounds <- rbind(d_reduced_cvrounds, d_temp_cvrounds)
      }
    }
  }
  d_fitting_all <- d
  d_fitting_reduced_cvrounds <-d_reduced_cvrounds
  
  
  
  d_raw_exp<- dataImport()
  
  unique_UIDs <- unique(d_raw_exp$UID)
  
  
  d_raw_exp <- d_raw_exp %>% 
    mutate(experiment_round=round) 
  
  d_ID_mapping <- d_raw_exp %>% 
    group_by(UID) %>% 
    summarise(participant=NA)
  
  d_ID_mapping$participant <- seq.int(nrow(d_ID_mapping)) 
  
  d_exp <- merge(d_raw_exp,d_ID_mapping)
  
  
  
  # ###############################################################################
  # ############################ Combine data into one df #########################
  # ###############################################################################
  
  d_fitting <- merge(d_fitting_all,d_ID_mapping) %>% 
    mutate(id=participant)
  
  d_demographics_exp <- d_raw_exp %>% dplyr::select(UID,age_bin,education)
  d_demographics_exp <- unique(d_demographics_exp)
  
  
  d_fitting <- merge(d_fitting,d_demographics_exp)
  d_fitting$age_bin = factor(d_fitting$age_bin,labels = c("18-27","28-37","38-47","48-57","58-67","68-77"))
  
  
  write.table(d_fitting, file=paste0(BASE_PATH_DATA,'fitting_data_',RAW_DATA,'.csv'),row.names = FALSE,col.names = TRUE,sep = ',')
  write.table(d_exp, file=paste0(BASE_PATH_DATA,'experiment_data_',RAW_DATA,'.csv'),row.names = FALSE,col.names = TRUE,sep = ',')
}

load_fitting_data<- function(buggedModel=FALSE){
  if(buggedModel){
    d <- read.csv( file=paste0(BASE_PATH_DATA,'bugged_data/','fitting_data_final.csv'))
  }else{
    
    d <- read.csv( file=paste0(BASE_PATH_DATA,'fitting_data_final.csv'))
    d <- d %>% 
      mutate(fitted_model=ifelse(fitted_model=='recency_exp','recency',
                                 ifelse(fitted_model=='full_exp_posterior','full',
                                        ifelse(fitted_model=='surprise+_exp_posterior','surprise+',
                                               ifelse(fitted_model=='null_exp','null','surprise+_static')))))
  }
  return(d)
}
load_exp_data<- function(extendedData=FALSE,includeLast=FALSE,removeSA=FALSE,removeLearning=FALSE){
  if(extendedData){
    d_exp <- read.csv( file=paste0(BASE_PATH_DATA,'experiment_data_extended.csv'))
  }else{
    d_exp <- read.csv( file=paste0(BASE_PATH_DATA,'experiment_data_clean.csv'))
  }
  d_demo <- load_anonymous_demographics()
  
  d_demo_simplified <- d_demo %>% 
    mutate(Country.of.residence=ifelse((!Country.of.residence %in% c('South Africa','United Kingdom')),'Rest/Europe',
                                       ifelse(Country.of.residence=='South Africa','South Africa',
                                              ifelse(Country.of.residence=='United Kingdom','United Kingdom',NA)))) %>% 
    select(-age,-sex,-time,-education)
  
  d <- merge(d_exp,d_demo_simplified,by='UID')
  
  d <- d %>% 
    mutate(age_bin_numeric= ifelse(age_bin=='18-27',20,
                                   ifelse(age_bin=='28-37',30,
                                          ifelse(age_bin=='38-47',40,
                                                 ifelse(age_bin=='48-57',50,
                                                        ifelse(age_bin=='58-67',60,70))))))
  
  if(!includeLast){
    d <- d %>% filter(round!=15)
  }
  if(removeSA){
    d <- d %>% filter(Country.of.residence!='South Africa')
  }
  if(removeLearning){
    d <- d %>% filter(round > 4)
  }
  return(d)
}



load_bugged_split_data_and_save_to_csvs <- function(){
  RAW_DATA= 'experiment_nobonus'
  RAW_DATA_APPENDICES <- c('_part1','_part2')
  ################################################################################################
  # Data import
  ################################################################################################
  models <- c("recency","surprise+","full","null")#,"null" # exclude null model from analysis since some of them stopped
  exp_models <- c("Memory", "NoMemory") #
  model_descriptions_sim <-c("GP + R","GP + S+","GP + RS","GP")#
  model_descriptions_exp <-c("Memory","Baseline")
  
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
  
  d<-data.frame()
  d <- data.frame(row.names = c('input_model','fitted_model','participant','nLL','lambda','beta','tau','recency','surprise_plus','asymmetry'))
  
  missing_files <- data.frame(row.names = c('input_model','fitted_model','participant'))
  
  for(input_m in exp_models){
    for(fitted_m in models){
      for(p in 1:NUM_PARTICIPANTS){
        partial_entry <- data.frame()
        prev_length <- NA
        for(part in RAW_DATA_APPENDICES){
          fitting_output_file <- paste0('fitting_results/output/',RAW_DATA, part,'/experiment_data_',input_m,'/fittedmodel_',fitted_m,'/part_',p,'.csv')
          if(!file.exists(fitting_output_file)) {
            missing_files <- rbind(missing_files, data.frame(input_model=input_m,fitted_model=fitted_m,participant=p))
            print(paste0('Missing participant ',p,' with model ',fitted_m,' ,condition ',input_m))
            next
          }
          if(is.na(prev_length)){
            partial_entry <- read.csv(fitting_output_file)
            prev_length <- nrow(partial_entry)
          }else{
            # if(prev_length<7)
            # {
            #   print('debugging')
            # }
            temp <- read.csv(fitting_output_file)
            partial_entry<- rbind(partial_entry,tail(temp,(7-prev_length)))
          }
        }
        
        d <- rbind(d,data.frame(input_model=input_m,fitted_model=fitted_m,participant=p,
                                nLL=partial_entry[,2],lambda=partial_entry[,3],beta=partial_entry[,4],tau=partial_entry[,5],
                                recency= if(dim(partial_entry)[2]>=6 && fitted_m%in%c('recency_exp','full_exp')) partial_entry[,6] else NA,
                                surprise_plus= ifelse(dim(partial_entry)[2]>=6 && fitted_m %in% c('surprise+_exp','surprise+_exp_movavg'), 
                                                      partial_entry[,6], ifelse(dim(partial_entry)[2]>=6 && fitted_m %in% c('full_exp'),partial_entry[,7],NA)),
                                asymmetry= ifelse( fitted_m %in% c('surprise+_exp','surprise+_exp_movavg','full_exp') ,ifelse(dim(partial_entry)[2]==7, partial_entry[,7],partial_entry[,8]), NA)))
        
      }
    }
    
  }
  
  # ###############################################################################
  # ############################ Code for identifying which combs to rerun ########
  # ###############################################################################
  d_temp <- d %>%
    # filter(fitted_model =='null') %>%
    select(participant,input_model,fitted_model,nLL)
  
  incomplete_runs <- data.frame(row.names = c('input_model','fitted_model','participant'))
  for(p in unique(d_temp$participant)){
    for(in_m in unique(d_temp$input_model)){
      inner_temp <- d_temp %>% filter(participant==p) %>% filter(input_model==in_m)
      for(f_m in unique(d_temp$fitted_model)){
        inner_inner_temp <- inner_temp %>% filter(fitted_model==f_m)
        rows_cv <- inner_inner_temp %>% count(nLL) %>% summarize(cv_rounds=sum(n)) %>% as.integer()
        
        if(rows_cv<7){
          incomplete_run <- unique(data.frame(input_model=in_m,fitted_model=f_m, participant=p))
          # print(incomplete_run)
          incomplete_runs <- rbind(incomplete_runs,incomplete_run) #no fitted_model=inner_temp$fitted_model, since we just do this for null here
        }
      }
    }
  }
  # d_test <- incomplete_runs %>% filter(fitted_model!='full')
  # d_test <- d_temp %>% filter(fitted_model=='full',participant==186,input_model=='Memory')
  
  d_test <- d_temp %>% filter(participant %in% unique(incomplete_runs$participant))
  
  d_test_rounds <- d_test %>% group_by(participant, input_model, fitted_model) %>% count(nLL) %>% summarize(cv_rounds=sum(n))
  minimum_rounds <- min(d_test_rounds$cv_rounds)
  
  d_reduced_cvrounds <- data.frame()
  for(p in unique(d$participant)){
    for(in_m in unique(d$input_model)){
      for(f_m in unique(d$fitted_model)){
        d_temp_cvrounds <- d %>% filter(participant==p) %>% filter(input_model==in_m) %>% filter(fitted_model==f_m) %>% head(minimum_rounds)
        
        d_reduced_cvrounds <- rbind(d_reduced_cvrounds, d_temp_cvrounds)
      }
    }
  }
  d_fitting_all <- d
  d_fitting_reduced_cvrounds <-d_reduced_cvrounds
  
  
  # d_test_exp <-load_exp_data(includeLast = TRUE)
  d_raw_exp<- dataImport()
  
  unique_UIDs <- unique(d_raw_exp$UID)
  
  
  d_raw_exp <- d_raw_exp %>% 
    mutate(experiment_round=round) 
  
  d_ID_mapping <- d_raw_exp %>% 
    group_by(UID) %>% 
    summarise(participant=NA)
  
  d_ID_mapping$participant <- seq.int(nrow(d_ID_mapping)) 
  
  d_exp <- merge(d_raw_exp,d_ID_mapping)
  
  
  
  # ###############################################################################
  # ############################ Combine data into one df #########################
  # ###############################################################################
  
  d_fitting <- merge(d_fitting_all,d_ID_mapping) %>% 
    mutate(id=participant)
  
  d_demographics_exp <- d_raw_exp %>% dplyr::select(UID,age_bin,education)
  d_demographics_exp <- unique(d_demographics_exp)
  
  
  d_fitting <- merge(d_fitting,d_demographics_exp)
  d_fitting$age_bin = factor(d_fitting$age_bin,labels = c("18-27","28-37","38-47","48-57","58-67","68-77"))
  
  
  write.table(d_fitting, file=paste0(BASE_PATH_DATA,'fitting_data_final.csv'),row.names = FALSE,col.names = TRUE,sep = ',')
  write.table(d_exp, file=paste0(BASE_PATH_DATA,'experiment_data_extended.csv'),row.names = FALSE,col.names = TRUE,sep = ',')
}

save_bonus_data <- function(){
  l_bonus_both<- dataImport(bonusRound = TRUE)
  write.table(l_bonus_both$bonus_judegements, file=paste0(BASE_PATH_DATA,'bonus_data_judgements','.csv'),row.names = FALSE,col.names = TRUE,sep = ',')
  write.table(l_bonus_both$bonus_history, file=paste0(BASE_PATH_DATA,'bonus_data_history','.csv'),row.names = FALSE,col.names = TRUE,sep = ',')
}

load_bonus_judgements <- function(){
  d_bonus<- read.csv(paste0(BASE_PATH_DATA,'bonus_data_judgements','.csv'))
  return(d_bonus)
}

load_bonus_history <- function(inclAFC=T){
  d_bonus<- read.csv(paste0(BASE_PATH_DATA,'bonus_data_history','.csv'))
  if(inclAFC){
    d_bonus <- d_bonus %>% filter(trial<=14)
  }else{
    d_bonus <- d_bonus %>% filter(trial<=13)
  }
  return(d_bonus)
}

loadSimulatedData <- function(modelname=NA, purpose='recovery'){
  modellist <- c('null','recency','surprise+','full')
  d_simulated<- data.frame()
  if(purpose=='recovery'){
    PURPOSE_PATH <-'EXPERIMENT_VALUES_RECOVERY'
  }
  if(purpose=='validation'){
    PURPOSE_PATH <-'EXPERIMENT_VALUES_VALIDATION'
  }
  if(is.na(modelname)){
    for(m in modellist){
      d_temp<- read.csv(paste0(SIMULATED_DATA_PATH,PURPOSE_PATH,'/simulatedData_',m,'.csv'))
      d_temp <- d_temp %>% 
        select(-X) %>% 
        mutate(z=(z-50)/100) %>% 
        mutate(prev_z=(prev_z-50)/100) %>% 
        mutate(model=m)
      
      d_simulated<-rbind(d_simulated,d_temp)
    }
  }else{
    d_simulated<- read.csv(paste0(SIMULATED_DATA_PATH,PURPOSE_PATH,'/simulatedData_',m,'.csv'))
    d_simulated <- d_simulated %>% 
      select(-X) %>% 
      mutate(z=(z-50)/100) %>% 
      mutate(prev_z=(prev_z-50)/100) %>% 
      mutate(model=modelname)
  }
  
  return(d_simulated)
}


dataImport2018 <- function(normalize=TRUE,bonusRound=FALSE,denormalize=FALSE,apply_agefactor=TRUE,apply_edufactor=TRUE){
  
  #read in data
  
  d_columns= c("UID","scenario","kernel","horizon","scale","envOrder","searchHistory") 
  d_raw = read.csv(DATA_PATH_2018,sep=",",header = TRUE)
  d_raw$UID=1:nrow(d_raw)
  
  
  dat <- d_raw %>% 
    filter(scenario==0) %>% #cumulative reward
    filter(kernel==1) #lenght-scale 2
  
  #dummy data frame
  data<-data.frame(UID=numeric(), trial=numeric(), x=numeric(), y=numeric(), 
                   z=numeric(),  kernel=numeric(), scenario=numeric(), 
                   round=numeric(), env=numeric(), delta_x=numeric(), 
                   maxFound=numeric())
  
  for(col in 1:ncol(dat)){
    dat[,col]<-as.character(dat[,col])
  }
  
  
  #Compile experiment  data
  for (i in 1:nrow(dat)){
    #parse JSON
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'xcollect:','"xcollect":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'ycollect:','"ycollect":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'zcollect:','"zcollect":')
    dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'zcollectScaled:','"zcollectScaled":')
    
    # dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'bonusCollect:','"bonusCollect":')
    # dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'bonusSelectionCollect:','"bonusSelectionCollect":')
    # dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'x:','"x":')
    # dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'y:','"y":')
    # dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'z:','"z":')
    # dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'est:','"est":')
    # dat$searchHistory[i]<-str_replace_all(dat$searchHistory[i],'conf:','"conf":')
    
    searchHistory <- fromJSON(as.character(dat$searchHistory[i])) # default is ,flatten=FALSE
    
    #sampled x value
    x<-unlist(searchHistory$xcollect)
    #sampled y value
    y<-unlist(searchHistory$ycollect)
    #sampled z value (UNSCALED!)
    z<-unlist(searchHistory$zcollect)
    z_scaled<-unlist(searchHistory$zcollectScaled)
    
    
    x_new <- data.frame()
    y_new <- data.frame()
    z_new <- data.frame()
    
    for(r in 1:8){
      if(dat$horizon[i]==0){
        #20,40,20,40,...
        #1,3,5,7
        if(r%%2!=0){
          temp_x <- c(x[(((as.integer((r-1)/2))*62)+1):(((as.integer((r-1)/2))*62)+21)],rep(NA,20))
          temp_y <- c(y[(((as.integer((r-1)/2))*62)+1):(((as.integer((r-1)/2))*62)+21)],rep(NA,20))
          temp_z <- c(z[(((as.integer((r-1)/2))*62)+1):(((as.integer((r-1)/2))*62)+21)],rep(NA,20))
        }
        #2,4,6,8
        if(r%%2==0){
          temp_x <- x[(((as.integer((r-1)/2))*62)+22):(((as.integer((r-1)/2))*62)+62)]
          temp_y <- y[(((as.integer((r-1)/2))*62)+22):(((as.integer((r-1)/2))*62)+62)]
          temp_z <- z[(((as.integer((r-1)/2))*62)+22):(((as.integer((r-1)/2))*62)+62)]
        }
        x_new<-rbind(x_new,temp_x)
        y_new<-rbind(y_new,temp_y)
        z_new<-rbind(z_new,temp_z)
      }else if(dat$horizon[i]==1){
        #40,20,40,20,...
        #1,3,5,7
        if(r%%2==0){
          temp_x <- c(x[(((as.integer((r-1)/2))*62)+1):(((as.integer((r-1)/2))*62)+21)],rep(NA,20))
          temp_y <- c(y[(((as.integer((r-1)/2))*62)+1):(((as.integer((r-1)/2))*62)+21)],rep(NA,20))
          temp_z <- c(z[(((as.integer((r-1)/2))*62)+1):(((as.integer((r-1)/2))*62)+21)],rep(NA,20))
        }
        #2,4,6,8
        if(r%%2!=0){
          temp_x <- x[(((as.integer((r-1)/2))*62)+22):(((as.integer((r-1)/2))*62)+62)]
          temp_y <- y[(((as.integer((r-1)/2))*62)+22):(((as.integer((r-1)/2))*62)+62)]
          temp_z <- z[(((as.integer((r-1)/2))*62)+22):(((as.integer((r-1)/2))*62)+62)]
        }
        x_new<-rbind(x_new,temp_x)
        y_new<-rbind(y_new,temp_y)
        z_new<-rbind(z_new,temp_z)
      }
    }
    x<-as.matrix(x_new)
    y<-as.matrix(y_new)
    z<-as.matrix(z_new)
    
    zmax <- maxton(z[1,],na.rm = TRUE)
    zmax<-t(apply(z,1, maxton,na.rm=TRUE))
    if (normalize==TRUE){
      #normalize z and zmax
      z <- (z-50)/100
      zmax <- (zmax-50)/100
    }
    # length of round
    len<-40+1
    trial<-rep(1:len,8)
    round <- c()
    for(r in 1:8){
      round <- rbind(round,rep(r,len))
    }
    #env number
    env_vec <-fromJSON(as.character(dat$envOrder[i]))
    env <- c()
    for(r in 1:len){
      env <- rbind(env,env_vec)
    } 
    
    #smoothness: 0 = rough (length scale = 1); 1 = smooth (length scale = 2)
    kernel<-1
    #UID number for each subject
    UID<-c()
    for(r in 1:8){
      UID<-rbind(UID,rep(dat$UID[i], len))
    }
    
    
    # env <- t(env)
    round <- t(round)
    scenario <- 0
    x <- t(x)
    y <- t(y)
    z <- t(z)
    zmax <- t(zmax)
    
    #dummy frame
    dim(UID)<-c(8*len,1)
    dim(trial)<-c(8*len,1)
    dim(x)<-c(8*len,1)
    dim(y)<-c(8*len,1)
    dim(z)<-c(8*len,1)
    dim(zmax)<-c(8*len,1)
    # dim(scenario)<-c(8*len,1)
    dim(round)<-c(8*len,1)
    dim(env)<-c(8*len,1)
    
    
    dummy<-data.frame(UID, trial, x, y, z,zmax, kernel, scenario, round, env)
    
    #calculate manhattan distance between clicks
    dummy <- dummy %>%
      group_by(round) %>%
      mutate(delta_x = abs(x - lag(x, default = NA)) + abs(y - lag(y, default = NA)) ) 
    dummy$delta_x[dummy$trial==1]<-NA #set as NA for all first clicks, since it was randomly selected
    
    #bind them together
    dummy <- as.data.frame(dummy)
    data<-rbind(data, dummy)
    
  }
  data <- data %>% filter(!is.na(x))
  
  #create a unique listing for each space in the grid
  allopts<-expand.grid(0:(GRIDSIZE-1), 0:(GRIDSIZE-1))
  data$chosen<- -99
  for (i in 1:nrow(data)){ #loop through data and assign the unique listing from allopts to data$chosen based on the chosen x and y values
    data$chosen[i]<-which(data$x[i]==allopts$Var1 & data$y[i]==allopts$Var2)
  }
  data$round <- factor(data$round) #convert round into factor
  #Rename variables
  data$kernel<-ifelse(data$kernel==0, "Rough", "Smooth")
  
  #If scenario ==0, means cumulative reward in always visible condition
  data$scenario<-ifelse(data$scenario==0, "Baseline Condition")
  
  data <- data %>% 
    mutate(step=trial) %>% 
    mutate(distance=delta_x) %>% 
    mutate(prev_z=lag(z, default = NA)) %>%
    mutate(id=UID)
  
  if(denormalize){
    data <- data %>% 
      mutate(z=z+0.5)
  }
  
  
  data$scenario = factor(data$scenario)
  
  return(data)
}


load_corrected_fitting_data_and_save_to_csvs <- function(){
  ################################################################################################
  # Data import
  ################################################################################################
  models <- c("recency_exp","surprise+_exp","surprise+_exp_movavg","full_exp","null_exp","surprise+_exp_posterior","full_exp_posterior")#,"null" # exclude null model from analysis since some of them stopped
  exp_models <- c("Memory", "NoMemory") #
  model_descriptions_sim <-c("GP + R","GP + S+","GP + RS","GP")#
  model_descriptions_exp <-c("Memory","Baseline")
  
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
  
  d<-data.frame()
  d <- data.frame(row.names = c('input_model','fitted_model','participant','nLL','lambda','beta','tau','recency','surprise_plus','asymmetry'))
  
  missing_files <- data.frame(row.names = c('input_model','fitted_model','participant'))
  
  for(input_m in exp_models){
    for(fitted_m in models){
      for(p in 1:NUM_PARTICIPANTS){
        partial_entry <- data.frame()
        prev_length <- NA
        if(fitted_m!='null'){
          fitting_output_file <- paste0('fitting_results/output/',RAW_DATA,'/experiment_data_',input_m,'/fittedmodel_',fitted_m,'/part_',p,'_init_1.csv')
          if(!file.exists(fitting_output_file)) {
            fitting_output_file <- paste0('fitting_results/output/',RAW_DATA,'/experiment_data_',input_m,'/fittedmodel_',fitted_m,'/part_',p,'.csv')
          }
          
          if(!file.exists(fitting_output_file)) {
            missing_files <- rbind(missing_files, data.frame(input_model=input_m,fitted_model=fitted_m,participant=p))
            print(paste0('Missing participant ',p,' with model ',fitted_m,' ,condition ',input_m))
            next
          }
          if(is.na(prev_length)){
            partial_entry <- read.csv(fitting_output_file)
            prev_length <- nrow(partial_entry)
          }else{
            # if(prev_length<7)
            # {
            #   print('debugging')
            # }
            temp <- read.csv(fitting_output_file)
            partial_entry<- rbind(partial_entry,tail(temp,(7-prev_length)))
          }
          
          
          d <- rbind(d,data.frame(input_model=input_m,fitted_model=fitted_m,participant=p,
                                  nLL=partial_entry[,2],lambda=partial_entry[,3],beta=partial_entry[,4],tau=partial_entry[,5],
                                  recency= if(dim(partial_entry)[2]>=6 && fitted_m%in%c('recency_exp','full_exp','full_exp_posterior')) partial_entry[,6] else NA,
                                  surprise_plus= ifelse(dim(partial_entry)[2]>=6 && fitted_m %in% c('surprise+_exp','surprise+_exp_movavg','surprise+_exp_posterior'), 
                                                        partial_entry[,6], ifelse(dim(partial_entry)[2]>=6 && fitted_m %in% c('full_exp','full_exp_posterior'),partial_entry[,7],NA)),
                                  asymmetry= ifelse( fitted_m %in% c('surprise+_exp','surprise+_exp_movavg','surprise+_exp_posterior','full_exp','full_exp_posterior') ,ifelse(dim(partial_entry)[2]==7, partial_entry[,7],partial_entry[,8]), NA)))
          
        }else{
          if(p==266 || p==270){
            print('debugging')
          }
          for(part in c(1,2)){
            fitting_output_file <- paste0('fitting_results/output/',RAW_DATA,'/experiment_data_',input_m,'/fittedmodel_',fitted_m,'/',part,':2/part_',p,'_init_1.csv')
            if(!file.exists(fitting_output_file)) {
              fitting_output_file <- paste0('fitting_results/output/',RAW_DATA,'/experiment_data_',input_m,'/fittedmodel_',fitted_m,'/',part,':2/part_',p,'.csv')
            }
            
            if(!file.exists(fitting_output_file)) {
              next
            }
            if(is.na(prev_length)){
              partial_entry <- read.csv(fitting_output_file)
              prev_length <- nrow(partial_entry)
            }else{
              # if(prev_length<7)
              # {
              #   print('debugging')
              # }
              temp <- read.csv(fitting_output_file)
              partial_entry<- rbind(partial_entry,tail(temp,(7-prev_length)))
            }
          }
          if(dim(partial_entry)==dim(data.frame())){
            print('Empty dataframe')
            next
          }
          d <- rbind(d,data.frame(input_model=input_m,fitted_model=fitted_m,participant=p,
                                  nLL=partial_entry[,2],lambda=partial_entry[,3],beta=partial_entry[,4],tau=partial_entry[,5],
                                  recency= if(dim(partial_entry)[2]>=6 && fitted_m%in%c('recency_exp','full_exp')) partial_entry[,6] else NA,
                                  surprise_plus= ifelse(dim(partial_entry)[2]>=6 && fitted_m %in% c('surprise+_exp','surprise+_exp_movavg'), 
                                                        partial_entry[,6], ifelse(dim(partial_entry)[2]>=6 && fitted_m %in% c('full_exp'),partial_entry[,7],NA)),
                                  asymmetry= ifelse( fitted_m %in% c('surprise+_exp','surprise+_exp_movavg','full_exp') ,ifelse(dim(partial_entry)[2]==7, partial_entry[,7],partial_entry[,8]), NA)))
          
        }
        
      }
    }
    
  }
  d_raw_exp<- dataImport()
  
  unique_UIDs <- unique(d_raw_exp$UID)
  
  
  d_raw_exp <- d_raw_exp %>% 
    mutate(experiment_round=round) 
  
  d_ID_mapping <- d_raw_exp %>% 
    group_by(UID) %>% 
    summarise(participant=NA)
  
  d_ID_mapping$participant <- seq.int(nrow(d_ID_mapping)) 
  
  d_exp <- merge(d_raw_exp,d_ID_mapping)
  
  
  
  # ###############################################################################
  # ############################ Combine data into one df #########################
  # ###############################################################################
  
  d_fitting <- merge(d,d_ID_mapping) %>% 
    mutate(id=participant)
  
  d_demographics_exp <- d_raw_exp %>% dplyr::select(UID,age_bin,education)
  d_demographics_exp <- unique(d_demographics_exp)
  
  
  d_fitting <- merge(d_fitting,d_demographics_exp)
  d_fitting$age_bin = factor(d_fitting$age_bin,labels = c("18-27","28-37","38-47","48-57","58-67","68-77"))
  
  
  write.table(d_fitting, file=paste0(BASE_PATH_DATA,'fitting_data_final.csv'),row.names = FALSE,col.names = TRUE,sep = ',')
}

save_GP_preds<-function(){
  d_hist <- load_bonus_history()
  BONUS_CHOICE_TRIAL <- 14
  
  df_GP_preds<-data.frame()
  
  modelList <- c('null','recency','surprise','full') 
  
  
  d_fitting <- load_fitting_data()
  d_params <- d_fitting %>% 
    filter(input_model=='Memory') %>% 
    group_by(fitted_model) %>% 
    summarise(medianL=median(lambda),
              medianB=median(beta),
              medianT=median(tau),
              medianR=median(recency),
              medianS=median(surprise_plus),
              medianA=median(asymmetry)) %>% 
    replace(is.na(.), 0) %>% 
    mutate(fitted_model=ifelse(fitted_model=='surprise+','surprise',fitted_model)) %>% 
    filter(fitted_model %in% modelList)
  
  
  DEFAULT_ERR_VAR=0.0001
  GRIDSIZE=11
  NUM_CLICKS=25
  
  for(id in unique(d_hist$UID)){
    d_hist_personal <- d_hist %>% filter(UID==id)
    
    chosen <- d_hist_personal$chosen[2:length(d_hist_personal$chosen)]
    y  <- d_hist_personal$z[1:(BONUS_CHOICE_TRIAL-1)] #trim off the last observation since we want to estimate this
    x1 <- d_hist_personal$x[1:(BONUS_CHOICE_TRIAL-1)]
    x2 <- d_hist_personal$y[1:(BONUS_CHOICE_TRIAL-1)]
    #create observation matrix
    X<-as.matrix(cbind(x1,x2))
    
    Xnew<-as.matrix(expand.grid(0:(GRIDSIZE-1),0:(GRIDSIZE-1)))
    
    
    
    for(m in modelList){
      d_model_params <- d_params %>% filter(fitted_model==m) %>% mutate(across(where(is.numeric), round, 4))
      
      lambda <- d_model_params %>% distinct(medianL) %>% as.numeric() %>% exp()
      beta <- d_model_params %>% distinct(medianB) %>% as.numeric() %>% exp()
      tau <- d_model_params %>% distinct(medianT) %>% as.numeric() %>% exp()
      
      recency <- d_model_params %>% distinct(medianR) %>% as.numeric()
      surprise <- d_model_params %>% distinct(medianS) %>% as.numeric()
      asymmetry <- d_model_params %>% distinct(medianA) %>% as.numeric() %>% exp()
      
      errVar_vec <-c(recency,surprise,asymmetry)
      
      #Utilties of each choice
      utilities <- NULL
      posterior_stack <- NA
      parVec_gpr <- c(lambda, lambda, 1,DEFAULT_ERR_VAR)
      
      
      # Predict next choice and estimates after trial 13 (12 choices)
      bonustrial=13
      for(trial in c(bonustrial-1,bonustrial)){
        #new observation
        X_t<-matrix((X[1:trial,]), ncol=2)
        y1<-matrix((y[1:trial]))
        #Which posterior function to use
        if (m=="null"){# Default GP #TODO: What to fit here?
          out <- gpr(X.test=Xnew, theta=parVec_gpr, X=X_t, y=y1, k=rbf) 
        }else {# Heteroscedastic GP - all memory models
          errVar <- gp_error_variance_exponential(obs=data.frame(x1=X_t[,1],x2=X_t[,2],y=y1+.5),theta=errVar_vec,clicks=NUM_CLICKS,prior_mean=.5,default_noise=DEFAULT_ERR_VAR,exponential_asymmetry=T,surprise_model=2,prev_posterior=posterior_stack[chosen[1:trial]])
          errVar_index <-1:length(errVar) + 3
          parVec_gpr <- replace(parVec_gpr, errVar_index, errVar)
          
          out <- gpr(X.test=Xnew, theta=parVec_gpr, X=X_t, y=y1, k=rbf) 
          
          posterior_stack <- out$mu
        }
        
        #UCB function
        utilityVec<-ucb(out, c(beta))
        utilityVec <- utilityVec - max(utilityVec) #avoid overflow
        
        # print(paste0(x1,',',x2,',',posterior_stack))
        #Softmax rule
        p <- exp(utilityVec/tau)
        #avoid NaN calculation when doing rowsums
        p <- (pmax(p, 0.00001))
        p <- p/colSums(p)
        #avoid underflow by setting a floor and a ceiling
        p <- (pmax(p, 0.00001))
        p <- (pmin(p, 0.99999))
        
        
      }
      
      df_GP_config <- data.frame(UID=unique(d_hist_personal$UID),scenario=unique(d_hist_personal$scenario),env=unique(d_hist_personal$env),model=m)
      df_GP_out <- data.frame(UID=unique(d_hist_personal$UID),x=Xnew[,1],y=Xnew[,2],est=out$mu,conf=out$sig,prob=(p),z=NA)
      
      # Get the actual choice from AFC task
      temp_chosen <- chosen[BONUS_CHOICE_TRIAL-1] # -1 since first choice was removed
      xy_loc <- Xnew[temp_chosen,]
      
      # Filter d_hist_personal for chosen location and get z value
      z_chosen <- d_hist_personal %>% 
        filter(x==xy_loc[1] & y==xy_loc[2]) %>% 
        distinct(z) %>% as.numeric()
      # Add z value for only this location (see Xnew for row to add not NA)
      df_GP_out <- df_GP_out %>% 
        mutate(z= ifelse(x==xy_loc[1] & y==xy_loc[2],z_chosen,NA))
      
      df_GP_personal <- merge(df_GP_config,df_GP_out,by='UID')
      
      df_GP_preds<- rbind(df_GP_preds,df_GP_personal)
      
      
    }
  }
  
  
  write.table(df_GP_preds, file=paste0(BASE_PATH_DATA,'bonus_GP_preds.csv'),row.names = FALSE,col.names = TRUE,sep = ',')
  
}

load_GP_preds <- function(){
  d <- read.csv( file=paste0(BASE_PATH_DATA,'bonus_GP_preds.csv'))
  return(d)
}

save_bonus_preds_combined <- function(){
  d_GP_preds <- load_GP_preds()
  d_H_preds <- load_bonus_judgements()
  
  df_preds_combined <- data.frame()
  for(id in unique(d_H_preds$UID)){
    
    d_GP_temp <-d_GP_preds %>% filter(UID==id) 
    d_H_temp <- d_H_preds %>% filter(UID==id)
    
    for(r in 1:nrow(d_H_temp)){
      x_filter <- d_H_temp[r,]$x
      y_filter <- d_H_temp[r,]$y
      
      gp_est <- d_GP_temp %>% filter(x==x_filter&y==y_filter) %>% select(model,est,conf,prob,z)
      h_est <- d_H_temp %>% filter(x==x_filter&y==y_filter) %>% select(scenario,env,est,conf)
      df_comb_row <- data.frame(UID=id,scenario=h_est$scenario,env=h_est$env, gp_model=gp_est$model,x=x_filter,y=y_filter,z=gp_est$z,gp_est=gp_est$est,gp_conf=gp_est$conf,gp_prob=gp_est$prob,h_est=h_est$est,h_conf=h_est$conf)
      df_preds_combined <- rbind(df_preds_combined,df_comb_row)
    }
  }
  df_preds_combined<- df_preds_combined %>% mutate(env=env+1)
  d_envs <- load_env_data()
  
  dbonus_ext <- data.frame()
  for(id in unique(df_preds_combined$UID)){
    d_bonus_personal <- df_preds_combined %>% filter(UID==id)
    for(r in 1:nrow(d_bonus_personal)){
      
      d_env_row <- d_envs %>% 
        filter(env==unique(d_bonus_personal$env)) %>% 
        filter(x==d_bonus_personal[r,]$x) %>% 
        filter(y==d_bonus_personal[r,]$y) 
      d_bonus_personal_row <- d_bonus_personal[r,] %>% 
        mutate(z_obs=z) %>% 
        select(-z)
      
      d_actual_row <- merge(d_env_row,d_bonus_personal_row,by=c('env','x','y'))
      dbonus_ext <- rbind(dbonus_ext,d_actual_row )
    }
  }
  write.table(dbonus_ext, file=paste0(BASE_PATH_DATA,'bonus_preds_combined.csv'),row.names = FALSE,col.names = TRUE,sep = ',')
  
}
load_bonus_preds_combined <- function(){
  d <- read.csv( file=paste0(BASE_PATH_DATA,'bonus_preds_combined.csv'))
  return(d)
}



save_env_data <- function(){
  env_list <- read_json('data/env_generation/kernelSmooth_11x11.json')
  df_envs <- data.frame()
  for(r in 1:length(env_list)){
    env <- env_list[[r]]
    for(r_e in 1:length(env)){
      row <- env[[r_e]]
      df_envs <- rbind(df_envs,data.frame(env=r,x=row$x1,y=row$x2,z=row$y))
    }
  }
  
  write.table(df_envs, file=paste0(BASE_PATH_DATA,'env_df.csv'),row.names = FALSE,col.names = TRUE,sep = ',')
}
load_env_data <- function(){
  d <- read.csv( file=paste0(BASE_PATH_DATA,'env_df.csv'))
  return(d)
}

# save_bonus_preds_combined()

load_recovery_data_and_save_to_csvs <- function(){
  ################################################################################################
  # Data import
  ################################################################################################
  models <- c("recency","surprise+","full","null")
  exp_models <- c("recency","surprise+","full","null")
  model_descriptions_sim <-c("GP + R","GP + S+","GP + RS","GP")#
  model_descriptions_exp <-c("Memory")
  
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
  
  d<-data.frame()
  d <- data.frame(row.names = c('input_model','fitted_model','participant','nLL','lambda','beta','tau','recency','surprise_plus','asymmetry'))
  
  missing_files <- data.frame(row.names = c('input_model','fitted_model','participant'))
  
  for(input_m in exp_models){
    for(fitted_m in models){
      for(p in 1:30){
        partial_entry <- data.frame()
        prev_length <- NA
          
        fitting_output_file <- paste0('fitting_results/output/','model_recovery','/simulatedData_',input_m,'/fittedmodel_',fitted_m,'/part_',p,'.csv')
        print(fitting_output_file)
        if(!file.exists(fitting_output_file)) {
          missing_files <- rbind(missing_files, data.frame(input_model=input_m,fitted_model=fitted_m,participant=p))
          print(paste0('Missing participant ',p,' with model ',fitted_m,' ,condition ',input_m))
          next
        }
        partial_entry <- read.csv(fitting_output_file)
        
        
        d <- rbind(d,data.frame(input_model=input_m,fitted_model=fitted_m,participant=p,
                                nLL=partial_entry[,2],lambda=partial_entry[,3],beta=partial_entry[,4],tau=partial_entry[,5],
                                recency= if(dim(partial_entry)[2]>=6 && fitted_m%in%c('recency','full')) partial_entry[,6] else NA,
                                surprise_plus= ifelse(dim(partial_entry)[2]>=6 && fitted_m %in% c('surprise+'), 
                                                      partial_entry[,6], ifelse(dim(partial_entry)[2]>=6 && fitted_m %in% c('full'),partial_entry[,7],NA)),
                                asymmetry= ifelse( fitted_m %in% c('surprise+','full') ,ifelse(dim(partial_entry)[2]==7, partial_entry[,7],partial_entry[,8]), NA)))
        
      }
    }
  }
  
  
  write.table(d, file=paste0(BASE_PATH_DATA,'recovery_data_final.csv'),row.names = FALSE,col.names = TRUE,sep = ',')
}

load_recovery_data <- function(){
  return(read.csv( file=paste0(BASE_PATH_DATA,'recovery_data_final.csv')))
}