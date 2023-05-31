# Calculate the experiment costs automatically


# house keeping
rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr','jsonlite','tidyverse')
#invisible(lapply(packages, install.packages, character.only = TRUE))
invisible(lapply(packages, require, character.only = TRUE)) #loads packages

homePath= paste0(getwd(),"/")


source('data_munging.R')

AGE_BINS <- 1:6
BASE_FEE <- 2.25

BASE_BONUS_PATH='~/Documents/ML-Nextcloud/experiment-output/pilot_2/bonus'
BONUS_PATH=paste0(BASE_BONUS_PATH,"/experiment_bonuspayments.csv")

d_columns= c("PID","bonus") 
d_raw = read.csv(BONUS_PATH,sep=",",header = FALSE)
colnames(d_raw) = d_columns

paid_participants <- d_raw %>% filter(bonus>0)
base_payment <- nrow(paid_participants)*BASE_FEE
bonus_payment <- paid_participants %>% 
  select(bonus) %>% 
  summarise_all(sum) 
bonus_payment <-   as.numeric(bonus_payment)
  
print(paste0("Base Payments: ",base_payment))
print(paste0("Bonus Payments: ",bonus_payment))


low_bonus_participants <- paid_participants %>% filter(bonus < 0.9)
print(paste0("Low Bonus: ",low_bonus_participants))

generate_bulk_bonus_files <- function(){
  
  d_demographics <- get_demographics() %>% 
    filter(Completion.code != '') %>% 
    mutate(PID=Participant.id)
  
  df_paid_x_demographics <- inner_join(paid_participants,d_demographics,by='PID') %>% 
    mutate(age_bin=NA)
  
  age_limits <- c(18,28,38,48,58,68,78)
  for(i in 1:nrow(df_paid_x_demographics)){
    test_row <- df_paid_x_demographics[i,]
    test_age <- (as.character(test_row$Age))
    test_age <- as.numeric(test_age)
    for(bin in 1:(length(age_limits)-1)){
      if(between(test_age,age_limits[bin],age_limits[bin+1]-1)){
        test_row$age_bin <-bin
      }
    }
    df_paid_x_demographics[i,] <- test_row
  }
  
  for(age in AGE_BINS){
    
    df_bonus_age <- df_paid_x_demographics %>% 
      filter(age_bin==age) %>% 
      select(PID,bonus)

    write.table(df_bonus_age, file=paste0(BASE_BONUS_PATH,'/bonus_',age,'o6.csv'),sep=',',col.names = FALSE,row.names = FALSE, quote = FALSE )
  }
  
}

# generate_bulk_bonus_files()
