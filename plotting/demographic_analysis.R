rm(list=ls())

packages <- c('plyr', 'dplyr', 'tidyr', 'ggplot2', 'viridis' , 'brms', 'lme4', 'sjPlot', 'cowplot','tidybayes','modelr','lmerTest','cowplot','xtable') #'cowplot',  'entropy', 'withr', 'ggbeeswarm','jsonlite'
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

COLOR_MODEL_BASIC = cbPalette[1]
COLOR_MODEL_RECENCY = cbPalette[2]
COLOR_MODEL_SURPRISE = cbPalette[3]
COLOR_MODEL_SURPRISE_PLUS = cbPalette[4]
countryPalette <- c('#1b9e77','#d95f02','#7570b3')

KERNEL_8= "env_generation/smoothKernel_8x8.json"
KERNEL_11= "env_generation/smoothKernel_11x11.json"

PLOT_PATH="plots/plots_demographics/"

d_demo <- load_anonymous_demographics()

# Country of residence is strongly correlated with age (most retirees are UK, but also correlated when excluding this)
residence <- d_demo %>% distinct(Country.of.residence)
d_residence <- data.frame(id_residence=seq(1,nrow(residence)),Country.of.residence=residence$Country.of.residence)
d_demo <- merge(d_demo, d_residence)
corTestPretty(d_demo$id_residence,d_demo$age)

# d_res <- ddply(d_demo, ~Country.of.residence, plyr::summarize, meanAge = mean(age),sdAge=) 
d_demo_count <- d_demo %>% group_by(Country.of.residence) %>% count %>% filter(n>0)
d_demo_no_outliers <- d_demo %>% filter(Country.of.residence %in% unique(d_demo_count$Country.of.residence)) 

d_demo_simplified <- d_demo_no_outliers %>% 
  mutate(Country.of.residence=ifelse((!Country.of.residence %in% c('South Africa','United Kingdom')),'Rest/Europe',
                                     ifelse(Country.of.residence=='South Africa','South Africa',
                                            ifelse(Country.of.residence=='United Kingdom','United Kingdom',NA))))
  
# pResidency <- ggplot(d_demo_simplified,mapping = aes(x=Country.of.residence,y=age))+
#   geom_boxplot()
# pResidency
# corTestPretty(d_demo_simplified$id_residence,d_demo_simplified$age)
# ggsave(filename=paste0(PLOT_PATH,'age_x_country.pdf'),plot=pResidency,height=4.5, width=8, units = "in")
# 
# 
# 
# 
# 
# # Employment status is strongly correlated with age (Young are mostly mixed, middle are mostly full-time, old are mostly retired)
# employment <- d_demo_simplified %>% distinct(Employment.status)
# d_employment <- data.frame(id_employment=seq(1,nrow(employment)),Employment.status=employment$Employment.status)
# d_employment <- d_employment %>% filter(!id_employment %in% c(1,6,7))
# d_demo_employmentinc <- merge(d_demo_simplified, d_employment)
# corTestPretty(d_demo_employmentinc$id_employment,d_demo_employmentinc$age)
# 
# pEmployment <- ggplot(d_demo_employmentinc,mapping = aes(x=Employment.status,y=age))+
#   geom_boxplot()
# pEmployment
# # ggsave(filename=paste0(PLOT_PATH,'age_x_employment.pdf'),plot=pEmployment,height=4.5, width=8, units = "in")
# 
# 
# pEmployment_x_Age_x_Country <- ggplot(d_demo_employmentinc,mapping = aes(x=Country.of.residence,y=age,color=Employment.status,fill=Employment.status))+
#   geom_boxplot()+
#   scale_color_viridis(discrete = T)+
#   scale_fill_viridis(discrete = T)
# pEmployment_x_Age_x_Country
# ggsave(filename=paste0(PLOT_PATH,'age_x_employment_x_country.pdf'),plot=pEmployment_x_Age_x_Country,height=4.5, width=8, units = "in")
# 
# pEmployment_x_Count_x_Country <- ggplot(d_demo_employmentinc,mapping = aes(x=Country.of.residence,color=Employment.status,fill=Employment.status))+
#   geom_bar()+
#   scale_color_viridis(discrete = T)+
#   scale_fill_viridis(discrete = T)
# pEmployment_x_Count_x_Country
# # ggsave(filename=paste0(PLOT_PATH,'count_x_employment_x_country.pdf'),plot=pEmployment_x_Count_x_Country,height=4.5, width=8, units = "in")
# 
# 
# 
# # No correlation between education and age
# education <- d_demo_simplified %>% distinct(Highest.education.level.completed)
# d_education <- data.frame(id_education=seq(1,nrow(education)),Highest.education.level.completed=education$Highest.education.level.completed)
# d_education <- d_education %>% 
#   mutate(id_education=ifelse(id_education==6,1,
#                              ifelse(id_education==3,1,
#                                     ifelse(id_education==4,2,
#                                            ifelse(id_education==1,3,
#                                                   ifelse(id_education==5,4,id_education))))))
# d_demo_educationinc<- merge(d_demo_simplified, d_education)
# corTestPretty(d_demo_educationinc$id_education,d_demo_educationinc$age)
# 
# 
# pEdu_x_Count_x_Country <- ggplot(d_demo_educationinc,mapping = aes(x=Country.of.residence,color=Highest.education.level.completed,fill=Highest.education.level.completed))+
#   geom_bar()+
#   scale_color_viridis(discrete = T)+
#   scale_fill_viridis(discrete = T)
# pEdu_x_Count_x_Country
# ggsave(filename=paste0(PLOT_PATH,'count_x_education_x_country.pdf'),plot=pEdu_x_Count_x_Country,height=4.5, width=8, units = "in")
# 
# 
# 












d_exp <- load_exp_data()
d_full<-d_exp
# # d_full <- merge(d_exp,d_demo_simplified,by='UID')
# 
# d_reward <- ddply(d_full, ~Country.of.residence, plyr::summarize, meanReward = mean(z)+.5) 
# corTestPretty(d_reward$meanReward, unique(d_reward$Country.of.residence))
# 
# 
# d_reward <- ddply(d_full, ~UID+Country.of.residence, plyr::summarize, meanReward = mean(z+.5))
# d_stats_reward <- ddply(d_full, ~Country.of.residence, plyr::summarize, meanReward = mean(z+.5),sdReward=sd(z+.5))
# 
# d_europe_reward <- subset(d_reward, Country.of.residence %in% c('United Kingdom','Rest/Europe') )
# d_southafrica_reward <-subset(d_reward, Country.of.residence %in% c('South Africa') )
# 
# t.test(d_europe_reward$meanReward, d_southafrica_reward$meanReward, var.equal=TRUE)
# # ttestPretty(d_europe_reward$meanReward, d_southafrica_reward$meanReward, paired=T) # does not work for inequal sample size?

# 
# d_eu_sa <- d_full %>% 
#   mutate(Country.of.residence=ifelse(Country.of.residence %in% c('United Kingdom','Rest/Europe'),'Rest/Europe','South Africa'))
# 
# d_age <- ddply(d_eu_sa, ~UID+Country.of.residence, plyr::summarize, meanAge = mean(age.x))
# d_stats_age <- ddply(d_eu_sa, ~Country.of.residence, plyr::summarize, meanAge = mean(age.x),sdAge=sd(age.x))
# 
# d_europe_age <- subset(d_age, Country.of.residence %in% c('United Kingdom','Rest/Europe') )
# d_southafrica_age <-subset(d_age, Country.of.residence %in% c('South Africa') )
# 
# t.test(d_europe_age$meanAge, d_southafrica_age$meanAge, var.equal=TRUE)
# t.test(d_europe_age$meanAge, d_southafrica_age$meanAge, var.equal=FALSE)
# 
# d_europe<-d_full %>% 
#   filter(Country.of.residence %in% c('United Kingdom','Rest/Europe'))
# d_reward_age_europe <- ddply(d_europe, ~UID+Country.of.residence+age, plyr::summarize, meanReward = mean(z+.5))
# anova_europe <- aov(meanReward ~ Country.of.residence * age, data = d_reward_age_europe)
# summary(anova_europe)
# #Only age effect, so safe to group?
# # Large age effect (F~10.9)
# 
# d_reward_age_eu_sa <- ddply(d_eu_sa, ~UID+Country.of.residence+age, plyr::summarize, meanReward = mean(z+.5))
# anova_eu_sa <- aov(meanReward ~ Country.of.residence * age, data = d_reward_age_eu_sa)
# summary(anova_eu_sa)
# # Strong Country effect: F~18.8
# # Much weaker age effect: F~4
# # No interaction


d_reward_age <- ddply(d_full, ~UID+Country.of.residence+age, plyr::summarize, meanReward = mean(z+.5))
anova_overall <- aov(meanReward ~ Country.of.residence * age, data = d_reward_age)
summary(anova_overall)

print(xtable(anova_overall, digits = 2,caption = 'None'), type = "latex", file = "anova.tex")
# Strong Country effect: F~9.5
# Medium Age effect: F~6.8
# Weak interaction: F~3.7

# d_europe_summary<- d_europe %>% group_by(UID,age) %>% summarise(meanReward=mean(z+.5)) %>% mutate(Age=as.double(age))
# p_age_europe <- ggplot(data=d_europe_summary,aes(Age))+
#   geom_histogram(bins = 20)
# p_age_europe
# ggsave(filename=paste0(PLOT_PATH,'age_dist_europe.png'),plot=p_age_europe,height=4.5, width=8, units = "in")
# 
# d_sa <- d_full %>% filter(Country.of.residence=='South Africa')
# hist(d_sa$age)

# age_residency_reward <- lmer(formula = z+.5 ~ age.x*Country.of.residence + (1+Country.of.residence+age.x|UID),
#                            data    = d_full) #to run the model
# 
# ranova(age_residency_reward)
# # Random slope for age is not significant and can be dropped
# # Random slope for Country.of.residence is not significant and can be dropped
age_residency_reward <- lmer(formula = z+.5 ~ age*Country.of.residence + (1|UID),
                      data    = d_full) #to run the model
ranova(age_residency_reward)
coef(summary(age_residency_reward))
print(xtable(coef(summary(age_residency_reward)), digits = 4,caption = 'None'), type = "latex", file = "anova.tex")#, digits = 4

# plot_model(age_residency_reward,type = 'pred',terms = c('age'))
# plot_model(age_residency_reward,type = 'pred',terms = c('Country.of.residence'))
p_regression <- plot_model(age_residency_reward,type='pred',terms = c('age','Country.of.residence'))+
  scale_color_manual(values = countryPalette, labels = c('Other','South Africa','UK')) +
  scale_fill_manual(values = countryPalette, labels = c('Other','South Africa','UK')) +
  theme(plot.title = element_blank())+
  scale_x_continuous(limits = c(10, 80), breaks = seq(10, 80, 10))+
  labs(x ="Age in years", y = "Mean reward",legend='Country')+
  guides(fill=guide_legend(title="Country"),color=guide_legend(title="Country"))+
  theme_classic()+
  ggtitle('')

# p_regression <- plot_model(age_residency_reward,type='pred',terms = c('age.x','Country.of.residence'))
p_regression
p_coefficients <- plot_model(age_residency_reward,type='est',
                      show.values=TRUE,
                      vline.color = "red")+
  labs(title = '')
p_coefficients

# # age_residency_distance <- lmer(formula = distance ~ age.x*Country.of.residence + (1+Country.of.residence+age.x|UID),
# #                              data    = d_full) #to run the model
# # 
# # ranova(age_residency_distance)
# # # Random slope for age is not significant and can be dropped
# # # Random slope for Country.of.residence is not significant and can be dropped
# age_residency_distance <- lmer(formula = distance ~ prev_z*age.x*Country.of.residence + (1|UID),
#                              data    = d_full) #to run the model
# ranova(age_residency_distance)
# summary(age_residency_distance)
# 
# plot_model(age_residency_distance,type='pred',terms = c('prev_z','Country.of.residence'))
# plot_model(age_residency_distance,type='pred',terms = c('age.x','prev_z','Country.of.residence'))
# 
# plot_model(age_residency_distance,type='est',
#            show.values=TRUE,
#            vline.color = "red")

d_demo_simplified %>% filter(Country.of.residence=='South Africa') %>% count()
d_demo_simplified %>% filter(Country.of.residence!='South Africa') %>% count()

d_exp_agebin <-load_exp_data() %>% distinct(UID,age_bin)

d_age_bins <- merge(d_demo_simplified,d_exp_agebin,by='UID')

d_bins_summary<- d_age_bins %>% 
  group_by(age_bin,Country.of.residence) %>% 
  summarise(binsize=n())

d_age_excluded <- d_bins_summary %>%
  filter(Country.of.residence!='South Africa') %>%
  group_by(age_bin) %>%
  summarize(binsize=sum(binsize))


pBins <- ggplot(d_age_excluded,aes(x=age_bin,y=binsize,fill=age_bin,color=age_bin))+
  geom_bar(stat='identity',width=0.7)+
  geom_hline(yintercept=58, linetype='dashed', color='red') + # Total binsize
  # geom_text(aes(label=binsize), vjust=1.6, color="black", size=3.5)+
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10))+
  scale_x_discrete(guide=guide_axis(n.dodge=2))+
  scale_fill_viridis(discrete = T)+
  scale_color_viridis(discrete=T)+
  theme(plot.title = element_blank(),legend.position = 'none')+
  labs(x ="Age group", y = "Participants",color='Age group',fill='Age group')+
  theme_classic()
pBins

pBins2 <- ggplot(d_bins_summary,aes(x=age_bin,y=binsize,color=Country.of.residence,fill=Country.of.residence))+
  geom_bar(stat='identity',width=0.7)+
  geom_hline(yintercept=58, linetype='dashed', color='red') + # Total binsize
  # geom_text(aes(label=binsize), vjust=1.6, color="black", size=3.5)+
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10))+
  scale_x_discrete(guide=guide_axis(n.dodge=2))+
  # scale_fill_viridis(discrete = T)+
  # scale_color_viridis(discrete=T)+
  scale_color_manual(values = countryPalette, labels = c('Other','South Africa','UK')) +
  scale_fill_manual(values = countryPalette, labels = c('Other','South Africa','UK')) +
  theme(plot.title = element_blank())+
  labs(x ="Age group", y = "Participants",color='Country',fill='Country')+
  theme_classic()
pBins2


leg_bins<-get_legend(pBins2)
pBins2_legless <- pBins2 + theme(legend.position = 'none')

leg_regression <- get_legend(p_regression)
p_regression_legless <- p_regression + theme(legend.position = 'none')

p_lower <- cowplot::plot_grid(pBins,pBins2_legless, leg_bins, ncol=3, rel_widths = c(0.45,0.45,0.1),labels = c('b)','c)',''))
p_upper <- cowplot::plot_grid(p_regression_legless,leg_regression,ncol=2,rel_widths = c(0.9,0.1),labels=c('a)',''))
p_demographics <- cowplot::plot_grid(p_upper,p_lower,nrow=2,align = 'h')
p_demographics

ggsave('plots/plots_demographics/demographics.pdf', p_demographics, width = 10, height = 5, units ='in')
