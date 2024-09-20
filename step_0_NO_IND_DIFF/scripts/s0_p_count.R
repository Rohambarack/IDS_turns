######### modelling IDS
library(tidyverse)
library(brms)
#setwd(".")
set.seed(1)
data <- read_csv("../../data/clean_data_mlu_correct_adj_dur.csv")


##### remove children, its only IDS 
data <- data %>% 
  filter(Interlocutor == "Parent")
### NA 0s from iqr
## NA 0s from articulation rate
data <- data %>% 
  mutate( iqr_f0 = ifelse(iqr_f0 == 0 | ArticulationRate == 0,
                          NA,iqr_f0),
          median_f0 = ifelse(iqr_f0 == 0 | ArticulationRate == 0,
                             NA,median_f0),
          ArticulationRate= ifelse(iqr_f0 == 0 | ArticulationRate == 0,
                                   NA,ArticulationRate),
          PauseCount = ifelse(iqr_f0 == 0 | ArticulationRate == 0,
                              NA,PauseCount),
          PauseDuration = ifelse(iqr_f0 == 0 | ArticulationRate == 0,
                                 NA,PauseDuration)
          
  )
#### asd to factor
data <- data %>% 
  mutate(ASD = as.factor(ASD))
### training / test set
#length(unique((data %>% filter(ASD == 0))$Participant)) 
#67 part ,35 asd 32not so 14 to reserve, keep roughly ratios, 6nt 8asd

asd_set <- unique((data %>%filter(ASD == 1))$Participant)
#asd_test <- sample(asd_set, 8)
asd_test <- c("Annie","AS2","JG2","CD2","Eduardo","Charles","Johan","Luis") 
non_asd_set <-unique((data %>%filter(ASD == 0))$Participant)
#non_asd_test <- sample(non_asd_set, 6)
non_asd_test <-  c("VC","JG","BC","SB","MM","AZ")


training_set <- data %>% 
  filter(! Participant %in% asd_test) %>% 
  filter(! Participant %in% non_asd_test)

test_set <- data %>% 
  filter(Participant %in% asd_test | Participant %in% non_asd_test)

##########################################################
h_pois_prior_function <- function(){
  
  h_pois_priors <- c(
    ### population level
    ##mean ASD
    prior(normal(0,1),class = b, coef = ASD0),
    prior(normal(0,1),class = b, coef = ASD1),
    ##mean Visit
    prior(normal(0,.33),class = b, coef = "ASD0:Visit"),
    prior(normal(0,.33),class = b, coef = "ASD1:Visit"),
    ##### participant level
    ## mean
    prior(normal(0,.33),class = sd, coef = "Intercept", group = Participant),
    ##mean Visit
    prior(normal(0,.33),class = sd, coef = "Visit", group = Participant),
    ###### pop level hu
    ##mean ASD
    prior(uniform(0,1),class = b, coef = ASD0, dpar = "hu"),
    prior(uniform(0,1),class = b, coef = ASD1, dpar = "hu"),
    ##mean Visit ( end of log scale)
    prior(normal(0,.33),class = b, coef = "ASD0:Visit", dpar = "hu"),
    prior(normal(0,.33),class = b, coef = "ASD1:Visit", dpar = "hu"),
    ### participant level 
    ## mean
    prior(normal(0,.33),class = sd, coef = "Intercept", group = Participant, dpar = "hu"),
    ##mean Visit
    prior(normal(0,.33),class = sd, coef = "Visit", group = Participant,dpar = "hu")
    
  )
  
  return(h_pois_priors)
  
}

##############################################################
y_pausecount<- bf(PauseCount ~ 0 + ASD + ASD:Visit + offset(log(Duration)) + (1 + Visit |gr(Participant, by=ASD)),
                  hu ~ 0 + ASD + ASD:Visit + offset(log(Duration)) + ( 1 + Visit |gr(Participant, by=ASD)),
                  family = hurdle_poisson())
###

#get_prior(y_pausecount, norm.trainData)

y_p_count_priors <- h_pois_prior_function()


pause_count_model_test <- brm(
  data = data,
  formula = y_pausecount,
  prior = y_p_count_priors,
  family = hurdle_poisson(),
  sample_prior = T,
  warmup = 1000,
  iter = 4000,
  cores = 8,
  chains = 4,
  init = 0,
  control = list(adapt_delta = 0.99)
)

#pp_check(pause_count_model_test,ndraws=100)

summary(pause_count_model_test)

saveRDS(pause_count_model_test,"../models/step_0_PauseCount_hurdle.rds")
