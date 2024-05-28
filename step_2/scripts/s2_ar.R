######### modelling IDS
library(tidyverse)
library(brms)
setwd(".")
data <- read_csv("../data/clean_data_mlu_correct.csv")


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

####standardize non categorical predictors
library(caret)

normParam <- preProcess(training_set[,c("Socialization",
                                        "MotorSkills",
                                        "CHI_MLU")])
norm.testData <- predict(normParam, test_set)
norm.trainData <- predict(normParam, training_set)
##########################################################
log_prior_function <- function(){
  
  log_priors <- c(
    ### population level
    ##mean ASD
    prior(normal(1,1),class = b, coef = ASD0),
    prior(normal(1,1),class = b, coef = ASD1),
    ###mean Individual skills
    prior(normal(0,.33),class = b, coef = "ASD0:CHI_MLU"),
    prior(normal(0,.33),class = b, coef = "ASD0:MotorSkills"),
    prior(normal(0,.33),class = b, coef = "ASD0:Socialization"),
    prior(normal(0,.33),class = b, coef = "ASD1:CHI_MLU"),
    prior(normal(0,.33),class = b, coef = "ASD1:MotorSkills"),
    prior(normal(0,.33),class = b, coef = "ASD1:Socialization"),
    ##mean Visit
    prior(normal(0,.33),class = b, coef = "ASD0:Visit"),
    prior(normal(0,.33),class = b, coef = "ASD1:Visit"),
    ##### participant level
    ## mean
    prior(normal(0,.33),class = sd, coef = "Intercept", group = Participant),
    ###mean Individual skills
    prior(normal(0,.33),class = sd, coef = "CHI_MLU", group = Participant),
    prior(normal(0,.33),class = sd, coef = "MotorSkills", group = Participant),
    prior(normal(0,.33),class = sd, coef = "Socialization", group = Participant),
    ##mean Visit
    prior(normal(0,.33),class = sd, coef = "Visit", group = Participant),
    ###### pop level sigma (logscale)
    ##mean ASD
    prior(normal(-1,.33),class = b, coef = ASD0, dpar = "sigma"),
    prior(normal(-1,.33),class = b, coef = ASD1, dpar = "sigma"),
    ###mean Individual skills
    prior(normal(0,.33),class = b, coef = "ASD0:CHI_MLU", dpar = "sigma"),
    prior(normal(0,.33),class = b, coef = "ASD0:MotorSkills", dpar = "sigma"),
    prior(normal(0,.33),class = b, coef = "ASD0:Socialization", dpar = "sigma"),
    prior(normal(0,.33),class = b, coef = "ASD1:CHI_MLU", dpar = "sigma"),
    prior(normal(0,.33),class = b, coef = "ASD1:MotorSkills", dpar = "sigma"),
    prior(normal(0,.33),class = b, coef = "ASD1:Socialization", dpar = "sigma"),
    ##mean Visit ( end of log scale)
    prior(normal(0,.33),class = b, coef = "ASD0:Visit", dpar = "sigma"),
    prior(normal(0,.33),class = b, coef = "ASD1:Visit", dpar = "sigma"),
    ### participant level 
    ## mean
    prior(normal(0,.33),class = sd, coef = "Intercept", group = Participant, dpar = "sigma")
    
  )
  
  return(log_priors)
  
}
#######################################################
#ar

y_ar<- bf(ArticulationRate ~ 0 + ASD + ASD:Visit +  ASD:Socialization + ASD:MotorSkills + ASD:CHI_MLU + (1 + Visit + Socialization + MotorSkills + CHI_MLU |gr(Participant, by=ASD)),
          sigma ~ 0 + ASD + ASD:Visit + ASD:Socialization + ASD:MotorSkills + ASD:CHI_MLU + ( 1 |gr(Participant, by=ASD)),
          family = lognormal())
###ar priors

#get_prior(y_ar, norm.trainData)

#getOption("max.print")
#options(max.print=1000)

ar_priors <- log_prior_function()


ar_model <- brm(
  data = norm.trainData,
  formula = y_ar,
  prior = ar_priors,
  family = lognormal(),
  sample_prior = T,
  warmup = 500,
  iter = 1500,
  cores = 8,
  chains = 4,
  init = 0,
)


pp_check(ar_model,ndraws=100)

summary(ar_model)

saveRDS(ar_model,"step_2_ar_2.rds")