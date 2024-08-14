######### modelling IDS pitch median
library(tidyverse)
library(brms)
#setwd(".")
set.seed(1)
data <- read_csv("../../data/data_turn_shifts.csv")


##### remove children, its only IDS 
data <- data %>% 
  filter(Interlocutor == "Parent")
#### asd to factor
data <- data %>% 
  mutate(ASD = as.factor(ASD))

##########################################################
log_prior_function <- function(){
  
  log_priors <- c(
    ### population level
    ##mean ASD
    prior(normal(6,1),class = b, coef = ASD0),
    prior(normal(6,1),class = b, coef = ASD1),
    ##mean Visit
    prior(normal(0,.33),class = b, coef = "ASD0:Visit"),
    prior(normal(0,.33),class = b, coef = "ASD1:Visit"),
    ##mean Visit:t_same
    prior(normal(0,.1),class = b, coef = "ASD0:Visit:Median_t_same"),
    prior(normal(0,.1),class = b, coef = "ASD1:Visit:Median_t_same"),
    ##mean Visit:t_other
    prior(normal(0,.1),class = b, coef = "ASD0:Visit:Median_t_other"),
    prior(normal(0,.1),class = b, coef = "ASD1:Visit:Median_t_other"),
    ##### participant level
    ## mean
    prior(normal(0,1),class = sd, coef = "Intercept", group = Participant),
    ##mean Visit:t_same
    prior(normal(0,.3),class = sd, coef = "Visit:Median_t_same", group = Participant),
    ##mean Visit:t_other
    prior(normal(0,.3),class = sd, coef = "Visit:Median_t_other", group = Participant),
    ###### pop level sigma (logscale)
    ##mean ASD
    prior(normal(0,1),class = b, coef = ASD0, dpar = "sigma"),
    prior(normal(0,1),class = b, coef = ASD1, dpar = "sigma"),
    ##mean Visit (end of log scale)
    prior(normal(0,.33),class = b, coef = "ASD0:Visit", dpar = "sigma"),
    prior(normal(0,.33),class = b, coef = "ASD1:Visit", dpar = "sigma"),
    ##mean Visit:t_same
    prior(normal(0,.1),class = b, coef = "ASD0:Visit:Median_t_same", dpar = "sigma"),
    prior(normal(0,.1),class = b, coef = "ASD1:Visit:Median_t_same", dpar = "sigma"),
    ##mean Visit:t_other
    prior(normal(0,.1),class = b, coef = "ASD0:Visit:Median_t_other", dpar = "sigma"),
    prior(normal(0,.1),class = b, coef = "ASD1:Visit:Median_t_other", dpar = "sigma"),
    ### participant level 
    ## mean
    prior(normal(0,.33),class = sd, coef = "Intercept", group = Participant, dpar = "sigma")
    
  )
  
  return(log_priors)
  
}
#######################################################


y_median_f0<- bf(Median_t1_same ~ 0 + ASD + ASD:Visit + ASD:Visit:Median_t_same + ASD:Visit:Median_t_other + (1 + Visit:Median_t_same + Visit:Median_t_other |gr(Participant, by=ASD)),
              sigma ~ 0 + ASD + ASD:Visit + ASD:Visit:Median_t_same + ASD:Visit:Median_t_other + ( 1 |gr(Participant, by=ASD)),
              family = lognormal())

y_median_f0_priors <- log_prior_function()

median_model <- brm(
  data =data,
  formula = y_median_f0,
  prior = y_median_f0_priors,
  family = lognormal(),
  sample_prior = T,
  warmup = 500,
  iter = 1500,
  cores = 16,
  chains = 4,
  init = 0,
)

saveRDS(median_model,"../models/step_3_median_f0_Parent.rds")