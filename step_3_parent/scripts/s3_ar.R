######### modelling IDS Articulation rate for Parents
library(tidyverse)
library(brms)
setwd(".")
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
    prior(normal(1,1),class = b, coef = ASD0),
    prior(normal(1,1),class = b, coef = ASD1),
    ##mean Visit
    prior(normal(0,.33),class = b, coef = "ASD0:Visit"),
    prior(normal(0,.33),class = b, coef = "ASD1:Visit"),
    ##mean Visit:t_same
    prior(normal(0,.1),class = b, coef = "ASD0:Visit:Ar_t_same"),
    prior(normal(0,.1),class = b, coef = "ASD1:Visit:Ar_t_same"),
    ##mean Visit:t_other
    prior(normal(0,.1),class = b, coef = "ASD0:Visit:Ar_t_other"),
    prior(normal(0,.1),class = b, coef = "ASD1:Visit:Ar_t_other"),
    ##### participant level
    ## mean
    prior(normal(0,.33),class = sd, coef = "Intercept", group = Participant),
    ##mean Visit:t_same
    prior(normal(0,.1),class = sd, coef = "Visit:Ar_t_same", group = Participant),
    ##mean Visit:t_other
    prior(normal(0,.1),class = sd, coef = "Visit:Ar_t_other", group = Participant),
    ###### pop level sigma (logscale)
    ##mean ASD
    prior(normal(-1,.33),class = b, coef = ASD0, dpar = "sigma"),
    prior(normal(-1,.33),class = b, coef = ASD1, dpar = "sigma"),
    ##mean Visit (end of log scale)
    prior(normal(0,.33),class = b, coef = "ASD0:Visit", dpar = "sigma"),
    prior(normal(0,.33),class = b, coef = "ASD1:Visit", dpar = "sigma"),
    ##mean Visit:t_same
    prior(normal(0,.1),class = b, coef = "ASD0:Visit:Ar_t_same", dpar = "sigma"),
    prior(normal(0,.1),class = b, coef = "ASD1:Visit:Ar_t_same", dpar = "sigma"),
    ##mean Visit:t_other
    prior(normal(0,.1),class = b, coef = "ASD0:Visit:Ar_t_other", dpar = "sigma"),
    prior(normal(0,.1),class = b, coef = "ASD1:Visit:Ar_t_other", dpar = "sigma"),
    ### participant level 
    ## mean
    prior(normal(0,.33),class = sd, coef = "Intercept", group = Participant, dpar = "sigma")
    
  )
  
  return(log_priors)
  
}
#######################################################
#ar


y_ar<- bf(Ar_t1_same ~ 0 + ASD + ASD:Visit + ASD:Visit:Ar_t_same + ASD:Visit:Ar_t_other + (1 + Visit:Ar_t_same + Visit:Ar_t_other |gr(Participant, by=ASD)),
          sigma ~ 0 + ASD + ASD:Visit + ASD:Visit:Ar_t_same + ASD:Visit:Ar_t_other + ( 1 |gr(Participant, by=ASD)),
          family = lognormal())




###ar priors

get_prior(y_ar, data)

#getOption("max.print")
#options(max.print=1000)

ar_priors <- log_prior_function()


ar_model <- brm(
  data = data,
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

saveRDS(ar_model,"../models/step_3_ar_Parent.rds")