library(tidyverse)
library(brms)
library(gtools)
#functions
sd_round_zero <- function(x){
  x_mod <- ifelse(x<0,0.00001,x)
  return(x_mod)
}

set.seed(123) 
#child directed speech pauses

#y_pausecount<- bf(PauseCount ~ 0 + ASD + ASD:Visit + offset(log(Duration)) + (1 + Visit |gr(Participant, by=ASD)),
#                 hu ~ 0 + ASD + ASD:Visit + ( 1 + Visit |gr(Participant, by=ASD)),
#                 family = hurdle_poisson())


############################################# no most akkor rendesen

k = 200
n = 60
s = 6
seed = 1
#non asd (ASD0 Intercept)
lambda = log(5)
lambda_sd = log(1)
hurdle = logit(0.6)
hurdle_sd = logit(0.2)
#duration as constant for both simplicity? runif 1-30?
#
#(ASD1 Intercept)
lambda_ASD = log(7)
hurdle_ASD = logit(0.5)
lambda_sd_ASD = log(1)
hurdle_sd_ASD = logit(0.2)
#non asd (ASD0 Visit beta)
b_Visit_on_lambda = -0.2
b_Visit_on_hurdle = 0.05
b_Visit_on_lambda_sd = -0.1
b_Visit_on_hurdle_sd = -0.05
#ASD effect (ASD1 visit beta)
b_Visit_on_lambda_ASD = -0.1
b_Visit_on_hurdle_ASD = 0.02
b_Visit_on_lambda_sd_ASD = -0.05
b_Visit_on_hurdle_sd_ASD = -0.02
#visit on duration

############### functions ################
sd_round_zero <- function(x){
  x_mod <- ifelse(x<0,0.00001,x)
  return(x_mod)
}
#### simulate function #####
s0_sim_func_hpois <- function(
    k = 200,
    n = 60,
    s = 6,
    seed,
    #non asd (ASD0 Intercept)
    lambda,
    lambda_sd,
    hurdle,
    hurdle_sd,
    #(ASD1 Intercept)
    lambda_ASD,
    hurdle_ASD,
    lambda_sd_ASD,
    hurdle_sd_ASD,
    #non asd (ASD0 Visit beta)
    b_Visit_on_lambda,
    b_Visit_on_hurdle,
    b_Visit_on_lambda_sd,
    b_Visit_on_hurdle_sd,
    #ASD effect (ASD1 visit beta)
    b_Visit_on_lambda_ASD,
    b_Visit_on_hurdle_ASD,
    b_Visit_on_lambda_sd_ASD,
    b_Visit_on_hurdle_sd_ASD){
  
  #seed
  set.seed(seed)
  #n Participants
  
  DF_sim <- tibble(
    Participant = seq(1:n),
    ASD = rep(c("0","1"),each = n/2)
  )
  
  
  #add intercepts for ASD0 and ASD1
  DF_sim <- DF_sim %>%
    rowwise() %>% 
    mutate(
      seed = seed,
      ASD_lambda = ifelse(ASD == "0",
                      lambda,
                      lambda_ASD),
      ASD_lambda_sd = ifelse(ASD == "0",
                         lambda_sd,
                         lambda_sd_ASD),
      ASD_hurdle = ifelse(ASD == "0",
                         hurdle,
                         hurdle_ASD),
      ASD_hurdle_sd = ifelse(ASD == "0",
                            hurdle_sd,
                            hurdle_sd_ASD),
      Duration = NA
    )
  
  
  #extend to Visits
  DF_sim <- DF_sim %>% expand(nesting(seed,
                                      Participant,
                                      ASD,
                                      ASD_lambda,
                                      ASD_lambda_sd,
                                      ASD_hurdle,
                                      ASD_hurdle_sd,
                                      Duration), Visit = 1:s)
  # add population level effect
  DF_sim <- DF_sim %>% 
    mutate(b_Visit_on_lambda = ifelse(ASD == "0",
                                  b_Visit_on_lambda,
                                  b_Visit_on_lambda_ASD),
           b_Visit_on_hurdle = ifelse(ASD == "0",
                                     b_Visit_on_hurdle,
                                     b_Visit_on_hurdle_ASD)
           )
  # add group level effect
  DF_sim <- DF_sim %>% 
    mutate(b_Visit_on_lambda_sd = ifelse(ASD == "0",
                                     b_Visit_on_lambda_sd,
                                     b_Visit_on_lambda_sd_ASD),
           b_Visit_on_hurdle_sd = ifelse(ASD == "0",
                                         b_Visit_on_hurdle_sd,
                                         b_Visit_on_hurdle_sd_ASD)
           )
  # pop_level ASD:Visit effects (visit-1 to not mess up 1st visit)
  DF_sim <- DF_sim %>% 
    mutate(
           `ASD:Visit_lambda` = ASD_lambda + b_Visit_on_lambda*(Visit-1),
           `ASD:Visit_hurdle`= ASD_hurdle + b_Visit_on_hurdle*(Visit-1),
          )
  
  # group_level ASD:Visit effects (visit-1)
  DF_sim <- DF_sim %>% 
    mutate(`ASD:Visit_lambda_sd` = ASD_lambda_sd + b_Visit_on_lambda_sd*(Visit-1),
           `ASD:Visit_hurdle_sd`= ASD_hurdle_sd + b_Visit_on_hurdle_sd*(Visit-1),
           #remove negatives
           `ASD:Visit_lambda_sd` = sd_round_zero(`ASD:Visit_lambda_sd`),
           `ASD:Visit_hurdle_sd`= sd_round_zero(`ASD:Visit_hurdle_sd`)
           )
  
  #draw values for each Visit
  DF_sim <- DF_sim %>% 
    rowwise() %>% 
    mutate( 
      visit_lambda = rnorm(1,`ASD:Visit_lambda`,`ASD:Visit_lambda_sd`),
      visit_hurdle = rnorm(1,`ASD:Visit_hurdle`,`ASD:Visit_hurdle_sd`),

      #remove -s
      visit_lambda = sd_round_zero(visit_lambda),
      visit_hurdle = sd_round_zero(visit_hurdle)
    )
  
  #extend to k turns
  DF_sim_k <- DF_sim %>% expand(nesting(seed,
                                      Participant,
                                      ASD,
                                      Visit,
                                      Duration,
                                      visit_lambda,
                                      visit_hurdle), Turn = 1:k)
  #get ylognorm
  DF_sim_k <- DF_sim_k %>% 
    rowwise() %>% 
    mutate(
      #add duration
      Duration = runif(1,0.1,30),
      #add measured lambda
      measured_lambda = exp(log(visit_lambda) + log(Duration)),
      #add hurdle phase 1
      h_help = rbinom(1,1,(1-visit_hurdle)),
      #draw value with duration offset
      y_pois = rpois(1,measured_lambda),
      #add hurdle phase 2
      y_pois = ifelse(h_help ==  0,
                      0,
                      y_pois)
    )
  
  
  return(DF_sim_k)
  
}
####################simulate


sim_data <- s0_sim_func_hpois(
  k = 200,
  n = 60,
  s = 6,
  seed = 1,
  #non asd (ASD0 Intercept)
  lambda = 5,
  lambda_sd = 1,
  hurdle = 0.6,
  hurdle_sd = 0.2,
  #duration as constant for both simplicity? runif 1-30?
  #
  #(ASD1 Intercept)
  lambda_ASD = 7,
  hurdle_ASD = 0.5,
  lambda_sd_ASD = 1,
  hurdle_sd_ASD = 0.2,
  #non asd (ASD0 Visit beta)
  b_Visit_on_lambda = -0.2,
  b_Visit_on_hurdle = 0.05,
  b_Visit_on_lambda_sd = -0.1,
  b_Visit_on_hurdle_sd = -0.05,
  #ASD effect (ASD1 visit beta)
  b_Visit_on_lambda_ASD = -0.1,
  b_Visit_on_hurdle_ASD = 0.02,
  b_Visit_on_lambda_sd_ASD = -0.05,
  b_Visit_on_hurdle_sd_ASD = -0.02
  
)
#################### plot


############### model it ?

model_h_pois <- bf(y_pois ~ 0 + ASD + ASD:Visit + offset(log(Duration)) + (1 + Visit |gr(Participant, by=ASD)),
                 hu ~ 0 + ASD + ASD:Visit + ( 1 + Visit |gr(Participant, by=ASD)),
                 family = hurdle_poisson())

#priors
h_pois_prior_function <- function(){
  
  h_pois_priors <- c(
    ### population level
    ##mean ASD
    prior(normal(0,3),class = b, coef = ASD0),
    prior(normal(0,3),class = b, coef = ASD1),
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
    prior(normal(0,3),class = b, coef = ASD0, dpar = "hu"),
    prior(normal(0,3),class = b, coef = ASD1, dpar = "hu"),
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

s0_priors <- h_pois_prior_function()

test <- brm(
  data = sim_data,
  formula = model_h_pois,
  prior = s0_priors,
  family = hurdle_poisson(),
  sample_prior = T,
  warmup = 2000,
  iter = 4000,
  chains = 4,
  cores = 16,
  
)

pp_check(test, ndraws = 100)

saveRDS(test,"./models/test_recov_h_pois.rds")
