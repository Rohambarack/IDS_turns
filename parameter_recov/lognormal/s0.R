library(tidyverse)
library(brms)
#functions
sd_round_zero <- function(x){
  x_mod <- ifelse(x<0,0.00001,x)
  return(x_mod)
}

set.seed(123) 
#child directed speech
#y<- bf(mu ~ 0 + ASD + ASD:Visit + (1 + Visit |gr(Participant, by=ASD)),
#       sigma ~ 0 + ASD + ASD:Visit + ( 1 |gr(Participant, by=ASD)),
#       family = lognormal())

############################################# no most akkor rendesen

k <- 250
n <- 40
s <- 6
ASD_mu <- 1.5
ASD_mu_sd <- 0.5
ASD_sigma <- 1
ASD_sigma_sd <- 0.3
seed  <-1
b_Visit_on_mu <- 0.3
b_Visit_on_sigma <- 0.1
b_Visit_on_mu_sd <- 0.1
b_Visit_on_sigma_sd <- 0.2
b_ASD_mu <- 0.3
b_ASD_sigma <- 0.1
b_ASD_mu_sd <- 0.2
b_ASD_sigma_sd <- 0.2

############### functions ################
sd_round_zero <- function(x){
  x_mod <- ifelse(x<0,0.00001,x)
  return(x_mod)
}
#### simulate function #####
s0_sim_func <- function(
    k = 250,
    n = 80,
    s = 6,
    seed,
    #non asd (ASD0 Intercept)
    mu,
    mu_sd,
    sigma,
    sigma_sd,
    #(ASD1 Intercept)
    mu_ASD,
    sigma_ASD,
    mu_sd_ASD,
    sigma_sd_ASD,
    #non asd (ASD0 Visit beta)
    b_Visit_on_mu,
    b_Visit_on_sigma,
    b_Visit_on_mu_sd,
    #ASD effect (ASD1 visit beta)
    b_Visit_on_mu_ASD,
    b_Visit_on_sigma_ASD,
    b_Visit_on_mu_sd_ASD){
  
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
      ASD_mu = ifelse(ASD == "0",
                      mu,
                      mu_ASD),
      ASD_mu_sd = ifelse(ASD == "0",
                         mu_sd,
                         mu_sd_ASD),
      ASD_sigma = ifelse(ASD == "0",
                         sigma,
                         sigma_ASD),
      ASD_sigma_sd = ifelse(ASD == "0",
                            sigma_sd,
                            sigma_sd_ASD)
    )
  
  
  #extend to Visits
  DF_sim <- DF_sim %>% expand(nesting(seed,
                                      Participant,
                                      ASD,
                                      ASD_mu,
                                      ASD_mu_sd,
                                      ASD_sigma,
                                      ASD_sigma_sd), Visit = 1:s)
  # add population level effect
  DF_sim <- DF_sim %>% 
    mutate(b_Visit_on_mu = ifelse(ASD == "0",
                                  b_Visit_on_mu,
                                  b_Visit_on_mu_ASD),
           b_Visit_on_sigma = ifelse(ASD == "0",
                                     b_Visit_on_sigma,
                                     b_Visit_on_sigma_ASD)
           )
  # add group level effect
  DF_sim <- DF_sim %>% 
    mutate(b_Visit_on_mu_sd = ifelse(ASD == "0",
                                     b_Visit_on_mu_sd,
                                     b_Visit_on_mu_sd_ASD))
  # pop_level ASD:Visit effects (visit-1 to not mess up 1st visit)
  DF_sim <- DF_sim %>% 
    mutate(`ASD:Visit_mu` = ASD_mu + b_Visit_on_mu*(Visit-1),
           `ASD:Visit_sigma`= ASD_sigma + b_Visit_on_sigma*(Visit-1))
  # group_level ASD:Visit effects (visit-1)
  DF_sim <- DF_sim %>% 
    mutate(`ASD:Visit_mu_sd` = ASD_mu_sd + b_Visit_on_mu_sd*(Visit-1)
           )
  
  #draw values for each Visit
  DF_sim <- DF_sim %>% 
    rowwise() %>% 
    mutate( 
      visit_mu = rnorm(1,`ASD:Visit_mu`,`ASD:Visit_mu_sd`),
      visit_sigma = rnorm(1,`ASD:Visit_sigma`,ASD_sigma_sd),
      #take care of negative values in sigma
      visit_sigma = sd_round_zero(visit_sigma)
    )
  
  #extend to k turns
  DF_sim_k <- DF_sim %>% expand(nesting(seed,
                                      Participant,
                                      ASD,
                                      Visit,
                                      visit_mu,
                                      visit_sigma), Turn = 1:k)
  #get ylognorm
  DF_sim_k <- DF_sim_k %>% 
    rowwise() %>% 
    mutate(
          y_lognorm = rlnorm(1,visit_mu,visit_sigma)      
    )
  
  
  return(DF_sim_k)
  
}
####################simulate

sim_data <- s0_sim_func(seed = 1,
                        mu = 1.36,
                        mu_sd = 0.12,
                        sigma = exp(-0.83),
                        sigma_sd = 0.13,
                        mu_ASD =1.41,
                        mu_sd_ASD = 0.18,
                        sigma_ASD = exp(-0.84),
                        sigma_sd_ASD = 0.18,
                        b_Visit_on_mu = 0.01,
                        b_Visit_on_sigma = exp(-0.02),
                        b_Visit_on_mu_sd = 0.03,
                        b_Visit_on_mu_ASD = 0.01,
                        b_Visit_on_sigma_ASD = exp(-0.02),
                        b_Visit_on_mu_sd_ASD = 0.03)


#################### plot
sim_data %>% 
  ggplot(aes(x = y_lognorm, fill = ASD)) +
  xlim(c(0,50)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Visit) +
  theme_classic()


############### model it ?

model_lognorm_s0 <- bf(y_lognorm ~ 0 + ASD + ASD:Visit + (1 + Visit |gr(Participant, by=ASD)),
                     sigma ~ 0 + ASD + ASD:Visit + ( 1 |gr(Participant, by=ASD)),
                     family = lognormal())

#priors
log_prior_function <- function(){
  
  log_priors <- c(
    ### population level
    ##mean ASD
    prior(normal(1,1),class = b, coef = ASD0),
    prior(normal(1,1),class = b, coef = ASD1),
    ###mean Individual skills
    ##mean Visit
    prior(normal(0,.33),class = b, coef = "ASD0:Visit"),
    prior(normal(0,.33),class = b, coef = "ASD1:Visit"),
    ##### participant level
    ## mean
    prior(normal(0,.33),class = sd, coef = "Intercept", group = Participant),
    ###mean Individual skills
    ##mean Visit
    prior(normal(0,.33),class = sd, coef = "Visit", group = Participant),
    ###### pop level sigma (logscale)
    ##mean ASD
    prior(normal(-1,.33),class = b, coef = ASD0, dpar = "sigma"),
    prior(normal(-1,.33),class = b, coef = ASD1, dpar = "sigma"),
    ###mean Individual skills
    ##mean Visit ( end of log scale)
    prior(normal(0,.33),class = b, coef = "ASD0:Visit", dpar = "sigma"),
    prior(normal(0,.33),class = b, coef = "ASD1:Visit", dpar = "sigma"),
    ### participant level 
    ## mean
    prior(normal(0,.33),class = sd, coef = "Intercept", group = Participant, dpar = "sigma")
    
  )
  
  return(log_priors)
  
}

s0_priors <- log_prior_function()

test <- brm(
  data = sim_data,
  formula = model_lognorm_s0,
  prior = s0_priors,
  family = lognormal(),
  sample_prior = T,
  warmup = 2000,
  iter = 4000,
  chains = 4,
  cores = 16
  
)

saveRDS(test,"./models/test_recov.rds")
