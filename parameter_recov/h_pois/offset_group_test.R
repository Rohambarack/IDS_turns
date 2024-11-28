library(brms)
library(tidyverse)


sd_round_zero <- function(x){
  x_mod <- ifelse(x<0,0.00001,x)
  return(x_mod)
}

n <- 30
k <- 200
t_pop_lambda <- 5
t_pop_lambda_B <- 10

t_sd_lambda <- 0.5
t_sd_lambda_B <- 1

test_o <- tibble(
  Participant = seq(1:(2*n)),
  ID = rep(c("A","B"), each = n),
  pop_lambda = ifelse(ID == "A",
                      t_pop_lambda,
                      t_pop_lambda_B),
  sd_lambda = ifelse(ID == "A",
                     t_sd_lambda,
                     t_sd_lambda_B),
  duration= rep(20, 2*n)
)
#draw group level values

test_o <- test_o %>% 
  rowwise() %>% 
  mutate(
    ind_lambda = rnorm(1,pop_lambda,sd_lambda),
    #let´s not get negative values in here
    ind_lambda =  sd_round_zero(ind_lambda),
    measured_ind_lambda = exp(log(ind_lambda) + log(duration))
  )

#expand to k turns
test_o <- test_o %>% expand(nesting(
                          Participant,
                          ID,
                          duration,
                          ind_lambda,
                          measured_ind_lambda), turn = 1:k)

test_o <- test_o %>% 
  rowwise() %>% 
  mutate(P_count = rpois(1,measured_ind_lambda))


test_formula <- bf(P_count ~0 + ID + offset(log(duration)) + (1|gr(Participant, by=ID)),
                   family = poisson())

test_prior <- c(
  prior(normal(0,3),class = "b"),
  prior(normal(0,3),class = "sd",group ="Participant")
)
get_prior(data = test_o,formula = test_formula)

test_mod <- brm(
  test_formula,
  test_o,
  family = poisson(),
  test_prior,
  chains = 4,
  cores = 8,
  warmup = 2000,
  iter = 4000
)



summary(test_mod)

pp_check(test_mod, ndraws = 100)


res <- as_draws_df(test_mod)




