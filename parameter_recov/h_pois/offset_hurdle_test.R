library(brms)
library(gtools)
library(tidyverse)
n <- 100
test_o <- tibble(
  ID = rep(c("A","B"), each = 2*n),
  lambda = rep(c(5,10), each = 2*n),
  duration= rep(200, 4*n),
  mes_lambda = exp(log(lambda) + log(duration))
) %>% 
  rowwise() %>% 
  mutate(P_count = rpois(1,mes_lambda),
         zero_help = rbinom(1,1,0.6)) %>% 
  ungroup() %>% 
  mutate(P_count = ifelse(zero_help == 0, 0 ,P_count))





test_formula <- bf(P_count ~0 + ID + offset(log(duration)),
                   hu ~ 0 + ID,
                   family = hurdle_poisson())

test_prior <- c(
  prior(normal(10,5),class = "b"),
  prior(normal(0,3),class = "b", dpar = "hu")
)
get_prior(data = test_o,formula = test_formula)

test_mod <- brm(
  test_formula,
  test_o,
  family = hurdle_poisson(),
  sample_prior = T,
  test_prior,
  chains = 4,
  cores = 4,
  warmup = 4000,
  iter = 8000
)


pp_check(test_mod, ndraws = 100)
summary(test_mod)
