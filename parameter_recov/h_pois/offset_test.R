library(brms)
library(tidyverse)
n <- 100
test_o <- tibble(
  ID = rep(c("A","B"), each = 2*n),
  lambda = rep(c(5,10), each = 2*n),
  duration= rep(200, 4*n),
  mes_lambda = exp(log(lambda) + log(duration))
) %>% 
  rowwise() %>% 
  mutate(P_count = rpois(1,mes_lambda))

 



test_formula <- bf(P_count ~0 + ID + offset(log(duration)), family = poisson())

test_prior <- c(
  prior(normal(10,5),class = "b")
)
get_prior(data = test_o,formula = test_formula)

test_mod <- brm(
  test_formula,
  test_o,
  family = poisson(),
  test_prior,
  chains = 4,
  cores = 4,
  warmup = 2000,
  iter = 4000
)



summary(test_mod)
