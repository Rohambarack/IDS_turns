#check
list.files()

files <- list.files()

files <- files [2:6]

for ( i in files){
  
  mod <- readRDS(i)
  print(summary(mod))
  
}
#visualize results, cheeck prior posterior updates
mod <- readRDS(files[1])
library(brms)
library(tidyverse)
library(bayesplot)
tst <- as_draws_df(mod)
summary(mod)

test_2 <- tst %>% 
  select(`b_ASD0:Visit:Ar_t_same`,
         `b_ASD1:Visit:Ar_t_same`,
         `b_ASD0:Visit:Ar_t_other`,
         `b_ASD1:Visit:Ar_t_other`,
         `prior_b_ASD0:Visit:Ar_t_other`,
         `prior_b_ASD0:Visit:Ar_t_same`)

test_2 %>% 
  ggplot()+
  geom_density(aes(x = `b_ASD0:Visit:Ar_t_other`), fill = "red", alpha = .5) +
  geom_density(aes(x = `b_ASD1:Visit:Ar_t_other`), fill = "blue", alpha = .5) 

test_2 %>% 
  ggplot()+
  geom_density(aes(x = `b_ASD0:Visit:Ar_t_same`), fill = "red", alpha = .5) +
  geom_density(aes(x = `b_ASD1:Visit:Ar_t_same`), fill = "blue", alpha = .5) 

test_2 %>% 
  ggplot()+
  geom_density(aes(x = `b_ASD0:Visit:Ar_t_same`), fill = "red", alpha = .5) +
  geom_density(aes(x = `b_ASD0:Visit:Ar_t_other`), fill = "blue", alpha = .5) 

test_2 %>% 
  ggplot()+
  geom_density(aes(x = `b_ASD1:Visit:Ar_t_same`), fill = "red", alpha = .5) +
  geom_density(aes(x = `b_ASD1:Visit:Ar_t_other`), fill = "blue", alpha = .5) +
  geom_density(aes(x = `b_ASD0:Visit:Ar_t_same`), fill = "green", alpha = .5) +
  geom_density(aes(x = `b_ASD0:Visit:Ar_t_other`), fill = "pink", alpha = .5)

prior_summary(mod)

summary(mod)
mcmc_trace(mod,
           pars = "b_ASD0:Visit:Ar_t_same")

?mcmc_trace
