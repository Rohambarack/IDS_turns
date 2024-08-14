#check
list.files()

files <- list.files()

files <- files [2:6]

for ( i in files){
  
  mod <- readRDS(i)
  print(summary(mod))
  
}
#visualize results, cheeck prior posterior updates
library(brms)
library(tidyverse)
tst <- as_draws_df(mod)

test_2 <- tst %>% 
  select(`b_ASD0:Visit:Ar_t_same`,
         `b_ASD1:Visit:Ar_t_same`,
         `b_ASD0:Visit:Ar_t_other`,
         `b_ASD1:Visit:Ar_t_other`)

test_2 %>% 
  ggplot()+
  geom_density(aes(x = `b_ASD0:Visit:Ar_t_other`), fill = "red", alpha = .5) +
  geom_density(aes(x = `b_ASD1:Visit:Ar_t_other`), fill = "blue", alpha = .5)

prior_summary(mod)
