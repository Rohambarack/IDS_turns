library(brms)
#check
list.files()

files <- list.files()

files <- files [2:6]

for ( i in files){
  
  mod <- readRDS(i)
  print(summary(mod))
  
}

mod_p_d <- readRDS(files[5])

mod_p_c <- readRDS(files[4])

summary(mod_p_c)


library(bayesplot)

df_mod_pc <- as_draws_df(mod_p_c)

mcmc_trace(mod_p_c,
           pars = "b_ASD1:Visit")
