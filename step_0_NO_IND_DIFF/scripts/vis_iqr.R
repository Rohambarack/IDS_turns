library(tidyverse)
library(brms)
library(grid)
library(gridExtra)

#functions
#helping with extracting useful variable names  for visualization
getname <- function(name){
  #getname in str
  #name <- deparse(substitute(vect))
  #turn all "" before $
  name <- sub("^(.*?)\\$","",name)
  
}
# a function to extract betas by groups for comparison
take_apart_4_vis <- function(vect1,vect2){
  
  vectname1 <- getname(deparse(substitute(vect1)))
  vectname2 <- getname(deparse(substitute(vect2)))
  #pivot longer basically?
  datvect = c(vect1, vect2)
  n = length(vect1)
  groupvect = c(rep(vectname1,n),
                rep(vectname2,n))
  
  df_1 <- tibble( gr = groupvect,
                  b = datvect)
  
  return(df_1)
}
#a function for extracting priors and group values to check how restricting were the priors
pp_take_apart_4_vis <- function(prior,vect1,vect2){
  
  priorname <- getname(deparse(substitute(prior)))
  vectname1 <- getname(deparse(substitute(vect1)))
  vectname2 <- getname(deparse(substitute(vect2)))
  #pivot longer basically?
  datvect = c(prior, vect1, vect2)
  n = length(vect1)
  groupvect = c(rep(priorname,n),
                rep(vectname1,n),
                rep(vectname2,n))
  
  df_1 <- tibble( gr = groupvect,
                  b = datvect)
  
  return(df_1)
}
#################
#save plots
save_it <- function(plot,name){
  
  ggsave(plot, 
         filename = name,
         device = "pdf",
         height = 5.5, width = 10, units = "in")
  
}



#read in model
iqr <- readRDS("../models/step_0_iqr_f0.rds")

summary(iqr)

#make it into df
med_vis <- as_draws_df(iqr)



######## check prior posterior updates for median fqr
pp_b_int <- pp_take_apart_4_vis(med_vis$prior_b_ASD0,med_vis$b_ASD0,med_vis$b_ASD1)
pp_b_visit <- pp_take_apart_4_vis(med_vis$`prior_b_ASD0:Visit`,med_vis$`b_ASD0:Visit`,med_vis$`b_ASD1:Visit`)

pp_1 <-grid.arrange(
  pp_b_int %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    theme_classic(),
  pp_b_visit %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
 
  top = textGrob("Prior-posterior update checks, IQR Pitch mu",gp=gpar(fontsize=15,font=3))
)

save_it(pp_1,"../plots/iqr/step_0_PP_iqr_mu.pdf")

######## check prior posterior updates for group level median fqr

pp_bg_int <- pp_take_apart_4_vis(med_vis$`prior_sd_Participant__Intercept:ASD0`,
                                 med_vis$`sd_Participant__Intercept:ASD0`,
                               med_vis$`sd_Participant__Intercept:ASD1`)
pp_bg_visit <- pp_take_apart_4_vis(med_vis$`prior_sd_Participant__Visit:ASD0`,
                                  med_vis$`sd_Participant__Visit:ASD0`,
                                 med_vis$`sd_Participant__Visit:ASD1`)

pp_2 <- grid.arrange(
  pp_bg_int %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    theme_classic(),
  pp_bg_visit %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  
  top = textGrob("Prior-Posterior, Group-level Effects, IQR Pitch Mu",gp=gpar(fontsize=15,font=3))
)

save_it(pp_2,"../plots/iqr/step_0_PP_iqr_mu_sd.pdf")

#################
############ betas for sigma
pp_s_int <- pp_take_apart_4_vis(med_vis$prior_b_sigma_ASD0,
                                med_vis$b_sigma_ASD0,
                              med_vis$b_sigma_ASD1)

pp_s_visit <- pp_take_apart_4_vis(med_vis$`prior_b_sigma_ASD0:Visit`,
                                  med_vis$`b_sigma_ASD0:Visit`,
                                med_vis$`b_sigma_ASD1:Visit`)


pp_3 <- grid.arrange(
  pp_s_int %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    theme_classic(),
  pp_s_visit %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  
  top = textGrob("Prior-posterior, Population-level effects, IQR Pitch Sigma",gp=gpar(fontsize=15,font=3))
)

save_it(pp_3,"../plots/iqr/step_0_PP_iqr_sigma.pdf")

###################### Visualize and compare betas
#betas for mu
vis_b_int <- take_apart_4_vis(med_vis$b_ASD0,med_vis$b_ASD1)
vis_b_visit <- take_apart_4_vis(med_vis$`b_ASD0:Visit`,med_vis$`b_ASD1:Visit`)

vis_1 <- grid.arrange(
  vis_b_int %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    theme_classic(),
  vis_b_visit %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
 
  top = textGrob("Population-level Effects, IQR Pitch mu",gp=gpar(fontsize=15,font=3))
)

save_it(vis_1,"../plots/iqr/step_0_iqr_mu.pdf")

############ betas for group level
vis_bg_int <- take_apart_4_vis(med_vis$`sd_Participant__Intercept:ASD0`,
                               med_vis$`sd_Participant__Intercept:ASD1`)
vis_bg_visit <- take_apart_4_vis(med_vis$`sd_Participant__Visit:ASD0`,
                                 med_vis$`sd_Participant__Visit:ASD1`)


vis_2 <- grid.arrange(
  vis_bg_int %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    theme_classic(),
  vis_bg_visit %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
 
  top = textGrob("Group-level Effects, IQR Pitch Mu",gp=gpar(fontsize=15,font=3))
)

save_it(vis_2,"../plots/iqr/step_0_iqr_mu_sd.pdf")

############ betas for sigma
vis_s_int <- take_apart_4_vis(med_vis$b_sigma_ASD0,
                              med_vis$b_sigma_ASD1)
###convert back from log scale:

vis_s_visit <- take_apart_4_vis(med_vis$`b_sigma_ASD0:Visit`,
                                med_vis$`b_sigma_ASD1:Visit`)

vis_3 <- grid.arrange(
  vis_s_int %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    theme_classic(),
  vis_s_visit %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  
  top = textGrob("Population-level effects, IQR Pitch Sigma (log)",gp=gpar(fontsize=15,font=3))
)

save_it(vis_3,"../plots/iqr/step_0_iqr_sigma.pdf")
