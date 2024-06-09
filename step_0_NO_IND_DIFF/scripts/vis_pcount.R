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
pcount <- readRDS("../models/step_2_PauseCount_4_hurdle.rds")

summary(pcount)

#make it into df
med_vis <- as_draws_df(pcount)



######## check prior posterior updates for median fqr
pp_b_int <- pp_take_apart_4_vis(med_vis$prior_b_ASD0,med_vis$b_ASD0,med_vis$b_ASD1)
pp_b_visit <- pp_take_apart_4_vis(med_vis$`prior_b_ASD0:Visit`,med_vis$`b_ASD0:Visit`,med_vis$`b_ASD1:Visit`)
pp_b_soc <- pp_take_apart_4_vis(med_vis$`prior_b_ASD0:Socialization`,med_vis$`b_ASD0:Socialization`,med_vis$`b_ASD1:Socialization`)
pp_b_mot <- pp_take_apart_4_vis(med_vis$`prior_b_ASD0:MotorSkills`,med_vis$`b_ASD0:MotorSkills`,med_vis$`b_ASD1:MotorSkills`)
pp_b_lan <- pp_take_apart_4_vis(med_vis$`prior_b_ASD0:CHI_MLU`,med_vis$`b_ASD0:CHI_MLU`,med_vis$`b_ASD1:CHI_MLU`)

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
  pp_b_soc %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  pp_b_mot %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  pp_b_lan %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  top = textGrob("Prior-posterior update checks, PauseCount (log)",gp=gpar(fontsize=15,font=3))
)

save_it(pp_1,"../plots/P_count/step_2_PP_pcount.pdf")

######## check prior posterior updates for group level median fqr

pp_bg_int <- pp_take_apart_4_vis(med_vis$`prior_sd_Participant__Intercept:ASD0`,
                                 med_vis$`sd_Participant__Intercept:ASD0`,
                               med_vis$`sd_Participant__Intercept:ASD1`)
pp_bg_visit <- pp_take_apart_4_vis(med_vis$`prior_sd_Participant__Visit:ASD0`,
                                  med_vis$`sd_Participant__Visit:ASD0`,
                                 med_vis$`sd_Participant__Visit:ASD1`)
pp_bg_soc <- pp_take_apart_4_vis(med_vis$`prior_sd_Participant__Socialization:ASD0`,
                                 med_vis$`sd_Participant__Socialization:ASD0`,
                               med_vis$`sd_Participant__Socialization:ASD1`)
pp_bg_mot <- pp_take_apart_4_vis(med_vis$`prior_sd_Participant__MotorSkills:ASD0`,
                                 med_vis$`sd_Participant__MotorSkills:ASD0`,
                               med_vis$`sd_Participant__MotorSkills:ASD1`)
pp_bg_lan <- pp_take_apart_4_vis(med_vis$`prior_sd_Participant__CHI_MLU:ASD0`,
                                med_vis$`sd_Participant__CHI_MLU:ASD0`,
                               med_vis$`sd_Participant__CHI_MLU:ASD1`)

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
  pp_bg_soc %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  pp_bg_mot %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  pp_bg_lan %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  top = textGrob("Prior-Posterior, Group-level Effects, Pause Count",gp=gpar(fontsize=15,font=3))
)

save_it(pp_2,"../plots/P_count/step_2_PP_pcount_sd.pdf")

#################
############ betas for sigma
pp_hu_int <- pp_take_apart_4_vis(med_vis$prior_b_hu_ASD0,
                                med_vis$b_hu_ASD0,
                              med_vis$b_hu_ASD1)

pp_hu_visit <- pp_take_apart_4_vis(med_vis$`prior_b_hu_ASD0:Visit`,
                                  med_vis$`b_hu_ASD0:Visit`,
                                med_vis$`b_hu_ASD1:Visit`)
pp_hu_soc <- pp_take_apart_4_vis(med_vis$`prior_b_hu_ASD0:Socialization`,
                                med_vis$`b_hu_ASD0:Socialization`,
                              med_vis$`b_hu_ASD1:Socialization`)
pp_hu_mot <- pp_take_apart_4_vis(med_vis$`prior_b_hu_ASD0:MotorSkills`,
                                med_vis$`b_hu_ASD0:MotorSkills`,
                              med_vis$`b_hu_ASD1:MotorSkills`)
pp_hu_lan <- pp_take_apart_4_vis(med_vis$`prior_b_hu_ASD0:CHI_MLU`,
                                med_vis$`b_hu_ASD0:CHI_MLU`,
                              med_vis$`b_hu_ASD1:CHI_MLU`)

pp_3 <- grid.arrange(
  pp_hu_int %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    theme_classic(),
  pp_hu_visit %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  pp_hu_soc %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  pp_hu_mot %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  pp_hu_lan %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  top = textGrob("Prior-posterior, Population-level effects, Pause Count Hurdle",gp=gpar(fontsize=15,font=3))
)

save_it(pp_3,"../plots/P_count/step_2_PP_pcount_hu.pdf")

###################### Visualize and compare betas
#betas for mu
vis_b_int <- take_apart_4_vis(med_vis$b_ASD0,med_vis$b_ASD1)
vis_b_visit <- take_apart_4_vis(med_vis$`b_ASD0:Visit`,med_vis$`b_ASD1:Visit`)
vis_b_soc <- take_apart_4_vis(med_vis$`b_ASD0:Socialization`,med_vis$`b_ASD1:Socialization`)
vis_b_mot <- take_apart_4_vis(med_vis$`b_ASD0:MotorSkills`,med_vis$`b_ASD1:MotorSkills`)
vis_b_lan <- take_apart_4_vis(med_vis$`b_ASD0:CHI_MLU`,med_vis$`b_ASD1:CHI_MLU`)

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
  vis_b_soc %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  vis_b_mot %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  vis_b_lan %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  top = textGrob("Population-level Effects, Pause Count (log)",gp=gpar(fontsize=15,font=3))
)

save_it(vis_1,"../plots/P_count/step_2_pcount.pdf")

############ betas for group level
vis_bg_int <- take_apart_4_vis(med_vis$`sd_Participant__Intercept:ASD0`,
                               med_vis$`sd_Participant__Intercept:ASD1`)
vis_bg_visit <- take_apart_4_vis(med_vis$`sd_Participant__Visit:ASD0`,
                                 med_vis$`sd_Participant__Visit:ASD1`)
vis_bg_soc <- take_apart_4_vis(med_vis$`sd_Participant__Socialization:ASD0`,
                               med_vis$`sd_Participant__Socialization:ASD1`)
vis_bg_mot <- take_apart_4_vis(med_vis$`sd_Participant__MotorSkills:ASD0`,
                               med_vis$`sd_Participant__MotorSkills:ASD1`)
vis_bg_lan <- take_apart_4_vis(med_vis$`sd_Participant__CHI_MLU:ASD0`,
                               med_vis$`sd_Participant__CHI_MLU:ASD1`)

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
  vis_bg_soc %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  vis_bg_mot %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  vis_bg_lan %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  top = textGrob("Group-level Effects, Pause Count",gp=gpar(fontsize=15,font=3))
)

save_it(vis_2,"../plots/P_count/step_2_pcount_sd.pdf")

############ betas for sigma
vis_hu_int <- take_apart_4_vis(med_vis$b_hu_ASD0,
                              med_vis$b_hu_ASD1)

vis_hu_visit <- take_apart_4_vis(med_vis$`b_hu_ASD0:Visit`,
                                med_vis$`b_hu_ASD1:Visit`)
vis_hu_soc <- take_apart_4_vis(med_vis$`b_hu_ASD0:Socialization`,
                              med_vis$`b_hu_ASD1:Socialization`)
vis_hu_mot <- take_apart_4_vis(med_vis$`b_hu_ASD0:MotorSkills`,
                              med_vis$`b_hu_ASD1:MotorSkills`)
vis_hu_lan <- take_apart_4_vis(med_vis$`b_hu_ASD0:CHI_MLU`,
                              med_vis$`b_hu_ASD1:CHI_MLU`)

vis_3 <- grid.arrange(
  vis_hu_int %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    theme_classic(),
  vis_hu_visit %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  vis_hu_soc %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  vis_hu_mot %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  vis_hu_lan %>%
    ggplot(aes(x=b, fill = gr))+
    geom_density(alpha = .4) +
    geom_vline(xintercept = 0)+
    theme_classic(),
  top = textGrob("Population-level effects, Pause Count HU",gp=gpar(fontsize=15,font=3))
)

save_it(vis_3,"../plots/P_count/step_2_pcount_hu.pdf")
