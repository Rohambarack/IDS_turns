library(tidyverse)
#metadata
md <- read_csv("./raw_data/USEnglishSummaryData.csv")

md_df <- md %>%
  select( !IDS_SD) %>%
  select( !ADS_SD) %>% 
  pivot_longer(cols = c("IDS_mean","ADS_mean"),
               names_to = "type",
               values_to = "mean") %>% 
  mutate(type = substr(type, start = 1, stop = 3))

md_2 <- md %>%
  select( !IDS_mean) %>%
  select( !ADS_mean) %>% 
  pivot_longer(cols = c("IDS_SD","ADS_SD"),
               names_to = "type",
               values_to = "SD") %>% 
  mutate(type = substr(type, start = 1, stop = 3))   


md_df$SD <- md_2$SD         
##############################################
#Uconn corpus

#Age2 variable was used, mainly consistent with Age small
#deviations probably because of rounding


age <- read_csv("./raw_data/TurnTakingData_f0.csv")


age <- age %>% 
  select(Participant, Visit, Age2) 

age <- age %>% 
  group_by(Visit, Participant) %>% 
  reframe( AGE = max(Age2)) 

###############################
#missing age problems:
# - missing value
# - rounding errors, missing last digit
#################
#solution
#if length(age) < 4, add zeros until length(age == 4)

ages <- age$AGE
#loop through ages
for (i in ages){
  #leave Nas for now
  if (is.na(i)){
    
    i = i
  }else{
    #add zeros until it has 4 digits
  
    while (i < 1000) {
      i = i*10
    }
  
  }
  
  #make list
  if(exists("new_ages")){
    
    new_ages[[length(new_ages)+1]] = i
    
  }else{
    new_ages <- list(i)
  }
}

age$AGE_f <- as.integer(new_ages)

#########################
#now nas

#look through each participant
names <- unique(age$Participant)
for (ID in names){
  
  ID_df <- age %>% 
    filter(Participant == ID)
  
  #No nas
  if (all(!is.na(ID_df$AGE_f))) {
    

    
  #only Nas  
  }else if (all(is.na(ID_df$AGE_f))) {
    
    
  #partial missing values
  }else{
    
    #find NA, make it previous visit + 4 months
    missing_age_index <- which(is.na(ID_df$AGE_f))
    print(missing_age_index)
    
    ID_df$AGE_f[missing_age_index] <- ID_df$AGE_f[(missing_age_index-1)] + 400
    
  }
  
  
  #make list
  if(exists("Missing_fix_list")){
    
    Missing_fix_list[[length(Missing_fix_list)+1]] = ID_df
    
  }else{
    Missing_fix_list <- list(ID_df)
  }
  
} 


library(data.table)
ages_c <- rbindlist(Missing_fix_list)

ages_c <- ages_c %>% 
  mutate(Age_months = AGE_f/100) %>% 
  select(Participant,Visit,Age_months)

##############################
### Clean data

cd <- read_csv("../data/clean_data_mlu_correct_adj_dur.csv")

##############################################
#merge clean data and age

cd_age <- merge(cd,ages_c, by = c("Participant","Visit"), all.x = T)

##############################################


write_csv(cd_age,"../data/clean_data_age.csv")


###############################################
#plot

cd_sum <- cd_age %>%
  filter(Interlocutor == "Parent")%>% 
  group_by(Participant, Visit,) %>% 
  reframe(ASD = max(ASD),
          medF0 = mean(na.omit(median_f0)),
          Age_months = max(Age_months)) %>% 
  mutate( type = ifelse(ASD == 1, "Uconn_ASD", "Uconn_NT")) %>% 
  pivot_longer(cols = c("medF0"),
               names_to = "Parameter",
               values_to = "mean")
  


corpus <- cd_sum %>% 
  select(Age_months,Parameter,type,mean)
  
md_test <- md_df %>% 
  select(Age_months,Parameter,type,mean)

test <- rbind(corpus,md_test)

test %>% 
  filter(Parameter == "medF0") %>% 
  ggplot(aes(x = Age_months, y = mean, group = type, colour = type)) +
  geom_point(alpha = .8) +
  geom_smooth(method = "lm",alpha = .5) + 
  #geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD), alpha = .5) +
  #facet_wrap(~Parameter,scales = "free") +
  theme_classic()





