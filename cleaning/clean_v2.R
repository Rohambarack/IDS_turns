### clean the data

#libraries:
library(tidyverse)
library(stringr)



###load data
setwd(".")
df_art <- read_csv("C:./raw_data/TurnTakingData_articulation.csv")
df_f0 <- read_csv("C:./raw_data/TurnTakingData_f0.csv")


##### clean data ##############################
### Check the two dataframes
all.equal(df_art[,1:56],df_f0[,1:56])
### all is good until column 57
df_57 <- df_art[,57:60]

df_full <- cbind(df_f0,df_57)

##### needed columns 
df_c_1 <- df_full %>% 
  select(Participant, 
         Visit, 
         ASD, 
         Interlocutor, 
         median_f0, 
         iqr_f0, 
         ArticulationRate, 
         PauseCount,
         PauseDuration,
         DurationSec,
         Socialization, 
         MotorSkills )

##################

# MLU

##################
mlu <- read_delim(delim = ";","C:./raw_data/MLUv2.csv")

#clean it up
#participant : regex to extract everything before first dot
#V extract everything between the two dots
#replace non digits with nothing
mlu_c_1 <- mlu %>% 
  mutate( Participant = str_extract(mlu$`File-name`, "^[^.]*+(?=\\.)"),
          V =  str_extract(mlu$`File-name`,"(?<=\\.).*?(?=\\.)"),
          Visit = as.integer(gsub("\\D","",V)))

mlu_c_2 <- mlu_c_1 %>% 
  rename(CHI_MLU=`CHI-MLU`) %>% 
  select(Participant, Visit, CHI_MLU)
###################################

### Data mismatches between sets

###################################
#MLU and rest of the data mismatch
#interlocutor nas, also  happen with durint visits.... can't  simply remove all
n1 <- sort(unique(df_c_1$Participant))
n2 <- sort(unique(mlu_c_2$Participant))
all.equal(n1,n2)

#find participants from the data, which have no MLU
#most are NA filled and will be removed, except JG2
na_list <- df_c_1 %>% 
  filter(!Participant %in% n2) %>% 
  select(Participant) %>% 
  unique()
na_list <- na_list[c(1:5,7:length(na_list$Participant)),]

##remove IDS with no values and mlus

df_c_2 <- df_c_1 %>% 
  filter(!Participant %in% na_list)
##################################################

######### add turn numbers

#################################################

count_turns <- function(df,id,vis){
  
  df_fin <- df %>% 
    filter(Participant == id,
           Visit == vis)
  #count rows (turns) if visit is not missing
  if (length(df_fin[,1])>1){
    
    df_fin$Turn <- seq(1:length(df_fin$Participant))
    
  } else if (length(df_fin[,1])==1){
    #if visit is notes as 1 line of NAs
    df_fin$Turn <- NA
    
  } else {
    #if visit is completely missing from df
    df_fin <- tibble( Participant = id,
                      Visit = vis,
                      Turn = NA)
    
  }
  
  
  
  
  
  return(df_fin)
  
}

####################

#add turn numbering
for (participant in unique(df_c_2$Participant)){
  for (visit in unique(df_c_2$Visit)){
    
    if (participant == unique(df_c_2$Participant)[1] &
        visit ==  unique(df_c_2$Visit)[1]){
      #separate by id and visit
      df_fin <- count_turns(df_c_2,participant,visit)
    } else {
      
      #separate by id and visit
      df_temp <- count_turns(df_c_2,participant,visit)
      
      
      df_fin<- plyr::rbind.fill(df_fin,df_temp)
      
    }
    
  }
}

################

#remove missing visits
df_c_3 <- df_fin %>% 
  filter(!is.na(Turn))

################
# note original order before mergint ot keep turns
df_c_4 <- df_c_3 %>% 
  mutate(ord = as.integer(rownames(.)))

df_mlu_merged <- merge(df_c_4,mlu_c_2, by = c("Participant","Visit"), all.x = T)

#reorder by original turns
df_mlu_merged <- df_mlu_merged[order(df_mlu_merged$ord), ]

#save
write_csv(df_mlu_merged,"../data/clean_data_mlu_correct_adj_dur.csv")
