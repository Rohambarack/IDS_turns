library(tidyverse)
library(caret)
#read in data
d <- read_csv("../data/clean_data_mlu_correct_adj_dur.csv")


## NA 0s from articulation rate and iqr, because probably they are noise
d <- d %>% 
  mutate( iqr_f0 = ifelse(iqr_f0 == 0 | ArticulationRate == 0,
                          NA,iqr_f0),
          median_f0 = ifelse(iqr_f0 == 0 | ArticulationRate == 0,
                             NA,median_f0),
          ArticulationRate= ifelse(iqr_f0 == 0 | ArticulationRate == 0,
                                   NA,ArticulationRate),
          PauseCount = ifelse(iqr_f0 == 0 | ArticulationRate == 0,
                              NA,PauseCount),
          PauseDuration = ifelse(iqr_f0 == 0 | ArticulationRate == 0,
                                 NA,PauseDuration)
  )


#separate by participant and visit
participants <- unique(d$Participant)
visits <- unique(d$Visit)

#counter for appending separated visits and participants again
participant_counter <- 1

for (i in participants){
  
  #counter for appending separated visits and participants again
  visit_counter <- 1
  
  for (j in visits){
    
    d_1 <- d %>% 
      filter(Participant == i) %>% 
      filter(Visit == j)
    
    #fix, skip missing visits
    if (nrow(d_1) == 0){
      
      next
      
    } else {
      
      #because of the structure, next own turn is lead(x,2) away
      d_1 <- d_1 %>% 
        mutate(Ar_t1_same = lead(ArticulationRate,2),
               Median_t1_same = lead(median_f0,2),
               Iqr_t1_same = lead(iqr_f0,2),
               Pc_t1_same = lead(PauseCount,2),
               Pd_t1_same = lead(PauseDuration,2),
               #others is lead(x,1) away
               Ar_t_other = lead(ArticulationRate,1),
               Median_t_other = lead(median_f0,1),
               Iqr_t_other = lead(iqr_f0,1),
               Pc_t_other = lead(PauseCount,1),
               Pd_t_other = lead(PauseDuration,1),
               #rename for consistency
               Ar_t_same = ArticulationRate,
               Median_t_same = median_f0,
               Iqr_t_same = iqr_f0,
               Pc_t_same = PauseCount,
               Pd_t_same = PauseDuration
               ) 
      
      # Separate children and parent
      
      d_child <- d_1 %>% 
        filter(Interlocutor == "Child")
      d_parent <- d_1 %>% 
        filter(Interlocutor == "Parent")
      
      ###########normalize ?
      
      
      #children
      norm_param_child <- preProcess(d_child[,c("ArticulationRate",
                                                "median_f0",
                                                "iqr_f0",
                                                "PauseCount",
                                                "PauseDuration")])
      
      norm_child <- predict(norm_param_child, d_child)
      
      #parents
      norm_param_parent <- preProcess(d_parent[,c("ArticulationRate",
                                                "median_f0",
                                                "iqr_f0",
                                                "PauseCount",
                                                "PauseDuration")])
      
      norm_parent <- predict(norm_param_parent, d_parent)
      
      #join them again
      norm_full <- rbind(norm_child,norm_parent)
      #order them back
      norm_full <- norm_full %>% 
        arrange(Turn)
      
      #do the time shifting again
      
      norm_full <- norm_full %>% 
        mutate(Norm_Ar_t1_same = lead(ArticulationRate,2),
               Norm_Median_t1_same = lead(median_f0,2),
               Norm_Iqr_t1_same = lead(iqr_f0,2),
               Norm_Pc_t1_same = lead(PauseCount,2),
               Norm_Pd_t1_same = lead(PauseDuration,2),
               #others is lead(x,1) away
               Norm_Ar_t_other = lead(ArticulationRate,1),
               Norm_Median_t_other = lead(median_f0,1),
               Norm_Iqr_t_other = lead(iqr_f0,1),
               Norm_Pc_t_other = lead(PauseCount,1),
               Norm_Pd_t_other = lead(PauseDuration,1)
               
               
        ) 
      
      ################# append visits back together
      
      if (visit_counter ==  1){
        
        fin_visits <- norm_full
        
      } else {
        
        temp_visits <- norm_full
        
        fin_visits <- rbind(fin_visits, temp_visits)
        
      }
    }
  
    visit_counter <- visit_counter + 1
  }
  
  #####append participants back together
  
  if(participant_counter == 1){
    
    fin_participant <- fin_visits
    
  }else{
    
    temp_participant <- fin_visits
    
    fin_participant <- rbind(fin_participant, temp_participant)
  }
  
  participant_counter <- participant_counter + 1
  
  
}


write.csv(fin_participant,"../data/data_turn_shifts.csv")
