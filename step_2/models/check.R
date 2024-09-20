#check
list.files()

files <- list.files()

files <- files [2:6]

for ( i in files){
  
  mod <- readRDS(i)
  print(summary(mod))
  
}


data <- read_csv("../../data/clean_data_mlu_correct_adj_dur.csv")

#no imbalance
data %>% 
  group_by(ASD, Visit) %>% 
  reframe(n())

#
data %>% 
  na.omit() %>% 
  group_by(ASD, Visit) %>% 
  reframe(n())
