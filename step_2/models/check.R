#check
list.files()

files <- list.files()

files <- files [2:6]

for ( i in files){
  
  mod <- readRDS(i)
  print(summary(mod))
  
}

