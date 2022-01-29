## Commissioner Data Merged
library(tidyverse)

#prepare path
temp = list.files(path = "Data/com_done",pattern="*.csv",full.names = TRUE)

#read data from all files
com_dat <- read_csv(temp)

com_dat$performance <-  com_dat$within_standard / com_dat$total_treated 


colSums(is.na(com_dat))

com_dat <- na.omit(com_dat)

colnames(com_dat)

com_dat <- com_dat[,c(9,1,2,8,3:7)]

write_csv(com_dat,"Data/commissioner_level_data.csv",na="")

## Load All Files as with their names
#for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
  
  

  
  
