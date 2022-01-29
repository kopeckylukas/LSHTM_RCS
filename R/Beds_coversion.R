
library(tidyverse)
library(readxl)


### File: Beds 1

b1 <- read_excel("Data/Beds/Beds.xlsx", sheet=1)
b2 <- read_excel("Data/Beds/Beds.xlsx", sheet=2)
b3 <- read_excel("Data/Beds/Beds.xlsx", sheet=3) 

beds <- rbind(b1, b2)
beds <- rbind(beds, b3)

beds$South_of_England <- beds$`South East` + beds$`South West`
beds$North_of_England <- beds$`North East and Yorkshire` + beds$`North West`
beds$Midlands_and_East_of_England <- beds$Midlands + beds$`East of England`

colnames(beds)

beds <- beds[,c(1,2,4,10:12)]

colnames(beds)

colnames(beds)[1] <- "Date"
colnames(beds)[2] <- "England"

colSums(is.na(beds))

write_csv(beds,"Data/Beds_regions.csv",na="")
