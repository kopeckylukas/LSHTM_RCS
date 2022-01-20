## HELEN!!! 

#necessary setup
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)

#load the dataset, prepared from CWT-commissioner-time-series by selecting the performance column from 2017 to 2021
#uploaded corresponding datasets to the data folder 
cwt_31d <- read_excel("31days_performance_comm.xlsx")
View(cwt_31d)
colnames(cwt_31d)

#use only complete cases for subsequent analysis 
cwt_31d_complete <- cwt_31d[complete.cases(cwt_31d), ]
View(cwt_31d_complete)
count(cwt_31d_complete, 'CCG code') 
# 73 complete observations - not enough observations? 

# save complete cases for later use 
save(cwt_31d_complete, file = 'cwt_31d_complete.Rdata')

# grouping by months for later use 
Jan <- select(cwt_31d_complete, 
              'jan2017' = '2017_01',
              'jan2018' = '2018_01',
              'jan2019' = '2019_01',
              'jan2020' = '2020_01',
              'jan2021' = '2021_01')

Feb <- select(cwt_31d_complete, 
              'feb2017' = '2017_02',
              'feb2018' = '2018_02',
              'feb2019' = '2019_02',
              'feb2020' = '2020_02',
              'feb2021' = '2021_02')

Mar <- select(cwt_31d_complete, 
              'mar2017' = '2017_03',
              'mar2018' = '2018_03',
              'mar2019' = '2019_03',
              'mar2020' = '2020_03',
              'mar2021' = '2021_03')

Apr <- select(cwt_31d_complete, 
              'apr2017' = '2017_04',
              'apr2018' = '2018_04',
              'apr2019' = '2019_04',
              'apr2020' = '2020_04',
              'apr2021' = '2021_04')

May <- select(cwt_31d_complete, 
              'may2017' = '2017_05',
              'may2018' = '2018_05',
              'may2019' = '2019_05',
              'may2020' = '2020_05',
              'may2021' = '2021_05')

Jun <- select(cwt_31d_complete, 
              'jun2017' = '2017_06',
              'jun2018' = '2018_06',
              'jun2019' = '2019_06',
              'jun2020' = '2020_06',
              'jun2021' = '2021_06')

# grouping by year 
year_2019 <- select(cwt_31d_complete, '2019_01','2019_02','2019_03','2019_04','2019_05','2019_06',
                    '2019_07','2019_08','2019_09','2019_10', '2019_11','2019_12')

year_2020 <- select(cwt_31d_complete, '2020_01','2020_02','2020_03','2020_04','2020_05','2020_06',
                    '2020_07','2020_08','2020_09','2020_10', '2020_11','2020_12')

year_2021 <- select(cwt_31d_complete, '2021_01','2021_02','2021_03','2021_04','2021_05','2021_06',
                    '2021_07','2021_08','2021_09','2021_10', '2021_11')

###############################################################################################
#compare mean perfmances by month across the years just to have a first look 
#can see a roughly 2-3% drop in the months of 2021 
###############################################################################################

#mean performance rate for the month of Jan in each year 
summarise_at(Jan,
             .vars = vars('jan2017', 'jan2018', 'jan2019', 'jan2020', 'jan2021'),
             .funs = mean)
#mean performance rate for the month of Feb in each year 
summarise_at(Feb,
             .vars = vars('feb2017', 'feb2018', 'feb2019', 'feb2020', 'feb2021'),
             .funs = mean)

#mean performance rate for the month of Mar in each year 
summarise_at(Mar,
             .vars = vars('mar2017', 'mar2018', 'mar2019', 'mar2020', 'mar2021'),
             .funs = mean)

#mean performance rate for the month of Apr in each year 
summarise_at(Apr,
             .vars = vars('apr2017', 'apr2018', 'apr2019', 'apr2020', 'apr2021'),
             .funs = mean)

#mean performance rate for the month of May in each year 
summarise_at(May,
             .vars = vars('may2017', 'may2018', 'may2019', 'may2020', 'may2021'),
             .funs = mean)

#mean performance rate for the month of June in each year 
summarise_at(Jun,
             .vars = vars('jun2017', 'jun2018', 'jun2019', 'jun2020', 'jun2021'),
             .funs = mean)


###############################################################################################
#visualization of the trend 
###############################################################################################












