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
#load the dataset from github 
#should be the one and only dataset needed for all analysis 
install.packages("dygraphs")
install.packages("zoo")

library(readr)
library(xlsx)
library(dygraphs)
library(xts)

urlfile = "https://raw.githubusercontent.com/kopeckylukas/LSHTM_RCS/main/Data/provider_level_data.csv"
complete_data <- read_csv(urlfile)

#setup 
library(tidyverse)

#looking at the average for all providers, grouped by months 
#summary statistics for performance and total_treated 
perform_months <- group_by(complete_data, period)
mean_perform <- summarise(perform_months, 
                          mean = mean(performance),
                          sd = sd(performance))

mean_number <- summarise(perform_months, 
                         mean = mean(total_treated),
                         sd = sd(total_treated))


#visualization of the above summaries 
ggplot(data = mean_perform, 
       aes(x = period,
           y = mean)) + 
  geom_point() + 
  geom_smooth(method = NULL, color = 'red', se = TRUE)
#can see an obvious change starting from 2018 and reach the lowest in the end of 2021

ggplot(data = mean_number, 
       aes(x = period,
           y = mean)) + 
  geom_point() + 
  geom_smooth(method = NULL, color = 'red', se = TRUE)
#slow rise until mid-2020, started to increase rapidly 
#line fit looks like exponential growth 
#questionable, is it simply the rebound effect? 

#compare the two summaries month by month 
#need to reformat the performance data first, and then convert to dataframe 
months <- c(seq(1,12,1))


mean_perform_wider <- pivot_wider(data = mean_perform[,c("period","mean")], 
                                  names_from = period, 
                                  values_from = mean)

perf_2016 <- c(mean_perform_wider$"2016-01-01",
               mean_perform_wider$"2016-02-01",
               mean_perform_wider$"2016-03-01", 
               mean_perform_wider$"2016-04-01", 
               mean_perform_wider$"2016-05-01", 
               mean_perform_wider$"2016-06-01", 
               mean_perform_wider$"2016-07-01", 
               mean_perform_wider$"2016-08-01", 
               mean_perform_wider$"2016-09-01", 
               mean_perform_wider$"2016-10-01", 
               mean_perform_wider$"2016-11-01", 
               mean_perform_wider$"2016-12-01")

perf_2017 <- c(mean_perform_wider$"2017-01-01",
               mean_perform_wider$"2017-02-01",
               mean_perform_wider$"2017-03-01", 
               mean_perform_wider$"2017-04-01", 
               mean_perform_wider$"2017-05-01", 
               mean_perform_wider$"2017-06-01", 
               mean_perform_wider$"2017-07-01", 
               mean_perform_wider$"2017-08-01", 
               mean_perform_wider$"2017-09-01", 
               mean_perform_wider$"2017-10-01", 
               mean_perform_wider$"2017-11-01", 
               mean_perform_wider$"2017-12-01")

perf_2018 <- c(mean_perform_wider$"2018-01-01",
               mean_perform_wider$"2018-02-01",
               mean_perform_wider$"2018-03-01", 
               mean_perform_wider$"2018-04-01", 
               mean_perform_wider$"2018-05-01", 
               mean_perform_wider$"2018-06-01", 
               mean_perform_wider$"2018-07-01", 
               mean_perform_wider$"2018-08-01", 
               mean_perform_wider$"2018-09-01", 
               mean_perform_wider$"2018-10-01", 
               mean_perform_wider$"2018-11-01", 
               mean_perform_wider$"2018-12-01")

perf_2019 <- c(mean_perform_wider$"2019-01-01",
               mean_perform_wider$"2019-02-01",
               mean_perform_wider$"2019-03-01", 
               mean_perform_wider$"2019-04-01", 
               mean_perform_wider$"2019-05-01", 
               mean_perform_wider$"2019-06-01", 
               mean_perform_wider$"2019-07-01", 
               mean_perform_wider$"2019-08-01", 
               mean_perform_wider$"2019-09-01", 
               mean_perform_wider$"2019-10-01", 
               mean_perform_wider$"2019-11-01", 
               mean_perform_wider$"2019-12-01")

perf_2020 <- c(mean_perform_wider$"2020-01-01",
               mean_perform_wider$"2020-02-01",
               mean_perform_wider$"2020-03-01", 
               mean_perform_wider$"2020-04-01", 
               mean_perform_wider$"2020-05-01", 
               mean_perform_wider$"2020-06-01", 
               mean_perform_wider$"2020-07-01", 
               mean_perform_wider$"2020-08-01", 
               mean_perform_wider$"2020-09-01", 
               mean_perform_wider$"2020-10-01", 
               mean_perform_wider$"2020-11-01", 
               mean_perform_wider$"2020-12-01")

perf_2021 <- c(mean_perform_wider$"2021-01-01",
               mean_perform_wider$"2021-02-01",
               mean_perform_wider$"2021-03-01", 
               mean_perform_wider$"2021-04-01", 
               mean_perform_wider$"2021-05-01", 
               mean_perform_wider$"2021-06-01", 
               mean_perform_wider$"2021-07-01", 
               mean_perform_wider$"2021-08-01", 
               mean_perform_wider$"2021-09-01", 
               mean_perform_wider$"2021-10-01", 
               mean_perform_wider$"2021-11-01", 
               mean_perform_wider$"2021-11-01") #filled in Dec2021 entry with Nov2021 data as a reference

perform_reorg <- cbind(months,perf_2016, perf_2017, perf_2018, perf_2019, perf_2020, perf_2021)
perform_reorg <- as.data.frame(perform_reorg)

#have 6 lines in one line plot, each representing changes in each year 
#interactive plot, use the range selector below the plot to zoom in or out 
#year of 2021 seems to have the worst performance over all 12 months  
dygraph(perform_reorg, main = "Performance from 2016 to 2021") %>%
  dyRangeSelector() %>%
  dyAxis("x", label = "Months") %>%
  dyAxis("y", label = "performance(%)") %>%
  dySeries("perf_2016", label = "2016") %>%
  dySeries("perf_2017", label = "2017") %>%
  dySeries("perf_2018", label = "2018") %>%
  dySeries("perf_2019", label = "2019", strokeWidth = 2.5) %>%
  dySeries("perf_2020", label = "2020", strokeWidth = 2.5) %>%
  dySeries("perf_2021", label = "2021", strokeWidth = 2.5) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2) %>%
  dyHighlight(highlightSeriesBackgroundAlpha = 0.2)

#choose the 2019 and 2020 data to compare, like the NPCA report 
year_19_20 <- select(perform_reorg, months, perf_2019, perf_2020)
dygraph(year_19_20, main = "Perfomance Comparison between 2019 and 2020") %>%
  dyRangeSelector() %>%
  dyAxis("x", label = "Months") %>%
  dyAxis("y", label = "performance(%)") %>%
  dySeries("perf_2019", label = "2019") %>%
  dySeries("perf_2020", label = "2020") %>% 
  dyOptions(drawPoints = TRUE, pointSize = 2) %>%
  dyHighlight(highlightSeriesBackgroundAlpha = 0.2)
#in March, the performance in 2020 is slightly higher, possibly indicates a delayed effect of COVID 
#judging by the plot, May is when the performance rate dropped by the most 
#a recover starting in June (quarantine lifted in June in England)











