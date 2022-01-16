# Amiiii! 

## Load libraries 
library(tidyverse)
library(ggplot2)


## Load Provider data for 2020-21
library(readxl)
Cancer20_21  <- read_excel("Cancer-Waiting-Times-2020-21-Data-Extract-Provider-Final.xlsx")





## View dataset 
View(Cancer20_21)
summary(Cancer20_21)
count(Cancer20_21, STANDARD)
count(Cancer20_21, `CANCER TYPE`)


 # Notice that we need to consider 
 # different cancer waiting times - "STANDARD"
 # different cancers - "CANCER TYPE"
 # different organizations - "ORG CODE"

## Calculate the Performance rate (WITHIN STANDARD/TOTOAL TREATED)
Cancer20_21$PERFORMANCE <- Cancer20_21$`WITHIN STANDARD`/Cancer20_21$`TOTAL TREATED`


## Count the number of each cancer type and visualise 
count(Cancer20_21, `CANCER TYPE`)
cancer_count <- as.data.frame(table(Cancer20_21$`CANCER TYPE`))

ggplot(data = cancer_count, aes(x = reorder(Var1, Freq), y = Freq)) + 
  labs(title = "Counts of Cancer Types", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip()


## We can see that Breast cancer is a major cancer type in the dataset. 
## Have a closer look into Breast Cancer

Cancer20_21_Breast <- filter(Cancer20_21, `CANCER TYPE` == "Breast")
count(Cancer20_21_Breast, STANDARD)

 # Notice that the STANDARD is only 31 and 62 days now. 


## Separate data frame into 31 and 62 days 
Cancer20_21_Breast_31 <- filter(Cancer20_21_Breast, STANDARD == "31 Days")
Cancer20_21_Breast_62 <- filter(Cancer20_21_Breast, STANDARD == "62 Days")

 # We also notice that each rows are for months and different ORG CODEs 
 # We could combine all the ORG CODEs 


## Firstly we will experimentally look at one of the ORG CODE: R0A, for 31 days
Cancer20_21_Breast_31_R0A <- filter(Cancer20_21_Breast_31, `ORG CODE` == "R0A")

 # There are two rows for the same month
 # Suspect that it is the first half and latter half of the month?


## Create a line plot
ggplot(data = Cancer20_21_Breast_31_R0A, aes(x = PERIOD, y = PERFORMANCE)) +
  labs(title = "Performance rate for ORG CODE = R0A, for period Apr 2020 - Jan 2021") +
  geom_line()

 # Very Experimental plot!! 
  



###################################### Comments ###################################### 
## We can try and extend this by binding the Provider datasets 
## Group them by the ORG codes -> A Map would be really cool 
## Try different cancer types






## Try looking at the other Provider data 
## Select shee "62 day wait -GP referral" 
Cancer_Provider_09_21 <- read_excel("Cancer-Waiting-Times-Provider-Time-Series-Oct-2009-Nov-2021-with-Revisions.xlsx",
                                    sheet = 8)
View(Cancer_Provider_09_21)


 # Very messy idk how to deal with this 


## Select the performance rate for provider RCF and make a dataframe with dates

ncol(Cancer_Provider_09_21)

PERFORMANCE <- t(Cancer_Provider_09_21[4, seq(6, 441, by = 3)])
PERIOD <- seq.Date(from=as.Date("2009-10-01"), to=as.Date("2021-11-01"), by="month")

Cancer_Provider_09_21_RCF <- data.frame(PERIOD, PERFORMANCE)
rownames(Cancer_Provider_09_21_RCF) <- 1:nrow(Cancer_Provider_09_21_RCF)


## Plot a time series plot
 # I gave up doing a ggplot
plot(x = Cancer_Provider_09_21_RCF$PERIOD, y = Cancer_Provider_09_21_RCF$PERFORMANCE, type = "l",
     main = "Time Series for CWT Performance in provider RCF (62 day wait - GP referral)", 
     xlab = "Time", ylab = "Performance Rate (%)")

 # We can see that there is a decrease in performance 
 # Can run a statistical test to see if the mean is different 

## Lets divide the dataset into before and after Covid outbreak -> 11 March 2020 (from WHO)
## From row 127 

Cancer_Provider_09_21_RCF_before <- Cancer_Provider_09_21_RCF[1:126,]
Cancer_Provider_09_21_RCF_after <- Cancer_Provider_09_21_RCF[127:132,]

t.test(Cancer_Provider_09_21_RCF_before$PERFORMANCE, Cancer_Provider_09_21_RCF_after$PERFORMANCE, var.equal = FALSE)
 # OKay doesn't work 


 # Change performance values to numeric
Cancer_Provider_09_21_RCF_before$PERFORMANCE <- as.numeric(Cancer_Provider_09_21_RCF_before$PERFORMANCE)
Cancer_Provider_09_21_RCF_after$PERFORMANCE <- as.numeric(Cancer_Provider_09_21_RCF_after$PERFORMANCE)

## Calculate mean
mean(Cancer_Provider_09_21_RCF_before$PERFORMANCE)
mean(Cancer_Provider_09_21_RCF_after$PERFORMANCE)

## Perform t-test
t.test(Cancer_Provider_09_21_RCF_before$PERFORMANCE, Cancer_Provider_09_21_RCF_after$PERFORMANCE, var.equal = FALSE)

## p-value is 0.8036 -> no evidence to reject that there is no difference in the means
## => There is a difference in the mean of performance before and after covid outbreak 
## It seems that the mean performance is significantly lower after the outbreak

