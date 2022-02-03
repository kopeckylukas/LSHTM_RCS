## HELEN!!! 

########################################################################################################################################################
#updates from Feb 2nd
########################################################################################################################################################

#try putting 2020 and 2021 changes in one plot 
changes_20 <- add_column(changes_20, year = "2020")
changes_21 <- add_column(changes_21, year = "2021")
all_changes <- rbind(changes_20, changes_21)

all_changes %>% ggplot(aes(x = period, 
                           y = change, 
                           fill = year)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) + 
  scale_fill_manual(values = c("#a6bddb", "#fdbb84")) +
  geom_text(aes(label = change), vj) + 
  labs(x = "months", 
       y = "changes in performance (percentage)", 
       title = "Performance Rate Changes in 2020 and 2021 (compared to same period in 2019") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))

#########################################################################################################
#lung cancer and breast cancer 
#just "Lung" and "Breast", not including "Suspected lung cancer" or "Suspected Breast Cancer"

lung<- filter(selected_data, cancer_type == "Lung")

breast <- filter(selected_data, cancer_type == "Breast")

grouped_lung <- lung%>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_lung$performance_avg <- grouped_lung$sum_within / grouped_lung$sum_treated


grouped_breast <- breast %>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_breast$performance_avg <- grouped_breast$sum_within / grouped_breast$sum_treated

lung_19 <- filter(grouped_lung, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
lung_20 <- filter(grouped_lung, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
lung_21 <- filter(grouped_lung, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))
lung_21 <- add_row(lung_21, period =as.Date("2021-12-01"), sum_treated=3916, sum_within=3420, performance_avg=0.873)


breast_19 <- filter(grouped_breast, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
breast_20 <- filter(grouped_breast, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
breast_21 <- filter(grouped_breast, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))
breast_21 <- add_row(breast_21, period =as.Date("2021-12-01"), sum_treated=6657, sum_within=5919, performance_avg=0.889)

#2020 vs 2019 
changes_lung_20 <- 100*round((lung_20$performance_avg - lung_19$performance_avg),3)

changes_lung_20 <- data.frame(period = months, 
                              change = changes_lung_20)

changes_breast_20 <- 100*round((breast_20$performance_avg - breast_19$performance_avg),3)

changes_breast_20 <- data.frame(period = months, 
                                change = changes_breast_20)

changes_lung_20 <- add_column(changes_lung_20, type = "Lung")
changes_breast_20 <- add_column(changes_breast_20, type = "Breast")
lung_breast_20 <- rbind(changes_lung_20, changes_breast_20)

lung_breast_20 %>% ggplot(aes(x = period, 
                           y = change, 
                           fill = type)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) + 
  scale_fill_manual(values = c("#DF65B0", "#3690C0")) +
  geom_text(aes(label = change)) + 
  labs(x = "months", 
       y = "changes in performance (percentage)", 
       title = "Performance Rate Changes in Lung and Breast Cancer, 2020 (compared to same period in 2019") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))


#2021 vs 2019
changes_lung_21 <- 100*round((lung_21$performance_avg - lung_19$performance_avg),3)

changes_lung_21 <- data.frame(period = months, 
                              change = changes_lung_21)

changes_breast_21 <- 100*round((breast_21$performance_avg - breast_19$performance_avg),3)

changes_breast_21 <- data.frame(period = months, 
                                change = changes_breast_21)

changes_lung_21 <- add_column(changes_lung_21, type = "Lung")
changes_breast_21 <- add_column(changes_breast_21, type = "Breast")
lung_breast_21 <- rbind(changes_lung_21, changes_breast_21)

lung_breast_21 %>% ggplot(aes(x = period, 
                              y = change, 
                              fill = type)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) + 
  scale_fill_manual(values = c("#DF65B0", "#3690C0")) +
  geom_text(aes(label = change)) + 
  labs(x = "months", 
       y = "changes in performance (percentage)", 
       title = "Performance Rate Changes in Lung and Breast Cancer, 2021 (compared to same period in 2019") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))

###############################################################################################################
#Lung by 31D and 62D standards 

lung_31<- filter(selected_data, cancer_type == "Lung" & standard == "31 Days")
lung_62<- filter(selected_data, cancer_type == "Lung"& standard == "62 Days")

grouped_lung_31 <- lung_31%>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_lung_31$performance_avg <- grouped_lung_31$sum_within / grouped_lung_31$sum_treated

grouped_lung_62 <- lung_62%>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_lung_62$performance_avg <- grouped_lung_62$sum_within / grouped_lung_62$sum_treated

lung_31_19 <- filter(grouped_lung_31, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
lung_31_20 <- filter(grouped_lung_31, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
lung_31_21 <- filter(grouped_lung_31, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))
lung_31_21 <- add_row(lung_31_21, period =as.Date("2021-12-01"), sum_treated=2985, sum_within=2870, performance_avg=0.961)

lung_62_19 <- filter(grouped_lung_62, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
lung_62_20 <- filter(grouped_lung_62, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
lung_62_21 <- filter(grouped_lung_62, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))
lung_62_21 <- add_row(lung_62_21, period =as.Date("2021-12-01"), sum_treated=931, sum_within=550, performance_avg=0.591)

#2020 vs 2019 
changes_lung_31_20 <- 100*round((lung_31_20$performance_avg - lung_31_19$performance_avg),3)

changes_lung_31_20 <- data.frame(period = months, 
                              change = changes_lung_31_20)

changes_lung_62_20 <- 100*round((lung_62_20$performance_avg - lung_62_19$performance_avg),3)

changes_lung_62_20 <- data.frame(period = months, 
                                 change = changes_lung_62_20)

changes_lung_31_20 <- add_column(changes_lung_31_20, standard = "31 Days")
changes_lung_62_20 <- add_column(changes_lung_62_20, standard = "62 Days")
lung_31d62d_20 <- rbind(changes_lung_31_20, changes_lung_62_20)

lung_31d62d_20 %>% ggplot(aes(x = period, 
                              y = change, 
                              fill = standard)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) + 
  scale_fill_manual(values = c("#bcbddc", "#756bb1")) +
  geom_text(aes(label = change)) + 
  labs(x = "months", 
       y = "changes in performance (percentage)", 
       title = "Performance Rate Changes in Lung Cancer by Standards, 2020 (compared to same period in 2019") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))


#2021 vs 2019 
changes_lung_31_21 <- 100*round((lung_31_21$performance_avg - lung_31_19$performance_avg),3)

changes_lung_31_21 <- data.frame(period = months, 
                                 change = changes_lung_31_21)

changes_lung_62_21 <- 100*round((lung_62_21$performance_avg - lung_62_19$performance_avg),3)

changes_lung_62_21 <- data.frame(period = months, 
                                 change = changes_lung_62_21)

changes_lung_31_21 <- add_column(changes_lung_31_21, standard = "31 Days")
changes_lung_62_21 <- add_column(changes_lung_62_21, standard = "62 Days")
lung_31d62d_21 <- rbind(changes_lung_31_21, changes_lung_62_21)

lung_31d62d_21 %>% ggplot(aes(x = period, 
                              y = change, 
                              fill = standard)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) + 
  scale_fill_manual(values = c("#bcbddc", "#756bb1")) +
  geom_text(aes(label = change)) + 
  labs(x = "months", 
       y = "changes in performance (percentage)", 
       title = "Performance Rate Changes in Lung Cancer by Standards, 2021 (compared to same period in 2019") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))



################################################################################################
#breast by 31D and 62D standards 

breast_31<- filter(selected_data, cancer_type == "Breast" & standard == "31 Days")
breast_62<- filter(selected_data, cancer_type == "Breast"& standard == "62 Days")

grouped_breast_31 <- breast_31%>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_breast_31$performance_avg <- grouped_breast_31$sum_within / grouped_breast_31$sum_treated

grouped_breast_62 <- breast_62%>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_breast_62$performance_avg <- grouped_breast_62$sum_within / grouped_breast_62$sum_treated

breast_31_19 <- filter(grouped_breast_31, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
breast_31_20 <- filter(grouped_breast_31, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
breast_31_21 <- filter(grouped_breast_31, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))
breast_31_21 <- add_row(breast_31_21, period =as.Date("2021-12-01"), sum_treated=4439, sum_within=4147, performance_avg=0.934)

breast_62_19 <- filter(grouped_breast_62, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
breast_62_20 <- filter(grouped_breast_62, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
breast_62_21 <- filter(grouped_breast_62, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))
breast_62_21 <- add_row(breast_62_21, period =as.Date("2021-12-01"), sum_treated=2218, sum_within=1772, performance_avg=0.799)

#2020 vs 2019 
changes_breast_31_20 <- 100*round((breast_31_20$performance_avg - breast_31_19$performance_avg),3)

changes_breast_31_20 <- data.frame(period = months, 
                                 change = changes_breast_31_20)

changes_breast_62_20 <- 100*round((breast_62_20$performance_avg - breast_62_19$performance_avg),3)

changes_breast_62_20 <- data.frame(period = months, 
                                 change = changes_breast_62_20)

changes_breast_31_20 <- add_column(changes_breast_31_20, standard = "31 Days")
changes_breast_62_20 <- add_column(changes_breast_62_20, standard = "62 Days")
breast_31d62d_20 <- rbind(changes_breast_31_20, changes_breast_62_20)

breast_31d62d_20 %>% ggplot(aes(x = period, 
                              y = change, 
                              fill = standard)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) + 
  scale_fill_manual(values = c("#bcbddc", "#756bb1")) +
  geom_text(aes(label = change)) + 
  labs(x = "months", 
       y = "changes in performance (percentage)", 
       title = "Performance Rate Changes in Breast Cancer by Standards, 2020 (compared to same period in 2019") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))


#2021 vs 2019 
changes_breast_31_21 <- 100*round((breast_31_21$performance_avg - breast_31_19$performance_avg),3)

changes_breast_31_21 <- data.frame(period = months, 
                                 change = changes_breast_31_21)

changes_breast_62_21 <- 100*round((breast_62_21$performance_avg - breast_62_19$performance_avg),3)

changes_breast_62_21 <- data.frame(period = months, 
                                 change = changes_breast_62_21)

changes_breast_31_21 <- add_column(changes_breast_31_21, standard = "31 Days")
changes_breast_62_21 <- add_column(changes_breast_62_21, standard = "62 Days")
breast_31d62d_21 <- rbind(changes_breast_31_21, changes_breast_62_21)

breast_31d62d_21 %>% ggplot(aes(x = period, 
                              y = change, 
                              fill = standard)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) + 
  scale_fill_manual(values = c("#bcbddc", "#756bb1")) +
  geom_text(aes(label = change)) + 
  labs(x = "months", 
       y = "changes in performance (percentage)", 
       title = "Performance Rate Changes in Breast Cancer by Standards, 2021 (compared to same period in 2019") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))



###################################################################################################
#2WW standard, Suspected lung and breast cancer 

suspected_lung<- filter(selected_data, cancer_type == "Suspected lung cancer" & standard == "2WW")
suspected_breast<- filter(selected_data, cancer_type == "Suspected breast cancer" & standard == "2WW")

grouped_suspected_lung <- suspected_lung%>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_suspected_lung$performance_avg <- grouped_suspected_lung$sum_within / grouped_suspected_lung$sum_treated

grouped_suspected_breast <- suspected_breast%>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_suspected_breast$performance_avg <- grouped_suspected_breast$sum_within / grouped_suspected_breast$sum_treated

suspected_lung_19 <- filter(grouped_suspected_lung, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
suspected_lung_20 <- filter(grouped_suspected_lung, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
suspected_lung_21 <- filter(grouped_suspected_lung, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))
suspected_lung_21 <- add_row(suspected_lung_21, period =as.Date("2021-12-01"), sum_treated=5317, sum_within=4848, performance_avg=0.912)

suspected_breast_19 <- filter(grouped_suspected_breast, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
suspected_breast_20 <- filter(grouped_suspected_breast, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
suspected_breast_21 <- filter(grouped_suspected_breast, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))
suspected_breast_21 <- add_row(suspected_breast_21, period =as.Date("2021-12-01"), sum_treated=49143, sum_within=25439, performance_avg=0.518)


#2020 vs 2019 
changes_suspected_lung_20 <- 100*round((suspected_lung_20$performance_avg - suspected_lung_19$performance_avg),3)

changes_suspected_lung_20 <- data.frame(period = months, 
                                   change = changes_suspected_lung_20)

changes_suspected_breast_20 <- 100*round((suspected_breast_20$performance_avg -suspected_breast_19$performance_avg),3)

changes_suspected_breast_20 <- data.frame(period = months, 
                                   change = changes_suspected_breast_20)

changes_suspected_lung_20 <- add_column(changes_suspected_lung_20, type = "Suspected lung cancer")
changes_suspected_breast_20 <- add_column(changes_suspected_breast_20, type = "Suspected breast cancer")
changes_suspected_20 <- rbind(changes_suspected_lung_20, changes_suspected_breast_20)

changes_suspected_20 %>% ggplot(aes(x = period, 
                                y = change, 
                                fill = type)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) + 
  scale_fill_manual(values = c("#DF65B0", "#3690C0")) +
  geom_text(aes(label = change)) + 
  labs(x = "months", 
       y = "changes in performance (percentage)", 
       title = "Performance Rate Changes in Suspected Lung and Breast Cancer, 2020 (compared to same period in 2019") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))


#2021 vs 2019 
changes_suspected_lung_21 <- 100*round((suspected_lung_21$performance_avg - suspected_lung_19$performance_avg),3)

changes_suspected_lung_21 <- data.frame(period = months, 
                                        change = changes_suspected_lung_21)

changes_suspected_breast_21 <- 100*round((suspected_breast_21$performance_avg -suspected_breast_19$performance_avg),3)

changes_suspected_breast_21 <- data.frame(period = months, 
                                          change = changes_suspected_breast_21)

changes_suspected_lung_21 <- add_column(changes_suspected_lung_21, type = "Suspected lung cancer")
changes_suspected_breast_21 <- add_column(changes_suspected_breast_21, type = "Suspected breast cancer")
changes_suspected_21 <- rbind(changes_suspected_lung_21, changes_suspected_breast_21)

changes_suspected_20 %>% ggplot(aes(x = period, 
                                    y = change, 
                                    fill = type)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) + 
  scale_fill_manual(values = c("#DF65B0", "#3690C0")) +
  geom_text(aes(label = change)) + 
  labs(x = "months", 
       y = "changes in performance (percentage)", 
       title = "Performance Rate Changes in Suspected Lung and Breast Cancer, 2021 (compared to same period in 2019") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))







########################################################################################################################################################
#updates from Jan 31st
########################################################################################################################################################

########################################################################################################################################################
#MAIN ANALYSIS 
#Quantify changes in 2020 and 2021 using 2019 data as a baseline 
########################################################################################################################################################

#setup
library(tidyverse)
library(ggplot2)

urlfile = "https://raw.githubusercontent.com/kopeckylukas/LSHTM_RCS/main/Data/provider_level_data.csv"
provider_data <- read_csv(urlfile)

#remove overlaps in standards 
unique(provider_data$standard)
#in order to avoid overlaps, I selected only the rows under these three standard: "2WW", "31 Days", "62 Days"
selected_data <- filter(provider_data, 
                        standard == "2WW" | standard == "31 Days" | standard == "62 Days")

unique(selected_data$standard) # 557898 obs vs 705530 obs in provider_data 

#check if there is still a significant increase in cancer cases without overlaps 
selected_data %>% 
  group_by(period) %>% 
  summarise(total_treated = sum(total_treated)) %>%
  ggplot(aes(x = period, 
             y = total_treated)) + geom_line()
#the increase in cancer cases isn't caused by counting overlapping cases 
#checked *new* cancer cases per year and *total* cancer diagnosis per year - no significant increase 
#the only explanation I can come up with is missing records in earlier years 
#one justification for  using only 2019 data as baseline 

#check if performance still fluctuates 
grouped_period <- selected_data %>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_period$performance_avg <- grouped_period$sum_within / grouped_period$sum_treated

grouped_period %>% 
  ggplot(aes(x = period, 
             y = performance_avg)) + geom_line()
#confirming the overall trend of average performance is similar to what Ami has

#closer look at individual performance rate from 2021 
#can confirm the drop in overall trend is caused by inadequate performance even for prevalent cancer types 
View(filter(selected_data, period >= as.Date("2021-01-01")))

#select 2019-2021 daata and separate into three groups by year 
avg_19 <- filter(grouped_period, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
avg_20 <- filter(grouped_period, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
avg_21 <- filter(grouped_period, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))

#visualize changes in 2020 = subtracting the average performance by 2019 data
months <- c(seq(1,12,1))
changes_20 <- round((avg_20$performance_avg - avg_19$performance_avg),3)

changes_20 <- data.frame(period = months, 
                         change = changes_20)
changes_20 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue", ) + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2020 (compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))
#can see the rebound we talked about in May and June 

#visualize changes in 2020 
changes_21 <- round((avg_21$performance_avg - avg_19$performance_avg),3)

changes_21 <- data.frame(period = months, 
                         change = changes_21)
changes_21 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2021 (compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))

#no rebound like what we've seen in the 2020 graph 

#I thought about doing t test on every month to prove the changes in performance rate are real
#but I read something about underlying autocorrelation in time series data 
#in order to properly perform a t test, removing autocorrelation needs to be done first 
#i thought that would be adding more problems to our project so i decided to not do t tests 

#same barplots for individual standard on all cancer types 
#2WW
#2020 vs 2019
stan_2ww <- filter(provider_data, standard == "2WW")

grouped_2ww <- stan_2ww %>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_2ww$performance_avg <- grouped_2ww$sum_within / grouped_2ww$sum_treated          
                   
stan_2ww_19 <- filter(grouped_2ww, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
stan_2ww_20 <- filter(grouped_2ww, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
stan_2ww_21 <- filter(grouped_2ww, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))

changes_2ww_20 <- round((stan_2ww_20$performance_avg - stan_2ww_19$performance_avg),3)

changes_2ww_20 <- data.frame(period = months, 
                         change = changes_2ww_20)
changes_2ww_20 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue", ) + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2020 in 2WW Standard(compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))

#2021 vs 2019 
changes_2ww_21 <- round((stan_2ww_21$performance_avg - stan_2ww_19$performance_avg),3)

changes_2ww_21 <- data.frame(period = months, 
                         change = changes_2ww_21)
changes_2ww_21 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2021 in 2WW standard(compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))


#31 Days
#2020 vs 2019 
stan_31d <- filter(provider_data, standard == "31 Days")

grouped_31d <- stan_31d %>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_31d$performance_avg <- grouped_31d$sum_within / grouped_31d$sum_treated          

stan_31d_19 <- filter(grouped_31d, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
stan_31d_20 <- filter(grouped_31d, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
stan_31d_21 <- filter(grouped_31d, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))

changes_31d_20 <- round((stan_31d_20$performance_avg - stan_31d_19$performance_avg),3)

changes_31d_20 <- data.frame(period = months, 
                             change = changes_31d_20)
changes_31d_20 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue", ) + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2020 in 31Days Standard(compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))

#2021 vs 2019 
changes_31d_21 <- round((stan_31d_21$performance_avg - stan_31d_19$performance_avg),3)

changes_31d_21 <- data.frame(period = months, 
                             change = changes_31d_21)
changes_31d_21 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2021 in 31Days standard(compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))


#62 Days 
#2020 vs 2019
stan_62d <- filter(provider_data, standard == "62 Days")

grouped_62d <- stan_62d %>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_62d$performance_avg <- grouped_62d$sum_within / grouped_62d$sum_treated          

stan_62d_19 <- filter(grouped_62d, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
stan_62d_20 <- filter(grouped_62d, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
stan_62d_21 <- filter(grouped_62d, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))

changes_62d_20 <- round((stan_62d_20$performance_avg - stan_62d_19$performance_avg),3)

changes_62d_20 <- data.frame(period = months, 
                             change = changes_62d_20)
changes_62d_20 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue", ) + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2020 in 62Days Standard(compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))

#2021 vs 2019 
changes_62d_21 <- round((stan_62d_21$performance_avg - stan_62d_19$performance_avg),3)

changes_62d_21 <- data.frame(period = months, 
                             change = changes_62d_21)
changes_62d_21 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2021 in 62Days standard(compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))

#cancer incidence grouped by cancer type
selected_data %>% group_by(cancer_type) %>% count()
#the sum of cases from "Lung" and "Suspected Lung cancer" roughly matches the number published by Cancer Research UK 
#same with breast cancer 

#same barplots for individual cancer types, all standards 
lung_cancer <- filter(selected_data, 
                      cancer_type == "Lung" | cancer_type == "Suspected lung cancer")

breast_cancer <- filter(selected_data, 
                      cancer_type == "Breast" | cancer_type == "Suspected breast cancer")

grouped_lung <- lung_cancer %>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_lung$performance_avg <- grouped_lung$sum_within / grouped_lung$sum_treated


grouped_breast <- breast_cancer %>%
  group_by(period) %>% 
  summarise(sum_treated = sum(total_treated), sum_within = sum(within_standard))

grouped_breast$performance_avg <- grouped_breast$sum_within / grouped_breast$sum_treated

#lung cancer 
lung_19 <- filter(grouped_lung, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
lung_20 <- filter(grouped_lung, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
lung_21 <- filter(grouped_lung, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))

#2020 vs 2019
changes_lung_20 <- round((lung_20$performance_avg - lung_19$performance_avg),3)

changes_lung_20 <- data.frame(period = months, 
                         change = changes_lung_20)
changes_lung_20 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue", ) + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2020 for Lung Cancer (compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))

#2021 vs 2019
changes_lung_21 <- round((lung_21$performance_avg - lung_19$performance_avg),3)

changes_lung_21 <- data.frame(period = months, 
                              change = changes_lung_21)
changes_lung_21 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue", ) + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2021 for Lung Cancer (compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))


#breast cancer 
breast_19 <- filter(grouped_breast, period >= as.Date("2019-01-01") & period <= as.Date("2019-12-01"))
breast_20 <- filter(grouped_breast, period >= as.Date("2020-01-01") & period <= as.Date("2020-12-01"))
breast_21 <- filter(grouped_breast, period >= as.Date("2021-01-01") & period <= as.Date("2021-11-01"))

#2020 vs 2019
changes_breast_20 <- round((breast_20$performance_avg - breast_19$performance_avg),3)

changes_breast_20 <- data.frame(period = months, 
                              change = changes_breast_20)
changes_breast_20 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue", ) + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2020 for Breast Cancer (compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))

#2021 vs 2019
changes_breast_21 <- round((breast_21$performance_avg - breast_19$performance_avg),3)

changes_breast_21 <- data.frame(period = months, 
                              change = changes_breast_21)
changes_breast_21 %>%
  ggplot(aes(x = period,
             y = change)) + 
  geom_bar(stat = "identity", fill = "steelblue", ) + 
  geom_text(aes(label = change), vjust = -0.5) +
  labs(x = "months", 
       y = "changes in performance (percentage)",
       title = "Percentage of Changes in Performance in 2021 for Breast Cancer (compared to same period in 2019)") +
  theme_bw() + 
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct","Nov","Dec"))

                   
                  

########################################################################################################################################################
#please ignore everything below 
########################################################################################################################################################




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
#install.packages("ggpubr")

#setup 
library(tidyverse)
library(readr)
library(xlsx)
library(dygraphs)
library(xts)
#library(ggpubr)

urlfile = "https://raw.githubusercontent.com/kopeckylukas/LSHTM_RCS/main/Data/provider_level_data.csv"
complete_data <- read_csv(urlfile)


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

###################################################################################################
#more summary statistics comparing 2019 and 2020
###################################################################################################

#start at March since the most critical point in time starts from March 2020
summary_set <- select(perform_months, 
                      period, provider_code, total_treated, performance)

Mar_19_20 <- filter(summary_set, 
                    period == as.Date("2019-03-01") || period == as.Date("2020-03-01") )
#all observations from March 2019 and March 2020 

data_19_03 <- Mar_19_20 %>% 
  filter(period == as.Date("2019-03-01"))
  
  summarise_at(data_19_03, 
               .vars = vars(total_treated, performance), 
               .funs = list(mean = mean, 
                            sd = sd))
#sd of total treated is way too big
#probably good to not look at it or exclude some outliers 

data_20_03 <- Mar_19_20 %>% 
  filter(period == as.Date("2020-03-01"))
 
   summarise_at(data_20_03, 
                .vars = vars(total_treated, performance), 
               .funs = list(mean = mean, 
                            sd = sd))
#same issue with total_treated-sd 

#t test 
t.test(data_20_03$performance, data_19_03$performance, var.equal = FALSE)
#t=0.45859, p-value = 0.6465 
#not enough evidence to show there is a difference between the performances in Mar2019 and Mar2020
  
  

#repeat for Apr2019 and Apr2020
#all observations from April 2019 and April 2020 

Apr_19_20 <- filter(summary_set, 
                    period == as.Date("2019-04-01") || period == as.Date("2020-04-01") )

data_19_04 <- Apr_19_20 %>% 
  filter(period == as.Date("2019-04-01"))

summarise_at(data_19_04, 
             .vars = vars(total_treated, performance), 
             .funs = list(mean = mean, 
                          sd = sd))

data_20_04 <- Apr_19_20 %>% 
  filter(period == as.Date("2020-04-01"))

summarise_at(data_20_04, 
             .vars = vars(total_treated, performance), 
             .funs = list(mean = mean, 
                          sd = sd))

#t test 
t.test(data_20_04$performance, data_19_04$performance, var.equal = FALSE)
#t = -4.9969, p-value = 5.897e-07

#repeat for May2019 and May2020
#all observations from May 2019 and May 2020 

May_19_20 <- filter(summary_set, 
                    period == as.Date("2019-05-01") || period == as.Date("2020-05-01") )

data_19_05 <- May_19_20 %>% 
  filter(period == as.Date("2019-05-01"))

summarise_at(data_19_05, 
             .vars = vars(total_treated, performance), 
             .funs = list(mean = mean, 
                          sd = sd))

data_20_05 <- May_19_20 %>% 
  filter(period == as.Date("2020-05-01"))

summarise_at(data_20_05, 
             .vars = vars(total_treated, performance), 
             .funs = list(mean = mean, 
                          sd = sd))

#t test 
t.test(data_20_05$performance, data_19_05$performance, var.equal = FALSE)
#t = -9.0813, p-value < 2.2e-16




