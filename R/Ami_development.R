# Amiiii! 


####################################################################################################
####################################################################################################
# Setup

install.packages("RColorBrewer")

## Load libraries 
library(tidyverse)
library(ggplot2)
library(xts)
library(reshape2)
library(RColorBrewer)
library(dplyr)

## Load Cancer waiting time data 
library(readr)
library(readxl)
Beds_Provider <- read_excel("Data/Beds_Provider.xlsx")
provider_level_data <- read_csv("Data/provider_level_data.csv")
Beds_regions <- read_csv("Data/Beds_regions.csv")
# names(Beds_regions)[names(Beds_regions) == "Date"] <- "period"
Beds_regions$Date = as.Date(Beds_regions$Date, format = "%Y-%m-%d")


## Check if there are any NAs 
any(is.na(provider_level_data))
#### TRUE

## Inspect rows with NAs
provider_NA <- provider_level_data[rowSums(is.na(provider_level_data)) > 0,]
nrow(provider_NA)
#### 17951 rows with NA data


## Inspect the provider_code for NA rows 
table(provider_NA$provider_code)
table(provider_NA$period)


## Remove rows with missing data -> We lack information and data to fill in the NA values 
provider_level_data <- na.omit(provider_level_data)


## Remove rows which have cancer_type = ALL CANCER
#### ALL CANCER is a summary of each cancer type. Remove to avoid repetition. 
provider_level_data <- filter(provider_level_data, cancer_type != "ALL CANCERS")


## Remove rows which standard equals to "28 Days FDS" and "28 Days FDS (By Route)
#### 28 Days Faster Diagnosis Standard started in 2019. Since our dataset starts in 2016, 
#### We will remove this standard to avoid inflation of total treatments. 
provider_level_data <- filter(provider_level_data, standard != "28 Days FDS")
provider_level_data <- filter(provider_level_data, standard != "28 Days FDS (By Route)")


#### We remove sub standards ( 31 Days Sub (Drugs), 31 Days Sub (Radio), 31 Days Sub (Surgery))

provider_level_data <- filter(provider_level_data, standard != "31 Days Sub (Drugs)")
provider_level_data <- filter(provider_level_data, standard != "31 Days Sub (Radio)")
provider_level_data <- filter(provider_level_data, standard != "31 Days Sub (Surgery)")


### Drop 2WW Breast as well 
provider_level_data <- filter(provider_level_data, standard != "2WW Breast")


### Change name of suspected breast cancer -> breast 
### and suspected lung cancer -> lung 
provider_level_data <- provider_level_data %>% 
  mutate(cancer_type = recode(cancer_type, "Suspected lung cancer" = "Lung", "Suspected breast cancer" = "Breast"))

### Caution!!!!! only when looking into breast and lung 
provider_level_data <- provider_level_data %>% 
  mutate(standard = replace(standard, standard == "2WW", "2WW for Suspected Cancer"))



####################################################################################################
####################################################################################################
# Exploratory Analysis 


# Check total treated number by cancer types 

## See the distribution of cancer types -> Number of rows with the cancer type 

table(provider_level_data$cancer_type)
prop.table(table(provider_level_data$cancer_type))

cancer_count <- as.data.frame(table(provider_level_data$cancer_type))

ggplot(data = cancer_count, aes(x = reorder(Var1, Freq), y = Freq)) + 
  labs(title = "Counts of Cancer Types", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip()+ 
  theme_bw()

#### We can see that Other, Lung, Lower Gastrointestinal and Breast appear frequently
####  But how about the most treated cancer?


## Total treated number by cancer type
treated_by_cancer <- 
  aggregate(provider_level_data$total_treated, by = list(Cancer = provider_level_data$cancer_type), FUN = sum)

ggplot(data = treated_by_cancer, aes(x = reorder(Cancer, x), y = x)) + 
  labs(title = "Number of Treated Cancers ", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme_bw()

#### It seems that the most treated cancer actually differs. 


## plot for total cases against covid  

plot_data1 <- provider_level_data %>% 
  select(period, total_treated) %>% 
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')

ylim.prim <- c(min(plot_data1$total_treated), max(plot_data1$total_treated))
ylim.sec <- c(min(Beds_regions$England), max(Beds_regions$England))

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

plot_data2 <- provider_level_data %>% 
  select(period, cancer_type, total_treated, standard) %>% 
  group_by(period, cancer_type) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Breast" | cancer_type == "Lung")

ggplot() +
  geom_line(data = plot_data1, 
            aes(x = period, y = total_treated), color = "deepskyblue4", size=1) +
  ggtitle("Number of Treated Cancers and Number of beds occupied by covid patients in Hospital") + 
  xlab("Time") + ylab("Number of Treated Cancer Cases")+
  geom_line(data = Beds_regions, 
            aes(x = Date, y = a + England*b), color = "black", size=1)+ 
  scale_y_continuous("\nNumber of Treated Cancer Cases", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "\nNumber of beds occupied by covid patients"),
                     expand = c(0,0))+
  theme_bw() + 
  theme(axis.title.y.left = element_text(colour = "deepskyblue4"), 
        axis.title.y.right = element_text(colour = "black")) + 
  annotate("rect", xmin = plot_data1$period[51], xmax = plot_data1$period[55],
           ymin = 100000, ymax = 280000, alpha = .2) + 
  annotate("rect", xmin = plot_data1$period[57], xmax = plot_data1$period[64],
           ymin = 100000, ymax = 280000, alpha = .2)



## Plot for performance with covid patients 




## plot a line plot for total treated number by cancer type 
plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated) %>% 
  group_by(period, cancer_type) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')

ggplot(data = plot_data, aes(x = period, y = total_treated, group = cancer_type, color = cancer_type)) +
  geom_line() + ggtitle("Number of Treated Cancers by Cancer Type") + 
  xlab("Years") + ylab("Number of Treated Cancer Cases") + 
  scale_color_discrete(name = "Cancer Type") +
  theme_bw()


### Select only relevant data 
plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated) %>% 
  group_by(period, cancer_type) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(total_treated > 10000 | cancer_type == "Breast" | cancer_type == "Lung" 
         | cancer_type == "Suspected lung cancer" | cancer_type == "Suspected breast cancer"
         | cancer_type == "Other")


ggplot(data = plot_data, aes(x = period, y = total_treated, group = cancer_type, color = cancer_type)) +
  geom_line() + ggtitle("Number of Treated Cancers by Cancer Type") + 
  xlab("Years") + ylab("Number of Treated Cancer Cases") + 
  scale_color_discrete(name = "Cancer Type") +
  theme_bw() 


#### Suspected skin cancer, Suspected breast cancer, suspected lower gastrointestinal cancer
#### (Call them The 3 relavent suspect cancer types)
#### Are the three most treated(or consulted) type of cancer
#### We can see a clear increasing trend of these three cancers. 
#### It doesn't seem that other cancers have too much of an increasing trend



## Close up to Breast and Lung cancer 
plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, standard) %>% 
  group_by(period, cancer_type) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Breast" | cancer_type == "Lung")

ggplot(data = plot_data, aes(x = period, y = total_treated, group = cancer_type, color = cancer_type)) +
  geom_line() + ggtitle("Number of Treated Cancers: Breast and Lung") + 
  xlab("Years") + ylab("Number of Treated Cancer Cases") + 
  scale_color_manual(breaks = plot_data$cancer_type, values = c("#DF65B0", "#3690C0")) +
  theme_bw()



#### It seems that Breast had Covid impact but Lung didn't have much 
#### Doesn't seem like there is large decrease before Covid 


## Breast and Lung by waiting time 
provider_level_data <- provider_level_data %>% 
  mutate(standard = replace(standard, standard == "2WW", "2WW for Suspected Cancer"))


plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, standard) %>% 
  group_by(period, cancer_type, standard) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Lung")

ggplot(data = plot_data, aes(x = period, y = total_treated, group = standard, color = standard)) +
  geom_line(size=1) + ggtitle("Number of Treated Lung Cancers by standard") + 
  xlab("Time") + ylab("Number of Treated Lung Cancer Cases") + 
  coord_cartesian(ylim = c(0, 40000)) +
  theme_bw() + 
  theme(legend.position="bottom") +
  scale_color_manual(values=c("darkslategray3", "darkslategray4", "darkslategray")) + 
  annotate("rect", xmin = plot_data1$period[51], xmax = plot_data1$period[55],
           ymin = 0, ymax = 40000, alpha = .2) + 
  annotate("rect", xmin = plot_data1$period[57], xmax = plot_data1$period[64],
           ymin = 0, ymax = 40000, alpha = .2) +
  scale_y_continuous(expand = c(0,0))


plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, standard) %>% 
  group_by(period, cancer_type, standard) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Breast")

ggplot(data = plot_data, aes(x = period, y = total_treated, group = standard, color = standard)) +
  geom_line(size=1) + ggtitle("Number of Treated Breast Cancers by standard") + 
  xlab("Time") + ylab("Number of Treated Breast Cancer Cases") + 
  coord_cartesian(ylim = c(0, 45000)) +
  theme_bw()+ 
  theme(legend.position="bottom") +
  scale_color_manual(values = c("indianred2", "indianred3", "indianred4")) + 
  annotate("rect", xmin = plot_data1$period[51], xmax = plot_data1$period[55],
           ymin = 0, ymax = 46000, alpha = .2) + 
  annotate("rect", xmin = plot_data1$period[57], xmax = plot_data1$period[64],
           ymin = 0, ymax = 46000, alpha = .2) +
  scale_y_continuous(expand = c(0,0))




### Breast and lung by performance 

plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, within_standard, standard) %>% 
  group_by(period, cancer_type, standard) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop') %>%
  filter(cancer_type == "Breast") 

ggplot(data = plot_data, aes(x = period, y = performance*100, group = standard, color = standard)) +
  geom_line(size = 1) + ggtitle("Performance for Breast Cancer Waiting Times by standard") + 
  xlab("Time") + ylab("Performance (%)") + 
  theme_bw() + 
  theme(legend.position="bottom") +
  scale_color_manual(values=c("indianred2", "indianred3", "indianred4"))+ 
  annotate("rect", xmin = plot_data1$period[51], xmax = plot_data1$period[55],
           ymin = 50, ymax = 100, alpha = .2) + 
  annotate("rect", xmin = plot_data1$period[57], xmax = plot_data1$period[64],
           ymin = 50, ymax = 100, alpha = .2) +
  scale_y_continuous(expand = c(0,0))


plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, within_standard, standard) %>% 
  group_by(period, cancer_type, standard) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop') %>%
  filter(cancer_type == "Lung") 

ggplot(data = plot_data, aes(x = period, y = performance*100, group = standard, color = standard)) +
  geom_line(size = 1) + ggtitle("Performance for Lung Cancer Waiting Times by standard") + 
  xlab("Time") + ylab("Performance (%)") + 
  theme_bw()+ 
  theme(legend.position="bottom") +
  scale_color_manual(values = c("darkslategray3", "darkslategray4", "darkslategray"))+ 
  annotate("rect", xmin = plot_data1$period[51], xmax = plot_data1$period[55],
           ymin = 50, ymax = 100, alpha = .2) + 
  annotate("rect", xmin = plot_data1$period[57], xmax = plot_data1$period[64],
           ymin = 50, ymax = 100, alpha = .2) +
  scale_y_continuous(expand = c(0,0))






# Check Total treated number by region 

## See distribution of regions -> Number of rows with this region 
table(provider_level_data$region_name)
prop.table(table(provider_level_data$region_name))

standard_count <- as.data.frame(table(provider_level_data$region_name))

ggplot(data = standard_count, aes(x = reorder(Var1, Freq), y = Freq)) + 
  labs(title = "Counts of Regions", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip()

#### London comissioning region has less counts 


## Total treated number by region
treated_by_region <- 
  aggregate(provider_level_data$total_treated, by = list(Region = provider_level_data$region_name), FUN = sum)

ggplot(data = treated_by_region, aes(x = reorder(Region, x), y = x)) + 
  labs(title = "Counts of Total Treated Cancers by Region", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip()

#### It seems that these two distributions are fairly similar 


# Plot total treated number by region 
plot_data <- provider_level_data %>% 
  select(period, region_name, total_treated) %>% 
  group_by(period, region_name) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')

ggplot(data = plot_data, aes(x = period, y = total_treated, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Number of Treated Cancers by Region") + 
  xlab("Years") + ylab("Number of Treated Cancer Cases") + 
  scale_shape_discrete(name = "test") + 
  theme_classic()

#### It seems that the 3 relevant suspected cancer types have a large impact on the pre-covid growth 


# See the regional differences for breast cancer and lung cancer 
plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, region_name) %>% 
  group_by(period, cancer_type, region_name) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Breast")

ggplot(data = plot_data, aes(x = period, y = total_treated, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Number of Treated Breast Cancer by Region") + 
  xlab("Years") + ylab("Number of Treated Breast Cancer Cases") + 
  theme_classic()


plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, region_name) %>% 
  group_by(period, cancer_type, region_name) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Lung")

ggplot(data = plot_data, aes(x = period, y = total_treated, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Number of Treated Lung Cancer by Region") + 
  xlab("Years") + ylab("Number of Treated Lung Cancer Cases") + 
  theme_classic()


#### Interesting difference 









## How do the waiting standards differ for Lung and Breast cancer?

lung_data <- provider_level_data %>% filter(cancer_type == "Lung")
table(lung_data$standard)

breast_data <- provider_level_data %>% filter(cancer_type == "Breast")
table(breast_data$standard)

#### Both are only for 31 Days and 62 Days 






## Look at how total treated numbers look for different standards 

## See distribution of standards -> Number of rows with this standard  
table(provider_level_data$standard)
prop.table(table(provider_level_data$standard))

count <- as.data.frame(table(provider_level_data$standard))

ggplot(data = count, aes(x = reorder(Var1, Freq), y = Freq)) + 
  labs(title = "Counts of Standards", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip()



## Total treated number by standard
treated_by_standard <- 
  aggregate(provider_level_data$total_treated, by = list(Standard = provider_level_data$standard), FUN = sum)

ggplot(data = treated_by_standard, aes(x = reorder(Standard, x), y = x)) + 
  labs(title = "Counts of Total Treated Cancers by Standards", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip()

#### 2WW holds most of the treated cases. 


## Check if the three suspected cancers are 2WW
three_suspected_data <- 
  provider_level_data %>% 
  filter(cancer_type == "Suspected skin cancer" | cancer_type == "Suspected breast cancer" | cancer_type == "Suspected lower gastrointestinal cancer")

table(three_suspected_data$standard)

#### Yes, they are. 




## Does Breast and cancer total treatments differ for the 31 and 62 waiting times?

plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, standard) %>% 
  group_by(period, cancer_type, standard) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Lung")

ggplot(data = plot_data, aes(x = period, y = total_treated, group = standard, color = standard)) +
  geom_line() + ggtitle("Number of Treated Lung Cancer by standard") + 
  xlab("Years") + ylab("Number of Treated Lung Cancer Cases") + 
  theme_classic()


plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, standard) %>% 
  group_by(period, cancer_type, standard) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Breast")

ggplot(data = plot_data, aes(x = period, y = total_treated, group = standard, color = standard)) +
  geom_line() + ggtitle("Number of Treated Breast Cancer by standard") + 
  xlab("Years") + ylab("Number of Treated Breast Cancer Cases") + 
  theme_classic()


## Plot data by cancer type, waiting time, and region
plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, standard, region_name) %>% 
  group_by(period, cancer_type, standard, region_name) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Breast") %>%
  filter(standard == "62 Days")

ggplot(data = plot_data, aes(x = period, y = total_treated, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Number of Treated 62 Days Breast Cancer by Region") + 
  xlab("Years") + ylab("Number of Treated Breast Cancer Cases") + 
  theme_classic()

plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, standard, region_name) %>% 
  group_by(period, cancer_type, standard, region_name) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Breast") %>%
  filter(standard == "31 Days")

ggplot(data = plot_data, aes(x = period, y = total_treated, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Number of Treated 31 Days Breast Cancer by Region") + 
  xlab("Years") + ylab("Number of Treated Breast Cancer Cases") + 
  theme_classic()

plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, standard, region_name) %>% 
  group_by(period, cancer_type, standard, region_name) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Lung") %>%
  filter(standard == "62 Days")

ggplot(data = plot_data, aes(x = period, y = total_treated, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Number of Treated 62 Days Lung Cancer by Region") + 
  xlab("Years") + ylab("Number of Treated Lung Cancer Cases") + 
  theme_classic()

plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, standard, region_name) %>% 
  group_by(period, cancer_type, standard, region_name) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type == "Lung") %>%
  filter(standard == "31 Days")

ggplot(data = plot_data, aes(x = period, y = total_treated, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Number of Treated 31 Days Lung Cancer by Region") + 
  xlab("Years") + ylab("Number of Treated Lung Cancer Cases") + 
  theme_classic()


###########################################################################################
## Check how the performance is 



## Performance by cancer type
plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, within_standard) %>% 
  group_by(period, cancer_type) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop')

ggplot(data = plot_data, aes(x = period, y = performance, group = cancer_type, color = cancer_type)) +
  geom_line() + ggtitle("Performance for Cancer Waiting Times by Cancer Type") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "Cancer Type")+
  theme_classic()


#### It is very hard to interpret anything
#### Urological (Excluding Testicular) seems to have low performance overall 


## Performance of CWT by region. 
## We will mean of sum to calculate average
plot_data <- provider_level_data %>% 
  select(period, region_name, total_treated, within_standard) %>% 
  group_by(period, region_name) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop')

ggplot(data = plot_data, aes(x = period, y = performance, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Performance for Cancer Waiting Times by Region") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "Commissioning Region")+
  theme_classic()



## Close up to Breast and Lung cancer 
plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, within_standard) %>% 
  group_by(period, cancer_type) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop') %>%
  filter(cancer_type == "Breast" | cancer_type == "Lung")

ggplot(data = plot_data, aes(x = period, y = performance, group = cancer_type, color = cancer_type)) +
  geom_line() + ggtitle("Performance for Cancer Waiting Times: Breast and Lung") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "Cancer Type")+
  theme_classic()


## Split by waiting time 
plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, within_standard, standard) %>% 
  group_by(period, cancer_type, standard) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop') %>%
  filter(cancer_type == "Breast") %>% 
  filter(standard == "62 Days" | standard == "31 Days")

ggplot(data = plot_data, aes(x = period, y = performance, group = standard, color = standard)) +
  geom_line() + ggtitle("Performance for Breast Cancer Waiting Times by standard") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "standard")+
  theme_classic()

plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, within_standard, standard) %>% 
  group_by(period, cancer_type, standard) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop') %>%
  filter(cancer_type == "Lung") %>% 
  filter(standard == "62 Days" | standard == "31 Days")

ggplot(data = plot_data, aes(x = period, y = performance, group = standard, color = standard)) +
  geom_line() + ggtitle("Performance for Lung Cancer Waiting Times by standard") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "standard")+
  theme_classic()



## Further consider region 
plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, within_standard, standard, region_name) %>% 
  group_by(period, cancer_type, standard, region_name) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop') %>%
  filter(cancer_type == "Lung") %>% 
  filter(standard == "62 Days")

ggplot(data = plot_data, aes(x = period, y = performance, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Performance for 62 Days Lung Cancer Waiting Times by Region") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "Region")+
  theme_classic()

plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, within_standard, standard, region_name) %>% 
  group_by(period, cancer_type, standard, region_name) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop') %>%
  filter(cancer_type == "Lung") %>% 
  filter(standard == "31 Days")

ggplot(data = plot_data, aes(x = period, y = performance, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Performance for 31 Days Lung Cancer Waiting Times by Region") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "Region")+
  theme_classic()

plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, within_standard, standard, region_name) %>% 
  group_by(period, cancer_type, standard, region_name) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop') %>%
  filter(cancer_type == "Breast") %>% 
  filter(standard == "62 Days")

ggplot(data = plot_data, aes(x = period, y = performance, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Performance for 62 Days Breast Cancer Waiting Times by Region") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "Region")+
  theme_classic()

plot_data <- provider_level_data %>% 
  select(period, cancer_type, total_treated, within_standard, standard, region_name) %>% 
  group_by(period, cancer_type, standard, region_name) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop') %>%
  filter(cancer_type == "Breast") %>% 
  filter(standard == "31 Days")

ggplot(data = plot_data, aes(x = period, y = performance, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Performance for 31 Days Breast Cancer Waiting Times by Region") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "Region")+
  theme_classic()














## Check Covid bed occupancy data 
ggplot(data = Beds_regions, aes(x = period, y = England)) + 
  geom_line() + ggtitle("Number of Beds Occupied by Covid Patients in England") + 
  xlab("Years") + ylab("Number of Beds") +
  theme_classic()



melt <- Beds_regions %>% 
  select(-England)
melt <- melt(melt, id.vars = "period", variable.name = "Region")

ggplot(melt, aes(period, value)) +
  geom_line(aes(colour = Region)) + 
  ggtitle("Number of Beds Occupied by Covid Patients by Commisioning Region") + 
  xlab("Years") + ylab("Number of Beds") + 
  theme_classic()




## Plot performance against bed occupancy 

## Determine the Peaks for Covid 

## Lets say for example 2020-03-20 to 2020-08-01 for first peak 
ggplot(data = Beds_regions, aes(x = period, y = England)) + 
  geom_line() + ggtitle("Number of Beds Occupied by Covid Patients in England") + 
  xlab("Years") + ylab("Number of Beds") +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-12")), linetype = 4) +
  theme_classic()

#### VLINE NOT WORKING IDK WHY





## plot peak1 
peak1_cancer <-
  provider_level_data %>% 
  subset(period == "2021-02-01")%>% 
  filter(cancer_type == "Suspected lung cancer") 


peak1_beds <- 
  Beds_Provider %>% 
  subset(period == "2021-02-01") %>%
  select(provider_code, Beds_Occupied)


peak1_data <- left_join(peak1_cancer, peak1_beds, by = c("provider_code"))


plot(peak1_data$Beds_Occupied, peak1_data$performance, main = "for Suspected lung cancer in 2021-02")



## Plot for RRK Birmingham 
peak_cancer <-
  provider_level_data %>% 
  subset(provider_code == "RRK")%>% 
  filter(cancer_type == "Suspected breast cancer")


peak_beds <- 
  Beds_Provider %>% 
  subset(provider_code == "RRK")%>%
  select(period, Beds_Occupied)

peak_data <- left_join(peak_cancer, peak_beds, by = c("period"))

plot(peak_data$Beds_Occupied, peak_data$performance, main = "Scatter Plot of the Suspected Breast Cancer Performance against Beds Occupied for provider code = RRK")


?plot

peak_cancer <-
  provider_level_data %>% 
  subset(provider_code == "RYJ")%>% 
  filter(cancer_type == "Suspected breast cancer")


peak_beds <- 
  Beds_Provider %>% 
  subset(provider_code == "RYJ")%>%
  select(period, Beds_Occupied)

peak_data <- left_join(peak_cancer, peak_beds, by = c("period"))

plot(peak_data$Beds_Occupied, peak_data$performance)






## Since there are repetition for days we will need to filter down 



peak1_cancer_breast_62 <- 
  provider_level_data %>% 
  subset(period > "2020-03-01" & period < "2020-08-01") %>% 
  filter(cancer_type == "Breast") %>% 
  filter(standard == "62 Days")

peak1_beds <- 
  Beds_regions %>% 
  subset(period > "2020-03-01" & period < "2020-08-01")

peak1_cancer_breast_62_data <- 
  left_join(peak1_cancer_breast_62, peak1_beds, by = c("period"))


ggplot(data = peak1_cancer_breast_62_data, 
       aes(x = England, y = performance, group = region_name, color = region_name), 
       group = region_name, color = region_name) + 
  geom_point()



#### Problem of the provider_level_data being monthly data while Beds is daily. 
#### We do not have enough data points 


#

peak1_cancer_breast_London <- 
  provider_level_data %>% 
  subset(period > "2020-03-01" & period < "2020-08-01") %>% 
  filter(cancer_type == "Breast") %>% 
  filter(region_name == "LONDON COMMISSIONING REGION")






##############################################################################################
##############################################################################################
##############################################################################################

### Try prophet on lung and breast total treated number 

install.packages('prophet')

library(prophet)
library(tibble)
library(caret)

install.packages(c("cmdstanr", "posterior"), repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()





provider_breast_2WW <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>% 
  filter(standard == "2WW for Suspected Cancer") %>%
  select(period, total_treated) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(period < "2020-03-01")

provider_breast_2WW_real <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>% 
  filter(standard == "2WW for Suspected Cancer") %>%
  select(period, total_treated) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')

provider_breast_2WW <- mutate(provider_breast_2WW, ds = period, y = total_treated)
breast_2WW_prophet <- prophet(provider_breast_2WW)
breast_2WW_prophet_future <- make_future_dataframe(breast_2WW_prophet, periods = 21, freq='month')
breast_2WW_prophet_forecast <- predict(breast_2WW_prophet, breast_2WW_prophet_future)
breast_2WW_prophet_forecast$real <- provider_breast_2WW_real$total_treated

plot(breast_2WW_prophet, breast_2WW_prophet_forecast) + theme_bw() + 
  labs(title = "Modelling of the Number of Treated 2WW Breast Cancer Cases in the Absence of the Pandemic",
       subtitle = "The blue line is the modelled time series and the red line is the observed time series.",
       x = "Time", 
       y = "Number of Treated Cancer Cases") + 
  geom_vline(xintercept = as.numeric(breast_2WW_prophet_forecast$ds[51]),
             linetype = "dotted", size = 1) +
  geom_line(aes(x = ds, y = real, colour = "indianred2"),size = 1, show.legend = FALSE) + 
  annotate("rect", xmin = breast_2WW_prophet_forecast$ds[51], xmax = breast_2WW_prophet_forecast$ds[55],
           ymin = 0, ymax = 50000, alpha = .2) + 
  annotate("rect", xmin = breast_2WW_prophet_forecast$ds[57], xmax = breast_2WW_prophet_forecast$ds[64],
           ymin = 0, ymax = 50000, alpha = .2) +
  scale_y_continuous(expand = c(0,0))

 
sum = 0
for (i in 51:55){
  count <- breast_2WW_prophet_forecast$yhat[i] - breast_2WW_prophet_forecast$real[i]
  sum = sum + count
}



#################################################################################################
#################################################################################################
## Expected plot for breast 

provider_breast_2WW_real <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>% 
  filter(standard == "2WW for Suspected Cancer") %>%
  select(period, total_treated) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')

provider_breast_2WW <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>% 
  filter(standard == "2WW for Suspected Cancer") %>%
  select(period, total_treated) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(period < "2020-03-01")


provider_breast_2WW <- mutate(provider_breast_2WW, ds = period, y = total_treated)
breast_2WW_prophet <- prophet(provider_breast_2WW)
breast_2WW_prophet_future <- make_future_dataframe(breast_2WW_prophet, periods = 21, freq='month')
breast_2WW_prophet_forecast <- predict(breast_2WW_prophet, breast_2WW_prophet_future)
breast_2WW_prophet_forecast$real <- provider_breast_2WW_real$total_treated


provider_breast_2WW <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>% 
  filter(standard == "2WW for Suspected Cancer") %>%
  select(period, total_treated, ) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')


breast_2WW_expected <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>%
  filter(standard == "2WW for Suspected Cancer") %>%
  select(period, within_standard, standard) %>%
  group_by(period, standard) %>%
  summarise(within_standard = sum(within_standard), .groups = 'drop')

breast_2WW_treated_exp <- breast_2WW_prophet_forecast %>% select(yhat)
breast_2WW_expected <- cbind(breast_2WW_expected, breast_2WW_treated_exp)
breast_2WW_expected$exp_performance <- breast_2WW_expected$within_standard/breast_2WW_expected$yhat
  



provider_breast_31 <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>% 
  filter(standard == "31 Days") %>%
  select(period, total_treated) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(period < "2020-03-01")


provider_breast_31 <- mutate(provider_breast_31, ds = period, y = total_treated)
breast_31_prophet <- prophet(provider_breast_31)
breast_31_prophet_future <- make_future_dataframe(breast_31_prophet, periods = 21, freq='month')
breast_31_prophet_forecast <- predict(breast_31_prophet, breast_31_prophet_future)
breast_31_prophet_forecast$real <- provider_breast_31_real$total_treated


provider_breast_31 <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>% 
  filter(standard == "31 Days") %>%
  select(period, total_treated, ) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')


breast_31_expected <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>%
  filter(standard == "31 Days") %>%
  select(period, within_standard, standard) %>%
  group_by(period, standard) %>%
  summarise(within_standard = sum(within_standard), .groups = 'drop')

breast_31_treated_exp <- breast_31_prophet_forecast %>% select(yhat)
breast_31_expected <- cbind(breast_31_expected, breast_31_treated_exp)
breast_31_expected$exp_performance <- breast_31_expected$within_standard/breast_31_expected$yhat



provider_breast_62 <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>% 
  filter(standard == "62 Days") %>%
  select(period, total_treated) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(period < "2020-04-01")


provider_breast_62 <- mutate(provider_breast_62, ds = period, y = total_treated)
breast_62_prophet <- prophet(provider_breast_62)
breast_62_prophet_future <- make_future_dataframe(breast_62_prophet, periods = 20, freq='month')
breast_62_prophet_forecast <- predict(breast_62_prophet, breast_62_prophet_future)
breast_62_prophet_forecast$real <- provider_breast_62_real$total_treated


provider_breast_62 <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>% 
  filter(standard == "62 Days") %>%
  select(period, total_treated, ) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')


breast_62_expected <- provider_level_data %>% 
  filter(cancer_type == "Breast") %>%
  filter(standard == "62 Days") %>%
  select(period, within_standard, standard) %>%
  group_by(period, standard) %>%
  summarise(within_standard = sum(within_standard), .groups = 'drop')

breast_62_treated_exp <- breast_62_prophet_forecast %>% select(yhat)
breast_62_expected <- cbind(breast_62_expected, breast_62_treated_exp)
breast_62_expected$exp_performance <- breast_62_expected$within_standard/breast_62_expected$yhat


breast_expected <- rbind(breast_62_expected, breast_31_expected, breast_2WW_expected)



ggplot(data = breast_expected, aes(x = period, y = exp_performance*100, group = standard, color = standard)) +
  geom_line(size = 1) + ggtitle("Estimated Performance for Breast Cancer Waiting Times by Standard") + 
  xlab("Time") + ylab("Performance (%)") + 
  theme_bw() + 
  theme(legend.position="bottom") +
  scale_color_manual(values=c("indianred2", "indianred3", "indianred4"))+ 
  annotate("rect", xmin = breast_expected$period[51], xmax = breast_expected$period[55],
           ymin = 0, ymax = 119, alpha = .2) + 
  annotate("rect", xmin = breast_expected$period[57], xmax = breast_expected$period[64],
           ymin = 0, ymax = 119, alpha = .2) +
  scale_y_continuous(expand = c(0,0))






#################################################################################################
#################################################################################################
## Expected plot for lung


provider_lung_2WW_real <- provider_level_data %>% 
  filter(cancer_type == "Lung") %>% 
  filter(standard == "2WW for Suspected Cancer") %>%
  select(period, total_treated) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')

provider_lung_2WW <- provider_level_data %>% 
  filter(cancer_type == "Lung") %>% 
  filter(standard == "2WW for Suspected Cancer") %>%
  select(period, total_treated) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(period < "2020-03-01")


provider_lung_2WW <- mutate(provider_lung_2WW, ds = period, y = total_treated)
lung_2WW_prophet <- prophet(provider_lung_2WW)
lung_2WW_prophet_future <- make_future_dataframe(lung_2WW_prophet, periods = 21, freq='month')
lung_2WW_prophet_forecast <- predict(lung_2WW_prophet, lung_2WW_prophet_future)
lung_2WW_prophet_forecast$real <- provider_lung_2WW_real$total_treated


provider_lung_2WW <- provider_level_data %>% 
  filter(cancer_type == "Lungt") %>% 
  filter(standard == "2WW for Suspected Cancer") %>%
  select(period, total_treated, ) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')


lung_2WW_expected <- provider_level_data %>% 
  filter(cancer_type == "Lung") %>%
  filter(standard == "2WW for Suspected Cancer") %>%
  select(period, within_standard, standard) %>%
  group_by(period, standard) %>%
  summarise(within_standard = sum(within_standard), .groups = 'drop')

lung_2WW_treated_exp <- lung_2WW_prophet_forecast %>% select(yhat)
lung_2WW_expected <- cbind(lung_2WW_expected, lung_2WW_treated_exp)
lung_2WW_expected$exp_performance <- lung_2WW_expected$within_standard/lung_2WW_expected$yhat




provider_lung_31 <- provider_level_data %>% 
  filter(cancer_type == "Lung") %>% 
  filter(standard == "31 Days") %>%
  select(period, total_treated) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(period < "2020-03-01")


provider_lung_31 <- mutate(provider_lung_31, ds = period, y = total_treated)
lung_31_prophet <- prophet(provider_lung_31)
lung_31_prophet_future <- make_future_dataframe(lung_31_prophet, periods = 21, freq='month')
lung_31_prophet_forecast <- predict(lung_31_prophet, lung_31_prophet_future)
lung_31_prophet_forecast$real <- provider_lung_31_real$total_treated


provider_lung_31 <- provider_level_data %>% 
  filter(cancer_type == "Lung") %>% 
  filter(standard == "31 Days") %>%
  select(period, total_treated, ) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')


lung_31_expected <- provider_level_data %>% 
  filter(cancer_type == "Lung") %>%
  filter(standard == "31 Days") %>%
  select(period, within_standard, standard) %>%
  group_by(period, standard) %>%
  summarise(within_standard = sum(within_standard), .groups = 'drop')

lung_31_treated_exp <- lung_31_prophet_forecast %>% select(yhat)
lung_31_expected <- cbind(lung_31_expected, lung_31_treated_exp)
lung_31_expected$exp_performance <- lung_31_expected$within_standard/lung_31_expected$yhat



provider_lung_62 <- provider_level_data %>% 
  filter(cancer_type == "Lung") %>% 
  filter(standard == "62 Days") %>%
  select(period, total_treated) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop') %>%
  filter(period < "2020-04-01")


provider_lung_62 <- mutate(provider_lung_62, ds = period, y = total_treated)
lung_62_prophet <- prophet(provider_lung_62)
lung_62_prophet_future <- make_future_dataframe(lung_62_prophet, periods = 20, freq='month')
lung_62_prophet_forecast <- predict(lung_62_prophet, lung_62_prophet_future)
lung_62_prophet_forecast$real <- provider_lung_62_real$total_treated


provider_lung_62 <- provider_level_data %>% 
  filter(cancer_type == "Lung") %>% 
  filter(standard == "62 Days") %>%
  select(period, total_treated, ) %>%
  group_by(period) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')


lung_62_expected <- provider_level_data %>% 
  filter(cancer_type == "Lung") %>%
  filter(standard == "62 Days") %>%
  select(period, within_standard, standard) %>%
  group_by(period, standard) %>%
  summarise(within_standard = sum(within_standard), .groups = 'drop')

lung_62_treated_exp <- lung_62_prophet_forecast %>% select(yhat)
lung_62_expected <- cbind(lung_62_expected, lung_62_treated_exp)
lung_62_expected$exp_performance <- lung_62_expected$within_standard/lung_62_expected$yhat


lung_expected <- rbind(lung_62_expected, lung_31_expected, lung_2WW_expected)



ggplot(data = lung_expected, aes(x = period, y = exp_performance*100, group = standard, color = standard)) +
  geom_line(size = 1) + ggtitle("Estimated Performance for Lung Cancer Waiting Times by Standard") + 
  xlab("Time") + ylab("Performance (%)") + 
  theme_bw() + 
  theme(legend.position="bottom") +
  scale_color_manual(values=c("darkslategray3", "darkslategray4", "darkslategray"))+ 
  annotate("rect", xmin = lung_expected$period[51], xmax = lung_expected$period[55],
           ymin = 0, ymax = 119, alpha = .2) + 
  annotate("rect", xmin = lung_expected$period[57], xmax = lung_expected$period[64],
           ymin = 0, ymax = 119, alpha = .2) +
  scale_y_continuous(expand = c(0,0))










  




#################################################################################################
#################################################################################################



























##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
#################################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
###########################################################################################
##############################################################################################

# Junk
 
# Load Number of covid patients in hospital data 
CovidPatients_in_hospital <- read_csv("Data/CovidPatients_in_hospital2.csv")
CovidPatients_in_hospital$date <- as.Date(CovidPatients_in_hospital$date,  format = "%d/%m/%Y")
colnames(CovidPatients_in_hospital) <- c("period", "hospital_cases")



# Check if there are any NAs 
any(is.na(provider_level_data))
 # TRUE


# Inspect rows with NAs
provider_NA <- provider_level_data[rowSums(is.na(provider_level_data)) > 0,]
nrow(provider_NA)
 # 17951 rows with NA data


# Inspect the provider_code for NA rows 
table(provider_NA$provider_code)

table(provider_NA$period)


# Remove rows with missing data
provider_level_data <- na.omit(provider_level_data)


# Remove rows which have cancer_type = ALL CANCER
provider_level_data <- filter(provider_level_data, cancer_type != "ALL CANCERS")


# Notice that there are some cases with performance = 0
# Notice that there are many rows where total_treated = 0 and decimals like 0.5

# Inspect rows with performance = 0
View(filter(provider_level_data, performance == 0))


# Remove rows which have total_treated < 1
# provider_level_data <- provider_level_data %>% 
 #  filter(total_treated >= 1)

# low total treated numbers affect the performance rate a lot
# Try removing total treated numbers smaller than 10
#  <- provider_level_data %>% 
 #  filter(total_treated >= 10)

# nrow(provider_level_data)

# See the distribution of cancer types

table(provider_level_data$cancer_type)
prop.table(table(provider_level_data$cancer_type))

cancer_count <- as.data.frame(table(provider_level_data$cancer_type))

ggplot(data = cancer_count, aes(x = reorder(Var1, Freq), y = Freq)) + 
  labs(title = "Counts of Cancer Types", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip()



# See the distribution of different standard times
table(provider_level_data$standard)
prop.table(table(provider_level_data$standard))

standard_count <- as.data.frame(table(provider_level_data$standard))

ggplot(data = standard_count, aes(x = reorder(Var1, Freq), y = Freq)) + 
  labs(title = "Counts of Standard Types", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip()


# See distribution of regions 
table(provider_level_data$region_name)
prop.table(table(provider_level_data$region_name))

standard_count <- as.data.frame(table(provider_level_data$region_name))

ggplot(data = standard_count, aes(x = reorder(Var1, Freq), y = Freq)) + 
  labs(title = "Counts of Regions", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip()


# See the distribution of the performance 
ggplot(data = provider_level_data, aes(x = performance)) +
  geom_histogram()

summary(provider_level_data$performance)

sum(provider_level_data$within_standard)/sum(provider_level_data$total_treated)


# Check performance by standard 
ggplot(data = provider_level_data, aes(x = performance)) +
  geom_histogram() + facet_wrap(~standard)


# Check performance by cancer type 
ggplot(data = provider_level_data, aes(x = performance)) +
  geom_histogram() + facet_wrap(~cancer_type)



# Explore region

# Check performance by region 
ggplot(data = provider_level_data, aes(x = performance)) +
  geom_histogram() + facet_wrap(~region_name)

provider_level_data %>% 
  group_by(region_name) %>% 
  summarise(mean_performance = mean(performance))

# Check total treated numbers by region 
ggplot(data = provider_level_data, aes(x = total_treated)) +
  geom_histogram()





# Visualise Performance by region 
## taking average of performance
provider_plot_data1 <- provider_level_data %>% 
  select(period, region_name, performance) %>% 
  group_by(period, region_name) %>%
  summarise(performance = mean(performance), .groups = 'drop')

ggplot(data = provider_plot_data1, aes(x = period, y = performance, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Performance for Cancer Waiting Times") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "Commissioning Region")+
  theme_minimal()


# Visualise Performance by region 
# adding total treated and within standard and taking the average
provider_plot_data1 <- provider_level_data %>% 
  select(period, region_name, total_treated, within_standard) %>% 
  group_by(period, region_name) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop')

ggplot(data = provider_plot_data1, aes(x = period, y = performance, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Performance for Cancer Waiting Times") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "Commissioning Region")+
  theme_minimal()


# Visualise total treated cancer numbers by region 
provider_plot_data2 <- provider_level_data %>% 
  select(period, region_name, total_treated) %>% 
  group_by(period, region_name) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')

ggplot(data = provider_plot_data2, aes(x = period, y = total_treated, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Number of Treated Cancers") + 
  xlab("Years (in months)") + ylab("Number of Treated Cancer Cases")



# Visualise breach numbers by region
provider_plot_data3 <- provider_level_data %>% 
  select(period, region_name, breaches) %>% 
  group_by(period, region_name) %>%
  summarise(breaches = sum(breaches), .groups = 'drop')

ggplot(data = provider_plot_data3, aes(x = period, y = breaches, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Number of Breaches of Cancer Treatments") + 
  xlab("Years (in months)") + ylab("Number of Breaches")



# Add number of covid patients in hospitals to the total_treated plot
ylim.prim <- c(0, 240000)
ylim.sec <- c(0, 40000)

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

provider_plot_data2 <- provider_level_data %>% 
  select(period, region_name, total_treated) %>% 
  group_by(period, region_name) %>%
  summarise(total_treated = sum(total_treated), .groups = 'drop')

ggplot() +
  geom_line(data = provider_plot_data2, 
            aes(x = period, y = total_treated, group = region_name, 
                color = region_name)) +
  ggtitle("Number of Treated Cancers and Number of Covid Patients in Hospital") + 
  xlab("Years") + ylab("Number of Treated Cancer Cases")+
  geom_line(data = CovidPatients_in_hospital, 
            aes(x = period, y = a + hospital_cases*b))+ 
  scale_color_discrete(name = "Commissioning Region")+
  scale_y_continuous("\nNumber of Treated Cancer Cases", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "\nNumber of Covid Patients in Hospital"))+
  theme_classic()




# add covid to performance plot 
ylim.prim <- c(0.7, 1)
ylim.sec <- c(0, 40000)

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

ggplot() +
  geom_line(data = provider_plot_data1, 
            aes(x = period, y = performance, group = region_name, 
                color = region_name)) +
  ggtitle("Number of Treated Cancers and Number of Covid Patients in Hospital") + 
  xlab("Years") + ylab("Number of Treated Cancer Cases")+
  geom_line(data = CovidPatients_in_hospital, 
            aes(x = period, y = a + hospital_cases*b))+ 
  scale_color_discrete(name = "Commissioning Region")+
  scale_y_continuous("Number of Treated Cancer Cases", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Number of Covid Patients in Hospital"))+
  theme_minimal()




## Performance by cancer type - useless

provider_plot_data4 <- provider_level_data %>% 
  select(period, cancer_type, total_treated, within_standard) %>% 
  group_by(period, cancer_type) %>%
  summarise(performance = mean(sum(within_standard)/sum(total_treated)), .groups = 'drop')

ggplot(data = provider_plot_data4, aes(x = period, y = performance, group = cancer_type, color = cancer_type)) +
  geom_line() + ggtitle("Performance for Cancer Waiting Times") + 
  xlab("Years") + ylab("Performance rate") + 
  scale_color_discrete(name = "Commissioning Region")+
  theme_classic()




# Explore performance differences in different regions 

# select 31 days and lung cancer

lung_31 <- provider_level_data %>% 
  filter(cancer_type == "Lung", standard == "31 Days") %>%
  select(period, region_name, performance) %>% 
  group_by(period, region_name) %>%
  summarise(performance = mean(performance), .groups = 'drop')


# performance of regions 
ggplot(data = lung_31, aes(x = period, y = performance, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Performance for Cancer Patient 31 Days Referrals") + 
  xlab("Years (in months)") + ylab("Performance (%)")



  







