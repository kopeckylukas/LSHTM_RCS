#Personal playground

library("readxl")
library("tidyverse")
library("read_csv")


########### Load Data #############

data_21_22    <- read_csv("data/CSV_data/data_21.csv")
data_20_21 <- read_csv("data/CSV_data/data_20_21.csv")
data_19_20 <- read_csv("data/CSV_data/data_19_20.csv")
data_18_19 <- read_csv("data/CSV_data/data_18_19.csv")
data_17_18 <- read_csv("data/CSV_data/data_17_18.csv")
data_16_17 <- read_csv("data/CSV_data/data_16_17.csv")
data_15_16 <- read_csv("data/CSV_data/data_15_16.csv")

############## Data Prep ###########

#delete empty rows in files 
data_21_22 <- data_21_22[rowSums(is.na(data_21_22)) != ncol(data_21_22),]
data_20_21 <- data_20_21[rowSums(is.na(data_20_21)) != ncol(data_20_21),]
data_19_20 <- data_19_20[rowSums(is.na(data_19_20)) != ncol(data_19_20),]
data_18_19 <- data_18_19[rowSums(is.na(data_18_19)) != ncol(data_18_19),]
data_17_18 <- data_17_18[rowSums(is.na(data_17_18)) != ncol(data_17_18),]
data_16_17 <- data_16_17[rowSums(is.na(data_16_17)) != ncol(data_16_17),]
data_15_16 <- data_15_16[rowSums(is.na(data_15_16)) != ncol(data_15_16),]

#add column to 19-21
data_19_20$area_team <- NA
data_20_21$area_team <- NA
data_21_22$area_team <- NA

#change oder of columns
data_19_20 <- data_19_20[, c(1:4, 11, 5:10)]
data_20_21 <- data_20_21[, c(1:4, 11, 5:10)]
data_21_22 <- data_21_22[, c(1:4, 11, 5:10)]

#standardise column names
standard_names <- c("period","year", "month", "standard", "area_team", "org_code", "care_settings",
                    "cancer_type", "total_treated", "within_standards", "breaches")

colnames(data_15_16) <- standard_names
colnames(data_16_17) <- standard_names
colnames(data_17_18) <- standard_names
colnames(data_18_19) <- standard_names
colnames(data_19_20) <- standard_names
colnames(data_20_21) <- standard_names
colnames(data_21_22) <- standard_names

#binding data together
rm(provider_data)
provider_data <- rbind(data_15_16,data_16_17)
provider_data <- rbind(provider_data,data_17_18)
provider_data <- rbind(provider_data,data_18_19)
provider_data <- rbind(provider_data,data_19_20)
provider_data <- rbind(provider_data,data_20_21)
provider_data <- rbind(provider_data,data_21_22)

View(provider_data)

#sort dataset
provider_data <- provider_data %>% arrange(period, standard) 

# do not ower write data if no necessary 
# write_csv(provider_data,"data/data_complete.csv",na="")

# THERE MUST BE A STEP IN BETWEEN WHERE DATES ARE CORRECTED IN MS-EXCEL TO FORMAT
# mm-dd-yyyy EG 11-01-2020

#Load data
data <- read_csv("data/provider_level_data.csv")

data <- read_csv("https://lshtm-my.sharepoint.com/:x:/g/personal/lsh2102097_student_lshtm_ac_uk/ESJZ6EtA20JBtroPRVobJl8BaENseHyqdQoN_awQryueNQ?e=blHOwT")

############### Merging provider data ##################
key <- read_csv("data/Commissioners_key.csv")

colnames(key) <- c("org_code","org_name","region_code","region_name","area","area_name")

data2 <- merge(data, key, by='org_code', all.x = TRUE)



data2 <- data2[,c(2,1,12:14,5,7:11)]



colnames(data2) <- c("period","provider_code","provider_name","region_code","region_name","standard"
                     ,"care_settings","cancer_type","total_treated","within_standard","breaches")

data2 <- data2 %>% arrange(period,  standard, provider_code) 

data2$performance <- data2$within_standard / data2$total_treated

##data2 <- data2[,c(1:6,8:12)]

write_csv(data2,"data/provider_level_data_y.csv",na="")


############### Basic Data Analysis ####################

#number records
nrow(provider_data) #812,119

#cancer types
n_distinct(data3$provider_code)  #30

unique(data3[c("provider_code")])

#number of organisations
  n_distinct(provider_data$org_code)     #245

#
  sum(provider_data$total_treated)
  sum(provider_data$breaches)
  
   summarise
  
  sum(provider_data$breaches)/sum(provider_data$total_treated)

  data2 <- data %>% filter(period >= as.Date("2016-01-01"))
  
  #percentage of breaches before 
  data %>% filter(period >= as.Date("2016-01-01") & standard == "2WW") %>% summarise(sum(breaches))/data %>% filter(period >= as.Date("2020-03-01") & standard == "2WW") %>% summarise(sum(total_treated))
    #0.1335218
  data %>% filter(period < as.Date("2020-03-01") & standard == "2WW") %>% summarise(sum(breaches))/data %>% filter(period < as.Date("2020-03-01") & standard == "2WW") %>% summarise(sum(total_treated))
    #0.07038327
  
  data %>% filter(period >= as.Date("2020-03-01") & standard == "31 Days") %>% summarise(sum(breaches)) / data %>% filter(period >= as.Date("2020-03-01") & standard == "31 Days") %>% summarise(sum(total_treated))
    #0.05369623
  data %>% filter(period < as.Date("2020-03-01") & standard == "31 Days") %>% summarise(sum(breaches)) / data %>% filter(period < as.Date("2020-03-01") & standard == "31 Days") %>% summarise(sum(total_treated))
    #0.02931742
  
  data %>% filter(period >= as.Date("2020-03-01") & standard == "31 Days" & cancer_type == "Breast") %>% summarise(sum(breaches))/data %>% filter(period >= as.Date("2020-03-01") & standard == "31 Days" & cancer_type == "Breast") %>% summarise(sum(total_treated))
    #0.0516772
  data %>% filter(period < as.Date("2020-03-01") & standard == "31 Days" & cancer_type == "Breast") %>% summarise(sum(breaches))/data %>% filter(period < as.Date("2020-03-01") & standard == "31 Days" & cancer_type == "Breast") %>% summarise(sum(total_treated))
    #0.01717123
  
  #total refered to specialist
  data %>% filter(standard == "2WW") %>% summarise(sum(total_treated))
    #14016320
  
  #total suspected breast C referrals 
  data %>% filter(standard == "2WW" & cancer_type == "Suspected breast cancer") %>% summarise(sum(total_treated))
    #2604556
  
  #total suspected lung C referrals 
  data %>% filter(standard == "2WW" & cancer_type == "Suspected lung cancer") %>% summarise(sum(total_treated))
    #397724
  
  
  #Provider Level
  plt.dt <- data %>% filter(standard == "2WW" & cancer_type == "Suspected lung cancer") %>% group_by(period, org_code) %>% summarise(sum(total_treated))
  
  
  ggplot(plt.dt, aes(period, `sum(total_treated)`,colour = org_code)) + 
    geom_point() + ggtitle("Number of Referrals for Suspected Lung Cacner") +
    xlab("Data in months") + ylab("Number of patient referred to specialist (providers)")+ theme(legend.position = "none")
  
  
  #National Levelx
  plt.dt <- data %>% filter(standard == "2WW" & cancer_type == "Suspected lung cancer") %>% group_by(period) %>% summarise(sum(total_treated))
  
  
  ggplot(plt.dt, aes(period, `sum(total_treated)`)) + 
    geom_point() + ggtitle("Number of Referrals for Suspected Lung Cacner") +
    xlab("Data in months") + ylab("Number of patient referred to specialist (nationally)")
  
  #commisioner level
  plt.dt <- data2 %>% filter(standard == "2WW" & cancer_type == "Suspected lung cancer") %>% group_by(period, commisioner_name) %>% summarise(sum(total_treated))
  
  
  ggplot(plt.dt, aes(period, `sum(total_treated)`, colour = commisioner_name)) + 
    geom_point() + ggtitle("Number of Referrals for Suspected Lung Cacner") +
    xlab("Data in months") + ylab("Number of patient referred to specialist (nationally)")
  
  
  
  data %>% summarise(max(period))
  
  
  
  prov <- read_csv("Data/provider_level_data.csv")

  post <- read_csv("Data/postcode_list.csv")  
  post <- post[,1:2]

  newprov <- merge(prov,post,by="provider_code", all.x = TRUE)
  
  write_csv(post,"data/postcodes.csv",na="")
  
  