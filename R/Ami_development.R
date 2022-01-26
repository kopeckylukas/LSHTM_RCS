# Amiiii! 

## Load libraries 
library(tidyverse)
library(ggplot2)
library(xts)

# Load Cancer waiting time data 
library(readr)
provider_level_data <- read_csv("provider_level_data.csv")

# Load Number of covid patients in hospital data 
# Convert daily data into monthly 
CovidPatients_in_hospital <- read_csv("CovidPatients_in_hospital.csv")
CovidPatients_in_hospital$date <- as.Date(CovidPatients_in_hospital$date,  format = "%d/%m/%Y")
colnames(CovidPatients_in_hospital) <- c("period", "hospital_cases")





# Remove rows with missing data
provider_level_data <- na.omit(provider_level_data)

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
cancer_count <- as.data.frame(table(provider_level_data$cancer_type))

ggplot(data = cancer_count, aes(x = reorder(Var1, Freq), y = Freq)) + 
  labs(title = "Counts of Cancer Types", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip()


# See the distribution of different standard times
standard_count <- as.data.frame(table(provider_level_data$standard))

ggplot(data = standard_count, aes(x = reorder(Var1, Freq), y = Freq)) + 
  labs(title = "Counts of Standard Types", x = "", y = "Counts") +
  geom_bar(stat="identity") + 
  coord_flip()


# See the distribution of the performance 
ggplot(data = provider_level_data, aes(x = performance)) +
  geom_histogram()


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
provider_plot_data1 <- provider_level_data %>% 
  select(period, region_name, performance) %>% 
  group_by(period, region_name) %>%
  summarise(performance = mean(performance), .groups = 'drop')

ggplot(data = provider_plot_data1, aes(x = period, y = performance, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Performance for Cancer Waiting Times") + 
  xlab("Years (in months)") + ylab("Performance rate") + 
  scale_color_discrete(name = "Commissioning Region")



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
ggplot(data = provider_plot_data2, aes(x = period, y = total_treated, group = region_name, color = region_name)) +
  geom_line() + ggtitle("Number of Treated Cancers") + 
  xlab("Years (in months)") + ylab("Number of Treated Cancer Cases") +
  geom_line(data = CovidPatients_in_hospital, aes(x = period, y = hospital_cases))


ggplot() +
  geom_line(data = provider_plot_data2, aes(x = period, y = total_treated, group = region_name, color = region_name)) +
  ggtitle("Number of Treated Cancers") + 
  xlab("Years (in months)") + ylab("Number of Treated Cancer Cases")+
  geom_line(data = CovidPatients_in_hospital, aes(x = period, y = hospital_cases))+ 
  scale_color_discrete(name = "Commissioning Region") + 
  scale_color_manual(values = c("black"))



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



  







