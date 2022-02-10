setwd("C:/Users/1015k/Desktop/LSHTM/Data Challenge/R")

install.packages('prophet')

library(prophet)
library(dplyr)
library(tibble)
library(caret)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(gridExtra)

packageVersion("prophet")

# R

install.packages(c("cmdstanr", "posterior"), repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

cmdstanr::install_cmdstan()

# Set the R_STAN_BACKEND environment variable
Sys.setenv(R_STAN_BACKEND = "CMDSTANR")

#Prepare data
df1 <- read.csv('../Dataset/Cleaned Data/provider_level_data.csv')

str(df1)
df1 <- na.omit(df1)

#IMPORTANT: Need to change datetime column name into 'ds' (datestamp) for prophet
#IMPORTANT: Need to change column name that we want to predict into 'y'
#WARNINGS: Too many df names below.
#Which means 1 df for 1 prophet model

names(df1)[1] <- 'ds'

df1$ds <- as.Date(df1$ds)
names(df1)[10] <- 'y'
df1$y = round(df1$y*100, 2)

#Change class
df1$period <- format(as.Date(df1$period), "%Y-%m-%d")
df1$cancer_type <- as.factor(df1$cancer_type)
df1$standard <- as.factor(df1$standard)
df1$region_name <- as.factor(df1$region_name)

#Remove unused cancer type
df1 <- filter(df1, cancer_type != "ALL CANCERS")


levels(df1$region_name) <- list(Unknown = "",
                       London = "LONDON COMMISSIONING REGION",
                       Midlands = "MIDLANDS AND EAST OF ENGLAND COMMISSIONING REGION",
                       North = "NORTH OF ENGLAND COMMISSIONING REGION",
                       South = "SOUTH OF ENGLAND COMMISSIONING REGION")

#Remove Unknown region
df1 <- filter(df1, region_name != "Unknown")

#Remove unused standard
df1 <- filter(df1, standard != "28 Days FDS")
df1 <- filter(df1, standard != "28 Days FDS (By Route)")
df1 <- filter(df1, standard != "31 Days Sub (Drugs)")
df1 <- filter(df1, standard != "31 Days Sub (Radio)")
df1 <- filter(df1, standard != "31 Days Sub (Surgery)")
df1 <- filter(df1, standard != "2WW Breast")

#Remove unused columns
df1 <- subset(df1, select = -c(provider_code, provider_name))

#Select data period
df_pre <- filter(df1, ds < "2020-3-1")
str(df_pre)

df <- subset(df_pre, select = c(ds, region_name, y))
str(df)

#################################################

#Create df for average performance prediction in each region (contains provider level data)
#IMPORTANT: The final df before using prophet should contain only 2 columns: 'y' and 'ds'
#Create df across UK
df_UK <- subset(df, select = c(ds, y))

#Create df in each region
df_lon <- subset(df, region_name == "London")
df_lon <- subset(df_lon, select = c(ds, y))

df_mid <- subset(df, region_name == "Midlands")
df_mid <- subset(df_mid, select = c(ds, y))

df_nor <- subset(df, region_name == "North")
df_nor <- subset(df_nor, select = c(ds, y))

df_sou <- subset(df, region_name == "South")
df_sou <- subset(df_sou, select = c(ds, y))

#################################################

#Prophet for average perfomance across UK
m_UK <- prophet(df_UK)

#################################################
#COVID period df use for every prophet model

future <- make_future_dataframe(m_UK, periods = 24, freq='month')
#################################################

forecast_UK <- predict(m_UK, future)
tail(forecast_UK[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Prophet in each region
m_lon <- prophet(df_lon)
forecast_lon <- predict(m_lon, future) 
#The future df should have all the date we need (because it combined all regions)

m_mid <- prophet(df_mid)
forecast_mid <- predict(m_mid, future)

m_nor <- prophet(df_nor)
forecast_nor <- predict(m_nor, future)

m_sou <- prophet(df_sou)
forecast_sou <- predict(m_sou, future)

#################################################

#Visualization from prophet package

plot(m_UK, forecast_UK)
prophet_plot_components(m_UK, forecast_UK)

plot(m_lon, forecast_lon)
prophet_plot_components(m_lon, forecast_lon)

plot(m_mid, forecast_mid)
prophet_plot_components(m_mid, forecast_mid)

plot(m_nor, forecast_nor)
prophet_plot_components(m_nor, forecast_nor)

plot(m_sou, forecast_sou)
prophet_plot_components(m_sou, forecast_sou)

#Create graph with GGplot

col_yhat <- c("Across UK" = "#FF6666", 
              "London" = "#FFC425", 
              "Midlands & East" = "#00CC66", 
              "North" = "#00CCFF", 
              "South" = "#CC66FF")

yhat_lon <- subset(forecast_lon, select = c(yhat))
yhat_mid <- subset(forecast_mid, select = c(yhat))
yhat_nor <- subset(forecast_nor, select = c(yhat))
yhat_sou <- subset(forecast_sou, select = c(yhat))

forecast_yhat <- as.data.frame(subset(forecast_UK, select = c(ds, yhat)))
forecast_yhat <- cbind(forecast_yhat, yhat_lon, yhat_mid, yhat_nor, yhat_sou)

names(forecast_yhat)[3] <- 'yhat_lon'
names(forecast_yhat)[4] <- 'yhat_mid'
names(forecast_yhat)[5] <- 'yhat_nor'
names(forecast_yhat)[6] <- 'yhat_sou'

yhat_plot <- ggplot(forecast_yhat, aes(x = ds)) +
  geom_line(aes(y = yhat, colour="Across UK"), lwd=0.75) +
  geom_line(aes(y = yhat_lon, colour="London"), lwd=0.75) +
  geom_line(aes(y = yhat_mid, colour="Midlands & East"), lwd=0.75) +
  geom_line(aes(y = yhat_nor, colour="North"), lwd=0.75) +
  geom_line(aes(y = yhat_sou, colour="South"), lwd=0.75) +
  labs(title="CWT pre COVID Average Performance Prediction in each UK Region",
       subtitle = "with using data before the cutpoint at 1st lock down in Mar 2020 & across All Cancer Types", 
       x="Time", y = "Performance") +
  geom_vline(xintercept = as.numeric(forecast_yhat$ds[51]),
             linetype = "dotted") +
  scale_color_manual(values = col_yhat) +
  theme_bw()

yhat_plot

#Compare with real average performance data

real_per <- df1 %>%
  group_by(ds, region_name) %>%
  summarise_at(vars(y), list(y = mean))

real_per_UK <- df1 %>%
  group_by(ds) %>%
  summarise_at(vars(y), list(y = mean))

real_lon <- filter(real_per, region_name == "London")
real_lon <- subset(real_lon, select = c(y))

real_mid <- filter(real_per, region_name == "Midlands")
real_mid <- subset(real_mid, select = c(y))

real_nor <- filter(real_per, region_name == "North")
real_nor <- subset(real_nor, select = c(y))

real_sou <- filter(real_per, region_name == "South")
real_sou <- subset(real_sou, select = c(y))

real_per_reg <- cbind(real_per_UK, real_lon, real_mid, real_nor, real_sou)

names(real_per_reg)[3] <- 'y_lon'
names(real_per_reg)[4] <- 'y_mid'
names(real_per_reg)[5] <- 'y_nor'
names(real_per_reg)[6] <- 'y_sou'

region_per <- ggplot(real_per_reg, aes(x = ds)) +
  geom_line(aes(y = y, colour="Across UK"), lwd=0.75) +
  geom_line(aes(y = y_lon, colour="London"), lwd=0.75) +
  geom_line(aes(y = y_mid, colour="Midlands & East"), lwd=0.75) +
  geom_line(aes(y = y_nor, colour="North"), lwd=0.75) +
  geom_line(aes(y = y_sou, colour="South"), lwd=0.75) +
  labs(title="CWT Average Performance in each UK Region",
       subtitle = "with cutpoint at 1st lock down in Mar 2020 & across All Cancer Types", 
       x="Time", y = "Performance") +
  geom_vline(xintercept = as.numeric(real_per$ds[201]),
             linetype = "dotted") +
  scale_color_manual(values = col_yhat) +
  theme_bw()

region_per

Avg_Per <- grid.arrange(yhat_plot, region_per)

#########################################
#Using summation of performance across region instead of average performance 
#for better representation. Because providers with very low performance (~0%)
#usually came from the provider with > 5 patients/month

#Create

grouped_period1 <- df1 %>%
  group_by(ds, region_name) %>%
  summarise(sum_treated = sum(total_treated))

grouped_period2 <- df1 %>%
  group_by(ds, region_name) %>%
  summarise(y = sum(within_standard))

df_new <- cbind(grouped_period1, grouped_period2[3])
df_new[4] <- (df_new[4])/(df_new[3])*100

df_new_pre <- filter(df_new, ds < "2020-3-1")

df_new2 <- subset(df_new_pre, select = c(ds, region_name, y))


#Create df across UK
df_UK_new <- subset(df_new2, select = c(ds, y))

#Create df in each region
df_lon_new <- subset(df_new2, region_name == "London")
df_lon_new <- subset(df_lon_new, select = c(ds, y))

df_mid_new <- subset(df_new2, region_name == "Midlands")
df_mid_new <- subset(df_mid_new, select = c(ds, y))

df_nor_new <- subset(df_new2, region_name == "North")
df_nor_new <- subset(df_nor_new, select = c(ds, y))

df_sou_new <- subset(df_new2, region_name == "South")
df_sou_new <- subset(df_sou_new, select = c(ds, y))

#Prophet across UK
m_UK_new <- prophet(df_UK_new)
future_new <- make_future_dataframe(m_UK_new, periods = 24, freq='month')
forecast_UK_new <- predict(m_UK_new, future_new)

#Prophet in each region
m_lon_new <- prophet(df_lon_new)
forecast_lon_new <- predict(m_lon_new, future_new)

m_mid_new <- prophet(df_mid_new)
forecast_mid_new <- predict(m_mid_new, future_new)

m_nor_new <- prophet(df_nor_new)
forecast_nor_new <- predict(m_nor_new, future_new)

m_sou_new <- prophet(df_sou_new)
forecast_sou_new <- predict(m_sou_new, future_new)

#############
# Visualization for prediction with summation of performance
# Prophet Component Visualization in each UK region

plot(m_UK_new, forecast_UK_new)
prophet_plot_components(m_UK_new, forecast_UK_new)

plot(m_lon_new, forecast_lon_new)
prophet_plot_components(m_lon_new, forecast_lon_new)

plot(m_mid_new, forecast_mid_new)
prophet_plot_components(m_mid_new, forecast_mid_new)

plot(m_nor_new, forecast_nor_new)
prophet_plot_components(m_nor_new, forecast_nor_new)

plot(m_sou_new, forecast_sou_new)
prophet_plot_components(m_sou_new, forecast_sou_new)

# Create summarize visualization for prediction
# Create new dataset for ggplot2

yhat_lon_new <- subset(forecast_lon_new, select = c(yhat))
yhat_mid_new <- subset(forecast_mid_new, select = c(yhat))
yhat_nor_new <- subset(forecast_nor_new, select = c(yhat))
yhat_sou_new <- subset(forecast_sou_new, select = c(yhat))

forecast_yhat_new <- as.data.frame(subset(forecast_UK_new, select = c(ds, yhat)))
forecast_yhat_new <- cbind(forecast_yhat_new, yhat_lon_new
                           , yhat_mid_new, yhat_nor_new, yhat_sou_new)

names(forecast_yhat_new)[3] <- 'yhat_lon'
names(forecast_yhat_new)[4] <- 'yhat_mid'
names(forecast_yhat_new)[5] <- 'yhat_nor'
names(forecast_yhat_new)[6] <- 'yhat_sou'

# Coding with ggplot2

yhat_plot_new <- ggplot(forecast_yhat_new, aes(x = ds)) +
  geom_line(aes(y = yhat, colour="Across UK"), lwd=0.75) +
  geom_line(aes(y = yhat_lon, colour="London"), lwd=0.75) +
  geom_line(aes(y = yhat_mid, colour="Midlands & East"), lwd=0.75) +
  geom_line(aes(y = yhat_nor, colour="North"), lwd=0.75) +
  geom_line(aes(y = yhat_sou, colour="South"), lwd=0.75) +
  labs(title="Pre-COVID Prediction CWT Using Summation Performance across UK Regions in All Cancer Types",
       subtitle = "cutpoint at 1st lock down in Mar 2020", 
       x="Time", y = "Performance") +
  geom_vline(xintercept = as.numeric(forecast_yhat_new$ds[51]),
             linetype = "dotted") +
  scale_color_manual(values = col_yhat) +
  theme_bw()

yhat_plot_new

# Create visualization to compare with real performance summation data
# Create new dataset for ggplot2

real_per_sum <- df_new %>%
  group_by(ds, region_name) %>%
  summarise_at(vars(y), list(y = mean))

real_per_UK_sum <- df_new %>%
  group_by(ds) %>%
  summarise_at(vars(y), list(y = mean))

real_lon_sum <- filter(real_per_sum, region_name == "London")
real_lon_sum <- subset(real_lon_sum, select = c(y))

real_mid_sum <- filter(real_per_sum, region_name == "Midlands")
real_mid_sum <- subset(real_mid_sum, select = c(y))

real_nor_sum <- filter(real_per_sum, region_name == "North")
real_nor_sum <- subset(real_nor_sum, select = c(y))

real_sou_sum <- filter(real_per_sum, region_name == "South")
real_sou_sum <- subset(real_sou_sum, select = c(y))

real_per_reg_sum <- cbind(real_per_UK_sum, real_lon_sum, real_mid_sum,
                          real_nor_sum, real_sou_sum)

names(real_per_reg_sum)[3] <- 'y_lon'
names(real_per_reg_sum)[4] <- 'y_mid'
names(real_per_reg_sum)[5] <- 'y_nor'
names(real_per_reg_sum)[6] <- 'y_sou'

region_per_sum <- ggplot(real_per_reg_sum, aes(x = ds)) +
  geom_line(aes(y = y, colour="Across UK"), lwd=0.75) +
  geom_line(aes(y = y_lon, colour="London"), lwd=0.75) +
  geom_line(aes(y = y_mid, colour="Midlands & East"), lwd=0.75) +
  geom_line(aes(y = y_nor, colour="North"), lwd=0.75) +
  geom_line(aes(y = y_sou, colour="South"), lwd=0.75) +
  labs(title="CWT Summation Performance across UK Regions in All Cancer Types",
       subtitle = "cutpoint at 1st lock down in Mar 2020", 
       x="Time", y = "Performance") +
  geom_vline(xintercept = as.numeric(real_per_sum$ds[201]),
             linetype = "dotted") +
  scale_color_manual(values = col_yhat) +
  theme_bw()

region_per_sum

Sum_Per <- grid.arrange(yhat_plot_new, region_per_sum)
########################################
# Total treated
# Repeat data preparation step, change 'y' column from performance/sum_performance to total_treated
df_tr1 <- subset(df1, select = -c(y))
df_tr1 <- filter(df_tr1, ds < "2020-3-1")

df_tr <- df_tr1 %>%
  group_by(ds, region_name) %>%
  summarise(y = sum(total_treated))

#Create df for total treated patient
#Create df across UK
df_UK_tr <- df_tr1 %>%
  group_by(ds) %>%
  summarise(y = sum(total_treated))

#Create df in each region
df_lon_tr <- subset(df_tr, region_name == "London")
df_lon_tr <- subset(df_lon_tr, select = c(ds, y))

df_mid_tr <- subset(df_tr, region_name == "Midlands")
df_mid_tr <- subset(df_mid_tr, select = c(ds, y))

df_nor_tr <- subset(df_tr, region_name == "North")
df_nor_tr <- subset(df_nor_tr, select = c(ds, y))

df_sou_tr <- subset(df_tr, region_name == "South")
df_sou_tr <- subset(df_sou_tr, select = c(ds, y))

#################################################

#Prophet for average perfomance across UK
m_UK_tr <- prophet(df_UK_tr)

forecast_UK_tr <- predict(m_UK_tr, future)
#The future df from previous prophet should have all the date we need 
#(because it combined all regions)

#Prophet in each region
m_lon_tr <- prophet(df_lon_tr)
forecast_lon_tr <- predict(m_lon_tr, future) 

m_mid_tr <- prophet(df_mid_tr)
forecast_mid_tr <- predict(m_mid_tr, future)

m_nor_tr <- prophet(df_nor_tr)
forecast_nor_tr <- predict(m_nor_tr, future)

m_sou_tr <- prophet(df_sou_tr)
forecast_sou_tr <- predict(m_sou_tr, future)

#################################################

#Visualization from prophet package

plot(m_UK_tr, forecast_UK_tr)
prophet_plot_components(m_UK_tr, forecast_UK_tr)

plot(m_lon_tr, forecast_lon_tr)
prophet_plot_components(m_lon_tr, forecast_lon_tr)

plot(m_mid_tr, forecast_mid_tr)
prophet_plot_components(m_mid_tr, forecast_mid_tr)

plot(m_nor_tr, forecast_nor_tr)
prophet_plot_components(m_nor_tr, forecast_nor_tr)

plot(m_sou_tr, forecast_sou_tr)
prophet_plot_components(m_sou_tr, forecast_sou_tr)

#Create graph with GGplot

yhat_lon_tr <- subset(forecast_lon_tr, select = c(yhat))
yhat_mid_tr <- subset(forecast_mid_tr, select = c(yhat))
yhat_nor_tr <- subset(forecast_nor_tr, select = c(yhat))
yhat_sou_tr <- subset(forecast_sou_tr, select = c(yhat))

forecast_yhat_tr <- as.data.frame(subset(forecast_UK_tr, select = c(ds, yhat)))
forecast_yhat_tr <- cbind(forecast_yhat_tr, yhat_lon_tr, yhat_mid_tr,
                          yhat_nor_tr, yhat_sou_tr)

names(forecast_yhat_tr)[3] <- 'yhat_lon'
names(forecast_yhat_tr)[4] <- 'yhat_mid'
names(forecast_yhat_tr)[5] <- 'yhat_nor'
names(forecast_yhat_tr)[6] <- 'yhat_sou'

yhat_plot_tr <- ggplot(forecast_yhat_tr, aes(x = ds)) +
  geom_line(aes(y = yhat, colour="Across UK"), lwd=0.75) +
  geom_line(aes(y = yhat_lon, colour="London"), lwd=0.75) +
  geom_line(aes(y = yhat_mid, colour="Midlands & East"), lwd=0.75) +
  geom_line(aes(y = yhat_nor, colour="North"), lwd=0.75) +
  geom_line(aes(y = yhat_sou, colour="South"), lwd=0.75) +
  labs(title="Total Treated Patients pre COVID Prediction in each UK Region",
       subtitle = "with using data before the cutpoint at 1st lock down in Mar 2020 & across All Cancer Types", 
       x="Time", y = "Total Treated") +
  geom_vline(xintercept = as.numeric(forecast_yhat_tr$ds[51]),
             linetype = "dotted") +
  scale_y_continuous(trans = log10_trans(),
                       breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = col_yhat) +
  theme_bw()

yhat_plot_tr

# Create visualization to compare with real total treated data
# Create new df for ggplot2

df_tr2 <- subset(df1, select = -c(y))

df_tr_r <- df_tr2 %>%
  group_by(ds, region_name) %>%
  summarise(y = sum(total_treated))

#Create df across UK
df_UK_tr_r <- df_tr2 %>%
  group_by(ds) %>%
  summarise(y = sum(total_treated))

df_lon_tr_r <- filter(df_tr_r, region_name == "London")
df_lon_tr_r <- subset(df_lon_tr_r, select = c(y))

df_mid_tr_r <- filter(df_tr_r, region_name == "Midlands")
df_mid_tr_r <- subset(df_mid_tr_r, select = c(y))

df_nor_tr_r <- filter(df_tr_r, region_name == "North")
df_nor_tr_r <- subset(df_nor_tr_r, select = c(y))

df_sou_tr_r <- filter(df_tr_r, region_name == "South")
df_sou_tr_r <- subset(df_sou_tr_r, select = c(y))

df_tr_r_com <- cbind(df_UK_tr_r, df_lon_tr_r, df_mid_tr_r, 
                     df_nor_tr_r, df_sou_tr_r)

names(df_tr_r_com)[3] <- 'y_lon'
names(df_tr_r_com)[4] <- 'y_mid'
names(df_tr_r_com)[5] <- 'y_nor'
names(df_tr_r_com)[6] <- 'y_sou'

region_tr <- ggplot(df_tr_r_com, aes(x = ds)) +
  geom_line(aes(y = y, colour="Across UK"), lwd=0.75) +
  geom_line(aes(y = y_lon, colour="London"), lwd=0.75) +
  geom_line(aes(y = y_mid, colour="Midlands & East"), lwd=0.75) +
  geom_line(aes(y = y_nor, colour="North"), lwd=0.75) +
  geom_line(aes(y = y_sou, colour="South"), lwd=0.75) +
  labs(title="Total Treated Patients in each UK Region",
       subtitle = "with the cutpoint at 1st lock down in Mar 2020 & across All Cancer Types", 
       x="Time", y = "Total Treated") +
  geom_vline(xintercept = as.numeric(df_tr_r_com$ds[51]),
             linetype = "dotted") +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = col_yhat) +
  theme_bw()

region_tr

Total_treated <- grid.arrange(yhat_plot_tr, region_tr)
########################################
# Specific cancer types
df_ca <- df_pre %>% 
  select(ds, cancer_type, total_treated, standard) %>% 
  group_by(ds, cancer_type, standard) %>%
  summarise(y = sum(total_treated), .groups = 'drop') %>%
  filter(cancer_type %in% c("Breast", "Lung", "Suspected breast cancer", 
  "Suspected lung cancer"))

# Breast
df_ca_b <- filter(df_ca, cancer_type == "Breast"| 
                    cancer_type == "Suspected breast cancer")

df_ca_b_31 <- filter(df_ca_b, standard == "31 Days")
df_ca_b_31 <- subset(df_ca_b_31, select = c(ds, y))

df_ca_b_62 <- filter(df_ca_b, standard == "62 Days")
df_ca_b_62 <- subset(df_ca_b_62, select = c(ds, y))

df_ca_b_2ww <- filter(df_ca_b, standard == "2WW")
df_ca_b_2ww <- subset(df_ca_b_2ww, select = c(ds, y))

#################################################
#Prophet for specific cancer types in each standard

m_ca_b_31 <- prophet(df_ca_b_31)
forecastca_b_31 <- predict(m_ca_b_31, future) 

m_ca_b_62 <- prophet(df_ca_b_62)
forecastca_b_62 <- predict(m_ca_b_62, future)

m_ca_b_2ww <- prophet(df_ca_b_2ww)
forecastca_b_2ww <- predict(m_ca_b_2ww, future)

#################################################
#Visualization from prophet package

plot(m_ca_b_31, forecastca_b_31)
prophet_plot_components(m_ca_b_31, forecastca_b_31)

plot(m_ca_b_62, forecastca_b_62)
prophet_plot_components(m_ca_b_62, forecastca_b_62)

plot(m_ca_b_2ww, forecastca_b_2ww)
prophet_plot_components(m_ca_b_2ww, forecastca_b_2ww)

#Create graph with GGplot

col_ca_b <- c("31 Days" = "#A50f15", 
              "62 Days" = "#67000D", 
              "2WW (Suspected)" = "#FB6A4A")

yhat_b_62 <- subset(forecastca_b_62, select = c(yhat))
yhat_b_2ww <- subset(forecastca_b_2ww, select = c(yhat))

forecast_yhat_ca_b <- as.data.frame(subset(forecastca_b_31, select = c(ds, yhat)))
forecast_yhat_ca_b <- cbind(forecast_yhat_ca_b, yhat_b_62, yhat_b_2ww)

names(forecast_yhat_ca_b)[3] <- 'yhat_62'
names(forecast_yhat_ca_b)[4] <- 'yhat_2ww'

yhat_plot_ca_b <- ggplot(forecast_yhat_ca_b, aes(x = ds)) +
  geom_line(aes(y = yhat, colour="31 Days"), lwd=0.75) +
  geom_line(aes(y = yhat_62, colour="62 Days"), lwd=0.75) +
  geom_line(aes(y = yhat_2ww, colour="2WW (Suspected)"), lwd=0.75) +
  labs(title="Pre-COVID Prediction in Breast Cancer Total Treated",
       subtitle = "with using data before the cutpoint at 1st lock down in Mar 2020", 
       x="Time", y = "Total Treated") +
  geom_vline(xintercept = as.numeric(forecast_yhat_ca_b$ds[51]),
             linetype = "dotted") +
  scale_color_manual(values = col_ca_b)

yhat_plot_ca_b

###########################################

##Create df for total performance prediction in each standard

grouped_period_st <- df1 %>%
  group_by(ds, standard) %>%
  summarise(sum_treated = sum(total_treated))

grouped_period_st1 <- df1 %>%
  group_by(ds, standard) %>%
  summarise(y = sum(within_standard))

df_st <- cbind(grouped_period_st, grouped_period_st1[3])
df_st[4] <- (df_st[4])/(df_st[3])*100

df_st_pre <- filter(df_st, ds < "2020-3-1")

df_st2 <- subset(df_st_pre, select = c(ds, standard, y))

#Create df in each standard
df_2ww <- subset(df_st2, standard == "2WW")
df_2ww <- subset(df_2ww, select = c(ds, y))

df_31d <- subset(df_st2, standard == "31 Days")
df_31d <- subset(df_31d, select = c(ds, y))

df_62d <- subset(df_st2, standard == "62 Days")
df_62d <- subset(df_62d, select = c(ds, y))

#Prophet in each standard
m_2ww <- prophet(df_2ww)
forecast_2ww <- predict(m_2ww, future)

m_31d <- prophet(df_31d)
forecast_31d <- predict(m_31d, future)

m_62d <- prophet(df_62d)
forecast_62d <- predict(m_62d, future)
#############
# Visualization for prediction with summation of performance
# Prophet Component Visualization in each UK region

plot(m_2ww, forecast_2ww)
prophet_plot_components(m_2ww, forecast_2ww)

plot(m_31d, forecast_31d)
prophet_plot_components(m_31d, forecast_31d)

plot(m_62d, forecast_62d)
prophet_plot_components(m_62d, forecast_62d)

# Create summarize visualization for prediction
# Create new dataset for ggplot2

col_yhat_st <- c("2WW" = "#FF6666", 
              "31 Days" = "#2B8CBE", 
              "62 Days" = "#00CC66")

yhat_31d <- subset(forecast_31d, select = c(yhat))
yhat_62d <- subset(forecast_62d, select = c(yhat))

forecast_yhat_st <- as.data.frame(subset(forecast_2ww, select = c(ds, yhat)))
forecast_yhat_st <- cbind(forecast_yhat_st, yhat_31d, yhat_62d)

names(forecast_yhat_st)[3] <- 'yhat_31d'
names(forecast_yhat_st)[4] <- 'yhat_62d'

# Coding with ggplot2

yhat_plot_st <- ggplot(forecast_yhat_st, aes(x = ds)) +
  geom_line(aes(y = yhat, colour="2WW"), lwd=0.75) +
  geom_line(aes(y = yhat_31d, colour="31 Days"), lwd=0.75) +
  geom_line(aes(y = yhat_62d, colour="62 Days"), lwd=0.75) +
  labs(title="Pre-COVID Prediction CWT Using Summation Performance in Each Standard",
       subtitle = "cutpoint at 1st lock down in Mar 2020", 
       x="Time", y = "Performance") +
  geom_vline(xintercept = as.numeric(forecast_yhat_st$ds[51]),
             linetype = "dotted") +
  geom_hline(yintercept = 93, linetype = "dashed", colour="#FF6666", lwd=0.75) +
  geom_hline(yintercept = 96, linetype = "dashed", colour="#2B8CBE", lwd=0.75) +
  geom_hline(yintercept = 85, linetype = "dashed", colour="#00CC66", lwd=0.75) +
  scale_color_manual(values = col_yhat_st) +
  theme_bw()

yhat_plot_st

# Create visualization to compare with real performance summation data
# Create new dataset for ggplot2

real_st <- df_st %>%
  group_by(ds, standard) %>%
  summarise_at(vars(y), list(y = mean))

real_2ww <- filter(real_st, standard == "2WW")
real_2ww <- subset(real_2ww, select = c(ds, y))

real_31d <- filter(real_st, standard == "31 Days")
real_31d <- subset(real_31d, select = c(y))

real_62d <- filter(real_st, standard == "62 Days")
real_62d <- subset(real_62d, select = c(y))

real_per_st <- cbind(real_2ww, real_31d, real_62d)

names(real_per_st)[2] <- 'y_2ww'
names(real_per_st)[3] <- 'y_31d'
names(real_per_st)[4] <- 'y_62d'

st_per_sum <- ggplot(real_per_st, aes(x = ds)) +
  geom_line(aes(y = y_2ww, colour="2WW"), lwd=0.75) +
  geom_line(aes(y = y_31d, colour="31 Days"), lwd=0.75) +
  geom_line(aes(y = y_62d, colour="62 Days"), lwd=0.75) +
  labs(title="CWT Summation Performance in Each Standard",
       subtitle = "cutpoint at 1st lock down in Mar 2020", 
       x="Time", y = "Performance") +
  geom_vline(xintercept = as.numeric(real_per_st$ds[51]),
             linetype = "dotted") +
  geom_hline(yintercept = 93, linetype = "dashed", colour="#FF6666", lwd=0.75) +
  geom_hline(yintercept = 96, linetype = "dashed", colour="#2B8CBE", lwd=0.75) +
  geom_hline(yintercept = 85, linetype = "dashed", colour="#00CC66", lwd=0.75) +
  scale_color_manual(values = col_yhat_st) +
  theme_bw()

st_per_sum

Sum_st <- grid.arrange(yhat_plot_st, st_per_sum)
