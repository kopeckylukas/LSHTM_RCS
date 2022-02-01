install.packages('prophet')

library(prophet)
library(dplyr)
library(tibble)
library(caret)
library(ggplot2)

# R

install.packages(c("cmdstanr", "posterior"), repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

cmdstanr::install_cmdstan()

# Set the R_STAN_BACKEND environment variable
Sys.setenv(R_STAN_BACKEND = "CMDSTANR")

library(prophet)

df <- read.csv('../Dataset/Cleaned Data/provider_level_data.csv')

str(df)
df$period <- format(as.Date(df$period), "%Y-%m-%d")
names(df)[1] <- 'ds'
df$ds <- as.Date(df$ds)
names(df)[10] <- 'y'
df$y = round(df$y*100, 2)
df <- filter(df, cancer_type == "ALL CANCERS")
df$region_name <- as.factor(df$region_name)
levels(df$region_name) <- list(Unknown = "",
                       London = "LONDON COMMISSIONING REGION",
                       Midlands = "MIDLANDS AND EAST OF ENGLAND COMMISSIONING REGION",
                       North = "NORTH OF ENGLAND COMMISSIONING REGION",
                       South = "SOUTH OF ENGLAND COMMISSIONING REGION")
df <- filter(df, ds < "2020-3-1")
df <- subset(df, select = c(ds, region_name, y))
str(df)

#Create df across UK
df_UK <- subset(df, select = c(ds, y))

#Create df in each region
df_un <- subset(df, region_name == "Unknown")
df_un <- subset(df_un, select = c(ds, y))

df_lon <- subset(df, region_name == "London")
df_lon <- subset(df_lon, select = c(ds, y))

df_mid <- subset(df, region_name == "Midlands")
df_mid <- subset(df_mid, select = c(ds, y))

df_nor <- subset(df, region_name == "North")
df_nor <- subset(df_nor, select = c(ds, y))

df_sou <- subset(df, region_name == "South")
df_sou <- subset(df_sou, select = c(ds, y))

#Prophet across UK
m_UK <- prophet(df_UK)

future <- make_future_dataframe(m_UK, periods = 36, freq='month')
head(future)
tail(future)

forecast_UK <- predict(m_UK, future)
tail(forecast_UK[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#Prophet in each region
m_un <- prophet(df_un)

future_un <- make_future_dataframe(m_un, periods = 36, freq='month')

forecast_un <- predict(m_un, future)
#############
m_lon <- prophet(df_lon)

future_lon <- make_future_dataframe(m_lon, periods = 36, freq='month')

forecast_lon <- predict(m_lon, future)
#############
m_mid <- prophet(df_mid)

future_mid <- make_future_dataframe(m_mid, periods = 36, freq='month')

forecast_mid <- predict(m_mid, future)
#############
m_nor <- prophet(df_nor)

future_nor <- make_future_dataframe(m_nor, periods = 36, freq='month')

forecast_nor <- predict(m_nor, future)
#############
m_sou <- prophet(df_sou)

future_sou <- make_future_dataframe(m_sou, periods = 36, freq='month')

forecast_sou <- predict(m_sou, future)

#BBC ==
if(!require(pacman))install.packages("pacman")

install.packages('devtools')
library(devtools)
devtools::install_github('bbc/bbplot')

pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')

#Visualization
plot(m_UK, forecast_UK)



col_yhat <- c("Across UK" = "#FF6666", 
              "Unknown" = "#FFCC00",
              "London" = "#00CC66", 
              "Midlands & East" = "#339999", 
              "North" = "#00CCFF", 
              "South" = "#CC66FF")

forecast_yhat <- as.data.frame(subset(forecast_UK, select = c(ds, yhat)))
forecast_yhat <- cbind(forecast_yhat, yhat_un, yhat_lon, yhat_mid, yhat_nor, yhat_sou)

yhat_un <- subset(forecast_un, select = c(yhat))
yhat_lon <- subset(forecast_lon, select = c(yhat))
yhat_mid <- subset(forecast_mid, select = c(yhat))
yhat_nor <- subset(forecast_nor, select = c(yhat))
yhat_sou <- subset(forecast_sou, select = c(yhat))

names(forecast_yhat)[3] <- 'yhat_un'
names(forecast_yhat)[4] <- 'yhat_lon'
names(forecast_yhat)[5] <- 'yhat_mid'
names(forecast_yhat)[6] <- 'yhat_nor'
names(forecast_yhat)[7] <- 'yhat_sou'

yhat_plot <- ggplot(forecast_yhat, aes(x = ds)) +
  geom_line(aes(y = yhat, colour="Across UK"), lwd=0.75) +
  geom_line(aes(y = yhat_un, colour="Unknown"), lwd=0.75) +
  geom_line(aes(y = yhat_lon, colour="London"), lwd=0.75) +
  geom_line(aes(y = yhat_mid, colour="Midlands & East"), lwd=0.75) +
  geom_line(aes(y = yhat_nor, colour="North"), lwd=0.75) +
  geom_line(aes(y = yhat_sou, colour="South"), lwd=0.75) +
  labs(title="CWT pre COVID Performance and Prediction in each UK Region",
       subtitle = "With true performance before Mar 2020 and perdiction after Mar 2020", 
       x="Time", y = "Performance") +
  geom_vline(xintercept = as.numeric(forecast_yhat$ds[51]),
             linetype = "dotted") +
  scale_color_manual(values = c("Across UK" = "#FF6666", 
                                "Unknown" = "#FFCC00",
                                "London" = "#00CC66", 
                                "Midlands & East" = "#339999", 
                                "North" = "#00CCFF", 
                                "South" = "#CC66FF"))
yhat_plot


yhat_plot_bbc <- yhat_plot + bbc_style() + 
  theme(plot.title = element_text(size = 16)) +
  theme(plot.subtitle = element_text(size = 14))

yhat_plot_bbc