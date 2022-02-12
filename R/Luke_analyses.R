library(tidyverse)
library(xts)

prov <- read_csv("Data/provider_level_data.csv")

com <- read_csv("Data/commissioner_level_data.csv")

nat <- read_csv("Data/national_level_data.csv")

beds <- read_csv("Data/Beds_Regions.csv")

summary(beds)

beds %>% filter(London == 4168)


## filter prov

provf <- prov %>% filter(standard == "2WW")
provf <- provf %>% group_by(period) %>% summarise(mean(performance))
provf$type = "Provider Level"

ggplot(provf, aes(period, `mean(performance)`)) + geom_line()

unique(prov[c("standard")])
unique(provf[c("cancer_type")])

unique(com[c("cancer_type")])

n_distinct(nat$standard)

r <- prov %>% filter( cancer_type == "Lung")
unique(r[c("")])


# 2 Week Wait data
com2ww <- com %>% filter (standard == "ALL CANCERS TWO WEEK WAIT" )
com2ww <- com2ww %>% group_by(period) %>% summarise(mean(performance),sum(within_standard)/sum(total_treated))
com2ww$level <- "Commissioner Level"


prov2ww <- prov %>% filter(standard == "2WW")
prov2ww <- prov2ww %>% group_by(period) %>% summarise(mean(performance), sum(within_standard)/sum(total_treated))
prov2ww$level <- "Provider Level"

nat2ww <- nat %>% filter(standard == "2WW")
nat2ww <- nat2ww %>% group_by(period) %>% summarise( sum(within_standard)/sum(total_treated))
nat2ww$level <- "National Level"


ggplot() + geom_line(data = com2ww, aes(x = period, y = `mean(performance)`, colour = 'Commissioner Level')) +
  geom_line(data = prov2ww, aes(x = period, y = `mean(performance)`, colour = 'Provider Level')) +
  geom_line(data = nat2ww, aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'National Level')) + 
  labs(title = "Two Week Wait From GP Urgent Referral to First Consultant Appointment", x = "Time", y = "Performance", colour = "Levels") + 
  scale_color_manual(values=c("brown4", "greenyellow", "deepskyblue4"))+ theme_bw() 



ww2 <- rbind( prov2ww, com2ww)

ggplot(ww2, aes(period, `sum(within_standard)/sum(total_treated)`, colour = level)) + geom_line()+ ggtitle("Performance for Cancer Waiting Times") + 
  xlab("Years") + ylab("Performance rate")

##31 days
com31 <- com %>% filter (standard == "31-DAY (DIAGNOSIS TO TREATMENT) WAIT FOR FIRST TREATMENT: ALL CANCERS" )
com31 <- com31 %>% group_by(period) %>% summarise(mean(performance),sum(within_standard)/sum(total_treated))
com31$`mean(performance)` <- com31$`mean(performance)`*100
com31$`sum(within_standard)/sum(total_treated)` <- com31$`sum(within_standard)/sum(total_treated)`*100

prov31 <- prov %>% filter(standard == "31 Days" & cancer_type == "ALL CANCERS")
prov31 <- prov31 %>% group_by(period) %>% summarise(mean(performance), sum(within_standard)/sum(total_treated))
prov31$`mean(performance)` <- prov31$`mean(performance)`*100
prov31$`sum(within_standard)/sum(total_treated)` <- prov31$`sum(within_standard)/sum(total_treated)`*100

nat31 <- nat %>% filter(standard == "31 Days")
nat31$performance <- nat31$within_standard / nat31$total_treated
nat31  <- nat31  %>% group_by(period) %>% summarise(mean(performance), sum(within_standard)/sum(total_treated))
nat31$`mean(performance)` <- nat31$`mean(performance)`*100
nat31$`sum(within_standard)/sum(total_treated)` <- nat31$`sum(within_standard)/sum(total_treated)`*100

ggplot() + geom_line(data = com31, aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'Commissioner Level')) +
  geom_line(data = prov31, aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'Provider Level'), size = 1.2) +
  geom_line(data = nat31 , aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'National Level'), size = 0.3) + 
  labs(title = "One Month Wait from a Decision to Treat to a First Treatment for Cancer", x = "Time", y = "Performance (%)", colour = "Levels") + 
  scale_color_manual(values=c("brown4", "greenyellow", "deepskyblue4"))+ theme_bw() + theme(legend.position = "none") 

ggplot() + geom_line(data = com31, aes(x = period, y = `mean(performance)`, colour = 'Commissioner Level')) +
  geom_line(data = prov31, aes(x = period, y = `mean(performance)`, colour = 'Provider Level')) +
  geom_line(data = nat31 , aes(x = period, y = `mean(performance)`, colour = 'National Level')) + 
  labs(title = "One Month Wait from a Decision to Treat to a First Treatment for Cancer", x = "Time", y = "Performance (%)", colour = "Levels") + 
  scale_color_manual(values=c("brown4", "greenyellow", "deepskyblue4"))+ theme_bw() 


##62 days
com62 <- com %>% filter (standard == "62-DAY (URGENT GP REFERRAL TO TREATMENT) WAIT FOR FIRST TREATMENT: ALL CANCERS" )
com62 <- com62 %>% group_by(period) %>% summarise(mean(performance), sum(within_standard)/sum(total_treated)*100)
com62$`mean(performance)` <- com62$`mean(performance)`*100
com62$`sum(within_standard)/sum(total_treated)` <- com62$`sum(within_standard)/sum(total_treated)`*100


prov62 <- prov %>% filter(standard == "62 Days" & cancer_type == "ALL CANCERS")
prov62 <- prov62 %>% group_by(period) %>% summarise(mean(performance), sum(within_standard)/sum(total_treated)*100)
com62$`mean(performance)` <- com62$`mean(performance)`*100
com62$`sum(within_standard)/sum(total_treated)` <- com62$`sum(within_standard)/sum(total_treated)`*100

nat62 <- nat %>% filter(standard == "62 Days")
nat62  <- nat62  %>% group_by(period) %>% summarise( sum(within_standard)/sum(total_treated))
colnames(nat62)[2] <- "performance"

ggplot() + geom_line(data = com62, aes(x = period, y = `mean(performance)`, colour = 'Commissioner Level')) +
  geom_line(data = prov62, aes(x = period, y = `mean(performance)`, colour = 'Provider Level')) +
geom_line(data = nat62, aes(x = period, y = performance, colour = 'National Level')) + 
  labs(title = "Two Month Wait from GP Urgent Referral to a First Treatment for Cancer", x = "Time", y = "Performance", colour = "Levels") + 
  scale_color_manual(values=c("brown4", "greenyellow", "deepskyblue4"))+ theme_bw() #+ theme(legend.position = "none")




  


