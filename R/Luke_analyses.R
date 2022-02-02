library(tidyverse)
library(xts)

prov <- read_csv("Data/provider_level_data.csv")

com <- read_csv("Data/commissioner_level_data.csv")

nat <- read_csv("Data/national_level_data.csv")



## filter prov

provf <- prov %>% filter(standard == "2WW")
provf <- provf %>% group_by(period) %>% summarise(mean(performance))
provf$type = "Provider Level"

ggplot(provf, aes(period, `mean(performance)`)) + geom_line()

unique(prov[c("standard")])
unique(provf[c("cancer_type")])

unique(com[c("standard")])

r <- prov %>% filter( cancer_type == "Lung")
unique(r[c("standard")])


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


ggplot() + geom_line(data = com2ww, aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'Commissioner Level')) +
  geom_line(data = prov2ww, aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'Provider Level')) +
  geom_line(data = nat2ww, aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'National Level')) 
+ theme_bw()

ww2 <- rbind( prov2ww, com2ww)

ggplot(ww2, aes(period, `sum(within_standard)/sum(total_treated)`, colour = level)) + geom_line()


##31 days
com31 <- com %>% filter (standard == "31-DAY (DIAGNOSIS TO TREATMENT) WAIT FOR FIRST TREATMENT: ALL CANCERS" )
com31 <- com31 %>% group_by(period) %>% summarise(mean(performance),sum(within_standard)/sum(total_treated))
com31$level <- "Commissioner Level"


prov31 <- prov %>% filter(standard == "31 Days" & cancer_type == "ALL CANCERS")
prov31 <- prov31 %>% group_by(period) %>% summarise(mean(performance), sum(within_standard)/sum(total_treated))
prov31$level <- "Provider Level"

nat31 <- nat %>% filter(standard == "31 Days")
nat31  <- nat31  %>% group_by(period) %>% summarise( sum(within_standard)/sum(total_treated))




ggplot() + geom_line(data = com31, aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'Commissioner Level')) +
  geom_line(data = prov31, aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'Provider Level')) +
  geom_line(data = nat31 , aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'National Level')) + theme_bw()

d31 <- rbind( prov31, com31)

ggplot(d31, aes(period, `mean(performance)`, colour = level)) + geom_line()


##62 days
com62 <- com %>% filter (standard == "62-DAY (URGENT GP REFERRAL TO TREATMENT) WAIT FOR FIRST TREATMENT: ALL CANCERS" )
com62 <- com62 %>% group_by(period) %>% summarise(mean(performance),sum(within_standard)/sum(total_treated))



prov62 <- prov %>% filter(standard == "62 Days" & cancer_type == "ALL CANCERS")
prov62 <- prov62 %>% group_by(period) %>% summarise(mean(performance), sum(within_standard)/sum(total_treated))


ggplot() + geom_line(data = com62, aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'Commissioner Level')) +
  geom_line(data = prov62, aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'Provider Level')) +
geom_line(data = nat2ww, aes(x = period, y = `sum(within_standard)/sum(total_treated)`, colour = 'National Level')) 
+ theme_bw()






