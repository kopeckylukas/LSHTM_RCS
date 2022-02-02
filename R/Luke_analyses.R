library(tidyverse)
library(xts)

prov <- read_csv("Data/provider_level_data.csv")

com <- read_csv("Data/commissioner_level_data.csv")




## filter prov

provf <- prov %>% filter(standard == "2WW")
provf <- provf %>% group_by(period) %>% summarise(mean(performance))
provf$type = "Provider Level"

ggplot(provf, aes(period, `mean(performance)`)) + geom_line()

unique(prov[c("standard")])
unique(provf[c("cancer_type")])

unique(com[c("standard")])

provf <- prov %>% filter( )


comf <- com %>% filter (standard == "ALL CANCERS TWO WEEK WAIT" )
comf <- comf %>% group_by(period) %>% summarise(mean(performance))
comf$type = "Commissioner Level"


ggplot(comf, aes(period, `mean(performance)`)) + geom_line()


  

ggplot() + geom_line(data = comf, aes(x = period, y = `mean(performance)`, colour = 'Commissioner Level')) +
  geom_line(data = provf, aes(x = period, y = `mean(performance)`, colour = 'Provider Level'))




com3 <- com %>% filter (standard == "31-DAY (DIAGNOSIS TO TREATMENT) WAIT FOR FIRST TREATMENT: ALL CANCERS" )
com3 <- com3 %>% group_by(period) %>% summarise(mean(performance),sum(within_standard)/sum(total_treated))


prov3 <- prov %>% filter(standard == "31 Days" & cancer_type == "ALL CANCERS")
prov3 <- prov3 %>% group_by(period) %>% summarise(mean(performance), sum(within_standard)/sum(total_treated))

ggplot() + geom_line(data = com3, aes(x = period, y = `mean(performance)`, colour = 'Commissioner Level')) +
  geom_line(data = prov3, aes(x = period, y = `mean(performance)`, colour = 'Provider Level')) + theme_bw()


r <- prov %>% filter( cancer_type == "Lung")
unique(r[c("standard")])


# 2 Week Wait data
com2ww <- com %>% filter (standard == "ALL CANCERS TWO WEEK WAIT" )
com2ww <- com2ww %>% group_by(period) %>% summarise(mean(performance),sum(within_standard)/sum(total_treated))
com2ww$level <- "Commissioner Level"


prov2ww <- prov %>% filter(standard == "2WW")
prov2ww <- prov2ww %>% group_by(period) %>% summarise(mean(performance), sum(within_standard)/sum(total_treated))

prov2ww$level <- "Provider Level"


ww2 <- rbind( prov2ww, com2ww)



