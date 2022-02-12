library(tidyverse)
library("sf")

prov <- read_csv("Data/provider_level_data.csv")

post <- read_csv("Data/postcodes.csv")  

newprov <- merge(prov,post,by="provider_code", all.x = TRUE)


df2016 <- newprov %>% filter(period == as.Date('2020-04-01') & 
                               (cancer_type == "Suspected breast cancer" |
                                  cancer_type == "Breast") &
                               (standard == "2WW"|
                                standard == "31 Days"|
                                standard ==  "62 Days")
                             ) %>%
                  group_by(postcode) %>% summarise(sum(total_treated))

df2016 <- na.omit(df2016)
colnames(df2016) <- c("name","value")

#df2016$value <- df2016$value*100

max(df2016$value)
min(df2016$value)

#
if(FALSE){ # don't run when sourcing file
  # Download a shapefile of postal codes into your working directory
  download.file(
    "http://www.opendoorlogistics.com/wp-content/uploads/Data/UK-postcode-boundaries-Jan-2015.zip",
    "postal_shapefile.zip"
  )
  # Unzip and read the shapefile
  unzip("postal_shapefile.zip")
}

# Merge data with map boundries to be drawn
postal <- sf::st_read("Distribution/Areas.shp") 
postal2 <- merge(postal, df2016, by="name")


ggplot(postal2) + geom_sf(aes(fill = value)) +
  labs(title = "Breast Canncer Cases for All Standards",
       subtitle = "April 2020") + theme_bw() +  scale_fill_gradient(  high = "springgreen",
                                                                        low = "white",
                                                                     limits = c(0,1700),
                                                                     name= "Number \nof Patinets")

