### Data Visualization (GOVT16-QSS17) Fall 2019
## GGplot Part III (3-5)
##
## Name: Osman Khan
## Date: October 29th, 2019


# Script will combine colored map of the world 
# with renewables data, create a gif of change over time

library(ggplot2)
library(tidyverse)
library(readxl)
library(maps)
library(ggmap)
library(mapproj)
library(animation)
library(dplyr)

world_map <- map_data("world")
world_map <- world_map %>%
  filter(region != "Antarctica" & 
           region != "French Southern and Antarctic Lands")
View(world_map)

ggplot(data = world_map, aes(y = lat, x = long, group = group)) +
  geom_polygon() +
  coord_map(xlim = c(-180, 180)) +
  theme_void()


#remove filler from head, tail, first column
renewable <- read_excel(
  "/Users/osmankhan/Desktop/19F/QSS 017/Lab02/renewable.xlsx")[2:226,]
colnames(renewable) <- c("country", "year_1990", "year_1991", "year_1992", "year_1993",
                         "year_1994", "year_1995","year_1996", "year_1997", "year_1998", 
                         "year_1999", "year_2000", "year_2001", "year_2002", "year_2003", 
                         "year_2004", "year_2005", "year_2006", "year_2007", "year_2008", 
                         "year_2009", "year_2010", "year_2011", "year_2012", "year_2013",
                         "year_2014", "year_2015")

renewable$country <- gsub('[0-9]+', '', renewable$country)

renewable$country[221] = "Vietnam"
renewable$country[68] = "Ethiopia"
renewable$country[181] = "South Sudan"
renewable$country[197] = "Syria"
renewable$country[1] = "Afghanistan"
renewable$country[192] = "Sudan"
renewable$country[97] = "Iran"
renewable$country[215] = "USA"
renewable$country[213] = "UK"
renewable$country[167] = "Russia"
renewable$country[215] = "USA"
renewable$country[24] = "Bolivia"
renewable$country[220] = "Venezuela"
renewable$country[3] = "Algeria"
renewable$country[28] = "Brazil"
renewable$country[47] = "Congo"
renewable$country[50] = "Ivory Coast"
renewable$country[163] = "South Korea"


View(renewable$country)

#joins the two datasets together
joined_data <- full_join(x = renewable, y = world_map, by = c("country" = "region"))
View(joined_data)

#filters out first years for which there is messy data, gathers all by year
final_dat <- joined_data %>%
  gather(key = "year", value = "renew_percent", year_1990:year_2015) %>%
  filter(year != "year_1990" & year != "year_1991" & year != "year_1992")

  
final_dat$renew_percent <- as.numeric(final_dat$renew_percent)



saveGIF({
  # Loop through all time points
  for (i in unique(final_dat$year)) {

    # Subset japan: data
    data <- subset(final_dat, year == i)
    
    # Finish the ggplot command
      
    a <- data %>%
      ggplot(aes(y = lat, x = long,group = group, fill = renew_percent)) +
      geom_polygon() + coord_map(xlim = c(-180, 180)) +
      theme_void() + ggtitle(paste("     Percentage of Total Energy Produced By Renewables By Country - ", 
                                   substr(i, start = 6, stop = 10)), "
                             
                             ") +
      scale_fill_gradientn(colors = c("#240E8B", "#F04393", "#F9C449")) +
      labs(fill = "% Renewable")
      
    print(a)
    
  }
  
}, movie.name = "map.gif", interval = 0.3)
  

