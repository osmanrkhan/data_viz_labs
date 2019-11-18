### Data Visualization (GOVT16-QSS17) Fall 2019
## LAB 3
##                  
## Name: Osman Khan
## Date: November 7th, 2019
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(extrafont)
library(maps)
library(ggmap)
library(dplyr)
library(mapproj)
library(grid)
#dependencies, needed files
cdc_state <- read.csv("/Users/osmankhan/Desktop/19F/QSS 017/Lab03/cdc_data_by_state.csv")
cdc_year <- read.csv("/Users/osmankhan/Desktop/19F/QSS 017/Lab03/cdc_year_data.csv")
mapdata <- read.csv("/Users/osmankhan/Desktop/19F/QSS 017/Lab03/state-medal-count.csv",
                    header=TRUE, stringsAsFactors=FALSE)
drug_map <- map_data("state")

## Plot will include two sub-plots

## One will show USA by state with spheres (OF VARYING 
## SIZE NOT COLOR) showing drug death rate in 2019 and earliest date.
## dark grey/orange pallete
## other will show rise in drug deaths over time.

#View(cdc)

# first, let's construct the bar graph.
# renames clunky cause of death column
colnames(cdc_year)[colnames(cdc_year) =="Drug.Alcohol.Induced"] <- "cause"


#only get drug deaths
bar_data <- cdc_year %>%
  filter(cause == "Drug-induced causes")

#Palette for bar graph, different coloes to use later.

palette_o_despair <- c(brewer.pal(8, "Oranges")[1], brewer.pal(8, "Oranges")[1],
                       brewer.pal(8, "Oranges")[2], brewer.pal(8, "Oranges")[2],
                       brewer.pal(8, "Oranges")[3], brewer.pal(8, "Oranges")[3],
                       brewer.pal(8, "Oranges")[4], brewer.pal(8, "Oranges")[4],
                       brewer.pal(8, "Oranges")[5], brewer.pal(8, "Oranges")[5],
                       brewer.pal(8, "Oranges")[6], brewer.pal(8, "Oranges")[6],
                       brewer.pal(8, "Oranges")[7], brewer.pal(8, "Oranges")[7],
                       brewer.pal(8, "Oranges")[8], brewer.pal(8, "Oranges")[8],
                       brewer.pal(8, "Oranges")[8], brewer.pal(8, "Oranges")[8],
                       brewer.pal(8, "Oranges")[8])
text_col <- "#D37000"
bg_col <- "#171A23"

theme_bar <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(colour = text_col, family = "Myriad Pro"),
      plot.background = element_rect(fill = bg_col, color = NA),
      axis.text = element_text(colour = text_col, family = "Myriad Pro"),
      axis.ticks = element_line(colour = text_col),
      panel.grid.major = element_blank(), 
      panel.background = element_rect(fill = bg_col, color = NA),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      ...
    )
}

#map bar graph

bar_plot <- bar_data %>%
  ggplot(aes(x = Year, y = Age.Adjusted.Rate, fill = factor(Year))) + geom_col() +
  coord_flip() +
  theme_bar() +
  labs(title = "Drug Death Rates Also Rising", caption = "CDC Data") +
  xlab("Year")+ ylab("US Drug-Induced Death Rate (per 100k)") + 
  scale_fill_manual(values = palette_o_despair) 


######################################################

#Now for map of us.

#first letter capitilization taken from stackoverflow
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

colnames(cdc_state)[colnames(cdc_state) =="Drug.Alcohol.Induced"] <- "cause"
drug_map$region <- firstup(drug_map$region)

cdc_state <- cdc_state %>%
  filter((Year == 1999 | Year == 2017) &
           cause == "Drug-induced causes" & State != "Hawaii" & State != "Alaska") %>%
  arrange(desc(Year))

joined_data <- full_join(x = cdc_state, y = mapdata, 
                         by = c("State" = "location"), na.rm = TRUE)[-c(99, 100),]

# fix missing values for S dakota and W Virginia
joined_data$lat[89] <- 44.4443; joined_data$lon[89] <- -100.2663
joined_data$lat[40] <- 44.4443; joined_data$lon[40] <- -100.2663
joined_data$lat[96] <- 38.6409; joined_data$lon[96] <- -80.6227
joined_data$lat[47] <- 38.6409; joined_data$lon[47] <- -80.6227

joined_data$Age.Adjusted.Rate <- as.numeric(joined_data$Age.Adjusted.Rate)



theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      text = element_text(colour = text_col, family = "Myriad Pro"),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.3, size = 20),
      plot.subtitle = element_text(hjust = 0.3, size = 15),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = bg_col), 
      panel.background = element_rect(fill = bg_col, color = NA), 
      panel.spacing.x = unit(100, "pt"),
      legend.background = element_rect(fill = bg_col, color = NA),
      panel.border = element_blank(),
      legend.justification = "top",
      legend.key.size = unit(0.4, "lines"),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 10),
      legend.box.spacing = unit(0.1, "cm"),
      plot.margin = unit(c(2, 2, 2, 0), "cm"), # t r b l
      ...
    )
}

map_plot <- ggplot(data = drug_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "#3C2A3D", col = "black") +
  coord_map("albers", lat0=30, lat1=40) +
  theme_map() +
  labs(title = "US Drug-Induced Deaths By State And Year", subtitle = "1999 / 2017") +
  
  geom_point(data=joined_data, aes(x=lon, y=lat, size = Deaths, col = factor(Year)) , inherit.aes = FALSE) +
  theme_map() +
  scale_color_manual(name = "Year", 
                     values = c(brewer.pal(8, "Oranges")[1], brewer.pal(8, "Oranges")[6]), 
                     labels = c("1999", "2017")
                     ) + 
  scale_size(name= "Deaths", range = c(1, 17), breaks = c(0, 1000, 5000)) +
  guides(size=guide_legend(override.aes=list(colour="#ff9041")))
 =


vp <- viewport(width = 0.2, height = 0.4, x = 0.75, y = 0.2)


full <- function() {
  theme_set(theme_map)
  print(map_plot) 
  theme_set(theme_bar)
  print(bar_plot, vp = vp) }
  
full()











