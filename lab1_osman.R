### Data Visualization (GOVT16-QSS17) Fall 2019
## LAB 1
##
## Name: Osman Khan
## Date: October 23rd, 2019


#dependencies, needed files
votes <- read.csv("/Users/osmankhan/Desktop/19F/QSS 017/Lab01/HSall_parties.csv")
library(tidyverse)
library(ggplot2)

### program to create a "polarization score" that 
# is the sum of the dimension 1 deviation from mean of republican and democrat parties 
# from the mean of their year and plot it agianst year


##constants: 
#these are the parties' whose polarization we are examining 
SELECT_PARTIES <- c("Democrat", "Republican")
#and the year from which we are examining the data
SELECT_YEAR <- 1930

#create a table just containing both houses' mean ideology scores
total_mean_df <- votes %>%
  mutate(year = 1787 + (congress * 2))%>%
  #only analzing houses, not president
  filter(year >= SELECT_YEAR & (chamber != "President") & party_name %in% SELECT_PARTIES)  %>%
  group_by(year, chamber) %>%
  summarize(total_mean = mean(nominate_dim1_mean, na.rm = TRUE))

#mean ideology score for dimension 1 for a year and chamber
mean_ideology <- total_mean_df$total_mean
mean_ideology


#table containing republican polarization values
rep_polar_df <- votes %>%
  mutate(year = 1787 + (congress * 2))%>%
  filter(year >= SELECT_YEAR & (chamber != "President") & party_name %in% SELECT_PARTIES[2])%>%
  
  group_by(chamber, year) %>%
  #mean_nom gives us mean of each republican party's ideology in each house in each year
  summarize(mean_nom = mean(nominate_dim1_mean)) %>%
  group_by() %>%
  #create a polarization variable, use absolute values to not confuse later sum
  mutate(rep_polar = abs(mean_nom - mean_ideology))

#same thing with Democrats
dem_polar_df <- votes %>%
  mutate(year = 1787 + (congress * 2))%>%
  filter(year >= SELECT_YEAR & (chamber != "President") & party_name %in% SELECT_PARTIES[1])  %>%
  group_by(chamber, year) %>%
  summarize(mean_nom = mean(nominate_dim1_mean)) %>%
  group_by() %>%
  mutate(dem_polar = abs(mean_nom - mean_ideology))

#put in one table
votefinal <- cbind(rep_polar_df, dem_polar_df$dem_polar)

#plot, span chosen somewhat arbitrarily
votefinal %>%
  ggplot(aes(x = year, y = rep_polar + dem_polar_df$dem_polar, color = chamber)) + theme_light() +
  stat_smooth(se = FALSE, span = 0.3) + 
  scale_x_continuous(limits = c(1932, 2020), breaks=seq(1935, 2019, 10)) +
  scale_color_manual(values = c("#0db4b9", "#1d1145"), labels = c("House", "Senate")) +
  labs(title = "Polarization In Chambers of Congress", color = "Chamber of Congress") +
  xlab("Year") + ylab("Polarization Level") 

#with some exceptions, both houses have gotten increasingly polarized, and Congress more than the Senate
