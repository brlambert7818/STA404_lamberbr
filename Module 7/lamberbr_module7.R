# ###############################################################
#                                                               #
# Author: Brian Lambert                                         #
# Name: lamberbr_module7.R                                      #
# Description: Plots of home state / town and choropleth map    #
# of the USA and shade each state with the % possessing a BS    #
# degree or higher.                                             #
#                                                               #
# setwd(“/Users/brianlambert/Desktop/STA404/Module 7”)          #
#                                                               #
#################################################################

library(tidyverse)
library(gapminder)
library(ggplot2)
library(forcats)
library(dplyr)
library(maps)          
library(ggplot2)
library(ggmap)            
library(mapproj)
library(ggthemes)
library(mapdata)
library(rworldmap)
library(choroplethr)


#========================== Home State and Town ==========================

# source: Krallman dashboard utiliized similar methods so I was able to repurpose
# parts of the code for these two plots

# long and lat data for every state
states_map <- map_data("state")
str(states_map)
unique(states_map$region)

# filter for just ohio long and lat
ohio_map <- subset(states_map, states_map$region=="ohio")
unique(ohio_map$region)

# plot Hamilton country based on long and lat I found online
Hamilton_County <- data.frame(long=-84.5641, lat=39.2355)

# plot of Ohio with Hamilton county labeled
ggplot()+
    geom_polygon(data=ohio_map, aes(x=long,y=lat,group=group,fill=region),fill="#FA6A64", colour="black") +
    geom_point(data=Hamilton_County,aes(x=long,y=lat)) +annotate(geom="text",x=-84.5641,y=39.2355,label=" Hamilton",adj=0, color="black") +
    coord_map()+
    theme_nothing()


#========================== US BS Degree or Higher Distribution ==========================

# census data was found on wikipedia, copied into excel, cleaned in excel to make merging easier 
# with the state long and lat data, saved as csv
census = read.csv("/Users/brianlambert/Desktop/STA404/Module 7/Bs_census_data.csv", header = T)
state_map = map_data("state")

# merge state long and lat data with the census data for % BS or higher degree
state_map = merge(state_map,census, by ="region", all.x = T)

# map of USA with each state filled witht % BS or higher degree
ggplot(state_map, aes(x=long, y=lat, group=region, fill=bsOrHigher))+
    geom_polygon(col="white")+
    coord_map() +       
    theme_map() +
    labs(title = "% of State with BS Degree or Higher",
         caption = "Data source: US Census Data 2009",
         fill="% BS or Higher") +
    theme(plot.title = element_text(hjust = 0.5))

