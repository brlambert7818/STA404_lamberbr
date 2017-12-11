# Author: Brian Lambert
# Name: module_5_homework_code.R
# Description: Graphics exploring the cut of diamonds in the "diamonds" data set, 
# as well as comparing the cut to diamond size (carats).

# setwd(“/Users/brianlambert/Desktop/STA404/Module_5”)
library(tidyverse)
library(scales)


#========================== Data ==========================

library(gapminder)

myGapData <- gapminder %>% 
    mutate(TotalGDP = pop*gdpPercap) %>% 
    mutate(order_continent = factor(continent,
                                    levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

GDPsummaryDF <- myGapData %>% 
    group_by(continent, year) %>% 
    summarise(ContinentTotalGDP = sum(TotalGDP), ncountries = n())
GDPyearDF <- myGapData %>% 
    group_by(year) %>% 
    summarise(YearTotalGDP = sum(TotalGDP))
GDPcombo <- left_join(GDPsummaryDF, GDPyearDF, by="year")
GDPcombo <- GDPcombo %>% mutate(PropWorldGDP = ContinentTotalGDP / YearTotalGDP,
                                PctWorldGDP = 100*PropWorldGDP,
                                ContGDPBillions = ContinentTotalGDP/1000000000)
GDPcombo <- GDPcombo %>% 
    mutate(order_continent = factor(continent,
                                    levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

#========================== Graphic 1 ==========================

# Africa dot plot
africa_dot <- myGapData %>% 
    filter(continent=="Africa") %>% 
    filter(year==2007) %>% 
    ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) + 
    geom_point() +
    theme(text = element_text(6),
          axis.text.y = element_text(6)) +
    scale_y_discrete(name=element_blank()) +
    scale_x_continuous(name="Total GDP ($10Billion))",
                       breaks=10000000000*c(5,10,15,20),
                       labels = c("5","10","15","20"))+
    labs(title="Africa") +
    theme_minimal() +
    theme(axis.text.y=element_text(size=7)) + 
    theme(legend.position = "none")


# Americas dot plot
americas_dot <- myGapData %>% 
    filter(continent=="Americas") %>% 
    filter(year==2007) %>% 
    ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) + 
    geom_point() +
    theme(text = element_text(6),
          axis.text.y = element_text(6)) +
    scale_y_discrete(name=element_blank()) +
    scale_x_continuous(name="Total GDP ($Trillion))",
                       breaks=1000000000000*c(5,10,15,20),
                       labels = c("5","10","15","20"))+
    labs(title="Americas") +
    theme_minimal() +
    theme(axis.text.y=element_text(size=7)) + 
    theme(legend.position = "none")


# Asia dot plot
asia_dot <- myGapData %>% 
    filter(continent=="Asia") %>% 
    filter(year==2007) %>% 
    ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) + 
    geom_point() +
    theme(text = element_text(6),
          axis.text.y = element_text(6)) +
    scale_y_discrete(name=element_blank()) +
    scale_x_continuous(name="Total GDP ($Trillion))",
                       breaks=1000000000000*c(5,10,15,20),
                       labels = c("5","10","15","20"))+
    labs(title="Asia") +
    theme_minimal() +
    theme(axis.text.y=element_text(size=7)) + 
    theme(legend.position = "none")


# Europe dot plot
europe_dot <- myGapData %>% 
    filter(continent=="Europe") %>% 
    filter(year==2007) %>% 
    ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) + 
    geom_point() +
    theme(text = element_text(6),
          axis.text.y = element_text(6)) +
    scale_y_discrete(name=element_blank()) +
    scale_x_continuous(name="Total GDP ($100Billion))",
                       breaks=100000000000*c(5,10,15,20),
                       labels = c("5","10","15","20"))+
    labs(title="Europe") +
    theme_minimal() +
    theme(axis.text.y=element_text(size=7)) + 
    theme(legend.position = "none")


# Oceania dot plot
oceania_dot <- myGapData %>% 
    filter(continent=="Oceania") %>% 
    filter(year==2007) %>% 
    ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) + 
    geom_point() +
    theme(text = element_text(6),
          axis.text.y = element_text(6)) +
    scale_y_discrete(name=element_blank()) +
    scale_x_continuous(name="Total GDP ($100Billion))",
                       breaks=100000000000*c(1,2,3,4),
                       labels = c("1","2","3","4"))+
    labs(title="Oceania", caption="Data source: Jennifer Bryan (2015) Gapminder package") +
    theme_minimal() +
    theme(axis.text.y=element_text(size=7)) + 
    theme(legend.position = "none")

# dashboard of gdp for each country, grouped by continent in 2007
grid.arrange(africa_dot, americas_dot, asia_dot, europe_dot, oceania_dot)


#========================== Graphic 2 ==========================

# dashboard of gdp for each country, grouped by continent in 2007, height adjusted for # countries
grid.arrange(africa_dot, americas_dot, asia_dot, europe_dot, oceania_dot,
             ncol = 2, nrow =3, heights = c(3,2,1))


#========================== Graphic 3 ==========================

library(waffle)
library(ggmosaic)
library(dplyr)

library(grid)
pushViewport(viewport(layout=grid.layout(2,2)))

myTitanic <- as.data.frame(Titanic)
titanic_class <- myTitanic %>%  
                group_by(Class) %>%
                summarize(passengers = sum(Freq))

count_passengers <- titanic_class$passengers 
names(count_passengers) <- as.vector(titanic_class$Class)
waffle(parts=count_passengers, 
       rows = 80, 
       xlab="Titanic Passengers by Class")

