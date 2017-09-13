# Author: Brian Lambert
# Name: module_2_homework_code.R
# Description: Graphics that compare the distribution of 
# percapita GDP in the different continents in 2007.

# setwd(“/Users/brianlambert/Desktop/STA404/Module_2”)


################### Data ###################

gap_2007 <- gapminder[gapminder$year==2007,]


################### Boxplots ###################

# solid fill, no points plotted besides outliers
ggplot(data=gap_2007, aes(x=continent, y=gdpPercap, fill=continent)) + 
    geom_boxplot() +
    labs(x="continent", y="GDP Percapita", title="GDP Percapita by Continent: 2007") +
    theme(legend.position="none")

# no fill, colored outline, points plotted
ggplot(data=gap_2007, aes(x=continent, y=gdpPercap, color=continent)) + 
    geom_boxplot() +
    geom_jitter(alpha=.3) +
    labs(x="continent", y="GDP Percapita", title="GDP Percapita by Continent: 2007") +
    theme(legend.position="none")

# flipped coordinates of second plot. Source: https://plot.ly/ggplot2/box-plots/
ggplot(data=gap_2007, aes(x=continent, y=gdpPercap, color=continent)) + 
    geom_boxplot() +
    geom_jitter(alpha=.3) +
    labs(x="continent", y="GDP Percapita", title="GDP Percapita by Continent: 2007") +
    theme(legend.position="none") +
    coord_flip()


################### Histograms ###################

ggplot() + geom_histogram(data=gap_2007, 
                          aes(x=gdpPercap, fill=continent)) +
    facet_grid(continent ~ ., scales = "free_y") +
    labs(x="GDP Percapita", title="GDP Percapita by Continent: 2007") 

# change facet grid y scale
ggplot() + geom_histogram(data=gap_2007, 
                          aes(x=gdpPercap, fill=continent)) + 
    labs(x="GDP Percapita", title="GDP Percapita by Continent: 2007") 

    
################### Density Plots ###################

# divide gdp by 2000 to help show more accurate densities
ggplot() + geom_density(data=gap_2007, 
                        aes(x=gdpPercap/2000, color=continent, fill=continent)) +
    labs(x="GDP Percapita", title="GDP Percapita by Continent: 2007") 

# divide gdp by 2000 to help show more accurate densities
ggplot() + geom_density(data=gap_2007, 
                        aes(x=gdpPercap/2000, color=continent, fill=continent),
                        alpha = .4) +
    labs(x="GDP Percapita", title="GDP Percapita by Continent: 2007") 

# change facet grid y scale
ggplot() + geom_density(data=gap_2007, 
                        aes(x=gdpPercap, color=continent, fill=continent)) +
    facet_grid(continent ~ ., scales = "free_y")+
    labs(x="GDP Percapita", title="GDP Percapita by Continent: 2007") 


################### Violin Plots ###################

ggplot(gap_2007, aes(x=continent, y=gdpPercap, color=continent)) +
    geom_violin()+
    labs(x="country", y="GDP Percapita", title="GDP Percapita by Continent: 2007") 

ggplot(gap_2007, aes(x=continent, y=gdpPercap, color=continent)) +
    geom_violin() +
    geom_jitter(alpha=0.4)+
    labs(x="country", y="GDP Percapita", title="GDP Percapita by Continent: 2007") 
