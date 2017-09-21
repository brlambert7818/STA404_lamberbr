# Author: Brian Lambert
# Name: module_3_homework_code.R
# Description: Graphics that compare the distribution of 
# percapita GDP in the different continents in 2007.

# setwd(“/Users/brianlambert/Desktop/STA404/Module_2”)

options(warn = 1)

#================== Data ===================
gap_2007 <- gapminder[gapminder$year==2007,]


#================== Graphic A ===================
ggplot(gap_2007, aes(x = lifeExp, y = gdpPercap)) +
    geom_jitter(alpha = 0.6, aes(col = continent, size = pop)) +
    labs(title = "life expectancy vs. percapita GDP in 2007",
         x = "life expectancy (years)",
         y = "percapita GDP",
         caption = "Source: Gapminder dataset") +
    theme_minimal()


#================== Graphic B ===================
ggplot(gap_2007, aes(x = lifeExp, y = gdpPercap)) +
    geom_jitter(alpha = 0.6, aes(col = continent, size = pop)) +
    geom_smooth(aes(x = lifeExp, y = gdpPercap), method="loess", se=F, alpha = 0.4) + 
    labs(title = "life expectancy vs. percapita GDP in 2007",
         x = "life expectancy (years)",
         y = "percapita GDP",
         caption = "Source: Gapminder dataset") +
    theme_minimal()


#================== Graphic C ===================
ggplot(gap_2007, aes(x = lifeExp, y = gdpPercap)) +
    geom_jitter(alpha = 0.6, aes(col = continent, size = pop)) +
    geom_smooth(aes(col = continent), method="loess", se=F, span = 1) + 
    labs(title = "life expectancy vs. percapita GDP in 2007",
         x = "life expectancy (years)",
         y = "percapita GDP",
         caption = "Source: Gapminder dataset") +
    theme_minimal()
