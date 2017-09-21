# sta404-R-categorical-data-BLANK-14sep17.R
# /home/baileraj/sta404

# References
# Chang - Ch 3 Bar Graphs, Ch 4 Line Graphs AND 13.6, 13.15, 13.16
# Yau - Ch. 5
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#
# Datasets
#   gapminder - data from gapminder package
#   myGapData = gapminder + TotalGDP
#   GDPsummaryDF = myGapData summarized into
#                year-continent values with
#                new variables ContinentTotalGDP, ncountries
#   GDPyearDF = myGapData summarized into
#                year values with new variable YearTotalGDP
#   GDPcombo = left join GDPsummary AND GDPyearDF
#                for continent specific visualization
#                new variables:
#                  PropWorldGDP, PctWorldGDP, ContGDPBillions





library(tidyverse)
library(gapminder)

# Questions to motivate our categorical data / proportional data display discussion
#  How is a country's GDP related to its population size?
#  What continent has the largest share of the world's GDP?
#  What country in each continent has the largest share of the continent's GDP?

# =====================================================================
# Challenge 1:  We don't have country GDP - only per capita GDP. Let's 
#               construct this.

# What were the variables in the gapminder data frame?

# Can you construct country GDP from these variables?


# work with the GDP data - total GDP from pop*gdpPercapita

head(gapminder)

# construction total GDP for each country for data provided
myGapData <- gapminder %>% 
    mutate(TotalGDP = pop*gdpPercap)

head(myGapData)

# Review:  Module 3 ideas
# How is country total GDP related to population size?

ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
    geom_point() 
    
ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
    geom_line(aes(group=country))


myGapData %>% 
    arrange(desc(TotalGDP)) %>%
    select(country, continent, TotalGDP)
    
# look in a particular country over time .................
myGapData %>% 
    filter(country=="China")  %>% 
    ggplot(aes(x=year,y=TotalGDP)) +
    geom_point()

# consider all countries with a facet by year ..............
ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
    geom_point() +
    facet_wrap(~year)

ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
    geom_point() +
    facet_wrap(~year)

ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
    geom_point() +
    facet_wrap(~year) +
    scale_x_log10() +
    scale_y_log10()

# let's clean this up ....................

# look at summary of the data frame to get a sense of the 
#  size of values of each of the variables.

summary(myGapData)

ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
    geom_point() +
    facet_wrap(~year) +
    scale_x_log10(name="Population Size (Millions)",
                  breaks=1000000*c(1,10,100),
                  labels=c("1","10","100")) +
    scale_y_log10(name="Gross Domestic Product (Billions)",
                  breaks=1000000000*c(1,100,1000),
                  labels=c("1000","10000","100000"))

# side question:  if a log-log relationship is linear, what does that
#  suggest about the relationship between the original variables?

# =====================================================================
# Challenge 2:  how can we determine total annual continent and world GDP?

# calculate continent-specific annual GDP ...................
GDPsummaryDF <- myGapData %>% 
    group_by(continent, year) %>% 
    summarise(ContinentTotalGDP = sum(TotalGDP), ncountries = n())

head(GDPsummaryDF)
tail(GDPsummaryDF)

# calculate world annual GDP ................................

GDPyearDF <- myGapData %>% 
    group_by(year) %>% 
    summarise(YearTotalGDP = sum(TotalGDP))

head(GDPsummaryDF)

head(GDPyearDF)

# how do you combine the two data sets so that the annual world GDP is 
#    associated with each continent
# combine the table with continent annual GDP with the world annual GDP

GDPcombo <- left_join(GDPsummaryDF, GDPyearDF, by="year")

head(GDPcombo)
tail(GDPcombo)

# calculate the proportion of the world GDP associated with each continent in each year

GDPcombo <- GDPcombo %>% mutate(PropWorldGDP = ContinentTotalGDP / YearTotalGDP,
                                PctWorldGDP = 100*PropWorldGDP,
                                ContGDPBillions = ContinentTotalGDP/1000000000)

head(GDPcombo)

# =====================================================================
# Challenge 3:  Display continent-specific GDP over time?

# Start with a single year ...................................
# Start with looking at the 2007 data ........................

gapminder2007 <- GDPcombo %>% 
    filter(year==2007)

# display of the number of countries in each continent

ggplot(gapminder2007, aes(x=ncountries, y=ncountries, color=continent)) +
    geom_bar()   # what happened here?

ggplot(gapminder2007, aes(x=ncountries, y=ncountries, color=continent)) +
    geom_bar(stat="identity")   # how about here?

ggplot(gapminder2007, aes(x=ncountries, y=ncountries, fill=continent)) +
    geom_bar(stat="identity")  

## visualizing GDP

ggplot(gapminder2007, aes(x=continent,y=ContinentTotalGDP,fill=continent)) + 
    geom_bar(stat="identity")  

ggplot(gapminder2007, aes(x=continent,y=ContinentTotalGDP,fill=continent)) + 
    geom_bar(stat="identity") +
    theme(legend.position = "top")

# change order of bars - can do with factor orderings
# http://www.cookbook-
r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/
    # https://blog.rstudio.com/2016/08/31/forcats-0-1-0/ 
    #
    gapminder2007 <- gapminder2007 %>% 
    mutate(order_continent = factor(continent,
                                    levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

with(gapminder2007, table(continent, order_continent))

ggplot(gapminder2007, aes(x=order_continent,y=ContinentTotalGDP,
                          fill=order_continent)) + 
    geom_bar(stat="identity") +
    theme(legend.position = "top")

# alternative using the forcat::fct_reorder function
# library(forcats)
# 
# ggplot(gapminder2007, aes(x=fct_reorder(continent,ContinentTotalGDP),
#                           y=ContinentTotalGDP)) + 
#   geom_bar(stat="identity")  

# Now, let's look over time ...................................

GDPcombo <- GDPcombo %>% 
    mutate(order_continent = factor(continent,
                                    levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

# default is to STACK this response
ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity") +
    theme(legend.position = "top")

# you can DODGE the position of this response

ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity",position="dodge") +
    theme(legend.position = "top")

# you can FILL the position of this response

ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity",position="fill") +
    theme(legend.position = "top")

ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity",position="fill") +
    theme(legend.position = "top") + 
    coord_flip()

# which do you like best?  why?
# what else might you like to change?

# pie charts are a variant of a fill
#  pie chart = stacked bar chart + polar coordinates
# http://ggplot2.tidyverse.org/reference/coord_polar.html

ggplot(gapminder2007, aes(x="1",y=ContinentTotalGDP,
                          fill=order_continent)) + 
    geom_bar(stat="identity", width=1) +
    theme(legend.position = "top") +
    coord_polar(theta="y")

# if use 'fill' then you have proportions in wedges
ggplot(gapminder2007, aes(x="1",y=ContinentTotalGDP,
                          fill=order_continent)) + 
    geom_bar(stat="identity", position="fill", width=1) +
    theme(legend.position = "top") +
    coord_polar(theta="y")

# Pie charts showing change in share of world GDP over time
ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity", width=1) +
    theme(legend.position = "top") +
    coord_polar(theta="y") +
    facet_wrap(~year)

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity", width=1, position="fill") +
    #  theme(legend.position = "top") +
    coord_polar(theta="y") +
    facet_wrap(~year)

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity", width=1, position="fill") +
    #  theme(legend.position = "top") +
    coord_polar(theta="y") +
    facet_wrap(~year) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_blank())

# what if you want to add components of theme_minimal?

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity", width=1, position="fill") +
    #  theme(legend.position = "top") +
    coord_polar(theta="y") +
    facet_wrap(~year) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_blank()) + 
    theme_minimal()

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity", width=1, position="fill") +
    coord_polar(theta="y") +
    facet_wrap(~year) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_blank())  

# modifying the colors a little
# using scale_brewer
#       Color Brewer provides color schemes and advice
# REF:  http://colorbrewer2.org/

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity", width=1, position="fill") +
    scale_fill_brewer(type="qual", palette="Accent") +
    coord_polar(theta="y") +
    facet_wrap(~year) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_blank())  

# Design advice for designs addressing color blind viewers ... 
# https://99designs.com/blog/tips/designers-need-to-understand-color-blindness/

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity", width=1, position="fill") +
    scale_fill_brewer(type="seq", palette="Blues") +
    coord_polar(theta="y") +
    facet_wrap(~year) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_blank())  

# get help for scale_fill_brewer and then modify
# this plot to consider other palettes - which do 
# you like the best?

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) + 
    geom_bar(stat="identity", width=1, position="fill") +
    scale_fill_brewer(type="seq", palette="Oranges") +
    coord_polar(theta="y") +
    facet_wrap(~year) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_blank())  

# Cleveland Dot Plot - Chang 3.10 ==============================================

GDPcombo %>%
    filter(year==2007) %>% 
    ggplot(aes(ContinentTotalGDP, continent)) + geom_point()

GDPcombo %>%
    filter(year==2007) %>% 
    ggplot(aes(ContinentTotalGDP, fct_reorder(continent, ContinentTotalGDP))) + 
    geom_point()

# clean up axes
GDPcombo %>%
    filter(year==2007) %>% 
    ggplot(aes(ContinentTotalGDP, fct_reorder(continent, ContinentTotalGDP))) + 
    geom_point() +
    scale_y_discrete(name=element_blank()) +
    scale_x_continuous(name="Total GDP ($Trillion))",
                       breaks=1000000000000*c(5,10,15,20),
                       labels = c("5","10","15","20"))+
    labs(title="Total GDP in each continent in 2007",
         caption="Data source: Jennifer Bryan (2015) Gapminder package") +
    theme_bw()

# alternative where you don't need to modify the factor

GDPcombo %>%
    filter(year==2007) %>% 
    ggplot(aes(ContinentTotalGDP, reorder(continent, ContinentTotalGDP))) + 
    geom_point() +
    scale_y_discrete(name=element_blank()) +
    scale_x_continuous(name="Total GDP ($Trillion))",
                       breaks=1000000000000*c(5,10,15,20),
                       labels = c("5","10","15","20"))+
    labs(title="Total GDP in each continent in 2007",
         caption="Data source: Jennifer Bryan (2015) Gapminder package") +
    theme_bw()

# Cairo likes a version with a segment to the name 
#  - he calls this a lollipop plot

lp_graph <- GDPcombo %>%
    filter(year==2007) %>% 
    ggplot(aes(ContinentTotalGDP, fct_reorder(continent, ContinentTotalGDP))) + 
    geom_point(size=3) +
    geom_segment(aes(yend=continent), xend=0) +
    scale_y_discrete(name=element_blank()) +
    scale_x_continuous(name="Total GDP ($Trillion))",
                       breaks=1000000000000*c(5,10,15,20),
                       labels = c("5","10","15","20"))+
    labs(title="Total GDP in each continent in 2007",
         caption="Data source: Jennifer Bryan (2015) Gapminder package") +
    theme_bw()

lp_graph

lp_graph + coord_flip()

lp_graph + coord_flip() + theme_minimal()

# countries within a continent .........................

myGapData %>% 
    filter(year==2007) %>% 
    ggplot(aes(TotalGDP, country)) + geom_point()

myGapData %>% 
    filter(year==2007) %>% 
    ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) + 
    geom_point() +
    facet_grid(continent ~ .) +
    theme(text = element_text(6)) 

# need to modify y axis text size 

myGapData %>% 
    filter(year==2007) %>% 
    ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) + 
    geom_point() +
    facet_wrap(~ continent, scales="free_y") +
    theme(text = element_text(6),
          axis.text.y = element_text(6)) +
    scale_y_discrete(name=element_blank()) +
    scale_x_continuous(name="Total GDP ($Trillion))",
                       breaks=1000000000000*c(5,10,15,20),
                       labels = c("5","10","15","20"))+
    labs(title="Total GDP in each continent in 2007",
         caption="Data source: Jennifer Bryan (2015) Gapminder package") +
    theme_bw() +
    theme(axis.text.y=element_text(size=6)) 

# better to restrict to a particular continent ....

myGapData %>% 
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
    labs(title="Total GDP in each continent in 2007 in Asia",
         caption="Data source: Jennifer Bryan (2015) Gapminder package") +
    theme_bw() +
    theme(axis.text.y=element_text(size=9)) 

# compare to per capita GDP

myGapData %>% 
    filter(continent=="Asia") %>% 
    filter(year==2007) %>% 
    ggplot(aes(gdpPercap, fct_reorder(country,gdpPercap))) + 
    geom_point() +
    theme(text = element_text(6),
          axis.text.y = element_text(6)) +
    scale_y_discrete(name=element_blank()) +
    scale_x_continuous(name="Per Capita GDP (USD)",
                       breaks=c(1000,10000,20000,40000),
                       labels = c("1000","10000","20000","40000")) +
    labs(title="Per capita GDP in each continent in 2007 in Asia",
         caption="Data source: Jennifer Bryan (2015) Gapminder package") +
    theme_bw() +
    theme(axis.text.y=element_text(size=9)) 

# IN CLASS:  
#    Modify this code block to produce similar displays
#    for Africa and for the Americas

# =====================================================================
# Stacked area chart
# Chang 4.7

ggplot(GDPcombo, aes(x=year, y=YearTotalGDP, fill=continent)) +
    geom_area() 

# IN CLASS - modify this code to remove the legend title,
#     to change the color scheme using color brewer and
#     to improve the axis labels

ggplot(GDPcombo, aes(x=year, y=YearTotalGDP, fill=continent)) +
    geom_area() +
    scale_fill_brewer(type="seq", palette="Oranges") +
    scale_y_continuous(name="Annual GDP (Trillion USD)",
                       breaks=100000000000000*c(1,2,3),
                       labels = c("100","200","300")) +
    theme_minimal() +
    theme(legend.title = element_blank())  

# How has the share of the world's economy changed between
#    1952 and 2007?
# Ref:  Chang 4.8:   Proportional Stacked Area Graph

# proportion of world economy over time ..................

ggplot(GDPcombo, aes(x=year, y=PropWorldGDP, fill=continent)) +
    geom_area() 

ggplot(GDPcombo, aes(x=year, y=PropWorldGDP, fill=order_continent)) +
    geom_area() 

ggplot(GDPcombo, aes(x=year, y=PctWorldGDP, fill=continent)) +
    geom_area() 

# =====================================================================

# treemap - portfolio package, treemapify package
# treemapify

#install.packages("treemapify")
#library(treemapify)


