# Author: Brian Lambert
# Name: module_4_homework_code.R
# Description: Graphics exploring the cut of diamonds in the "diamonds" data set, 
# as well as comparing the cut to diamond size (carats).

# setwd(“/Users/brianlambert/Desktop/STA404/Module_4”)
library(tidyverse)


#========================== Data ==========================

myDiamonds <- diamonds %>% 
    mutate(Gcarat = cut(carat, breaks=seq(from=0,to=3.5,by=.25)))


#========================== Graphic 1 ==========================

# displaying diamond cut by count with a veritcal bar graph and a gradient color scale
ggplot(myDiamonds) +
    geom_bar(aes(cut, fill = cut)) +
    scale_fill_brewer(palette = "RdYlGn") +
    labs(caption = "Data source: ggplot2 diamonds dataset") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 


#========================== Graphic 2 ==========================

# displaying diamond cut by count with a horizontal bar graph and a gradient color scale
ggplot(myDiamonds) +
    geom_bar(aes(cut, fill = cut)) +
    scale_fill_brewer(palette = "RdYlGn") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    labs(caption = "Data source: ggplot2 diamonds dataset") +
    coord_flip()


#========================== Graphic 3 ==========================

# pie chart displaying the counts of eacch diamond cut type
# source for position = "fill": https://stackoverflow.com/questions/31165823/ggplot-making-a-descriptive-bar-graph-with-no-clear-y-variable
ggplot(myDiamonds, aes(x=factor("1"), fill = cut)) + 
    geom_bar(position="fill", width=1, ) +
    scale_fill_brewer(type="qual", palette="RdYlGn") +
    coord_polar(theta="y") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_blank()) +
    ggtitle("Diamond Cut Qualities") +
    labs(caption = "Data source: ggplot2 diamonds dataset") +
    theme(plot.title = element_text(hjust = 0.5))


#========================== Graphic 4 ==========================

# pie charts  displaying counts of each diamond cut type 
# with facet wrap grouped by Gcarat.
ggplot(myDiamonds, aes(x=factor("1"), fill = cut)) + 
    geom_bar(position="fill", width=1, ) +
    scale_fill_brewer(type="qual", palette="RdYlGn") +
    coord_polar(theta="y") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_blank()) +
    ggtitle("Diamond Cut Qualities") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    labs(caption = "Data source: ggplot2 diamonds dataset") +
    facet_wrap(~Gcarat)


