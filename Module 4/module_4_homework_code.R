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
    coord_flip()


#========================== Graphic 3 ==========================



#========================== Graphic 4 ==========================