# ###############################################################
#                                                               #
# Author: Brian Lambert,                                        #
# Name:                                                         #
# Description:                                                  #
#                                                               #
#                                                               #
#                                                               #
#################################################################

library(ggplot2)
setwd("/Users/brianlambert/Desktop/STA404/Health2") 

#========================== Data ==========================

# raw biometric screening data
load("data/raw_data.RData")
# raw biometric screening data gathered by test type
load("data/raw_gathered.RData")


#========================== Graphics ==========================

# manipulate variable names for legend
raw_data$PSB[raw_data$PSB == 1] = "Participated"
raw_data$PSB[raw_data$PSB == 0] = "Did Not Participate"

# reorder PSB levels
raw_data$PSB<-factor(raw_data$PSB,
                     levels = c("Did Not Participate","Not Required","Participated"))

# stacked bar chart for PSB participation by year
ggplot(raw_data, 
       aes(x=factor(year), 
           fill=PSB)) + 
    geom_bar(position = "fill") +
    labs(x="Year",
         y="Breast Cancer Screening Count") +
    ggtitle("Breast Cancer Screening Participants by Year") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())

    


