# ###############################################################
#                                                               #
# Author: Brian Lambert,                                        #
# Name:                                                         #
# Description:                                                  #
#                                                               #
#            #
#                                                               #
#################################################################

setwd("/Users/brianlambert/Desktop/STA404/health2/data")

library(tidyverse)
library(forcats)
library(dplyr)
library(gdata)


#========================== Data ==========================

# wellness vist and biometric screening data
raw_data_2011 = read.xls("2011 - 2016 Raw Data.xlsx", sheet=2, header=T)
raw_data_2011$year = 2011
raw_data_2012 = read.xls("2011 - 2016 Raw Data.xlsx", sheet=3, header=T)
raw_data_2012$year = 2012
myvars <- names(raw_data_2012) %in% c("X", "X.1")
raw_data_2012 = raw_data_2012[!myvars]
raw_data_2013 = read.xls("2011 - 2016 Raw Data.xlsx", sheet=4, header=T)
raw_data_2013$year = 2013
raw_data_2014 = read.xls("2011 - 2016 Raw Data.xlsx", sheet=5, header=T)
raw_data_2014$year = 2014
raw_data_2015 = read.xls("2011 - 2016 Raw Data.xlsx", sheet=6, header=T)
raw_data_2015$year = 2015
raw_data_2016 = read.xls("2011 - 2016 Raw Data.xlsx", sheet=7, header=T)
raw_data_2016$year = 2016

raw_data = rbind(raw_data_2011, raw_data_2012, raw_data_2013, raw_data_2014, raw_data_2015, raw_data_2016)
raw_gathered = raw_data %>% gather(test, value, -year)

name_vec = c("PSC","PSB","PSCO")

# manipulate variable names for legend
raw_data$PSC[raw_data$PSC == 1] = "Participated"
raw_data$PSC[raw_data$PSC == 0] = "Did Not Participate"

raw_data$PSB[raw_data$PSB == 1] = "Participated"
raw_data$PSB[raw_data$PSB == 0] = "Did Not Participate"

raw_data$PSCO[raw_data$PSCO == 1] = "Participated"
raw_data$PSCO[raw_data$PSCO == 0] = "Did Not Participate"

raw_data$WV[raw_data$WV == 1] = "Participated"
raw_data$WV[raw_data$WV == 0] = "Did Not Participate"

# reorder levels
raw_data$PSC<-factor(raw_data$PSC,levels = c("Did Not Participate","Not Required","Participated"))
raw_data$PSB<-factor(raw_data$PSB,levels = c("Did Not Participate","Not Required","Participated"))
raw_data$PSCO<-factor(raw_data$PSCO,levels = c("Did Not Participate","Not Required","Participated"))

# save data files
save(raw_data, file="raw_data.RData")
save(raw_gathered, file="raw_gathered.RData")



