# ###############################################################
#                                                               #
# Author: Brian Lambert                                         #
# Name: module_6_homework_code.R                                #
# Description: Generating visualizations to analyze UN tourism  #
# data from 1995-2014.                                          #
#                                                               #
# setwd(“/Users/brianlambert/Desktop/STA404/Module_6”)          #
#                                                               #
#################################################################


#========================== Data ==========================

# Preprocessing of Excel file
# deleted 5 rows 
# deleted last column ("notes")
# deleted first row (country, series, 1995--2014)
#
# Deleted rows at bottom of the spreadsheet
# The information is the same as that provided in the "Compendium of Tourism Statistics"
# Inbound tourism:	
# TF	Arrivals of non-resident tourists at national borders
# VF	Arrivals of non-resident visitors at national borders
# THS	Arrivals of non-resident tourists in hotels and similar establishments
# TCE	Arrivals of non-resident tourists in all types of accommodation establishments
# Outbound tourism:	
# TF	Departures - trips abroad by resident tourists
# VF	Departures - trips abroad by resident visitors
# IMF	International Monetary Fund
# CB	Central Bank
# ..	Data not available
# Source: World Tourism Organization (UNWTO)


# read in data ================================================================
library(readr)
hw6_subset_csv <- read_xlsx("/Users/brianlambert/Desktop/STA404/Module 6/tourism_data.xlsx",
                           col_names = FALSE)
hw6_subset_csv = tail(hw6_subset_csv, -6)
hw6_subset_csv = head(hw6_subset_csv, -14)
View(hw6_subset_csv)

# add country name to each row =================================================

hw6_out <- data.frame(NULL)

for (ii in 1:nrow(hw6_subset_csv)) {
    if (!is.na(hw6_subset_csv[ii,"X__1"])) {
        country <- hw6_subset_csv[ii,"X__2"]
    }
    else {
        hw6_out <- rbind(hw6_out,
                         cbind(country, hw6_subset_csv[ii,2:24]) )
    } # end of 'else'
} # end of 'for'

# clean up names ================================================================

names(hw6_out) <- c("country", "v2", "group", "series", paste0("X",1995:2014)) 

# issues: ======================================================================
# countries have different numbers of rows
# inbound and outbound tourism have different numbers of ros

hw6_out %>% 
    group_by(country) %>% 
    summarize(nvars = n())

# search for presence of patterns - grep
# http://www.statmethods.net/management/functions.html
# chaining if-else statements
# http://www.dummies.com/programming/r/how-to-chain-ifelse-statements-in-r/ 

hw6_out2 <- data.frame(NULL)

for (ii in 1:nrow(hw6_out)) {
    if (length(grep("Inbound", hw6_out[ii,"v2"]) )) {
        prefix <- "Inbound"
    } else if (length(grep("Outbound", hw6_out[ii,"v2"]))) {
        prefix <- "Outbound"
    } else {
        vars <- paste(prefix, hw6_out[ii,"v2"], sep="_")
        hw6_out2 <- rbind(hw6_out2,
                          cbind(vars, hw6_out[ii,c(1,3:24)])) 
    }
}

# examining a couple of example cases ===================================================================
as.data.frame(hw6_out2 %>% 
                  group_by(country) %>% 
                  summarize(nvars = n())
)

hw6_out2 %>% 
    filter(country=="POLAND")

hw6_out2 %>% 
    filter(country=="ZIMBABWE")

# clean up variable names ================================================================================
hw6_out2 <- hw6_out2 %>% 
    mutate(new_vars = sub(" - ","_",vars)) %>% 
    mutate(new_vars = gsub(" ","_",new_vars)) %>% 
    mutate(new_vars = paste(new_vars,series,sep="_")) 

unique(hw6_out2$new_vars)


hw6_wide <- hw6_out2 %>% 
    select(country, new_vars, X1995:X2014)

View(hw6_wide)

# now can think about restructuring the data ============================================================

library(tidyr)
hw6_long <- hw6_wide %>% 
    group_by(new_vars) %>% 
    gather(key=cyear, value=cmeasure, X1995:X2014) %>% 
    mutate(year = as.numeric(substring(cyear,2)), 
           measure = as.numeric(sub(',','',cmeasure))) %>% # commas in cell entries! 
    select(-cyear, -cmeasure) 

View(hw6_long)

hw6_analysis <- hw6_long%>% 
    spread(key=new_vars, value=measure)

View(hw6_analysis)

as.data.frame(hw6_analysis %>% 
                  filter(country=="UNITED KINGDOM") 
)


#========================== Head % Tail of UN Tourism Data ==========================
head(hw6_analysis)
tail(hw6_analysis)


#========================== Expenditure vs Tourists Plot ==========================

# more data preprocessing to extract relevant variables, remove rows with na values, and clean up variable names
hw6_analysis = data.frame(hw6_analysis$country, hw6_analysis$year, hw6_analysis$Inbound_Arrivals_Thousands_TF, hw6_analysis$`Inbound_Tourism_expenditure_in_the_country_US$_Mn_IMF`)
hw6_analysis = na.omit(hw6_analysis)                          
names(hw6_analysis) = c("country", "year", "inbound_tourists", "expenditure")

# comparing the inbound tourism expendidture with the number of tourists from 1995-2014
# with the United States data highlighted
ggplot(hw6_analysis, aes(x = inbound_tourists, y = expenditure)) +
    geom_jitter(data = hw6_analysis[which(hw6_analysis$country == "UNITED STATES OF AMERICA"),],
                aes(color = "blue"), 
                alpha = 0.6) +
    geom_jitter(data = hw6_analysis[which(hw6_analysis$country != "UNITED STATES OF AMERICA"),], 
                aes(color = "black"), 
                alpha = 0.6) +
    ggtitle("Inbound Tourism Expenditure by Inbound Tourists (1995-2014)") +
    labs(x="Inbound Tourists (Thousands)", y="Inbound Tourist Expenditure in Millions (US Dollars)", 
         caption="Source: World Toursim Organization", size = 8) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    # source: https://groups.google.com/forum/#!topic/ggplot2/GD2kZuOQS1Q
    scale_colour_manual(name = 'country', 
                        values =c('blue'='blue', 'black'='black'), labels = c('rest of the world','United States'))


#========================== Greatest Tourism Growth by Country Plot ==========================

# create data frame ordering the countries by their increase in tourists between their first year 
# reflected and last year reflected in the data.
countries = unique(hw6_analysis$country)
growth_vector = c()
country_vector = c()

for (c in countries) {
    new_df = hw6_analysis[which(hw6_analysis$country == c),]
    min_year = min(new_df$year)
    min_year_tourists = new_df[which(new_df$year == min_year), 'inbound_tourists']
    max_year = max(new_df$year)
    max_year_tourists = new_df[which(new_df$year == max_year), 'inbound_tourists']
    growth_vector = append(growth_vector, max_year_tourists - min_year_tourists)
    country_vector = append(country_vector, c)
}

country_growth = data.frame(growth_vector, country_vector)
country_growth = country_growth[order(growth_vector),] 
names(country_growth) = c("growth", "country")
country_growth$country <- factor(country_growth$country, levels = country_growth$country[order(-country_growth$growth)])

# plots showing the countries with the greatest growth in tourism from 1995-2014
ggplot(data = tail(country_growth, n = 5), aes(x = country, y = growth, fill = country)) +
    geom_bar(stat = "identity") +
    ggtitle("Countries with Greatest Increase in Tourism between 1995-2014") +
    labs(x="Country", y="Increase in Tourists (Thousands)", 
         caption="Source: World Toursim Organization", size = 8) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "none") 


#========================== Greatest Tourism Decline by Country Plot ==========================

# plots showing the countries with the greatest decline in tourism from 1995-2014
# plots showing the countries with the greatest growth in tourism from 1995-2014
ggplot(data = head(country_growth, n = 5), aes(x = country, y = growth, fill = country)) +
    geom_bar(stat = "identity") +
    ggtitle("Countries with Greatest Decrease in Tourism between 1995-2014") +
    labs(x="Country", y="Decrease in Tourists (Thousands)", 
         caption="Source: World Toursim Organization", size = 8) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "none") 
