# ###############################################################
#                                                               #
# Author: Brian Lambert, Minyue Wang, Mary Yu                   #
# Name: krallman_adminG2_code.R                                 #
# Description: .                                                #
#                                                               #
# setwd(“/Users/brianlambert/Desktop/STA404/krallman”)          #
#                                                               #
#################################################################

library(gridExtra)
library(tidyverse)
library(gapminder)
library(ggplot2)
library(forcats)
library(plyr)


#========================== Data ==========================

basic_demographics = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Basic demographics fall 17 ftft Oxford.csv", header = T)
act_composites = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/ACT Composite Test score dummy id.csv", header = T)
courses_enrolled = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Courses enrolled 15th day - dummy id.csv", header = T)
hs_rank_type= read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/HS percentile rank and hs type.csv", header = T)
legacy= read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Legacy dummy id.csv", header = T)
financial_need= read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Level of financial need dummy id.csv", header = T)
parental_education = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Parental education dummy id.csv", header = T):w



#========================== Graphics ==========================

levels(basic_demographics$Division)[levels(basic_demographics$Division)=="AP"] <- "CEC"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="AS"] <- "CAS"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="BU"] <- "FSB"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="EA"] <- "EHS"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="FA"] <- "CCA"

View(basic_demographics)
basic_demographics$Division <- fct_infreq(basic_demographics$Division)

basic_demographics <- basic_demographics %>% 
    mutate(order_minority = factor(Minority.Status,
                                    levels=c("Domestic Minority", "International", "Domestic Nonminority")))


# ordered by minority
g1 <- ggplot(basic_demographics, aes(x = as.factor(Division), fill = order_minority)) +
    geom_bar() +
    scale_fill_brewer(palette = "GnBu") +
    ggtitle("College Distribution of First Year Students") +
    labs(x="College and Divisions", 
         caption="Source: Krallman Institutional Research", size = 8) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())

g1
# order by minority, faceted by gender
g2 <- ggplot(basic_demographics, aes(x = as.factor(Division), fill = order_minority)) +
    geom_bar() +
    facet_wrap(~Gender) +
    ggtitle("College Distribution of First Year Students with Gender") +
    labs(x="College and Divisions", 
         caption="Source: Krallman Institutional Research", size = 8) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())





business_courses = c("BUS", "ECO", "ACC", "MKT", "MGT", "ISA", "CMR", "ESP")
stem_courses = c("GLG", "MTH", "CHM", "BIO", "PMD", "CEC", "PHY", "GEO", "MME", "STA", "CSE", 
                 "MBI", "MSC", "IES", "NSC", "AES", "CPB", "ECE")
art_courses = c("AMS", "MUS", "THE", "MAC", "ARC", "FAS", "ART", "IMS", "FST", "CCA", "CLS")
education_socialScience_courses = c("SPN", "ENG", "WST", "UNV", "HST", "EDL", "JRN", "POL", 
                                    "FRE", "BWS", "EDT", "ATH", "GER", "CAS", "PHL", "SOC", 
                                    "GTY", "KNH", "ARB", "ITS", "PSY", "EDP", "JPN", "GIC",
                                    "SPA", "FSW", "WGS", "LAT", "IDS", "CHI", "KOR", "ITL",
                                    "REL", "LAS", "DST", "RUS", "STC", "PLW", "SJS", "ACE",
                                    "HBW", "AAA", "POR", "GRK")


course_group = c()
for (c in courses_enrolled$SUBJ_CODE){
    c = as.character(c)
    group = ''
    if(c %in% business_courses){
        group = "Business"
    }
    else if(c %in% stem_courses){
        group = "STEM"
    }
    else if(c %in% art_courses){
        group = "Art"
    }
    else if(c %in% education_socialScience_courses){
        group = "Education & Social Science"
    }
    else {
        group = "Other"
    }
    course_group = append(course_group, group)
}

new_courses_enrolled = data.frame(courses_enrolled, course_group)
newtable = merge(new_courses_enrolled, basic_demographics, by = "Dummy.ID")
newtable$course_group<-factor(newtable$course_group,
                                          levels = c("Education & Social Science", "STEM", "Art", "Business", "Other"))

newtable <- newtable %>% 
    mutate(order_minority = factor(Minority.Status,
                                   levels=c("Domestic Minority", "International", "Domestic Nonminority")))

View(newtable)
# bar chart of course groups
g3 <- ggplot(newtable, aes(x = as.factor(course_group), fill = order_minority)) +
    geom_bar() +
    ggtitle("Distribution of Cognate Areas of First Year Students") +
    labs(x="Cognate Areas") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())


# pie chart of minority status
g4 <- ggplot(basic_demographics, aes(x=factor("1"), fill = Minority.Status)) + 
    geom_bar(position="fill", width=1, ) +
    scale_fill_brewer(type="qual", palette="RdYlGn") +
    coord_polar(theta="y") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_blank()) +
    ggtitle("Minority Status Distribution") +
    theme(plot.title = element_text(hjust = 0.5))
grid.arrange(g1, g2, g4, g3)

