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
library(dplyr)
library(maps)          
library(ggplot2)
library(ggmap)            
library(mapproj)
library(ggthemes)
library(mapdata)
library(rworldmap)
library(choroplethr)

#========================== Data ==========================

basic_demographics = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Basic demographics fall 17 ftft Oxford.csv", header = T)
act_composites = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/ACT Composite Test score dummy id.csv", header = T)
courses_enrolled = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Courses enrolled 15th day - dummy id.csv", header = T)
hs_rank_type= read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/HS percentile rank and hs type.csv", header = T)
legacy= read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Legacy dummy id.csv", header = T)
financial_need= read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Level of financial need dummy id.csv", header = T)
parental_education = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/Parental education dummy id.csv", header = T)
countdata2 = read.csv("/Users/brianlambert/Desktop/STA404/Krallman/data/countdata2.csv", header = T)


#========================== Graphics ==========================

levels(basic_demographics$Division)[levels(basic_demographics$Division)=="AP"] <- "CEC"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="AS"] <- "CAS"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="BU"] <- "FSB"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="EA"] <- "EHS"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="FA"] <- "CCA"

basic_demographics$Division <- fct_infreq(basic_demographics$Division)

basic_demographics <- basic_demographics %>% 
    mutate(order_minority = factor(Minority.Status,
                                    levels=c("Domestic Minority", "International", "Domestic Nonminority")))

# ordered by minority
g1 <- ggplot(basic_demographics, aes(x = as.factor(Division), fill = order_minority)) +
    geom_bar() +
    labs(x="College Division") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "none") +
    scale_fill_manual(name = 'Minority Status', 
                      values =c("#BDBDBD", "#455A64", "#F44336"))

basic_demographics <- basic_demographics %>% 
    mutate(Gender = ifelse(Gender=="M","Male","Female"))

# order by minority, faceted by gender
g2 <- ggplot(basic_demographics, aes(x = as.factor(Division), fill = order_minority)) +
    geom_bar(position = "dodge") +
    facet_wrap(~Gender) +
    labs(x="College Division") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "none") +
    scale_fill_manual(name = 'order minority', 
                      values =c("#BDBDBD", "#455A64", "#F44336"))

business_courses = c("BUS", "ECO", "ACC", "MKT", "MGT", "ISA", "CMR", "ESP")
stem_courses = c("GLG", "MTH", "CHM", "BIO", "PMD", "CEC", "PHY", "MME", "STA", "CSE", 
                 "MBI", "MSC", "IES", "NSC", "AES", "CPB", "ECE", "IMS")
humanities_socialScience_courses = c("GEO", "SPN", "ENG", "WST", "UNV", "HST", "JRN", "POL", 
                                    "FRE", "BWS", "ATH", "GER", "CAS", "PHL", "SOC", 
                                    "GTY", "ARB", "ITS", "PSY", "JPN", "GIC",
                                    "SPA", "WGS", "LAT", "IDS", "CHI", "KOR", "ITL",
                                    "REL", "LAS", "DST", "RUS", "STC", "PLW", "SJS", "ACE",
                                    "HBW", "AAA", "POR", "GRK", "MAC", "FST", "CLS", "AMS")
art_courses = c("ART","MUS","THE","ARC", "CCA")
education_courses = c("EDL", "EDT", "KNH", "EDP", "FSW")

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
    else if(c %in% education_courses){
        group = "Education"
    }
    else {
        group = "Humanities & Social Science"
    }
    course_group = append(course_group, group)
}

new_courses_enrolled = data.frame(courses_enrolled, course_group)
newtable = merge(new_courses_enrolled, basic_demographics, by = "Dummy.ID")
newtable$course_group<-factor(newtable$course_group,
                                          levels = c("STEM","Humanities & Social Science", "Business","Art","Education"))

newtable <- newtable %>% 
    mutate(order_minority = factor(Minority.Status,
                                   levels=c("Domestic Minority", "International", "Domestic Nonminority")))

# bar chart of course groups
g3 <- ggplot(newtable, aes(x = as.factor(course_group), fill = order_minority)) +
    geom_bar() +
    labs(x="Course Type") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "none") +
    scale_fill_manual(name = 'order minority', 
                      values =c("#BDBDBD", "#455A64", "#F44336"))

# pie chart of minority status
g4 <- ggplot(basic_demographics, aes(x=factor("1"), fill = Minority.Status)) + 
    geom_bar(position="fill", width=1, ) +
    coord_polar(theta="y") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_blank()) +
    ggtitle("Minority Status Distribution") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(name = 'order minority', 
                      values =c("#BDBDBD", "#455A64", "#F44336"))

# convert pie chart to percentages
# Domestic Minority = 652, 17% 
# Domestic Nonminorty = 2865, 75%
# International = 301, 8%
# SUM = 3818
table(unlist(basic_demographics$order_minority))

#world map
state_map <- map_data("state")
str(state_map)
unique(state_map$region)

ggplot(state_map, aes(x=long,y=lat,group=group))+
    geom_polygon(fill="white", colour="black")

mw_map <- map_data("state", region=c("alabama","arizona","arkansas","california","colorado",
                                     "connecticut","delaware","district of columbia",
                                     "florida","georgia","idaho","illinois",
                                     "indiana","iowa","kansas","kentucky",
                                     "louisiana","maine","maryland","massachusetts", 
                                     "michigan","minnesota","mississippi","missouri",
                                     "montana","nebraska","nevada","new hampshire",
                                     "new jersey","new mexico","new york","north carolina",
                                     "north dakota","ohio","oklahoma","oregon",
                                     "pennsylvania","rhode island","south carolina","south dakota",
                                     "tennessee","texas","utah","vermont",
                                     "virginia","washington","west virginia","wisconsin",
                                     "wyoming"))

newData <- basic_demographics %>%
    filter (Minority.Status == "Domestic Minority")


countState <- newData %>%
    group_by(State) %>%
    summarise(Freq = n())


countState <- tail(countState,-1)

#countdata2 <- as.character(countdata2$V1)
names(countdata2) = c("region", "freq")
statemap2<- merge(state_map,countdata2, by ="region", all.x = T)
statemap2$freq[is.na(statemap2$freq)] <- 0
statemap2 = statemap2[order(statemap2$order),]
statemap2 <- statemap2 %>% 
    mutate(freq = ifelse(freq==0, 0, ifelse(freq > 0 & freq <= 10, 1, ifelse(freq <= 50, 2, ifelse(freq <= 100, 3, 4)))))

# US map
ggplot(statemap2,aes(x=long, y=lat, group=region, fill=factor(freq)))+
    geom_polygon(col="white")+
    coord_map() +       # map projection correction
    theme_map() +
    scale_fill_manual(values = c("#fee0d2", "#fc9272", "#de2d26","#8D1209", "#4D0A05"))


# Ohio map
states_map <- map_data("state")
str(states_map)
unique(states_map$region)

# longitude :=  east/west direction (relative to Greenwich, England)
# latitude  :=  north/south direction (relative to the equator)

ohio_map <- subset(states_map, states_map$region=="ohio")
unique(ohio_map$region)

ggplot(ohio_map, aes(x=long,y=lat,group=group))+
    geom_polygon(fill="red", colour="black")

ohio<-subset(basic_demographics, basic_demographics$State == "OH")
ohiominority <- subset (ohio,ohio$Minority.Status == "Domestic Minority")
View(ohiominority)

countohio <- ohiominority %>%
    group_by(ohiominority$County.Description) %>%
    summarise(Freq = n())
head(countohio$Freq)

Butler_County <- data.frame(long=-84.5222, lat=39.4405)
Hamilton_County <- data.frame(long=-84.5641, lat=39.2355)
Franklin_County <- data.frame(long=-82.9932, lat=40.0155)
Cuyahoga_County <- data.frame(long=-81.6758, lat=41.4339)
Montgomery_County <- data.frame(long=-84.3542, lat=39.7620)


ggplot()+
    geom_polygon(data=ohio_map, aes(x=long,y=lat,group=group,fill=region),fill="#FA6A64", colour="black") +
    geom_point(data=Hamilton_County,aes(x=long,y=lat)) +annotate(geom="text",x=-84.5641,y=39.2355,label="Hamilton",adj=0, color="black") +
    geom_point(data=Butler_County,aes(x=long,y=lat)) +annotate(geom="text",x=-84.5222,y=39.4405,label="Butler",adj=0, color="black") +
    geom_point(data=Franklin_County,aes(x=long,y=lat)) +annotate(geom="text",x=-82.9932,y=40.0155,label="Franklin",adj=0, color="black") +
    geom_point(data=Cuyahoga_County,aes(x=long,y=lat)) +annotate(geom="text",x=-81.6758,y=41.4339,label="Cuyahoga",adj=0, color="black") +
    geom_point(data=Montgomery_County,aes(x=long,y=lat)) +annotate(geom="text",x=-84.3542,y=39.7620,label="Montgomery",adj=0, color="black") +
    coord_map()+
    theme_nothing()

