# sta404-R-ggplot2-intro-BLANK-24aug17.R

########### NOTES ##############
# use jittering to add radom noise to data to deal w/ overplotting, aka points are too dense 

# save work in class subdirectory

setwd("/Users/brianlambert/Desktop/STA 404")

# load packages that we will use for initial exploration

library("tidyverse")   # includes ggplot2, dplyr and other tools
library("gapminder")   # dataset with country, may need to install this if it is not available

# getting help ==========================================================================
    
    help( package = "tidyverse")
?tidyverse
tidyverse_packages(include_self = TRUE)   # list all of the packages in the tidyverse

?gapminder

browseVignettes(package="dplyr")  # will work on a local installation of RStudio

# exploring a data set ===================================================================

gapminder   # typing name displays contents of object

names(gapminder)    # list names of variables

str(gapminder)      # characteristics and structure of object.  For data frames, variable names, type, ...

glimpse(gapminder)  # alternative to "str" for tibble

View(gapminder)     # open a tab displaying the gapminder data set

# summary of data set =====================================================================
    
    summary(gapminder)  # what is produced?

# has life expectancy improved between 1952 and 2007?

ggplot() + geom_point(data=gapminder, aes(x=year,y=lifeExp))     # scatterplot

ggplot() + geom_jitter(data=gapminder, aes(x=year,y=lifeExp))    # jittering to deal with overplotting

ggplot() + geom_boxplot(data=gapminder, aes(x=as.factor(year),y=lifeExp))  # distribution changes

# combining layers
ggplot(data=gapminder, aes(x=as.factor(year),y=lifeExp)) + geom_boxplot() +
    geom_jitter(alpha=.2)

# what question do you want to answer for these data?

gapminder %>%
    arrange(lifeExp) %>% 
    head()

gapminder %>% 
    arrange(lifeExp) %>% 
    tail()

# for those who have used Base R before...
gapminder[gapminder$year==min(gapminder$lifeExp),]
# or
with(gapminder, gapminder[lifeExp==min(lifeExp),])

# What is being missed in this display?

ggplot() + geom_line(data=gapminder, aes(x=year,y=lifeExp))     # line graph - what happened here?

ggplot() + 
    geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country))

ggplot() + 
    geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country, color=country))   # what happens here?

ggplot() + 
    geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country, color=country)) +
    guides(color="none")


# highlighting countries ================================================================================
    
    ggplot() + 
    geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country), color="lightgrey") +
    guides(color="none")

# create dataframe with Rwanda and Japan
#   What is a data frame?
#   What does it mean to pipe commands?
#        Reading " %>% " as " and then "

rwanda_japan <- gapminder %>% 
    filter(country %in% c("Rwanda", "Japan"))

rwanda_japan

ggplot() + 
    geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country), color="lightgrey") +
    geom_line(data=rwanda_japan, aes(year,lifeExp, color=country), lwd=1.1) +
    # guides(color="none") removes the legend from the graph
    guides(color="none")+
    theme_minimal()

# cleaning up annotations

ggplot() + 
    geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country), color="lightgrey") +
    geom_line(data=rwanda_japan, aes(year,lifeExp, color=country), lwd=1.1) +
    guides(color="none") +
    labs(x="Year", y="Life Expectancy", 
         caption="Source:  Jennifer Bryan (2015). gapminder: Data from Gapminder. R package version 
         0.2.0.") +
    annotate("text", x=1985, y=80, label="Japan") +
    annotate("text", x=1985, y=30, label="Rwanda") +
    theme_minimal()

# saving this for use in a presentation

myplot <- ggplot() + 
    geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country), color="lightgrey") +
    geom_line(data=rwanda_japan, aes(year,lifeExp, color=country), lwd=1.1) +
    guides(color="none") +
    labs(x="Year", y="Life Expectancy", 
         caption="Source:  Jennifer Bryan (2015). gapminder: Data from Gapminder. R package version 
         0.2.0.") +
    annotate("text", x=1985, y=80, label="Japan") +
    annotate("text", x=1985, y=30, label="Rwanda") +
    theme_minimal()

ggsave("/Users/brianlambert/Desktop/STA 404/gapminder-plot-25aug17.pdf", device="pdf", width=4, height=3, units="in")

# saved on server, would need to export to your computer

# future investigations - faceting ==========================================

ggplot() + 
    geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country, color=country)) +
    facet_grid(. ~ continent) +
    guides(color="none")


# homework ==================================================================

gapminder2007 <- gapminder %>% 
    filter(year==2007)



