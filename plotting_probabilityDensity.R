# sta404-R-comparing-distributions-BLANK-04sep17.R

########################## Notes ##########################


# save work in class subdirectory
#setwd("/home/baileraj/sta404")

# loading packages
library("tidyverse")   # includes ggplot2, dplyr and other tools
library("gapminder")   # dataset with country, may need to install this if it is not available

# create test data set for exploration ===============================
# if not interested in the details, go to line #62
# 4 different groups are being being generated

vec1 <- c(rnorm(n=400,mean=54,sd=10),rnorm(n=200,mean=79,sd=8))
mean(vec1)
sd(vec1)

vec2 <- c(rnorm(n=600, mean=62.75275, sd=15.28787))
mean(vec2)
sd(vec2)

# Technical aside (ignore unless you are interested):  
#                   Gamma variate generation with particular mean/sd
#                   requires reparameterization
#
# mean = a*s = 62.75275 => a= 62.75275/s
# var = a*s^2 = 15.28787^2  =>  (62.75275/s)*s^2 = 15.28787^2
#                     =>  62.75275*s = 15.28787^2
#                     =>  s = 15.28787^2  / 62.75275
#                     =>  s = 3.724442
#                     =>  a (shape) = 62.75275 / 3.724442 = 16.8489

vec3 <- rgamma(n=600, shape = 16.8489, scale=3.724442)
mean(vec3)
sd(vec3)

# Technical aside (ignore unless you are interested):  
#        Uniform requires reparameterization too
# (b+a)/2 = 62.75275
# (b-a)^2/12 = 15.28787^2
#
# b+a = 125.5055 => b = 125.5055 - a
# (b-a)^2/12 = (125.5055 - a -a)^2/12 = 15.28787^2
#           (125.5055 - 2 a)^2 = 2804.628
#             125.5055 - 2a = 59.95874
#                  2a = 65.54676
#                   a = 32.77338
#                   b = 125.5055 - a = 125.5055 - 32.77338 = 92.73212
#

vec4 <- runif(n=600, min=32.77338, max=92.73212)
mean(vec4)
sd(vec4)

myDF <- data.frame(group=rep(c(1,2,3,4), c(600,600,600,600)),
                   response = c(vec1, vec2, vec3, vec4))

myDF <- myDF %>% mutate(fgroup=factor(group))

str(myDF)

# dynamite plot =========================================================

# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
# https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot
# Beware of the dynamite plot
#   http://biostat.mc.vanderbilt.edu/wiki/pub/Main/TatsukiRcode/Poster3.pdf

sumstatDF <- myDF %>% 
    group_by(fgroup) %>% 
    summarise(ybar=mean(response),sdev=sd(response))

sumstatDF

ggplot(data=sumstatDF, aes(x=fgroup, y=ybar, fill=fgroup)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=ybar, ymax=ybar+sdev),
                  width=.2) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

# boxplots =====================================================

ggplot(data=myDF, aes(x=fgroup, y=response, fill=fgroup)) + 
    geom_boxplot() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

# can add data points
ggplot(data=myDF, aes(x=fgroup, y=response, color=fgroup)) + 
    geom_boxplot(size=1.25) +
    geom_jitter(alpha=.3) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

# histograms ========================================================

ggplot() + geom_histogram(data=myDF, 
                          aes(x=response, color=fgroup, fill=fgroup),
                          position="dodge",binwidth=1)

ggplot() + geom_histogram(data=myDF, 
                          aes(x=response, color=fgroup, fill=fgroup),
                          binwidth=6) +
    facet_grid(fgroup ~ .)

# density plots =====================================================

ggplot() + geom_density(data=myDF, 
                        aes(x=response, color=fgroup, fill=fgroup))


ggplot() + geom_density(data=myDF,
                        aes(x=response, color=fgroup, fill=fgroup),
                        alpha = .4)

ggplot() + geom_density(data=myDF, 
                        aes(x=response, color=fgroup, fill=fgroup)) +
    facet_grid(fgroup ~ .)

# how about both histogram and density?
# Chang 6.3
# first convert from frequency to relative frequency / density
ggplot(myDF) + 
    geom_histogram(aes(x=response, y=..density..)) +
    facet_grid(fgroup ~ .)

ggplot(myDF) + 
    geom_histogram(aes(x=response, y=..density..)) +
    geom_density(aes(x=response)) +
    facet_grid(fgroup ~ .)

# what happens if you only do the color= aesthetic?
# what happens when you add the fill=  aesthetic?

ggplot(myDF, aes(color=fgroup)) + 
    geom_histogram(aes(x=response, y=..density..,fill=fgroup),alpha=.5) +
    geom_density(aes(x=response)) +
    facet_grid(fgroup ~ .)

# violin plots are an interesting option =============================
# Chang 6.9

ggplot(myDF, aes(x=fgroup, y=response, color=fgroup)) +
    geom_violin()

ggplot(myDF, aes(x=fgroup, y=response, color=fgroup)) +
    geom_violin() +
    geom_jitter(alpha=0.4)

#
# In class lab exercise - using the gapminder data set
# 

#  How has the average life expectancy changed in Asia from 1957 to
#      1982 to 2007?

#  1. create a data frame with these 3 years of data and Asia

Asia_subset <- gapminder %>%
    filter(continent=="Asia") %>%
    filter(year %in% c(1957, 1982, 2007))
table(Asia_subset$year)

#  2. generate plot of the data for the three years - add jitter to the plot

ggplot() + geom_jitter(data=Asia_subset, aes(x=as.factor(year),y=lifeExp)) 

#  3. generate a boxplot to compare life expectancy between the 3 years

ggplot() + geom_boxplot(data=Asia_subset, aes(x=as.factor(year),y=lifeExp))

#  4. generate a violin plot to compare life expectancy distribution changes

ggplot(Asia_subset, aes(x=as.factor(year), y=lifeExp, color=as.factor(year))) +
    geom_violin()

#  5. superimpose violin plot on the jittered points

ggplot(Asia_subset, aes(x=as.factor(year), y=lifeExp, color=as.factor(year))) +
    geom_violin() +
    geom_jitter(alpha=0.4) 

#  6. clean up the plot to make it better for sharing

ggplot(Asia_subset, aes(x=as.factor(year), y=lifeExp, color=as.factor(year))) +
    geom_violin() +
    geom_jitter(alpha=0.4) +
    labs(x="Year", y="Life Expectancy (years)", title = "Life Expectancy by Year for Asian Countries",
         caption="Source:  Jennifer Bryan (2015). gapminder: Data from Gapminder. R package version 
         0.2.0.")

#  7.  what is your conclusion from your analysis?

# The life expectancy rose dramtically for Asian countries between the years 1957 
# and 2007. Changes in modality are also apparent over this time range. In 1957 
# the disttribution appears to be bimodal with the the lower mode mode retaining 
# most of the density. Then, in 1982 we see a shift to a unimodal distribtution
# that is actaully skewed left, showing that the average life expectancy is rising. 
# Finally, 2007 shows another bimodal distribution like 1957, however, this distribution
# skewed left, again showing a rise in the life expectancy over time. 
# has 
