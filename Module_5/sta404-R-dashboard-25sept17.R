# sta404-R-dashboard-data-BLANK-25sep17.R
# /home/baileraj/sta404

# References
#
# Arranging multiple grobs on a page
#    https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
# Arranging ggplot
#    https://github.com/baptiste/gridextra/wiki/arranging-ggplot

library(gridExtra)
library(tidyverse)
library(gapminder)

# A dashboard is often constructed with multiple graphs and other other information. These
# can be combined together is a single display.  Not that this differs from faceting.

# First rebuild the data frames for plotting ... 

myGapData <- gapminder %>% 
    mutate(TotalGDP = pop*gdpPercap) %>% 
    mutate(order_continent = factor(continent,
                                    levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

GDPsummaryDF <- myGapData %>% 
    group_by(continent, year) %>% 
    summarise(ContinentTotalGDP = sum(TotalGDP), ncountries = n())
GDPyearDF <- myGapData %>% 
    group_by(year) %>% 
    summarise(YearTotalGDP = sum(TotalGDP))
GDPcombo <- left_join(GDPsummaryDF, GDPyearDF, by="year")
GDPcombo <- GDPcombo %>% mutate(PropWorldGDP = ContinentTotalGDP / YearTotalGDP,
                                PctWorldGDP = 100*PropWorldGDP,
                                ContGDPBillions = ContinentTotalGDP/1000000000)
GDPcombo <- GDPcombo %>% 
    mutate(order_continent = factor(continent,
                                    levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

#
# next, generate the plots for diplaying - note that ggplot grob = graphical object can be saved
#

# trace of graph of individual countries with continent colors
g1_trace <- ggplot(myGapData, aes(x=pop,y=TotalGDP,color=order_continent)) +
    geom_line(aes(group=country)) +
    theme_minimal() +
    labs(x ="Population",
         y="GDP")+ 
    theme(legend.position = "none")
 g1_trace
# str(g1_trace)

# scatterplot with facets
g2_scatter <- ggplot(myGapData, aes(x=pop,y=TotalGDP,color=order_continent)) +
    geom_point() +
    facet_wrap(~year) +
    scale_x_log10(name="Population Size (Millions)",
                  breaks=1000000*c(1,10,100),
                  labels=c("1","10","100")) +
    scale_y_log10(name="Gross Domestic Product (Billions)",
                  breaks=1000000000*c(1,100,1000),
                  labels=c("1000","10000","100000")) + 
    theme_minimal()+ 
    theme(legend.position = "none")

# stacked bar graph
g3_stackbar <- ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                                    fill=order_continent)) + 
    geom_bar(stat="identity") +
    theme(legend.position = "top") +
    theme_minimal()+ 
    theme(legend.position = "none")

# Cleveland dot chart
g4_dot <- myGapData %>% 
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
    theme_minimal() +
    theme(axis.text.y=element_text(size=9)) + 
    theme(legend.position = "none")

g4_dot

# stacked area graph
g5_stackarea <- ggplot(GDPcombo, aes(x=year, y=ContinentTotalGDP, fill=order_continent)) +
    geom_area() + 
    theme_minimal() + 
    theme(legend.position = "none")

# TRY THIS:  rewrite this code using function from forcats package

# grid.arrange = "rectangular layouts with non-overlapping cells" - baptiste / gridextra
# how can we place these displays on the same graph? ==========================
# g1_trace, g2_scatter, g3_stackbar, g4_dot, g5_stackarea

grid.arrange(g1_trace, g2_scatter, g3_stackbar, g4_dot, g5_stackarea) 

grid.arrange(g2_scatter, g3_stackbar, g4_dot, g5_stackarea)

grid.arrange(g5_stackarea, g4_dot)

# can change widths ===========================================================

grid.arrange(g1_trace, g2_scatter, g3_stackbar, g4_dot, g5_stackarea,
             widths=c(1,2))

# and heights
grid.arrange(g1_trace, g2_scatter, g3_stackbar, g4_dot, g5_stackarea,
             widths=c(1,2),heights=c(1,2,2))

# can change layout ===========================================================
#                   NA = placeholder
grid.arrange(g1_trace, g2_scatter, g3_stackbar, g4_dot, g5_stackarea,
             widths=c(1,2),heights=c(1,2,2),
             layout_matrix = rbind(c(1,NA),
                                   c(3,2),
                                   c(4,5)))


# alternatives using grid viewports ==========================================
# ref:  baptiste / gridextra GitHub gridextra Wiki

library(grid)
pushViewport(viewport(layout=grid.layout(2,2)))
# vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)
print(g1_trace, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(g5_stackarea, vp=viewport(layout.pos.row=1, layout.pos.col=2))
print(g4_dot, vp=viewport(layout.pos.row=2, layout.pos.col=1:2))

# plot insets ================================================================
# http://ggplot2.tidyverse.org/reference/annotation_custom.html

g1_trace

summary(myGapData)
# pop Q1 = 2.8e+06
# TotalGDP = 5.9e+09

# construct inset plot
# For changing x or y axis limits without dropping data observations, 
#         see coord_cartesian
g1_trace +
    scale_x_continuous(limits=c(0,2.8e+06))+
    scale_y_continuous(limits=c(0,5.9e+09))

g1_trace +
    coord_cartesian(xlim=c(0,2.8e+06),ylim=c(0,5.9e+09))

g1_trace_inset <- g1_trace +
    coord_cartesian(xlim=c(0,2.8e+06),ylim=c(0,5.9e+09)) +
    guides(color=FALSE)

g <- ggplotGrob(g1_trace_inset)
g1_trace +
    annotation_custom(grob=g,xmin=5e+08,xmax=11e+08, ymin=6e+12, ymax=12e+12) +
    theme(legend.title=element_blank())

#############################################################################
# Two final options for categorical data
# waffle plots - https://www.rdocumentation.org/packages/waffle/versions/0.7.0
# mosaic plots - https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html
library(waffle)
library(ggmosaic)


class(GDPcombo)

GDPshare07 <- GDPcombo %>%  
    filter(year==2007) %>% 
    select(continent,PctWorldGDP)

typeof(GDPshare07)

waffle(parts=GDPshare07$PctWorldGDP,
       xlab="1 sq == 1% world GDP in 2007",
       rows=10)

pctGDP <- GDPshare07$PctWorldGDP 
typeof(pctGDP)
names(pctGDP) <- as.vector(GDPshare07$continent)
View(pctGDP)

waffle(parts=pctGDP,
       xlab="1 sq == 1% world GDP in 2007",
       rows=10)

# not quite correct ... 

pctGDP
round(pctGDP)

pctGDP2 <- pctGDP
pctGDP2[4] <- 26
rpctGDP2 <- round(pctGDP2)
waffle(parts=rpctGDP2[oo],
       xlab="1 sq == 1% world GDP in 2007",
       rows=10)

# you may want to order this from the smallest to largest responses
#  use order() to do this
oo <- order(rpctGDP2)
waffle(parts=rpctGDP2[oo],
       xlab="1 sq == 1% world GDP in 2007",
       rows=10)

dd <- order(rpctGDP2, decreasing=TRUE)
waffle(parts=rpctGDP2[dd],
       xlab="1 sq == 1% world GDP in 2007",
       rows=10)

# mosaic plots ========================================================



#
# mosaic plots - ggmosaic is one option with ggplot2 world
# vcd::mosaic is another 
# Chang 13.15 - categorical, contingency data visualization

# example from vcd::mosaic
library(vcd)

# example from mosaic help
mosaic(~ Sex + Age + Survived, data = Titanic,
       main = "Survival on the Titanic", shade = TRUE, legend = TRUE)

# extending example from mosaic help

mosaic(~ Class + Sex + Age + Survived, data=Titanic, shade=TRUE)

# what does the ~ mean? 
# what is the p-value that is reported here?
# what is a Pearson residual?

# Extra stuff:  won't be discussed

# ggmosaic - 
#https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html
# Filled bar graph
NAmericas <- myGapData %>% 
    filter(continent=="Americas") %>%
    filter(country %in% c("United States","Canada","Mexico")) %>% 
    mutate(c_country=as.character(country))

NAmericas

unique(NAmericas$c_country)

ggplot(data=NAmericas) +
    geom_mosaic(aes(weight=pop, x=product(c_country, year),
                    fill=c_country)) +
    labs(x="Year",y="Proportion",
         title="N. America Population Share") +
    scale_x_discrete(limits=paste(seq(from=1952,to=2007,by=5)))
