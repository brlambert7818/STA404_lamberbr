# Author: Brian Lambert
# Description: Use the gapminder dataset to explore the relationship
# between average life expectancy and GDP for 2007.


################### Data ###################

# tbl_df object containing only records with year = 2007
gap_2007 <- gapminder[gapminder$year==2007,]
#print(str(gap_2007))


################### Visualization ###################

# displays (life expectancy, gdpPercap) for each country, color coded by continent
myplot <- ggplot() +
    geom_point(data=gap_2007, aes(x=lifeExp, y=gdpPercap, group=country, color=continent)) +  
    labs(x="Life Expectancy (years)", y="GDP per Capita (gdp/population)",
         title = "Life Expectancy vs GDP per Capita by Country: 2007",
         caption="Source:  Jennifer Bryan (2015). gapminder: Data from Gapminder. R package version 
         0.2.0.") 
myplot
# save chart to pdf. Removed w,h,inch to increase graph size to fit title and spread data
ggsave(plot=myplot, "/Users/brianlambert/Desktop/STA404/Module_1/gapminder_lifeExpVsGdp_2007.pdf", device="pdf")

################### Extra Analysis ###################

# max and min gdp per capita by country
gdp_min <- gap_2007[gap_2007$gdpPercap == min(gap_2007$gdpPercap),]
gdp_max <- gap_2007[gap_2007$gdpPercap == max(gap_2007$gdpPercap),]
print(gdp_min)
print(gdp_max)

# max and min life expectancy by country
lifeExp_min <- gap_2007[gap_2007$lifeExp == min(gap_2007$lifeExp),]
lifeExp_max <- gap_2007[gap_2007$lifeExp == max(gap_2007$lifeExp),]
print(lifeExp_min)
print(lifeExp_max)
