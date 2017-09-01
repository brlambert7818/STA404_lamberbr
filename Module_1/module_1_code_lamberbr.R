# Author: Brian Lambert
# Use the gapminder dataset to explore the relationship
# between average life expectancy and GDP for 2007.

# tbl_df object containing only records with year = 2007
gap_2007 <- gapminder[gapminder$year==2007,]
#print(summary(gap_2007))

# displays (life expectancy,gdpPercap) for each country, color coded by continent
ggplot() + 
    geom_point(data=gap_2007, aes(x=lifeExp, y=gdpPercap, group=country, color=continent)) 

# max and min gdpPercap
gdp_max <- gapminder[gapminder$gdpPercap == max(gapminder$gdpPercap),]
gdp_min <- gapminder[gapminder$gdpPercap == min(gapminder$gdpPercap),]
print(gdp_max)
print(gdp_min)

# max and min lifeExp
lifeExp_max <- gapminder[gapminder$lifeExp == max(gapminder$lifeExp),]
lifeExp_min <- gapminder[gapminder$lifeExp == min(gapminder$lifeExp),]
print(lifeExp_max)
print(lifeExp_min)