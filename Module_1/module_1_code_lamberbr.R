# Author: Brian Lambert
# Use the gapminder dataset to explore the relationship
# between average life expectancy and GDP for 2007.

# tbl_df object containing only records with year = 2007
gap_2007 <- gapminder[gapminder$year==2007,]
#print(summary(gap_2007))


ggplot() + 
    geom_line(data=gap_2007, aes(x=gdpPercap, y=lifeExp, group=country), color="lightgrey") +
    guides(color="none") +
    labs(x="Life Expectancy", y="GDP per Capita", 
         caption="Source:  Jennifer Bryan (2015). gapminder: Data from Gapminder. R package version 
         0.2.0.") +
    theme_minimal()