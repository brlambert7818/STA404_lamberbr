# sta404-R-exploring-relationships-BLANK-11sep17.R
# directory:  /home/baileraj/sta404

# ====================================================================
# load packages we will need
library(tidyverse)
library(read)

# ====================================================================
# let's import a couple of data files 

# this command will work on a local installation of RStudio but not on the server
#fev_DF <- read.table("http://www.users.miamioh.edu/baileraj/classes/sta404/fev_data.txt")

# so, let's download
#      http://www.users.miamioh.edu/baileraj/classes/sta404/fev_data.txt
# to our local disk and then upload this to the server

# open the data file on your file system.  comment on the structure of the file.

fev_DF <- read.table("/Users/brianlambert/Desktop/STA404/Module_3/fev_data.txt", header=T)
manatee_DF <- read.csv("/Users/brianlambert/Desktop/STA404/Module_3/manatee-updateTF.csv", header=T)
lakeDepth_DF <- read.csv("/Users/brianlambert/Desktop/STA404/Module_3/lake-do-depth.csv", header=T)

head(fev_DF)

# what happens if header=T is omitted?

# ====================================================================
# basic scatter plots

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_point()

# what if you have a large data set with lots of overplotting?
# for example, diamonds from ggplot2 package has 50K observations

# jitter

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_jitter(width=.25)

# change the plotting symbol - e.g. open circle - shape=1

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_jitter(shape=1,width=.25)

# change the transparency - e.g. alpha= .3

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_point(alpha=.1)

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_jitter(alpha=.3, width=.25)

# plot some binned values - think histogram in 2 dimensions

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_bin2d()

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_bin2d() +
    scale_fill_continuous(low = "lightgrey", high="black") +
    theme_minimal()

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    stat_binhex()   # probably better for continuous vs. integer values

# adding a statistical layer - model fits
# linear regression

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_jitter(width=.25, shape=1) +
    geom_smooth(method="lm")

# fitting a linear model

fev_linreg <- lm(ht.in ~ age.yrs, data=fev_DF)

fev_DF <- fev_DF %>% 
    mutate(yhat=predict(fev_linreg), r = residuals(fev_linreg))

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_jitter(width=.25) +
    geom_line(aes(y=yhat))

# residual plot

ggplot(fev_DF, aes(x=age.yrs, y=r) ) +
    geom_jitter(width=.25,alpha=.4)


# ==================================================================
# options to the parametric models
# loess - local polynomial regression fitting

xx <- seq(-8*pi,8*pi,length=100)
yy <- 3 + 2+xx + 4*sin(xx) + rnorm(100, sd = 1.5)

curvy_DF <- data.frame(x=xx, y=yy)
ggplot(curvy_DF, aes(x,y)) +
    geom_point()

ggplot(curvy_DF, aes(x,y)) +
    geom_point() +
    geom_smooth(method="lm")

ggplot(curvy_DF, aes(x,y)) +
    geom_point() +
    geom_smooth(method="loess")

# you can change the span of the data considered for the local fits
ggplot(curvy_DF, aes(x,y)) +
    geom_point() +
    geom_smooth(method="loess",span=.4)

ggplot(curvy_DF, aes(x,y)) +
    geom_point() +
    geom_smooth(method="loess",span=.2)

ggplot(curvy_DF, aes(x,y)) +
    geom_point() +
    geom_smooth(method="loess",span=.1)

# let's look at the ht-age relationship data again

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_jitter(width=.25) +
    geom_smooth(method="loess",se=F, span=.5)

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_jitter(width=.25) +
    geom_smooth(method="loess",se=F, span=.4)


# residual plot - revisited

ggplot(fev_DF, aes(x=age.yrs, y=r) ) +
    geom_jitter(width=.25,alpha=.4) +
    geom_smooth(method="loess", se=F, color="red") +
    geom_hline(yintercept=0, color="black") +
    theme_minimal()


#======================================================================

# What about the other variables in the fev_DF?

ggplot(fev_DF,aes(x=age.yrs, y=ht.in, color=factor(ind.Male))) + 
    geom_jitter(width=.25, alpha=.3) +
    geom_smooth()

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_jitter(width=.25, alpha=.3) +
    facet_grid(. ~ ind.Male)

fev_DF <- fev_DF %>% 
    mutate(gender = factor(ind.Male, labels=c("Female","Male")))

with(fev_DF, table(ind.Male, gender))

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
    geom_jitter(width=.25, alpha=.3) +
    geom_smooth(method="lm") +
    facet_grid(. ~ gender)


#======================================================================

# what if you want to add a model fit to an existing plot?
# Using Challenger disaster data
# http://www.asktog.com/books/challengerExerpt.html
# https://archive.ics.uci.edu/ml/datasets/Challenger+USA+Space+Shuttle+O-Ring
# Temp forecast at launch of Challenger = 31 F

# data set is text with spaces separating values
oring <- read_table(file="https://archive.ics.uci.edu/ml/machine-learning-databases/space-shuttle/o-ring-erosion-only.data",
                    col_names=c("Nring","Ndistress","TempF","LeakCheckPSI","LaunchOrder"))

str(oring)
mode(oring)                        # so what type of object did read_table produce?

oring_DF <- as.data.frame(oring)   # convert to a data frame
oring_DF

# adding variable indicating distress to at least one O-ring
oring_DF <- oring_DF %>% 
    mutate(ind_distress = ifelse(Ndistress>0, 1, 0))    # adding variable indicating distress 

# oring_DF <- oring_DF %>% 
#   mutate(ind_distress = as.numeric(Ndistress>0))  

# plot of original data

ggplot(oring_DF, aes(x=TempF, y=ind_distress)) +
    geom_point()

# suppose a simple logistic regression model was fit
# log(odds of O-ring distress) = b0 + b1 TempF

oring_logreg <- glm(ind_distress ~ TempF, data=oring_DF, family=binomial)

summary(oring_logreg)

oring_DF <- oring_DF %>% 
    mutate(phat = predict(oring_logreg, type="response"))

ggplot(oring_DF, aes(x=TempF, y=ind_distress)) +
    geom_point() +
    geom_line(aes(y=phat))

# predictions are a little jagged - clean this up

pred_DF <- data.frame(TempF = seq(from=min(oring_DF$TempF),
                                  to=max(oring_DF$Temp),
                                  length=1000))
pred_DF$phat <- predict(oring_logreg, newdata=pred_DF, type="response")

ggplot(oring_DF, aes(x=TempF, y=ind_distress)) +
    geom_point() +
    geom_line(data=pred_DF, aes(x=TempF,y=phat))

# cleaning up figure
# http://ggplot2.tidyverse.org/reference/annotate.html

ggplot(oring_DF, aes(x=TempF, y=ind_distress)) +
    geom_point() +
    geom_line(data=pred_DF, aes(x=TempF,y=phat), color="darkgrey")+
    scale_x_continuous(name="Temperature (F)", limits=c(50,84)) +
    scale_y_continuous(name="O-ring distress observed", breaks=c(0,1),
                       labels=c("No","Yes")) +
    annotate("text", x=71,y=.25, 
             label="No problems for\nlaunch Temp > 70 F",
             hjust=0, color="blue") +
    annotate("rect", xmin = 71, xmax = 83, ymin = 0, ymax = 1,
             alpha = .2, fill="lightblue") +
    annotate("text", x=50, y=.50,
             label="Challenger launch\ntemperature forecast\nwas 31 F!",
             hjust=0, color="red") +
    annotate("text",x=60,y=.9,label="Logistic Regression Prediction",
             hjust=0, color="darkgrey") +
    labs(caption="Source: UCI Machine Learning Repository") +
    theme_minimal()

#================================================================

# Lab Exercise

#work with the lake-do-depth data

#display the fev_DF data with superimposed loess smooth separate for girls and boys

