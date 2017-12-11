# ###############################################################
#                                                               #
# Author: Brian Lambert,                                        #
# Name:                                                         #
# Description:                                                  #
#                                                               #
#                                                               #
#                                                               #
#################################################################

setwd("/Users/brianlambert/Desktop/STA404/Health2") 

library(shiny)
library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr)
library(ggthemes)
library(plotly)
library(gdata)

#========================== Data ==========================

# raw biometric screening data
load("data/raw_data.RData")
# raw biometric screening data gathered by test type
load("data/raw_gathered.RData")

pha = read.xls("/Users/brianlambert/Desktop/STA404/Health2/data/PHA.xlsx", header=T)
pha_gather = pha %>% gather(test, value, -Year)


#========================== Graphics ==========================

name_vec = c("PSC","PSB","PSCO")

# manipulate variable names for legend
raw_data$PSC[raw_data$PSC == 1] = "Participated"
raw_data$PSC[raw_data$PSC == 0] = "Did Not Participate"

raw_data$PSB[raw_data$PSB == 1] = "Participated"
raw_data$PSB[raw_data$PSB == 0] = "Did Not Participate"

raw_data$PSCO[raw_data$PSCO == 1] = "Participated"
raw_data$PSCO[raw_data$PSCO == 0] = "Did Not Participate"

# reorder levels
raw_data$PSC<-factor(raw_data$PSC,levels = c("Did Not Participate","Not Required","Participated"))
raw_data$PSB<-factor(raw_data$PSB,levels = c("Did Not Participate","Not Required","Participated"))
raw_data$PSCO<-factor(raw_data$PSCO,levels = c("Did Not Participate","Not Required","Participated"))
    


View(raw_data)




### Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel(title = "Health2 Shiny App"),
    sidebarLayout(
        
        # Sidebar typically used to house input controls
        sidebarPanel(
            
            selectInput(inputId = "testId",
                        label = "Select Screening / Participation to Display",
                        choices=c("PSC","PSB","PSCO"),
                        selected="PSC"),
            
            selectizeInput(inputId = "assessmentId",
                           label= "Select Health Condition",
                           choices = c("Exercise", "Diet", "Sleep", "Smoking", "Alcohol", "Stress"), 
                           selected = "Exercise")
        ),
        
        # Main panel typically used to display outputs
        mainPanel(
            plotlyOutput(outputId = "myscatterplot")
        )
    )
)

### Define server behavior for application here
#  Expressions such as in renderPlot MUST be in {} 

server <- function(input, output) {
    
    output$myscatterplot <-

        renderPlotly({
            
            cancer_type = NULL
            if(input$testId == "PSC"){
                cancer_type = "Cervical"
            }
            else if(input$testId == "PSB"){
                cancer_type = "Breast"
            }
            else {
                cancer_type = "Colon"
            }
            
            # stacked bar chart for PSB participation by year
            p = ggplot(raw_data, 
                   aes(x=factor(year), 
                       fill=raw_data[,input$testId])) + 
                geom_bar(position = "fill") +
                labs(x="Year",
                     y=paste(cancer_type,"Screening Count")) +
                ggtitle(paste(cancer_type, "Cancer Screening Participants by Year")) +
                guides(fill=guide_legend(title="Participation Status")) +
                theme(plot.title = element_text(hjust = 0.5),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
            
            # give top margin so title doesn't overlap ggplotly menu. Clean up ggplotly menu options
            # source: https://stackoverflow.com/questions/42821171/fix-plotly-ggplotly-title-overlapping-plot-when-title-is-split-on-two-lines
            gp <- ggplotly() %>% config(collaborate=FALSE, cloud=FALSE, displaylogo=FALSE, modeBarButtonsToRemove=c("select2d", "sendDataToCloud", "pan2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "zoomIn2d", "zoomOut2d"))
            gp <- layout(gp, margin=list(t = 70))
            
            ########################################################################################
            
            phaPlot = ggplot(pha_gather, aes(x=Year, y=value, group = test, color = test)) +
                geom_line()
        }
        )
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)

