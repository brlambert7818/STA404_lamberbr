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

#========================== Graphics ==========================


### Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel(title = "Health2 Shiny App"),
    sidebarLayout(
        
        # Sidebar typically used to house input controls
        sidebarPanel(
            
            selectInput(inputId = "testId",
                        label = "Select Screening / Participation to Display",
                        choices=c("PSC","PSB","PSCO","WV"),
                        selected="PSC"),
            
            checkboxGroupInput(inputId = "valuesId", 
                               label = "Select Values to Display", 
                               choices = c("Participated", "Did Not Participate", "Not Required"), 
                               selected = c("Participated", "Did Not Participate", "Not Required"))
        ),
        
        # Main panel typically used to display outputs
        mainPanel(
            plotOutput(outputId = "myscatterplot")
        )
    )
)

### Define server behavior for application here
#  Expressions such as in renderPlot MUST be in {} 

server <- function(input, output) {
    
    # get counts of participants each year for geom bar width
    occurences = table(unlist(raw_data))
    
    raw_data$size = 0

    for (i in 1:nrow(raw_data)) {
        
        year_size = 0
        if(raw_data[i, "year"] == 2011) {
            year_size = 4554
        } else if(raw_data[i, "year"] == 2012) {
            year_size = 4644
        } else if(raw_data[i, "year"] == 2013) {
            year_size = 3403
        } else if(raw_data[i, "year"] == 2014) {
            year_size = 3377
        } else if(raw_data[i, "year"] == 2015) {
            year_size = 10000
        } else {
            year_size = 3664
        }

        raw_data$size[i] = year_size
    }
    
    output$myscatterplot <-

        renderPlot({
            
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
            p = ggplot(temp_data,aes(x=factor(year), fill=temp_data[,input$testId])) + 
                geom_bar(position = "fill") +
                labs(x="Year",y=paste(cancer_type,"Screening Count")) +
                ggtitle(paste(cancer_type, "Cancer Screening Participants by Year")) +
                guides(fill=guide_legend(title="Participation Status")) +
                theme(plot.title = element_text(hjust = 0.5),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
            
            p
            
            # give top margin so title doesn't overlap ggplotly menu. Clean up ggplotly menu options
            # source: https://stackoverflow.com/questions/42821171/fix-plotly-ggplotly-title-overlapping-plot-when-title-is-split-on-two-lines
            # gp <- ggplotly() %>% config(collaborate=FALSE, cloud=FALSE, displaylogo=FALSE, modeBarButtonsToRemove=c("select2d", "sendDataToCloud", "pan2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "zoomIn2d", "zoomOut2d"))
            # gp <- layout(gp, margin=list(t = 70))
        }
        )
}


### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)

