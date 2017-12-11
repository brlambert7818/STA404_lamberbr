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

pha = read.xls("/Users/brianlambert/Desktop/STA404/Health2/data/PHA.xlsx", header=T)
pha_gather = pha %>% gather(assessment, value, -Year)

#========================== Graphics ==========================

assessments = c("Exercise", "Diet", "Sleep", "Smoking", "Alcohol", "Stress")

### Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel(title = "Health2 Shiny App"),
    sidebarLayout(
        
        # Sidebar typically used to house input controls
        sidebarPanel(
            
            # group checkbox to display certain minority groups
            checkboxGroupInput(inputId="assessmentId", label="Select Assessments to Display",
                               choices=assessments, selected = "Exercise"),
            
            # group checkbox to display certain minority groups
            checkboxGroupInput(inputId="benchmarksId", label="Select Assessment Benchmarks to Display",
                               choices=assessments, selected = NULL)
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
            
            # excludes minority status type if not selected in select_minority
            original = pha_gather
            if(!("Exercise" %in% input$assessmentId)) {
                pha_gather = pha_gather[pha_gather$assessment != "Exercise",]
            }
            if(!("Diet" %in% input$assessmentId)) {
                pha_gather = pha_gather[pha_gather$assessment != "Diet",]
            }
            if(!("Sleep" %in% input$assessmentId)) {
                pha_gather = pha_gather[pha_gather$assessment != "Sleep",]
            }
            if(!("Smoking" %in% input$assessmentId)) {
                pha_gather = pha_gather[pha_gather$assessment != "Smoking",]
            }
            if(!("Alcohol" %in% input$assessmentId)) {
                pha_gather = pha_gather[pha_gather$assessment != "Alcohol",]
            }
            if(!("Stress" %in% input$assessmentId)) {
                pha_gather = pha_gather[pha_gather$assessment != "Stress",]
            }
            
            # pop up modal telling user to select at least one of the minorities to display if
            # all have been unchecked. 
            # source: https://shiny.rstudio.com/articles/modal-dialogs.html
            if(length(input$assessmentId) == 0) {
                showModal(modalDialog(title="Whoops!",
                                      "Please select at least of one the assessments to display!",
                                      easyClose = T))
            }
            
            phaPlot = ggplot(pha_gather, aes(x=Year, y=value, group = assessment, color = assessment)) +
                geom_line() +
                geom_point() +
                labs(x="Year",
                     y="Participation Percentage") +
                scale_y_continuous(limits = c(0, 1)) +
                ggtitle("Participation in Personal Health Assessments") +
                guides(fill=guide_legend(title="Assessment Type")) +
                theme(plot.title = element_text(hjust = 0.5),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
            
            if ("Exercise" %in% input$benchmarksId) {
                phaPlot +
                    geom_hline(aes(yintercept=.435), color="#F8766D", linetype="dashed")
            }
            
            # if ("Diet" %in% input$benchmarksId) {
            #     phaPlot +
            #         geom_hline(aes(yintercept=.42), color="#C49A00", linetype="dashed")
            # }
            # if ("Sleep" %in% input$benchmarksId) {
            #     phaPlot +
            #         geom_hline(aes(yintercept=.686), color="#53B400", linetype="dashed")
            # }
            # if ("Smoking" %in% input$benchmarksId) {
            #     phaPlot +
            #         geom_hline(aes(yintercept=.221), color="#00C094", linetype="dashed")
            # }
            # if ("Alcohol" %in% input$benchmarksId) {
            #     phaPlot +
            #         geom_hline(aes(yintercept=.23), color="#00B6EB", linetype="dashed")
            # }
            # if ("Stress" %in% input$benchmarksId) {
            #     phaPlot +
            #         geom_hline(aes(yintercept=.26), color="#A58AFF", linetype="dashed")
            # }
            
            
            gp <- ggplotly() %>% config(collaborate=FALSE, cloud=FALSE, displaylogo=FALSE, modeBarButtonsToRemove=c("select2d", "sendDataToCloud", "pan2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "zoomIn2d", "zoomOut2d"))
            gp <- layout(gp, margin=list(t = 100, l=50, b=50))
        }
        )
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)
