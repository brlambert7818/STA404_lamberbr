# ###############################################################
#                                                               #
# Author: Brian Lambert                                         #
# Name: app.R                                                   #
# Description: Krallman shiny application. Default is bar chart #
# of divisions chosen by 2021 freshman broken down by minority  #
# status. With shiny, user has option to facet by gender and    #
# can hover over bar to view the count in that bar, division,   #
# and minority status. Use may also choose to only display      #
# certain minority groups. If none are selected an error        #
# messge will be displayed.                                     #
#                                                               #   
#                                                               #
#################################################################

library(shiny)
library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr)
library(ggthemes)
library(plotly)

#========================== Data ==========================

load("basic_demographics.RData")


#========================== Graphics ==========================

# rename divisons to more commmon naming protocols
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="AP"] <- "CEC"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="AS"] <- "CAS"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="BU"] <- "FSB"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="EA"] <- "EHS"
levels(basic_demographics$Division)[levels(basic_demographics$Division)=="FA"] <- "CCA"

basic_demographics$Division <- fct_infreq(basic_demographics$Division)

# reorder by minority.status 
basic_demographics <- basic_demographics %>% 
    mutate(status = factor(Minority.Status,
                                   levels=c("Domestic Nonminority", "Domestic Minority", "International")))

# rename gender variables for cleaner hover display
basic_demographics <- basic_demographics %>% 
    mutate(Gender = ifelse(Gender=="M","Male","Female"))

# create minority status list for checkboxGroupInput
min_status = c("Domestic Nonminority", "Domestic Minority", "International")


### Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel(title = "Krallman Shiny App"),
    sidebarLayout(
        
        # Sidebar typically used to house input controls
        sidebarPanel(
            
            # checkbox to facet by gender. Default: not selected
            checkboxInput(inputId= "facet_gender", label = "Facet by Gender"),
            
            # group checkbox to display certain minority groups
            checkboxGroupInput(inputId="select_minority", label="Filter by Minority Status",
                        choices=min_status, selected = min_status)
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
            original = basic_demographics
            if(!("Domestic Nonminority" %in% input$select_minority)) {
                basic_demographics = basic_demographics[basic_demographics$status != "Domestic Nonminority",]
            }
            if(!("Domestic Minority" %in% input$select_minority)) {
                basic_demographics = basic_demographics[basic_demographics$status != "Domestic Minority",]
            }
            if(!("International" %in% input$select_minority)) {
                basic_demographics = basic_demographics[basic_demographics$status != "International",]
            }
            
            # pop up modal telling user to select at least one of the minorities to display if
            # all have been unchecked. 
            # source: https://shiny.rstudio.com/articles/modal-dialogs.html
            if(length(input$select_minority) == 0) {
                showModal(modalDialog(title="Whoops!",
                                      "Please select at least of one the minority groups to display!",
                                      easyClose = T))
            }
            
            
            # rename variable for cleaner hover display
            divison = as.factor(basic_demographics$Division)
            
            # bar chart of divisons chosen, order by minority, faceted by gender
            plot <- ggplot(basic_demographics, aes(x = divison, fill = status)) +
                geom_bar(position = "dodge") +
                xlab("College Division") +
                ylab("Student Count\n") +
                guides(fill=guide_legend(title="Minority Status")) +
                ggtitle("Count of College Division Chosen by Minority Status") +
                theme(plot.title = element_text(hjust = 0.5),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank()) + 
                scale_fill_manual(name = 'order minority', 
                                  values =c("#F44336", "#BDBDBD", "#455A64"))
            
            # facets by gender if button is selected. Default: not selected
            if(input$facet_gender) {
                plot = plot +
                    facet_wrap(~Gender)
            }
            
            # render plot
            plot
            
            # give top margin so title doesn't overlap ggplotly menu. Clean up ggplotly menu options
            # source: https://stackoverflow.com/questions/42821171/fix-plotly-ggplotly-title-overlapping-plot-when-title-is-split-on-two-lines
            gp <- ggplotly() %>% config(collaborate=FALSE, cloud=FALSE, displaylogo=FALSE, modeBarButtonsToRemove=c("select2d", "sendDataToCloud", "pan2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "lasso2d", "zoomIn2d", "zoomOut2d"))
            gp <- layout(gp, margin=list(t = 70))
        }
        )
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)

