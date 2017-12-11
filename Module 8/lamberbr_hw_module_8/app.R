# ###############################################################
#                                                               #
# Author: Brian Lambert                                         #
# Name: app.R                                                   #
# Description: Shiny display of FEV data with options to        #
# dynamically change the data being displayed and other         #
# aspects of the display.                                       #
#                                                               #         
#################################################################

setwd("/Users/brianlambert/Desktop/STA404/Module 8/lamberbr_hw_module_8")   

library(shiny)
library(tidyverse)

fev = load("fev.RData")

# correspondence vector between ui variable names and server variable names
varnames <- c("Age (years)" = "age.yrs",
              "FEV (L)" = "fev.L",
              "Height (in)" = "ht.in")

# read in the data and do any modifications
fev_DF <- read.table("/Users/brianlambert/Desktop/STA404/Module 8/fev_data copy.txt", header=T)
fev_DF <- fev_DF %>% 
    mutate(gender = ifelse(ind.Male==0,"Female","Male"))

### Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel(title = "FEV Scatterplot Explorer"),
    sidebarLayout(
        
        # Sidebar typically used to house input controls
        sidebarPanel(
            
            selectInput(inputId = "yvar", label= "Select an y-variable", 
                        choices = varnames),
            
            # selectInput(inputId = "xvar", label= "Select an x-variable", 
            #             choices = varnames),
            
            uiOutput(outputId = "xvar"),
            
            checkboxInput(inputId= "facet_gender", label = "Facet by Gender"),
            
            checkboxInput(inputId= "color_gender", label = "Color by Gender"),
            
            checkboxInput(inputId= "smooth", label = "Add LOESS Smooth Curve"),
            
            checkboxInput(inputId= "jitter", label = "Add Jittering")
            
            
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
    
    output$xvar <- renderUI({
        xvarsubset <- varnames[varnames!=input$yvar]
        selectInput(inputId = "xvar",
                    label = "Select an X-variable",
                    choices = xvarsubset)
    })
    
    output$myscatterplot <-
        renderPlot({
            
            # create initial plot with selected variables
            p1 <- ggplot() + 
                labs(x=names(varnames)[varnames==input$xvar],
                     y=names(varnames)[varnames==input$yvar]) +
                ggtitle(paste("Scatterplot of ",y=names(varnames)[varnames==input$yvar],"by",x=names(varnames)[varnames==input$xvar])) +
                theme_bw()+
                theme(plot.title = element_text(hjust = 0.5))
            
            # add jitter and color by gender
            if (input$jitter & input$color_gender){
                p1 = p1 +
                    geom_jitter(aes_string(x=input$xvar,y=input$yvar, color="gender"),data=fev_DF, method = "loess", se=FALSE) 
            }
            
            # add jitter
            if (input$jitter & !input$color_gender) {
                p1 = p1 +
                    geom_jitter(aes_string(x=input$xvar,y=input$yvar),data=fev_DF, method = "loess", se=FALSE) 
            }
            
            # add color by gender
            if(input$color_gender) {
                 p1 = p1 +
                     geom_point(aes_string(x=input$xvar,y=input$yvar,color="gender"),
                                data=fev_DF)
            } 
            
            # simple scatter plot, none of above options selected
            else {
                p1 = p1 + 
                    geom_point(aes_string(x=input$xvar,y=input$yvar),data=fev_DF)
            }
            
            # add loess smooth
            if (input$smooth){
                p1 = p1 +
                    geom_smooth(aes_string(x=input$xvar,y=input$yvar),data=fev_DF, method = "loess", se=FALSE)
            }
            
            # add loess smooth and color by gender
            if (input$smooth & input$color_gender){
                p1 = p1 +
                    geom_smooth(aes_string(x=input$xvar,y=input$yvar, col="gender"),
                                data=fev_DF,
                                method = "loess",
                                se=FALSE)
            }
            
            # add facet by gender
            if(input$facet_gender) {
                p1 = p1 +
                    facet_grid(. ~ gender)
            }
            
            # render plot
            p1
            
        }
        )
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)

