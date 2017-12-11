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

library(shinydashboard)
library(shiny)
library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr)
library(ggthemes)
library(plotly)
library(gdata)
library(reshape2)
library(readxl)
library(waffle)
library(RColorBrewer)

#========================== DATA ==========================

    ########################  EMPLOYEE DATA ######################## 

pha = read.xls("/Users/brianlambert/Desktop/STA404/Health2/data/PHA.xlsx", header=T)
pha_gather = pha %>% gather(assessment, value, -Year)

biomet <- read_excel("~/Desktop/STA404/Health2/data/biomet.xlsx")


    ########################  ADMIN DATA ######################## 

# raw biometric screening data
load("data/raw_data.RData")
# raw biometric screening data gathered by test type
load("data/raw_gathered.RData")

Screen1 = read_excel("~/Desktop/STA404/Health2/data/Screen1.xlsx")

#========================== UI ==========================

ui <- dashboardPage(
    
    skin = "red",
    
    # header
    dashboardHeader(title = "Miami Health"),
    
    # sidebar. navigation bewtween admin and employee dashboards
    dashboardSidebar(
        sidebarMenu(
                    menuItem("Employee", tabName = "employee", icon = icon("dashboard")),
                    menuItem("Administrator", tabName = "admin", icon = icon("dashboard"))
        )
    ),
    
    # body
    dashboardBody(
        
        tabItems(
            
            #==========================              ==========================
            #========================== EMPLOYEE UI  ==========================
            #==========================              ==========================
            
            tabItem(tabName = "employee",
                    
                    # PHA plot and control panel
                    fluidRow(
                        box(title="Participation in Personal Health Assessments",
                            plotOutput("phaPlot"), width=8),
                        
                        box(
                            title = "Select Assessments to Display",
                            uiOutput("assessments"),
                            width = 4, 
                            solidHeader = TRUE, 
                            status = "danger"
                        )
                    ),
                    
                    # add whitespace
                    fluidRow(
                        column(
                            width = 2,
                            offset = 0, 
                            style='padding:20px;'
                        )
                    ),
                    
                    # title for benchmarks
                    fluidRow(
                        column(12, align="center",
                            box(title="Personal Health Assessment National Benchmarks",
                                height=45,
                                width=15)
                        )
                    ),
                    
                    # PHA national benchmarks
                    fluidRow(
                        box(
                            title = "Exercise", 
                            textOutput("exercise_num"),
                            # inline css to format the percentages in each box
                            # source: https://stackoverflow.com/questions/32152827/r-shiny-center-and-resize-textinput
                            tags$style(type="text/css", "#exercise_num { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "success",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90

                        ),
                        box(
                            title = "Diet", 
                            textOutput("diet_num"),
                            tags$style(type="text/css", "#diet_num { height: 100px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "primary",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90

                        ),
                        box(
                            title = "Sleep", 
                            textOutput("sleep_num"),
                            tags$style(type="text/css", "#sleep_num { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "info",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90
                        ),
                        box(
                            title = "Smoking", 
                            textOutput("smoking_num"),
                            tags$style(type="text/css", "#smoking_num { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "warning",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90
                            #DD4B38
                        ),
                        box(
                            title = "Alcohol", 
                            textOutput("alcohol_num"),
                            tags$style(type="text/css", "#alcohol_num { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "danger",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90
                        ),
                        box(
                            title = "Stress", 
                            textOutput("stress_num"),
                            tags$style(type="text/css", "#stress_num { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "success",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90
                        )
                    ),
                    
                    ######################  Miami PHA 2016 #######################
                    
                    # title for benchmarks
                    fluidRow(
                        column(12, align="center",
                               box(title="Miami Personal Health Assessment: 2016",
                                   height=45,
                                   width=15,
                                   background = "red")
                        )
                    ),
                    
                    # PHA national benchmarks
                    fluidRow(
                        box(
                            title = "Exercise", 
                            textOutput("exercise_num2"),
                            # inline css to format the percentages in each box
                            # source: https://stackoverflow.com/questions/32152827/r-shiny-center-and-resize-textinput
                            tags$style(type="text/css", "#exercise_num2 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "success",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90
                            
                        ),
                        box(
                            title = "Diet", 
                            textOutput("diet_num2"),
                            tags$style(type="text/css", "#diet_num2 { height: 100px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "primary",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90
                            
                        ),
                        box(
                            title = "Sleep", 
                            textOutput("sleep_num2"),
                            tags$style(type="text/css", "#sleep_num2 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "info",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90
                        ),
                        box(
                            title = "Smoking", 
                            textOutput("smoking_num2"),
                            tags$style(type="text/css", "#smoking_num2 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "warning",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90
                            #DD4B38
                        ),
                        box(
                            title = "Alcohol", 
                            textOutput("alcohol_num2"),
                            tags$style(type="text/css", "#alcohol_num2 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "danger",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90
                        ),
                        box(
                            title = "Stress", 
                            textOutput("stress_num2"),
                            tags$style(type="text/css", "#stress_num2 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                            status = "success",
                            solidHeader = TRUE,
                            width = 2,
                            height = 90
                        )
                    ),
                    
                    # add whitespace
                    fluidRow(
                        column(
                            width = 2,
                            offset = 0, 
                            style='padding:40px;'
                        )
                    ),
                    
                    # biometric screening data
                    fluidRow(
                        box(title="Biometric Screening Results",
                            plotOutput("bioPlot"), width=12)
                    )
            ),
            
            #==========================          ==========================
            #========================== ADMIN UI ==========================
            #==========================          ==========================
            
            tabItem(tabName = "admin",
                    
                    # cancer screening plot and control panel
                    fluidRow(
                        box(title= textOutput("cancer_name"),
                            plotOutput("pcsPlot"), width=8),
                        
                        column(
                            width = 4,
                            # pcs controls
                            box(
                                title = "Select Cancer Type",
                                uiOutput("screenings"),
                                width = NULL, 
                                solidHeader = TRUE, 
                                status = "danger"
                            ),
                            box(
                                title = "Change in Participation", 
                                textOutput("participation_per"),
                                tags$style(type="text/css", "#participation_per { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                                status = "primary",
                                solidHeader = TRUE,
                                width = 20,
                                height = 90
                            )
                        )
 
                    ),
                    
                    # add whitespace
                    fluidRow(
                        column(
                            width = 2,
                            offset = 0, 
                            style='padding:40px;'
                        )
                    ),
                    
                    # cancer screening, alternative
                    fluidRow(
                        box(title= "Cancer Screening Participation: Completion Stats Data",
                            plotOutput("pcs2Plot"), width=12)
                    ),
                    
                    # add whitespace
                    fluidRow(
                        column(
                            width = 2,
                            offset = 0, 
                            style='padding:40px;'
                        )
                    ),
                    
                    # title for wellness prograns
                    fluidRow(
                        column(12, align="center",
                               box(title="Wellness Program Participation Change: 2011-2015",
                                   height=45,
                                   width=15)
                        )
                    ),
                    
                    # wellness program stats
                    fluidRow(
                        infoBox(title = "Active Living", 
                                value = "14%", subtitle = NULL,
                                icon = shiny::icon("bar-chart"), color = "green", width = 4
                                ),
                        infoBox(title = "Group Fitness", 
                                value = "20.8%", subtitle = NULL,
                                icon = shiny::icon("bar-chart"), color = "green", width = 4
                        ),
                        infoBox(title = "Healthy Eating", 
                                value = "54.9%", subtitle = NULL,
                                icon = shiny::icon("bar-chart"), color = "green", width = 4
                        ),
                        infoBox(title = "Tobacco Cessation Classes", 
                                value = "-14.9%", subtitle = NULL,
                                icon = shiny::icon("bar-chart"), color = "red", width = 4
                        ),
                        infoBox(title = "Preventive Health", 
                                value = "-13.1%", subtitle = NULL,
                                icon = shiny::icon("bar-chart"), color = "red", width = 4
                        )
                    )
            )
        )
    )
)


#========================== APP ==========================

server <- function(input, output) {
    
    
    #==========================              ==========================
    #========================== EMPLOYEE APP ==========================
    #==========================              ==========================
    
    
    ########################  PHA Controls Box ######################## 
    
    assessmentList = c("Exercise", "Diet", "Sleep", "Smoking", "Alcohol", "Stress")
    
    output$assessments <- renderUI({
        # group checkbox to display certain minority groups
        checkboxGroupInput(inputId="assessmentId",
                           label=NULL,
                           choices=assessmentList, 
                           selected = "Exercise")
    })

    
    ########################  PHA Plot ######################## 
    
    dataset = reactive({
        # excludes minority status type if not selected in select_minority
        original = pha_gather
        if(!("Exercise" %in% input$assessmentId)) {
            original = original[original$assessment != "Exercise",]
        }
        if(!("Diet" %in% input$assessmentId)) {
            original = original[original$assessment != "Diet",]
        }
        if(!("Sleep" %in% input$assessmentId)) {
            original = original[original$assessment != "Sleep",]
        }
        if(!("Smoking" %in% input$assessmentId)) {
            original = original[original$assessment != "Smoking",]
        }
        if(!("Alcohol" %in% input$assessmentId)) {
            original = original[original$assessment != "Alcohol",]
        }
        if(!("Stress" %in% input$assessmentId)) {
            original = original[original$assessment != "Stress",]
        }
        return(original)
    })
    
    
    output$phaPlot <- renderPlot({
        
        phaPlot = ggplot(dataset(), aes(x=Year, y=value, group = assessment, color = assessment)) +
            geom_line() +
            geom_point() +
            labs(x="Year",
                 y="Participation Percentage") +
            # source: https://stackoverflow.com/questions/37375768/ggplot-ggplotly-scale-y-continuous-ylim-and-percentage
            scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
            guides(fill=guide_legend(title="Assessment Type")) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank())
        
        return(phaPlot)
    })
    
    ######################## Benchmarks ########################     
    
    # make percentages available for box to render and adjust text elements with css
    output$exercise_num = renderText("43.5%")
    output$diet_num = renderText("42%")
    output$sleep_num = renderText("68.6%")
    output$smoking_num = renderText("22.1%")
    output$alcohol_num = renderText("23%")
    output$stress_num = renderText("30%")
    
    output$exercise_num2 = renderText("69%")
    output$diet_num2 = renderText("13%")
    output$sleep_num2 = renderText("71%")
    output$smoking_num2 = renderText("3%")
    output$alcohol_num2 = renderText("7%")
    output$stress_num2 = renderText("28%")
    
    ########################  BIOMETRIC Plot ######################## 
    
    bio_change<-  melt(biomet, id.vars = "Condition", value.name = "value", variable.name = "Year")
    
    output$bioPlot <- renderPlot({
        
        bioPlot = qplot(Year, value, data = bio_change, geom = "line", group = Condition, color = Condition) + 
                    facet_grid(Condition ~ .) +
                    ylab("Proportion of Results with Condition") +
                    theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank(),
                        panel.background = element_blank())
        return(bioPlot)
    })
    
    

    
    #==========================           ==========================
    #========================== ADMIN APP ==========================
    #==========================           ========================== 
    
    
    ########################  PCS Controls Box ######################## 
    
    output$screenings <- renderUI({
        # group checkbox to display types of cancer screenings
        selectInput(inputId = "screeningId",
                    label = NULL,
                    choices=c("Cervical","Breast","Colon"), 
                    selected = "Cervical")
    })
    
    output$cancer_name = renderText(paste(input$screeningId, "Cancer Screening Participation: Raw Data", sep=" "))
    
    
    ########################  participation change box ########################
    
    participation = reactive({
        per = NULL
        if(input$screeningId == "Cervical") {
            per = "5.8%" 
        } else if(input$screeningId == "Breast") {
            per = "-9.5%" 
        } else {
            per = "5.3%"
        }
        return(per)
        
    })
    
    output$participation_per = renderText(participation())
    
    
    ########################  PCS Plot ######################## 
    
    # convert test names to user readable
    test_name = reactive({
        test=NULL
        if(input$screeningId == "Cervical") {
            test = "PSC" 
        } else if(input$screeningId == "Breast") {
            test = "PSB" 
        } else {
            test = "PSCO"
        }
        return(test)
    })
    
    # cancer screening plot
    output$pcsPlot <- renderPlot({
        
        pcsPlot = ggplot(raw_data,aes(x=factor(year), fill=raw_data[,test_name()])) + 
                    geom_bar(position = "fill") +
                    labs(x="Year",y="Participation Percentage") +
                    guides(fill=guide_legend(title="Participation Status")) +
                    theme(panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank())
        
        return(pcsPlot)
    })
    
    ########################  PCS Alternative Plot ######################## 
    
    change <- melt(Screen1, id.vars = "Type", value.name = "value", variable.name = "Year")
    
    output$pcs2Plot <- renderPlot({
        
        pcs2Plot = ggplot(data=change, aes(x=change$Year, y=change$value, group = change$Type, colour = change$Type)) +
                    geom_line() +
                    geom_point( size=4, shape=21, fill="white") + 
                    xlab("Year") +
                    ylab("Participation Percentage") +
                    guides(color = guide_legend("Type of Cancer Screen")) +
                    theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank(),
                        panel.background = element_blank())
        
        return(pcs2Plot)
    })
    
    ########################  participation waffle ########################
    
    

    
 
    
    
    

    
}

shinyApp(ui, server)


