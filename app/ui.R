# Peter Hanula
# Dimitar Kirilov
# Tarush Vig

library(shiny)
library(shinyBS)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)

library(plyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(DT)
library(leaflet)

library(tidyr)
library(streamgraph)
library(plotly)


# ---------------------------

# month, day, year ("%m-%d-%Y")
dateToShow <- "6-9-2017" # don't change

# 2017 only
choices_month <- format(seq.Date(from = as.Date('1-1-2017', "%m-%d-%Y"), to = as.Date('12-31-2017', "%m-%d-%Y"), by="month"), "(%m)  %b")
# depends on what date we want to show
choices_day <- format(seq.Date(from = as.Date(paste(month(mdy(dateToShow)), '1-2017', sep = "-"), "%m-%d-%Y"), length.out = as.numeric(days_in_month(as.Date(dateToShow, "%m-%d-%Y"))), by="day"), "(%d)  %a")

# ---------------------------

# Define UI
ui <- fluidPage(
  
  # Styling ---------------------------
  
  theme = shinytheme("darkly"),
  tags$style(type='text/css', "body {background-color: #888888;}"),
  
  # tabsetPanel / tabBox / tabPanel
  
  tags$style(HTML("

    .tabbable > .nav[id=mainNav] > li                     {font-size: 20px;}
    .tabbable > .nav[id=mainNav] > li > a                 {color: #CFCFCF; border-radius: 4px; border: 0px;}
    .tabbable > .nav[id=mainNav] > li[class=active] > a   {background-color: #484747; color: white; border: 0px;}
    .tabbable > .nav[id=mainNav] > li > a:hover           {background-color: #484747;}

    .tabbable > .nav[id=timelineNav] > li > a               {color: #CFCFCF; border-radius: 4px; border: 4px solid #888888;}
    .tabbable > .nav[id=timelineNav] > li[class=active] > a {background-color: #888888; color: white; border: 4px solid #484747; }
    .tabbable > .nav[id=timelineNav] > li > a:hover         {background-color: #888888; border: 4px solid #484747;}

    .tabbable > .nav[id=overviewNav] > li > a               {color: #CFCFCF; border-radius: 4px; border: 4px solid #888888;}
    .tabbable > .nav[id=overviewNav] > li[class=active] > a {background-color: #888888; color: white; border: 4px solid #484747; }
    .tabbable > .nav[id=overviewNav] > li > a:hover         {background-color: #888888; border: 4px solid #484747;}

    .tabbable > .nav > li > a                             {color: #CFCFCF; border-radius: 4px;}
    .tabbable > .nav > li[class=active] > a               {background-color: #484747; color: white;}
    .tabbable > .nav > li > a:hover                       {background-color: #484747;}
                  
    .nav-tabs-custom > .nav > li > a                      {color: #666666; border-radius: 4px; margin: 0px; padding: 10px;}
    .nav-tabs-custom > .nav > li[class=active] > a        {background-color: #DDDDDD; color: #484747; border: 0px; margin: 1px;}
    .nav-tabs-custom > .nav > li > a:hover                {background-color: #DDDDDD; border: 0px solid #DDDDDD; margin: 1px;}

    .nav-tabs-custom > .nav > li                          {float: right;}
    .pull-right                                           {float: left !important; font-size: 24px;}
    .nav-tabs-custom > .nav                               {border-bottom: 0px;}")),
  
  tags$style(type='text/css', ".nav-tabs-custom {padding: 20px; margin-bottom: 20px; background-color: #AAAAAA; color: #484747; border-radius: 4px;}"),
  tags$style(type='text/css', "#mainNav {background-color: #666666; border-radius: 4px; border-bottom: 1px solid #666666;}"), # margin: -85px 120px 0px 350px !important;
  tags$style(type='text/css', "#timelineNav {border-radius: 4px; border-bottom: 0px solid #188888;}"), #  float: left; margin-top: -48px;
  tags$style(type='text/css', "#overviewNav {border-radius: 4px; border-bottom: 0px solid #188888;}"), #  float: left; margin-top: -48px;
  
  # Selectize input
  
  tags$style(type='text/css', ".selectize-input {background: #DDDDDD !important;} .selectize-dropdown {}"),
  
  # Slider input
  
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
  
  # Modal windows
  
  tags$style(type='text/css', "#modal_Settings .modal-footer, #modal_About .modal-footer {display:none;}"),
  
  # Top title and buttons
  
  tags$style(type='text/css', "#action_Settings, #action_About {padding: 10px 10px 10px 10px; margin-top: 23px;}"),
  tags$style(type='text/css', "h1 {font-size: 50px; font-weight: bold; color: #484747;}"),
  
  # App title & buttons  ---------------------------
  
  fluidRow(column(4, h1("Illinois Flights '17")), 
           column(3, offset = 5, align = 'right',
                  actionButton("action_Settings", label = "", icon = icon("cog", "fa-2x")),
                  actionButton("action_About", label = "", icon = icon("info", "fa-2x")))),
  br(),
  
  # ---------------------------
  
  tabsetPanel(id = "mainNav",
              
  # Overview ---------------------------
              
              tabPanel("Overview", br(),
                       tabsetPanel(id = "overviewNav",
                                   
  # Dygraphs ---------------------------
                                   
                                   tabPanel("Dygraphs", 
                                            br(), h4("aaaaaaaa")),
                                   
  # Heatmaps ---------------------------
                                   
                                   tabPanel("Heatmaps", 
                                            br(), h4("aaaaaaaa"))
                       )
              ),
              
  # Explore Further ---------------------------
              
              tabPanel("Explore Further",
                       br(),
                       fluidRow(column(3, offset = 9, align = 'right',
                                       checkboxInput("checkbox_scale", label = "Common Scale", value = TRUE))),
                       #fluidRow(column(6, offset = 3, align = 'justify',
                       #                sliderInput("slider_month", label = NULL, min = 1, max = 12, value = month(mdy(dateToShow)), width='100%'))),
                       fluidRow(column(6, offset = 3, align = 'justify',
                                       sliderTextInput(
                                         inputId = "slider_month", 
                                         label = NULL, width = '100%', grid = TRUE, force_edges = TRUE,
                                         choices = choices_month, selected = choices_month[month(mdy(dateToShow))]
                                       ))),
                       fluidRow(column(6, offset = 3, align = 'justify',
                                       uiOutput("dynamicSlider"))),
                       
                       tabsetPanel(id = "timelineNav",
                                   
  # Day ---------------------------
                                   
                                   tabPanel("Day", 
                                            br(), h4("aaaaaaaa")),
                                   
  # Month ---------------------------
                                   
                                   tabPanel("Month",
                                            br(),
                                            fluidRow(column(4,  selectInput("select_Month", label = NULL, 
                                                                            choices = list("01 - January" = 1, "02 - February" = 2, "03 - March" = 3, "04 - April" = 4, "05 - May" = 5, "06 - June" = 6, "07 - July" = 7, "08 - August" = 8, "09 - September" = 9, "10 - October" = 10, "11 - November" = 11, "12 - December" = 12), 
                                                                            selected = 1))),
                                            fluidRow(
                                              tabBox(
                                                title = "Flights for Each Hour of the Day",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("hourlyArrDepTable")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("hourlyArrDep2Table"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("hourlyArrDep")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("hourlyArrDep2"))
                                                         )))
                                            )
                                            
                                            
                                            
                                            
                                            ),
  # Year ---------------------------
  
                                   tabPanel("Year", 
                                            br(), h4("aaaaaaaa"))
                      )
              )
  ),
  
  
  
  
  
  #  ---------------------------
  
  bsModal(id = "modal_Settings", "Settings", trigger = "action_Settings", size = "small",
          h3("Time"), 
          fluidRow(column(6, offset = 1, checkboxInput("checkbox_24h", label = "24h format", value = FALSE)))),
  bsModal(id = "modal_About", "About", trigger = "action_About", size = "large",
          #h4("By Peter Hanula, Dimitar Kirilov, Tarush Vig"),
          h3("Project 2 - CS 424", align = "center"),
          h3("Learning to Fly", align = "center"),
          h4("By: Tarush Vig (tvig2), Peter Hanula (phanul2), and Dimitar Kirilov (dkiril4).", align = "center"),
          h5("DATA INFORMATION", align = "center"),
          h6("The data for this project comes from The Bureau of Transportation Statistics of The United States Department of Transportation."),
          h6("Direct link: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time"),
          h5("APPLICATION INFORMATION", align = "center"),
          h6("The purpose of this application is to extrapolate upon the data given regarding the flight statistics for the state of Illinois and create dynamic/interactive visualizations to obtain a clearer picture of flight activity, broken down into multiple components (i. e. types of airlines, arrival/departure airport frequencies, hourly/weekly/monthly frequencies, etc.). This application can be run on a multi-display wall."),
          h5("LIBRARIES USED", align = "center"),
          h6("- shiny", align = "left"),
          h6("- shinydashboard", align = "left"),
          h6("- plyr", align = "left"),
          h6("- ggplot2", align = "left"),
          h6("- lubridate", align = "left"),
          h6("- dplyr", align = "left"),
          h6("- reshape2", align = "left"),
          h6("- rsconnect", align = "left"),
          h6("- tidyr", align = "left"),
          h6("- magrittr", align = "left"),
          h6("- DT", align = "left"),
          h6("- streamgraph", align = "left"),
          h6("- plotly", align = "left"))

)




# extra --------

#fluidRow(column(6, offset = 3, align = 'justify',
#               sliderInput(
#                 inputId = "slider_day",
#                 label = NULL, width='100%',
#                 min = as.Date(paste(month(mdy(dateToShow)), '1', year(mdy(dateToShow)), sep = "-"), "%m-%d-%Y"),
#                 max = ceiling_date(as.Date(dateToShow, "%m-%d-%Y"), "month") - days(1),
#                 value = as.Date(dateToShow, "%m-%d-%Y"),
#                 timeFormat = "(%d)  %a"
#                 
#                 
#                 ))),

# ----

# check out of bounds
#if (is.na(day(mdy(dateToShow)))) {
#  dateToShow <- paste(substr(input$slider_month, 2, 3), as.character(length(choices_day)), "2017", sep = "-")
#  
#}

#if (as.numeric(currentDay) > length(choices_day)) {
#  dateToShow <- gsub(currentDay, as.character(length(choices_day)), dateToShow)
#  #dateToShow <- paste(substr(input$slider_month, 2, 3), as.character(length(choices_day)), "2017", sep = "-")
#}

# last day of month
# format(ceiling_date(as.Date('2-11-2017', "%m-%d-%Y"), "month") - days(1), "%m-%d-%Y")

#tempDate <- paste(substr(input$slider_month, 2, 3), substr(input$slider_day, 2, 3), "2017", sep = "-")


# ----

# output$value <- renderPrint({ getDate()})

#fluidRow(
#  column(4, verbatimTextOutput("value"))),

# ----
# ----
# ----
# ----


