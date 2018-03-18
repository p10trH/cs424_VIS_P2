# Peter Hanula
# Dimitar Kirilov
# Tarush Vig

# ---------------------------
load("data/allFlights24.RData")
top50Airports <- allFlights24 %>% group_by(ORIGIN_AIRPORT) %>% summarise(Flights = n()) %>% top_n(50) %>% arrange(desc(Flights))
# ---------------------------

# Define UI
ui <- fluidPage(
  
  # Styling ---------------------------
  
  theme = shinytheme("darkly"),
  tags$style(type='text/css', "body {background-color: #888888;}"),
  includeCSS("css/dygraph.css"),
  
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

    .tabbable > .nav[id=mapTabs1] > li > a               {color: #CFCFCF; border-radius: 4px; border: 0px; border-bottom: 4px solid #AAAAAA;}
    .tabbable > .nav[id=mapTabs1] > li[class=active] > a {background-color: #AAAAAA; color: white; border: 0px; border-bottom: 4px solid #484747; }
    .tabbable > .nav[id=mapTabs1] > li > a:hover         {background-color: #AAAAAA; border: 0px; border-bottom: 4px solid #484747;}
    
    .tabbable > .nav[id=mapTabs2] > li > a               {color: #CFCFCF; border-radius: 4px; border: 0px; border-bottom: 4px solid #AAAAAA;}
    .tabbable > .nav[id=mapTabs2] > li[class=active] > a {background-color: #AAAAAA; color: white; border: 0px; border-bottom: 4px solid #484747; }
    .tabbable > .nav[id=mapTabs2] > li > a:hover         {background-color: #AAAAAA; border: 0px; border-bottom: 4px solid #484747;}

    .tabbable > .nav[id=mapTabs3] > li > a               {color: #CFCFCF; border-radius: 4px; border: 0px; border-bottom: 4px solid #AAAAAA;}
    .tabbable > .nav[id=mapTabs3] > li[class=active] > a {background-color: #AAAAAA; color: white; border: 0px; border-bottom: 4px solid #484747; }
    .tabbable > .nav[id=mapTabs3] > li > a:hover         {background-color: #AAAAAA; border: 0px; border-bottom: 4px solid #484747;}



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
  tags$style(type='text/css', "#mapTabs1 {border-radius: 4px; border-bottom: 0px solid #AAAAAA; margin-bottom: 10px;}"),
  tags$style(type='text/css', "#mapTabs2 {border-radius: 4px; border-bottom: 0px solid #AAAAAA; margin-bottom: 10px;}"),
  tags$style(type='text/css', "#mapTabs3 {border-radius: 4px; border-bottom: 0px solid #AAAAAA; margin-bottom: 10px;}"),
  
  
  # Selectize input
  
  tags$style(type='text/css', ".selectize-input {background: #DDDDDD !important;} .selectize-dropdown {}"),
  
  # Slider input
  
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}
                                 .irs-grid-pol {height: 15px; width: 5px; top: 17px; background: #666666;}"),
  tags$style(type = "text/css", ".irs-grid-text {font-size: 15px; color: white; z-index: 0; bottom: -20px;}
                                 .irs-single {font-size: 18px; color: white; background: #484747; top: 63px; z-index:1; padding-left: 5px; padding-right: 5px;}
                                 .irs-slider {background: #666666; width: 30px; height: 45px; top: 15px; border-radius: 4px;}
                                 .irs-slider:hover {background: #666666; width: 30px; height: 45px; top: 15px; border-radius: 4px;}
                                 .irs-bar {width: 100%; height: 25px; background: #CFCFCF; border-top: 1px solid #CFCFCF; border-bottom: 1px solid #CFCFCF;}
                                 .irs-bar-edge {background: #CFCFCF; border: 1px solid #CFCFCF; height: 25px; border-radius: 4px; width: 20px;}
                                 .irs-line {background: #CFCFCF; border: 1px solid #CFCFCF; height: 25px; border-radius: 4px;}"),
  
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
                                            br(), 
                        
                                            fluidRow(
                                              tabBox(
                                                title = "original dygraph + A6",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp49")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp50"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(12,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  dygraphOutput("dygraphTotalFlights"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "a1",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp53")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp54"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("tmp55")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("tmp56"))
                                                         )))
                                            )
                                            
                                            
                                            ),
                                   
  # Heatmaps ---------------------------
                                   
                                   tabPanel("Heatmaps", 
                                            br(), 
                                            
                                        
                                            fluidRow(
                                              tabBox(
                                                title = "heatmaps",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp21")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp22"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("tmp23")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("tmp24"))
                                                         )))
                                            )
                                            
                                            
                                            )
                       )
              ),
              
  # Explore Further ---------------------------
              
              tabPanel("Explore Further",
                       br(),
                       fluidRow(column(3, offset = 9, align = 'right',
                                       checkboxInput("checkbox_scale", label = "Common Scale", value = TRUE))),
                       #fluidRow(column(6, offset = 3, align = 'justify',
                       #                sliderInput("slider_month", label = NULL, min = 1, max = 12, value = month(mdy(dateToShow)), width='100%'))),
                       fluidRow(column(8, offset = 2, align = 'justify',
                                       sliderTextInput(
                                         inputId = "slider_month", 
                                         label = NULL, width = '100%', grid = TRUE, force_edges = TRUE, hide_min_max = TRUE,
                                         choices = choices_month, selected = choices_month[month(mdy(dateToShow))]
                                       ))),
                       br(),
                       fluidRow(column(8, offset = 2, align = 'justify',
                                       uiOutput("dynamicSlider"))),
                       
                       fluidRow(column(4, offset = 2, align = 'justify', br(),
                                       selectInput("Airline", "Select Airline", 
                                                   choices = c("Aces Airlines", "Alaska Airlines Inc.", "American Airlines Inc.", 
                                                               'Atlantic Southeast Airlines', "Delta Air Lines Inc.", 
                                                               "Frontier Airlines Inc.", "JetBlue Airways", "SkyWest Airlines Inc.", 
                                                               "Southwest Airlines Co.", "Spirit Air Lines", "United Air Lines Inc.")
                                                   , selected = "SkyWest Airlines Inc.")
                       ),
                               column(4, align = 'justify', br(),
                                      selectInput("Airport", "Arrival/Destination Airport", 
                                                  choices = top50Airports$ORIGIN_AIRPORT))),
                       
                       tabsetPanel(id = "timelineNav",
                                   
  # Day ---------------------------
                                   
                                   tabPanel("Day", 
                                            br(),
                                            fluidRow(
                                              tabBox(
                                                title = "a2",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("Ohare1YearAirlineHourlyArrivalsTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("Ohare1YearAirlineHourlyDeparturesTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("Midway1YearAirlineHourlyArrivalsTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("Midway1YearAirlineHourlyDeparturesTable"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("Ohare1YearAirlineHourlyArrivals")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("Ohare1YearAirlineHourlyDepartures")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("Midway1YearAirlineHourlyArrivals")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("Midway1YearAirlineHourlyDepartures"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "a3",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("Ohare1DayHourlyArrDepTable")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("Midway1DayHourlyArrDepTable"))
                                                         ),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("Ohare1WeekdayHourlyDelaysTable")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("Midway1WeekdayHourlyDelaysTable"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("Ohare1DayHourlyArrDep")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("Midway1DayHourlyArrDep"))
                                                         ),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("Ohare1WeekdayHourlyDelays")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("Midway1WeekdayHourlyDelays"))
                                                         )
                                                         ))
                                              
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "a5 - it's kinda combined, should be done?",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp9a")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp10b"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("tmp11")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("tmp12"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "a7 + G1 + G2",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp9b")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp10a"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), #br(),
                                                         
                                                         
                                                         tabsetPanel(id = "mapTabs1",
                                                           tabPanel("Arrivals", 
                                                                    fluidRow(
                                                                      column(6,
                                                                             #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                             leafletOutput("leafDay1")), #, height = "80vh"
                                                                      column(6,
                                                                             #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                             leafletOutput("leafDay2"))
                                                                    )
                                                                    ),
                                                           tabPanel("Departures", 
                                                                    fluidRow(
                                                                      column(6,
                                                                             #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                             leafletOutput("leafDay3")),
                                                                      column(6,
                                                                             #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                             leafletOutput("leafDay4"))
                                                                    )
                                                                    )
                                                         )
                                                         
                                                         
                                                         
                                                         # fluidRow(column(6, h4("Departures")), column(6, h4("Arrivals"))),
                                                         # fluidRow(
                                                         #   column(3,
                                                         #          #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                         #          leafletOutput("leafDay1")), #, height = "80vh"
                                                         #   column(3,
                                                         #          #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                         #          leafletOutput("leafDay2")), 
                                                         #   column(3,
                                                         #          #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                         #          leafletOutput("leafDay3")),
                                                         #   column(3,
                                                         #          #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                         #          leafletOutput("leafDay4"))
                                                         # )
                                                         ))
                                            )
                                            
                                            
                                            
                                            
                                            ),
                                   
  # Month ---------------------------
                                   
                                   tabPanel("Month",
                                            br(),
                                            fluidRow(
                                              tabBox(
                                                title = "c1",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("OhareAirlineArrDepTable")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("MidwayAirlineArrDepTable"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotlyOutput("OhareAirlineArrDep")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("MidwayAirlineArrDep"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "c2 + c4",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("OhareHourlyArrDepTable")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("MidwayHourlyArrDepTable"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("OhareHourlyArrDep")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("MidwayHourlyArrDep"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "c3",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("OhareWeeklyArrDepTable")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("MidwayWeeklyArrDepTable"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("OhareWeeklyArrDep")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("MidwayWeeklyArrDep"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "c4",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("OhareHourlyDelaysTable")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("MidwayHourlyDelaysTable"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("OhareHourlyDelays")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("MidwayHourlyDelays"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "c5",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("OhareMostCommonArrivalAirportsTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("OhareMostCommonDestinationAirportsTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("MidwayMostCommonArrivalAirportsTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("MidwayMostCommonDestinationAirportsTable"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("OhareMostCommonArrivalAirports")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("OhareMostCommonDestinationAirports")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("MidwayMostCommonArrivalAirports")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("MidwayMostCommonDestinationAirports"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "a7 + G1 + G2",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp21a")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp22s"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"),# br(),
                                                         tabsetPanel(id = "mapTabs2",
                                                                     tabPanel("Arrivals", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                                       leafletOutput("leafMonth1")), #, height = "80vh"
                                                                                column(6,
                                                                                       #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                                       leafletOutput("leafMonth2"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Departures", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                                       leafletOutput("leafMonth3")),
                                                                                column(6,
                                                                                       #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                                       leafletOutput("leafMonth4"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                         
                                                         ))
                                            )
                                            
                                            
                                            
                                            
                                            ),
  # Year ---------------------------
  
                                   tabPanel("Year", 
                                            br(), 

                                            fluidRow(
                                              tabBox(
                                                title = "b1 (a2 is the same info)",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("Ohare1YearAirlinesArrivalsTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("Ohare1YearAirlinesDeparturesTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("Midway1YearAirlinesArrivalsTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("Midway1YearAirlinesDeparturesTable"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("Ohare1YearAirlinesArrivals")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("Ohare1YearAirlinesDepartures")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("Midway1YearAirlinesArrivals")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("Midway1YearAirlinesDepartures"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "b2",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("Ohare1YearHourlyArrivalsTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("Ohare1YearHourlyDeparturesTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("Midway1YearHourlyArrivalsTable")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("Midway1YearHourlyDeparturesTable"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("Ohare1YearHourlyArrivals")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("Ohare1YearHourlyDepartures")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("Midway1YearHourlyArrivals")),
                                                           column(3,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("Midway1YearHourlyDepartures"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "a4",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp33")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp34"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("tmp35")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("tmp36"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "b3",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("Ohare1YearlMostCommonTable")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("Midway1YearlMostCommonTable"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  plotOutput("Ohare1YearlMostCommon")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  plotOutput("Midway1YearlMostCommon"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "b4 (a5 is the same info)",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("Ohare1YearDelaysTable")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("Midway1YearDelaysTable"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  streamgraphOutput("Ohare1YearDelays")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  streamgraphOutput("Midway1YearDelays"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "a7 + G1 + G2",
                                                side = "left",
                                                width = 12,
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp45")),
                                                           column(6,
                                                                  #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                  DTOutput("tmp46"))
                                                         )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), #br(),
                                                         tabsetPanel(id = "mapTabs3",
                                                                     tabPanel("Arrivals", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                                       leafletOutput("leafYear1")), #, height = "80vh"
                                                                                column(6,
                                                                                       #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                                       leafletOutput("leafYear2"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Departures", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                                       leafletOutput("leafYear3")),
                                                                                column(6,
                                                                                       #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                                                       leafletOutput("leafYear4"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                         ))
                                            )
                                            
                                            
                                            
                                            
                                            )
                      )
              )
  ),
  
  
  
  
  
  #  ---------------------------
  
  bsModal(id = "modal_Settings", "Settings", trigger = "action_Settings", size = "small",
          h3("Time"), 
          fluidRow(column(6, offset = 1, checkboxInput("HourFormat", label = "24h format", value = FALSE)))),
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

# fluidRow(column(4,  selectInput("select_Month", label = NULL, 
#                                 choices = list("01 - January" = 1, "02 - February" = 2, "03 - March" = 3, "04 - April" = 4, "05 - May" = 5, "06 - June" = 6, "07 - July" = 7, "08 - August" = 8, "09 - September" = 9, "10 - October" = 10, "11 - November" = 11, "12 - December" = 12), 
#                                 selected = 1))),

# ----
# ----
# ----


