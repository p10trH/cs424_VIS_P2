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

    .tabbable > .nav > li                     {font-size: 24px; font-weight: bold;}

    .tabbable > .nav[id=mainNav] > li                     {font-size: 24px;}
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

    .tabbable > .nav[id=a3Tabs] > li > a               {color: #CFCFCF; border-radius: 4px; border: 0px; border-bottom: 4px solid #AAAAAA;}
    .tabbable > .nav[id=a3Tabs] > li[class=active] > a {background-color: #AAAAAA; color: white; border: 0px; border-bottom: 4px solid #484747; }
    .tabbable > .nav[id=a3Tabs] > li > a:hover         {background-color: #AAAAAA; border: 0px; border-bottom: 4px solid #484747;}
        
    .tabbable > .nav[id=a2Tabs] > li > a               {color: #CFCFCF; border-radius: 4px; border: 0px; border-bottom: 4px solid #AAAAAA;}
    .tabbable > .nav[id=a2Tabs] > li[class=active] > a {background-color: #AAAAAA; color: white; border: 0px; border-bottom: 4px solid #484747; }
    .tabbable > .nav[id=a2Tabs] > li > a:hover         {background-color: #AAAAAA; border: 0px; border-bottom: 4px solid #484747;}
       
    .tabbable > .nav[id=b1Tabs] > li > a               {color: #CFCFCF; border-radius: 4px; border: 0px; border-bottom: 4px solid #AAAAAA;}
    .tabbable > .nav[id=b1Tabs] > li[class=active] > a {background-color: #AAAAAA; color: white; border: 0px; border-bottom: 4px solid #484747; }
    .tabbable > .nav[id=b1Tabs] > li > a:hover         {background-color: #AAAAAA; border: 0px; border-bottom: 4px solid #484747;}
         
    .tabbable > .nav[id=b2Tabs] > li > a               {color: #CFCFCF; border-radius: 4px; border: 0px; border-bottom: 4px solid #AAAAAA;}
    .tabbable > .nav[id=b2Tabs] > li[class=active] > a {background-color: #AAAAAA; color: white; border: 0px; border-bottom: 4px solid #484747; }
    .tabbable > .nav[id=b2Tabs] > li > a:hover         {background-color: #AAAAAA; border: 0px; border-bottom: 4px solid #484747;}
              
    .tabbable > .nav[id=c2c4tabs] > li > a               {color: #CFCFCF; border-radius: 4px; border: 0px; border-bottom: 4px solid #AAAAAA;}
    .tabbable > .nav[id=c2c4tabs] > li[class=active] > a {background-color: #AAAAAA; color: white; border: 0px; border-bottom: 4px solid #484747; }
    .tabbable > .nav[id=c2c4tabs] > li > a:hover         {background-color: #AAAAAA; border: 0px; border-bottom: 4px solid #484747;}
         
    .tabbable > .nav[id=c5tabs1] > li > a               {color: #CFCFCF; border-radius: 4px; border: 0px; border-bottom: 4px solid #AAAAAA;}
    .tabbable > .nav[id=c5tabs1] > li[class=active] > a {background-color: #AAAAAA; color: white; border: 0px; border-bottom: 4px solid #484747; }
    .tabbable > .nav[id=c5tabs1] > li > a:hover         {background-color: #AAAAAA; border: 0px; border-bottom: 4px solid #484747;}
          
    .tabbable > .nav[id=c5tabs2] > li > a               {color: #CFCFCF; border-radius: 4px; border: 0px; border-bottom: 4px solid #AAAAAA;}
    .tabbable > .nav[id=c5tabs2] > li[class=active] > a {background-color: #AAAAAA; color: white; border: 0px; border-bottom: 4px solid #484747; }
    .tabbable > .nav[id=c5tabs2] > li > a:hover         {background-color: #AAAAAA; border: 0px; border-bottom: 4px solid #484747;}

    .tabbable > .nav > li > a                             {color: #CFCFCF; border-radius: 4px;}
    .tabbable > .nav > li[class=active] > a               {background-color: #484747; color: white;}
    .tabbable > .nav > li > a:hover                       {background-color: #484747;}

  

    .nav-tabs-custom > .nav > li                     {font-size: 26px;}
                  
    .nav-tabs-custom > .nav > li > a                      {color: #666666; border-radius: 4px; margin: 0px; padding: 10px;}
    .nav-tabs-custom > .nav > li[class=active] > a        {background-color: #DDDDDD; color: #484747; border: 0px; margin: 1px;}
    .nav-tabs-custom > .nav > li > a:hover                {background-color: #DDDDDD; border: 0px solid #DDDDDD; margin: 1px;}

    .nav-tabs-custom > .nav > li                          {float: right;}
    .pull-right                                           {float: left !important; font-size: 26px;}
    .nav-tabs-custom > .nav                               {border-bottom: 0px;}")),
  
  tags$style(type='text/css', ".nav-tabs-custom {padding: 20px; margin-bottom: 20px; background-color: #AAAAAA; color: #484747; border-radius: 4px;}"),
  tags$style(type='text/css', "#mainNav {background-color: #666666; border-radius: 4px; border-bottom: 1px solid #666666;}"), # margin: -85px 120px 0px 350px !important;
  tags$style(type='text/css', "#timelineNav {border-radius: 4px; border-bottom: 0px solid #188888;}"), #  float: left; margin-top: -48px;
  tags$style(type='text/css', "#overviewNav {border-radius: 4px; border-bottom: 0px solid #188888;}"), #  float: left; margin-top: -48px;
  tags$style(type='text/css', "#mapTabs1, #mapTabs2, #mapTabs3, #a3Tabs, #a2Tabs, #b2Tabs, #b1Tabs, #c2c4tabs, #c5tabs1, #c5tabs2
                                    {border-radius: 4px; border-bottom: 0px solid #AAAAAA; margin-bottom: 10px;}"),
  
  
  # Selectize input
  
  tags$style(type='text/css', ".selectize-input {background: #DDDDDD !important; font-size:24px; width:100%;} .selectize-dropdown {font-size:24px;width:100%;}"),
  
  # Slider input
  
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}
                                 .irs-grid-pol {height: 15px; width: 5px; top: 17px; background: #666666;}"),
  tags$style(type = "text/css", ".irs-grid-text {font-size: 18px; color: white; z-index: 0; bottom: -20px;}
                                 .irs-single {font-size: 24px; color: white; background: #484747; top: 63px; z-index:1; padding-left: 5px; padding-right: 5px;}
                                 .irs-slider {background: #666666; width: 30px; height: 45px; top: 15px; border-radius: 4px;}
                                 .irs-slider:hover {background: #666666; width: 30px; height: 45px; top: 15px; border-radius: 4px;}
                                 .irs-bar {width: 100%; height: 25px; background: #484747; border-top: 1px solid #484747; border-bottom: 1px solid #484747;}
                                 .irs-bar-edge {background: #484747; border: 1px solid #484747; height: 25px; border-radius: 4px; width: 20px;}
                                 .irs-line {background: #CFCFCF; border: 1px solid #CFCFCF; height: 25px; border-radius: 4px;}"),
  
  tags$style(type = "text/css", ".irs-min, .irs-max {display: none;}
                                 .irs-to, .irs-from {font-size: 24px; color: white; background: #484747; top: -15px;}"),
  
  
  
  
  
  # Modal windows
  
  tags$style(type='text/css', "#modal_Settings .modal-footer, #modal_About .modal-footer {display:none;}"),
  tags$style(type='text/css', "#modal_Settings, #modal_About {font-size:25px;}"),
  
  # Top title and buttons
  
  tags$style(type='text/css', "#action_Settings, #action_About {padding: 10px 10px 10px 10px; margin-top: 23px;}"),
  tags$style(type='text/css', "h1 {font-size: 60px; font-weight: bold; color: #484747;}"),
  tags$style(type='text/css', "h2 {font-size: 35px; font-weight: bold;  }"),
  
  
  # App title & buttons  ---------------------------
  
  fluidRow(column(4, h1("Illinois Flights '17")), 
           column(3, offset = 5, align = 'right',
                  actionButton("action_Settings", label = "", icon = icon("cog", "fa-3x")),
                  actionButton("action_About", label = "", icon = icon("info", "fa-3x")))),
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
                                                title = "Total Flights",#"original dygraph + A6",
                                                side = "left",
                                                width = 12,
                                                
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(12,
                                                                  tags$style(type = "text/css", "#dygraphTotalFlights {min-height:35vh !important;}"),
                                                                  dygraphOutput("dygraphTotalFlights"))
                                                         )),
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(12,
                                                                  #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                                  DTOutput("InterestingThingsTable"))
                                                         )))
                                            ),
                                            fluidRow(
                                              tabBox(
                                                title = "Total Flights Between a Specific Airport", # "a1",
                                                side = "left",
                                                width = 12,
                                                # tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                #          fluidRow(
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                #                   DTOutput("tmp53")),
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                #                   DTOutput("tmp54"))
                                                #          )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"),
                                                         fluidRow(
                                                           column(6, align = 'left',
                                                                  selectInput("Airport",label=NULL, width = "100%",
                                                                              choices = top50Airports$ORIGIN_AIRPORT, selected = "LaGuardia"))),
                                                         fluidRow(
                                                           column(12,
                                                                  tags$style(type = "text/css", "#dygraphAirport {min-height:35vh !important;}"),
                                                                  dygraphOutput("dygraphAirport"))
                                                         )))
                                            )
                                            
                                            
                                            ),
                                   
  # Heatmaps ---------------------------
                                   
                                   tabPanel("Heatmaps", 
                                            br(), 
                                            
                                        
                                            fluidRow(
                                              tabBox(
                                                title = "Delays",
                                                side = "left",
                                                width = 12,
                                                # tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                #          fluidRow(
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                #                   DTOutput("tmp21")),
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                #                   DTOutput("tmp22"))
                                                #          )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(column(6,selectInput("Delays", label = NULL,width = "100%", c("All", "NAS", "Weather", "Security", "Late Aircraft", "Carrier")))),
                                                         fluidRow(
                                                           column(6,
                                                                  tags$style(type = "text/css", "#OhareDelays1YearAll {min-height:35vh !important;}"),
                                                                  plotlyOutput("OhareDelays1YearAll")),
                                                           column(6,
                                                                  tags$style(type = "text/css", "#MidwayDelays1YearAll {min-height:35vh !important;}"),
                                                                  plotlyOutput("MidwayDelays1YearAll"))
                                                           
                                                         ),
                                                         fluidRow(
                                                           column(6,
                                                                  tags$style(type = "text/css", "#OhareDelays1YearAllWeekday {min-height:35vh !important;}"),
                                                                  plotlyOutput("OhareDelays1YearAllWeekday")),
                                                           column(6,
                                                                  tags$style(type = "text/css", "#MidwayDelays1YearAllWeekday {min-height:35vh !important;}"),
                                                                  plotlyOutput("MidwayDelays1YearAllWeekday"))
                                                         )))
                                            )
                                            
                                            
                                            )
                       )
              ),
              
  # Explore Further ---------------------------
              
              tabPanel("Explore Further",
                       br(),
                       fluidRow(),
                       #fluidRow(column(6, offset = 3, align = 'justify',
                       #                sliderInput("slider_month", label = NULL, min = 1, max = 12, value = month(mdy(dateToShow)), width='100%'))),
                       fluidRow(column(8, offset = 2, align = 'justify',
                                       sliderTextInput(
                                         inputId = "slider_month", 
                                         label = NULL, width = '100%', grid = TRUE, force_edges = TRUE, hide_min_max = TRUE,
                                         choices = choices_month, selected = choices_month[month(mdy(dateToShow))]
                                       )),
                                column(2, offset = 0, align = 'right',
                                       checkboxInput("checkbox_scale", label = "Common Scale", value = TRUE))),
                       br(),
                       fluidRow(column(8, offset = 2, align = 'justify',
                                       uiOutput("dynamicSlider"))), br(),
                       
                       
                       
                       tabsetPanel(id = "timelineNav",
                                   
  # Day ---------------------------
                                   
                                   tabPanel("Day", 
                                            br(),
   
                                            fluidRow(
                                              tabBox(
                                                title = "24 Hour Breakdown",
                                                side = "left",
                                                width = 12,
                                                
                                                tabPanel("Arrivals & Departures",# icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  tags$style(type = "text/css", "#Ohare1DayHourlyArrDep {min-height:30vh !important;}"),
                                                                  plotlyOutput("Ohare1DayHourlyArrDep")),
                                                           column(6,
                                                                  tags$style(type = "text/css", "#Midway1DayHourlyArrDep {min-height:30vh !important;}"),
                                                                  plotlyOutput("Midway1DayHourlyArrDep"))
                                                         )),
                                                tabPanel("Delays", #icon = icon("line-chart", "fa-2x"), br(),
                                                         
                                                         fluidRow(
                                                           column(6,
                                                                  tags$style(type = "text/css", "#Ohare1DayHourlyDelays {min-height:30vh !important;}"),
                                                                  plotlyOutput("Ohare1DayHourlyDelays")),
                                                           column(6,
                                                                  tags$style(type = "text/css", "#Midway1DayHourlyDelays {min-height:30vh !important;}"),
                                                                  plotlyOutput("Midway1DayHourlyDelays"))
                                                         )
                                                         
                                                         
                                                         
                                                         
                                                         
                                                         # fluidRow(
                                                         #   column(6,
                                                         #          #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                         #          plotlyOutput("Ohare1DayHourlyArrDep")),
                                                         #   column(6,
                                                         #          #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                         #          plotlyOutput("Midway1DayHourlyArrDep"))
                                                         # ),
                                                         # fluidRow(
                                                         #   column(6,
                                                         #          #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                         #          plotlyOutput("Ohare1DayHourlyDelays")),
                                                         #   column(6,
                                                         #          #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                         #          plotlyOutput("Midway1DayHourlyDelays"))
                                                         # )
                                                         ),
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         
                                                         tabsetPanel(id = "a3Tabs",
                                                                     tabPanel("Arrivals & Departures", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Ohare1DayHourlyArrDepTable {min-height:25vh !important;}"),
                                                                                       DTOutput("Ohare1DayHourlyArrDepTable")), #, height = "80vh"
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Midway1DayHourlyArrDepTable {min-height:25vh !important;}"),
                                                                                       DTOutput("Midway1DayHourlyArrDepTable"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Delays", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Ohare1WeekdayHourlyDelaysTable {min-height:25vh !important;}"),
                                                                                       DTOutput("Ohare1WeekdayHourlyDelaysTable")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Midway1WeekdayHourlyDelaysTable {min-height:25vh !important;}"),
                                                                                       DTOutput("Midway1WeekdayHourlyDelaysTable"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                )
                                                )
                                              
                                            ),
                                            # fluidRow(
                                            #   tabBox(
                                            #     title = "a5 - it's kinda combined, should be done?",
                                            #     side = "left",
                                            #     width = 12,
                                            #     tabPanel("", icon = icon("table", "fa-2x"), br(),
                                            #              fluidRow(
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                            #                       DTOutput("tmp9a")),
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                            #                       DTOutput("tmp10b"))
                                            #              )),
                                            #     tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                            #              fluidRow(
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                            #                       plotOutput("tmp11")),
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                            #                       plotOutput("tmp12"))
                                            #              )))
                                            # ),
                                            fluidRow(
                                              # tabBox(
                                              #   title = "a2",
                                              #   side = "left",
                                              #   width = 6,
                                              #   # tabPanel("", icon = icon("table", "fa-2x"), br(),
                                              #   #          fluidRow(
                                              #   #            column(3,
                                              #   #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                              #   #                   DTOutput("Ohare1YearAirlineHourlyArrivalsTable")),
                                              #   #            column(3,
                                              #   #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                              #   #                   DTOutput("Ohare1YearAirlineHourlyDeparturesTable")),
                                              #   #            column(3,
                                              #   #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                              #   #                   DTOutput("Midway1YearAirlineHourlyArrivalsTable")),
                                              #   #            column(3,
                                              #   #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                              #   #                   DTOutput("Midway1YearAirlineHourlyDeparturesTable"))
                                              #   #          )),
                                              #   tabPanel("", icon = icon("line-chart", "fa-2x"),
                                              # 
                                              #            fluidRow(column(8, offset = 0, align = 'left',
                                              #                            selectInput("Airline", label = NULL,
                                              #                                        choices = c("Aces Airlines", "Alaska Airlines Inc.", "American Airlines Inc.",
                                              #                                                    'Atlantic Southeast Airlines', "Delta Air Lines Inc.",
                                              #                                                    "Frontier Airlines Inc.", "JetBlue Airways", "SkyWest Airlines Inc.",
                                              #                                                    "Southwest Airlines Co.", "Spirit Air Lines", "United Air Lines Inc.")
                                              #                                        , selected = "SkyWest Airlines Inc.")
                                              #            )),
                                              # 
                                              #            tabsetPanel(id = "a2Tabs",
                                              #                        tabPanel("Arrivals",
                                              #                                 fluidRow(
                                              #                                   column(6,
                                              #                                          #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                              #                                          plotOutput("Ohare1YearAirlineHourlyArrivals")), #, height = "80vh"
                                              #                                   column(6,
                                              #                                          #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                              #                                          plotOutput("Midway1YearAirlineHourlyArrivals"))
                                              #                                 )
                                              #                        ),
                                              #                        tabPanel("Departures",
                                              #                                 fluidRow(
                                              #                                   column(6,
                                              #                                          #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                              #                                          plotOutput("Ohare1YearAirlineHourlyDepartures")),
                                              #                                   column(6,
                                              #                                          #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                              #                                          plotOutput("Midway1YearAirlineHourlyDepartures"))
                                              #                                 )
                                              #                        )
                                              #            )
                                              # 
                                              # 
                                              # 
                                              #            # fluidRow(
                                              #            #   column(3,
                                              #            #          #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                              #            #          plotOutput("Ohare1YearAirlineHourlyArrivals")),
                                              #            #   column(3,
                                              #            #          #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                              #            #          plotOutput("Ohare1YearAirlineHourlyDepartures")),
                                              #            #   column(3,
                                              #            #          #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                              #            #          plotOutput("Midway1YearAirlineHourlyArrivals")),
                                              #            #   column(3,
                                              #            #          #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                              #            #          plotOutput("Midway1YearAirlineHourlyDepartures"))
                                              #            # )
                                              #   )
                                              #   ),
                                              column(6, offset=0),tabBox(
                                                title = "Total flights Across the U.S.", #a7g1g2
                                                side = "left",
                                                width = 6,
                                                # tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                #          fluidRow(
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                #                   DTOutput("tmp9b")),
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                #                   DTOutput("tmp10a"))
                                                #          )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), #br(),
                                                         
                                                         
                                                         tabsetPanel(id = "mapTabs1",
                                                           tabPanel("Arrivals", 
                                                                    fluidRow(
                                                                      column(6,
                                                                             tags$style(type = "text/css", "#leafDay1 {min-height:30vh !important;}"),
                                                                             leafletOutput("leafDay1")), #, height = "80vh"
                                                                      column(6,
                                                                             tags$style(type = "text/css", "#leafDay2 {min-height:30vh !important;}"),
                                                                             leafletOutput("leafDay2"))
                                                                    )
                                                                    ),
                                                           tabPanel("Departures", 
                                                                    fluidRow(
                                                                      column(6,
                                                                             tags$style(type = "text/css", "#leafDay3 {min-height:30vh !important;}"),
                                                                             leafletOutput("leafDay3")),
                                                                      column(6,
                                                                             tags$style(type = "text/css", "#leafDay4 {min-height:30vh !important;}"),
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
                                                title =  "Arrivals & Departures",#"c2 + c4",
                                                side = "left",
                                                width = 7,
                                                
                                                tabPanel("Hour", br(), #icon = icon("line-chart", "fa-2x"), br(),
                                                         
                                                         fluidRow(
                                                           column(6,
                                                                  tags$style(type = "text/css", "#OhareHourlyArrDep {min-height:30vh !important;}"),
                                                                  plotlyOutput("OhareHourlyArrDep")), #, height = "80vh"
                                                           column(6,
                                                                  tags$style(type = "text/css", "#MidwayHourlyArrDep {min-height:30vh !important;}"),
                                                                  plotlyOutput("MidwayHourlyArrDep"))
                                                         )
                                                        
                                                    ),
                                                tabPanel("Hour Delays", br(), #icon = icon("line-chart", "fa-2x"), br(),
                                                         
                                                         fluidRow(
                                                           column(6,
                                                                  tags$style(type = "text/css", "#OhareHourlyDelays {min-height:30vh !important;}"),
                                                                  plotlyOutput("OhareHourlyDelays")), #, height = "80vh"
                                                           column(6,
                                                                  tags$style(type = "text/css", "#MidwayHourlyDelays {min-height:30vh !important;}"),
                                                                  plotlyOutput("MidwayHourlyDelays"))
                                                         )
                                                         
                                                        
                                                         
                                                         
                                                         
                                                         
                                                ),
                                                tabPanel("Week", br(), #icon = icon("line-chart", "fa-2x"), br(),
                                                         
                                                         
                                                         
                                                         
                                                         fluidRow(
                                                           column(6,
                                                                  tags$style(type = "text/css", "#OhareWeeklyArrDep {min-height:30vh !important;}"),
                                                                  plotlyOutput("OhareWeeklyArrDep")), #, height = "80vh"
                                                           column(6,
                                                                  tags$style(type = "text/css", "#MidwayWeeklyArrDep {min-height:30vh !important;}"),
                                                                  plotlyOutput("MidwayWeeklyArrDep"))
                                                         )
                                       
                                                         ),
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         
                                                         
                                                         tabsetPanel(id = "c2c4tabs",
                                                                     tabPanel("Week", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#OhareWeeklyArrDepTable {min-height:25vh !important;}"),
                                                                                       DTOutput("OhareWeeklyArrDepTable")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#MidwayWeeklyArrDepTable {min-height:25vh !important;}"),
                                                                                       DTOutput("MidwayWeeklyArrDepTable"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Hour Delays", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#OhareHourlyDelaysTable {min-height:25vh !important;}"),
                                                                                       DTOutput("OhareHourlyDelaysTable")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#MidwayHourlyDelaysTable {min-height:25vh !important;}"),
                                                                                       DTOutput("MidwayHourlyDelaysTable"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Hour", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#OhareHourlyArrDepTable {min-height:25vh !important;}"),
                                                                                       DTOutput("OhareHourlyArrDepTable")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#MidwayHourlyArrDepTable {min-height:25vh !important;}"),
                                                                                       DTOutput("MidwayHourlyArrDepTable"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                )
                                                
                                                
                                                ),
                                              tabBox(
                                                title =  "Top 15 Airports",#"c5",
                                                side = "left",
                                                width = 5,
                                                
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         
                                                         tabsetPanel(id = "c5tabs2",
                                                                     tabPanel("Arrivals", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#OhareMostCommonArrivalAirports {min-height:30vh !important;}"),
                                                                                       plotOutput("OhareMostCommonArrivalAirports")), #, height = "80vh"
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#MidwayMostCommonArrivalAirports {min-height:30vh !important;}"),
                                                                                       plotOutput("MidwayMostCommonArrivalAirports"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Departures", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#OhareMostCommonDestinationAirports {min-height:30vh !important;}"),
                                                                                       plotOutput("OhareMostCommonDestinationAirports")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#MidwayMostCommonDestinationAirports {min-height:30vh !important;}"),
                                                                                       plotOutput("MidwayMostCommonDestinationAirports"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                         
                                                         
                                                ),
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         
                                                         tabsetPanel(id = "c5tabs1",
                                                                     tabPanel("Arrivals", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#OhareMostCommonArrivalAirportsTable {min-height:25vh !important;}"),
                                                                                       DTOutput("OhareMostCommonArrivalAirportsTable")), #, height = "80vh"
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#MidwayMostCommonArrivalAirportsTable {min-height:25vh !important;}"),
                                                                                       DTOutput("MidwayMostCommonArrivalAirportsTable"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Departures", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#OhareMostCommonDestinationAirportsTable {min-height:25vh !important;}"),
                                                                                       DTOutput("OhareMostCommonDestinationAirportsTable")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#MidwayMostCommonDestinationAirportsTable {min-height:25vh !important;}"),
                                                                                       DTOutput("MidwayMostCommonDestinationAirportsTable"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                         
                                                         
                                                         
                                                )
                                                )
                                            ),
                                            
                                            fluidRow(
                                              tabBox(
                                                title = "Flights For All Domestic Airlines",#"c1",
                                                side = "left",
                                                width = 6,
                                                
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  tags$style(type = "text/css", "#OhareAirlineArrDep {min-height:30vh !important;}"),
                                                                  plotOutput("OhareAirlineArrDep")),
                                                           column(6,
                                                                  tags$style(type = "text/css", "#MidwayAirlineArrDep {min-height:30vh !important;}"),
                                                                  plotOutput("MidwayAirlineArrDep"))
                                                         )),
                                                tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  tags$style(type = "text/css", "#OhareAirlineArrDepTable {min-height:25vh !important;}"),
                                                                  DTOutput("OhareAirlineArrDepTable")),
                                                           column(6,
                                                                  tags$style(type = "text/css", "#MidwayAirlineArrDepTable {min-height:25vh !important;}"),
                                                                  DTOutput("MidwayAirlineArrDepTable"))
                                                         ))
                                                
                                                ),
                                              tabBox(
                                                title = "Total flights Across the U.S.",
                                                side = "left",
                                                width = 6,
                                                # tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                #          fluidRow(
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                #                   DTOutput("tmp21a")),
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                #                   DTOutput("tmp22s"))
                                                #          )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"),# br(),
                                                         tabsetPanel(id = "mapTabs2",
                                                                     tabPanel("Arrivals", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#leafMonth1 {min-height:30vh !important;}"),
                                                                                       leafletOutput("leafMonth1")), #, height = "80vh"
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#leafMonth2 {min-height:30vh !important;}"),
                                                                                       leafletOutput("leafMonth2"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Departures", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#leafMonth3 {min-height:30vh !important;}"),
                                                                                       leafletOutput("leafMonth3")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#leafMonth4 {min-height:30vh !important;}"),
                                                                                       leafletOutput("leafMonth4"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                         
                                                ))
                                            )
                                            # fluidRow(
                                            #   tabBox(
                                            #     title = "c3",
                                            #     side = "left",
                                            #     width = 12,
                                            #     tabPanel("", icon = icon("table", "fa-2x"), br(),
                                            #              fluidRow(
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                            #                       DTOutput("OhareWeeklyArrDepTable")),
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                            #                       DTOutput("MidwayWeeklyArrDepTable"))
                                            #              )),
                                            #     tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                            #              fluidRow(
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                            #                       plotlyOutput("OhareWeeklyArrDep")),
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                            #                       plotlyOutput("MidwayWeeklyArrDep"))
                                            #              )))
                                            # # ),
                                            # fluidRow(
                                            #   tabBox(
                                            #     title = "c4",
                                            #     side = "left",
                                            #     width = 12,
                                            #     tabPanel("", icon = icon("table", "fa-2x"), br(),
                                            #              fluidRow(
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                            #                       DTOutput("OhareHourlyDelaysTable")),
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                            #                       DTOutput("MidwayHourlyDelaysTable"))
                                            #              )),
                                            #     tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                            #              fluidRow(
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                            #                       plotlyOutput("OhareHourlyDelays")),
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                            #                       plotlyOutput("MidwayHourlyDelays"))
                                            #              )))
                                            # ),
                                  
                               
                                            
                                            
                                            
                                            ),
  # Year ---------------------------
  
                                   tabPanel("Year", 
                                            br(), 
                                            
                                            fluidRow(
                                              tabBox(
                                                title = "Arrivals & Departures",#"b2",
                                                side = "left",
                                                width = 6,
                                                # tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                #          fluidRow(
                                                #            column(3,
                                                #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                #                   DTOutput("Ohare1YearHourlyArrivalsTable")),
                                                #            column(3,
                                                #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                #                   DTOutput("Ohare1YearHourlyDeparturesTable")),
                                                #            column(3,
                                                #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                #                   DTOutput("Midway1YearHourlyArrivalsTable")),
                                                #            column(3,
                                                #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                #                   DTOutput("Midway1YearHourlyDeparturesTable"))
                                                #          )),
                                                tabPanel("Hour", br(),#icon = icon("line-chart", "fa-2x"), br(),
                                                         
                                                         
                                                         tabsetPanel(id = "b2Tabs",
                                                                     tabPanel("Arrivals", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Ohare1YearHourlyArrivals {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Ohare1YearHourlyArrivals")), #, height = "80vh"
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Midway1YearHourlyArrivals {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Midway1YearHourlyArrivals"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Departures", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Ohare1YearHourlyDepartures {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Ohare1YearHourlyDepartures")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Midway1YearHourlyDepartures {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Midway1YearHourlyDepartures"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                ),
                                                tabPanel("Day of Week", br(),#icon = icon("line-chart", "fa-2x"), br(),
                                                         
                                                         
                                                         fluidRow(
                                                           column(6,
                                                                  tags$style(type = "text/css", "#Ohare1WeekdayHourlyArrDep {min-height:30vh !important;}"),
                                                                  plotOutput("Ohare1WeekdayHourlyArrDep")),
                                                           column(6,
                                                                  tags$style(type = "text/css", "#Midway1WeekdayHourlyArrDep {min-height:30vh !important;}"),
                                                                  plotOutput("Midway1WeekdayHourlyArrDep"))
                                                         )
                                                         
                                                )
                                                
                                                
                                                
                                                
                                                
                                              ),
                                              tabBox(
                                                title = "Top 15 Destinations",
                                                side = "left",
                                                width = 6,
                                                # tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                #          fluidRow(
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                #                   DTOutput("Ohare1YearlMostCommonTable")),
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                #                   DTOutput("Midway1YearlMostCommonTable"))
                                                #          )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                                         fluidRow(
                                                           column(6,
                                                                  tags$style(type = "text/css", "#Ohare1YearlMostCommon {min-height:30vh !important;}"),
                                                                  plotlyOutput("Ohare1YearlMostCommon")),
                                                           column(6,
                                                                  tags$style(type = "text/css", "#Midway1YearlMostCommon {min-height:30vh !important;}"),
                                                                  plotlyOutput("Midway1YearlMostCommon"))
                                                         )))
                                            ),

                                            fluidRow(
                                              tabBox(
                                                title = "Flights for Domestic Airlines & Delays",#"b1 (a2 is the same info)",
                                                side = "left",
                                                width = 6,
                                                # tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                #          fluidRow(
                                                #            column(3,
                                                #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                #                   DTOutput("Ohare1YearAirlinesArrivalsTable")),
                                                #            column(3,
                                                #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                #                   DTOutput("Ohare1YearAirlinesDeparturesTable")),
                                                #            column(3,
                                                #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                #                   DTOutput("Midway1YearAirlinesArrivalsTable")),
                                                #            column(3,
                                                #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                #                   DTOutput("Midway1YearAirlinesDeparturesTable"))
                                                #          )),
                                                tabPanel("Arrivals & Departures", br(),#icon = icon("line-chart", "fa-2x"), br(),
                                                         
                                                         tabsetPanel(id = "b1Tabs",
                                                                     tabPanel("Arrivals", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Ohare1YearAirlinesArrivals {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Ohare1YearAirlinesArrivals")), #, height = "80vh"
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Midway1YearAirlinesArrivals {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Midway1YearAirlinesArrivals"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Departures", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Ohare1YearAirlinesDepartures {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Ohare1YearAirlinesDepartures")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Midway1YearAirlinesDepartures {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Midway1YearAirlinesDepartures"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                 
                                                         ),
                                                tabPanel("Specific Airline", #icon = icon("line-chart", "fa-2x"), 
                                                         
                                                         fluidRow(column(6, offset = 0, align = 'left',
                                                                         selectInput("Airline", label = NULL, width = "100%",
                                                                                     choices = c("Aces Airlines", "Alaska Airlines Inc.", "American Airlines Inc.", 
                                                                                                 'Atlantic Southeast Airlines', "Delta Air Lines Inc.", 
                                                                                                 "Frontier Airlines Inc.", "JetBlue Airways", "SkyWest Airlines Inc.", 
                                                                                                 "Southwest Airlines Co.", "Spirit Air Lines", "United Air Lines Inc.")
                                                                                     , selected = "SkyWest Airlines Inc.")
                                                         )),
                                                         
                                                         tabsetPanel(id = "a2Tabs",
                                                                     tabPanel("Arrivals", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Ohare1YearAirlineHourlyArrivals {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Ohare1YearAirlineHourlyArrivals")), #, height = "80vh"
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Midway1YearAirlineHourlyArrivals {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Midway1YearAirlineHourlyArrivals"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Departures", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Ohare1YearAirlineHourlyDepartures {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Ohare1YearAirlineHourlyDepartures")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Midway1YearAirlineHourlyDepartures {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Midway1YearAirlineHourlyDepartures"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                         
                                                         
                                                         # fluidRow(
                                                         #   column(3,
                                                         #          #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                         #          plotOutput("Ohare1YearAirlineHourlyArrivals")),
                                                         #   column(3,
                                                         #          #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                         #          plotOutput("Ohare1YearAirlineHourlyDepartures")),
                                                         #   column(3,
                                                         #          #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                         #          plotOutput("Midway1YearAirlineHourlyArrivals")),
                                                         #   column(3,
                                                         #          #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                         #          plotOutput("Midway1YearAirlineHourlyDepartures"))
                                                         # )
                                                ),
                                                tabPanel("Delays", br(),#icon = icon("line-chart", "fa-2x"), br(),
                                                         
                                                         tabsetPanel(id = "b1Tabs",
                                                                     tabPanel("Chart", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Ohare1YearDelays {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Ohare1YearDelays")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Midway1YearDelays {min-height:30vh !important;}"),
                                                                                       plotlyOutput("Midway1YearDelays"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Table", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Ohare1YearDelaysTable {min-height:25vh !important;}"),
                                                                                       DTOutput("Ohare1YearDelaysTable")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#Midway1YearDelaysTable {min-height:25vh !important;}"),
                                                                                       DTOutput("Midway1YearDelaysTable"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                         
                                                         
                                                        )),
                                              tabBox(
                                                title = "Total Flights Across the U.S.",
                                                side = "left",
                                                width = 6,
                                                # tabPanel("", icon = icon("table", "fa-2x"), br(),
                                                #          fluidRow(
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                                #                   DTOutput("tmp45")),
                                                #            column(6,
                                                #                   #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                                #                   DTOutput("tmp46"))
                                                #          )),
                                                tabPanel("", icon = icon("line-chart", "fa-2x"), #br(),
                                                         tabsetPanel(id = "mapTabs3",
                                                                     tabPanel("Arrivals", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#leafYear1 {min-height:30vh !important;}"),
                                                                                       leafletOutput("leafYear1")), #, height = "80vh"
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#leafYear2 {min-height:30vh !important;}"),
                                                                                       leafletOutput("leafYear2"))
                                                                              )
                                                                     ),
                                                                     tabPanel("Departures", 
                                                                              fluidRow(
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#leafYear3 {min-height:30vh !important;}"),
                                                                                       leafletOutput("leafYear3")),
                                                                                column(6,
                                                                                       tags$style(type = "text/css", "#leafYear4 {min-height:30vh !important;}"),
                                                                                       leafletOutput("leafYear4"))
                                                                              )
                                                                     )
                                                         )
                                                         
                                                ))
                                            )
                                            
                                            # fluidRow(
                                            #   tabBox(
                                            #     title = "a4",
                                            #     side = "left",
                                            #     width = 12,
                                            # 
                                            #     tabPanel("", icon = icon("line-chart", "fa-2x"), br(),
                                            #              fluidRow(
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart1 {min-height:40vh !important;}"),
                                            #                       plotOutput("Ohare1WeekdayHourlyArrDep")),
                                            #                column(6,
                                            #                       #tags$style(type = "text/css", "#pieChart2 {min-height:40vh !important;}"),
                                            #                       plotOutput("Midway1WeekdayHourlyArrDep"))
                                            #              )))
                                            # ),
           

                  
                                            
                                            
                                            
                                            
                                            )
                      ),
  
              fluidRow(column(1,offset = 6, h3("Distance:"), align='right'), 
                       column(2, offset=0, sliderInput("slider_Distance", label = NULL, width = "100%", post = "mi.", step = 1,
                                    min = 65, max = 4250, value = c(65, 4250))),
                       column(1,offset = 0, h3("Air Time:"), align='right'), 
                       column(2, offset=0, sliderInput("slider_AirTime", label = NULL, width = "100%", post = "hrs", step = 0.1,
                                                       min = 0.0, max = 10.0, value = c(0.0, 10.0)))
                       
                       
                       )
              ) 
  ),
  
  
  
  
  
  
  
  #  ---------------------------
  
  bsModal(id = "modal_Settings", h2("Settings"), trigger = "action_Settings", size = "small",
          h2("Time"), 
          fluidRow(column(11, offset = 1, checkboxInput("HourFormat", label = "24h format", value = FALSE)))),
  bsModal(id = "modal_About", h2("About"), trigger = "action_About", size = "large",
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

# .tabbable > .nav[id=mainNav] > li                     {font-size: 20px;}
# .tabbable > .nav[id=mainNav] > li > a                 {color: #CFCFCF; border-radius: 4px; border: 0px;}
#     .tabbable > .nav[id=mainNav] > li[class=active] > a   {background-color: #484747; color: white; border: 0px;}
#         .tabbable > .nav[id=mainNav] > li > a:hover           {background-color: #484747;}
#             
            
# ----
# ----


