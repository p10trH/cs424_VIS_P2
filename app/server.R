# Peter Hanula
# Dimitar Kirilov
# Tarush Vig

# ---------------------------



# ---------------------------

# seriesData = data.frame(time = sample(seq(as.Date('2017-1-1'), as.Date('2017-12-31'), by="day"), 900, replace=TRUE), value=runif(900))
# seriesData = data.frame(time=seq(from=as.POSIXct("2017-1-1", format = '%Y-%m-%d'), to=as.POSIXct("2017-12-31", format = '%Y-%m-%d'), by="day" ), value=runif(365))
#
# agg1 = aggregate(. ~time, data=seriesData, sum, na.rm=TRUE)
#
# seriesData=xts(x = seriesData$value, order.by = seriesData$time)
# agg1=xts(x = agg1$value, order.by = agg1$time)
#
# dygraph(seriesData) %>% dyRangeSelector()


date <- rep('2017-01-01', each=24)
times <- c('0030', '0100', '0200', '0300', '0400', '0500', '0623', '0700', '0800', '0900', '1034', '1100', '1233', '1300', '1445', '1500', '1600', '1700', '1812', '1900', '2011', '2111', '2200', '2356')
data <- c(12, 55, 2, 37, 98, 33, 66, 41, 11, 12, 13, 6, 44, 45, 55, 89, 23, 24, 11, 2, 2, 3, 4, 5)

#times <- lapply(times, as.character)

hourFrame <- data.frame(date, times, data)

hourFrame$fullDate <- paste(hourFrame$date, hourFrame$times, sep = " ")
hourFrame$timesFormat <- as.POSIXct(hourFrame$fullDate, format = '%Y-%m-%d %H%M')

chart  <- ggplot(hourFrame, aes(x = timesFormat, y = data)) + geom_line(na.rm = TRUE) + labs(x = NULL, y = NULL)
chart2 <- ggplot(hourFrame, aes(x = timesFormat, y = data)) + geom_line(na.rm = TRUE) + labs(x = NULL, y = NULL) + scale_x_datetime(date_labels = "%H", date_breaks = "1 hour")
chart3 <- ggplot(hourFrame, aes(x = timesFormat, y = data)) + geom_line(na.rm = TRUE) + labs(x = NULL, y = NULL) + scale_x_datetime(date_labels = "%H:%M", date_breaks = "3 hour")
chart4 <- ggplot(hourFrame, aes(x = timesFormat, y = data)) + geom_line(na.rm = TRUE) + labs(x = NULL, y = NULL) + scale_x_datetime(date_labels = "%l %p", date_breaks = "3 hour")

# ---------------------------

# statesData <-
#   read.csv(file = 'data/statesData.csv',
#            header = TRUE)
# 
# states <- geojsonio::geojson_read("data/states.geojson", what = "sp")
# 
# statesWData <- merge(states, statesData, by = "NAME")
# 
# # try CartoDB.Positron
# m <- leaflet(statesWData) %>%
#   setView(-96, 37.8, 4) %>%
#   addProviderTiles(providers$Stamen.TonerLite,
#                    options = providerTileOptions(noWrap = TRUE))
# 
# bins <- c(0, 3, 6, 9, 12, 15, 18, 21, Inf)
# pal <- colorBin("YlOrRd", domain = statesWData$value, bins = bins)
# 
# labels <- sprintf(
#   "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
#   statesWData$NAME, statesWData$value
# ) %>% lapply(htmltools::HTML)
# 
# m <- m %>% addPolygons(
#   fillColor = ~pal(statesWData$value),
#   weight = 2,
#   opacity = 1,
#   color = "black",
#   dashArray = "3",
#   fillOpacity = 0.7,
#   highlight = highlightOptions(
#     weight = 5,
#     color = "#666",
#     dashArray = "",
#     fillOpacity = 0.7,
#     bringToFront = TRUE),
#   label = labels,
#   labelOptions = labelOptions(
#     style = list("font-weight" = "normal", padding = "3px 8px"),
#     textsize = "15px",
#     direction = "auto")) %>%
#   leaflet::addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
#                                        position = "bottomright")

# ---------------------------

# Overview. Dygraph, total flights + delays

# allFlightsCondensed <- data.frame(allFlights24$FL_DATE, allFlights24$ORIGIN_AIRPORT, allFlights24$DEST_AIRPORT)
# colnames (allFlightsCondensed) <- c("FL_DATE", "ORIGIN_AIRPORT", "DEST_AIRPORT")
# 
# # ohare
# allFlightsOHare <- filter(allFlightsCondensed, ORIGIN_AIRPORT == "Chicago O'Hare International" | DEST_AIRPORT == "Chicago O'Hare International")
# allFlightsOHare <- allFlightsOHare %>% add_count(FL_DATE)
# #allFlightsOHare <- distinct(allFlightsOHare, FL_DATE)
# allFlightsOHare <- allFlightsOHare[!duplicated(allFlightsOHare$FL_DATE), ]
# 
# series1 = xts(x = allFlightsOHare$n, order.by = allFlightsOHare$FL_DATE)
# 
# # midway
# allFlightsMidway <- filter(allFlightsCondensed, ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago Midway International")
# allFlightsMidway <- allFlightsMidway %>% add_count(FL_DATE)
# allFlightsMidway <- allFlightsMidway[!duplicated(allFlightsMidway$FL_DATE), ]
# 
# series2 = xts(x = allFlightsMidway$n, order.by = allFlightsMidway$FL_DATE)
# 
# #series1 <- data.frame(FL_DATE = allFlightsOHare$FL_DATE, OHARE = allFlightsOHare$n, MIDWAY = allFlightsMidway$n)
# #series1 = xts(x1 = series1$OHARE, x2 = series1$MIDWAY, order.by = series1$FL_DATE)
# 
# allData <- cbind(series1, series2)
# colnames (allData) <- c("O\'Hare", "Midway")
# #dygraph(allData) %>% dyRangeSelector()


# ---------------------------
#Data----
#allOnTimeFlights <- read.csv(file = "2017_ontime_flights.cleaned.csv", header = TRUE)
load("data/allFlights12.RData")
load("data/allFlights24.RData")
load("data/allFlights.RData")

#Format Date & Time:
#allOnTimeFlights$FL_DATE <- as.Date(allOnTimeFlights$FL_DATE, format = "%m/%d/%Y")

#allOnTimeFlights$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allOnTimeFlights$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
#allOnTimeFlights$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allOnTimeFlights$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')




# Define server
server <- function(input, output) {
  
  #Other Data:
  hours24 <- as.data.frame(c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"))
  hours12 <- as.data.frame(c("12:00 AM", "01:00 AM", "02:00 AM", "03:00 AM", "04:00 AM", "05:00 AM", "06:00 AM", "07:00 AM", "08:00 AM", "09:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "01:00 PM", "02:00 PM", "03:00 PM", "04:00 PM", "05:00 PM", "06:00 PM", "07:00 PM", "08:00 PM", "09:00 PM", "10:00 PM", "11:00 PM"))
  colnames(hours24) <- c("Hour")
  colnames(hours12) <- c("Hour")
  
  weekdays <- as.data.frame(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  colnames(weekdays) <- c("Day")
  
  airlines <- as.data.frame(c("Aces Airlines", "Alaska Airlines Inc.", "American Airlines Inc.", 'Atlantic Southeast Airlines', "Delta Air Lines Inc.", "Frontier Airlines Inc.", "JetBlue Airways", "SkyWest Airlines Inc.", "Southwest Airlines Co.", "Spirit Air Lines", "United Air Lines Inc."))
  colnames(airlines) <- c("Airline")
  
  top50Airports <- allFlights24 %>% group_by(ORIGIN_AIRPORT) %>% summarise(Flights = n()) %>% top_n(50) %>% arrange(desc(Flights))
  #--------
  
  #output$value <- renderPrint({ getDate()})
  
  getDate <- reactive({
    
    # check if valid date first, otherwise set to last day of current month
    if (is.na(day(mdy(paste(substr(input$slider_month, 2, 3), substr(input$slider_day, 2, 3), "2017", sep = "-"))))) {
       tempMonthYear <- paste(substr(input$slider_month, 2, 3), "2017", sep = "-")
       tempDays <- as.numeric(days_in_month(as.yearmon(tempMonthYear, "%m-%Y")))
       paste(substr(input$slider_month, 2, 3), as.character(tempDays), "2017", sep = "-")
    } else{
       paste(substr(input$slider_month, 2, 3), substr(input$slider_day, 2, 3), "2017", sep = "-")
    }
  })
  
  output$dynamicSlider <- renderUI({
    
    startDate <- as.Date(paste(substr(input$slider_month, 2, 3), '01-2017', sep = "-"), "%m-%d-%Y")
    
    choices_day <- format(seq.Date(from = startDate, length.out = as.numeric(days_in_month(startDate)), by="day"), "(%d)  %a")
  
    if(!is.null(input$slider_day)) {
      
      dateToShow <- getDate()
    }

    sliderTextInput(
      inputId = "slider_day",
      label = NULL, width = '100%', grid = TRUE, force_edges = TRUE, hide_min_max = TRUE,
      choices = choices_day, selected = choices_day[day(mdy(dateToShow))]
    )
  })
  
  
 
  # ------------
  
  # Reactive:
  oneMonthReactive <- reactive({
    if (input$HourFormat)
    {
      filter(allFlights24, month(FL_DATE) == input$Month)
    }
    else
    {
      filter(allFlights12, month(FL_DATE) == input$Month)
    }
    
  })
  oneAirportReactive <- reactive({
    if (input$HourFormat)
    {
      filter(allFlights24, ORIGIN_AIRPORT == input$Airport | DEST_AIRPORT == input$Airport)
    }
    else
    {
      filter(allFlights24, ORIGIN_AIRPORT == input$Airport | DEST_AIRPORT == input$Airport)
    }
  })
  oneDayReactive <- reactive({
    if (input$HourFormat)
    {
      message(input$Date)
      filter(allFlights24, date(FL_DATE) == input$Date)
    }
    else
    {
      filter(allFlights12, date(FL_DATE) == input$Date)
    }
  })
  oneWeekdayReactive <- reactive({
    if (input$HourFormat)
    {
      filter(allFlights24, weekdays(FL_DATE) == input$Weekday)
    }
    else
    {
      filter(allFlights12, weekdays(FL_DATE) == input$Weekday)
    }
  })
  oneAirlineReactive <- reactive({
    if (input$HourFormat)
    {
      filter(allFlights24, CARRIER_NAME == input$Airline)
    }
    else
    {
      filter(allFlights12, CARRIER_NAME == input$Airline)
    }
  })
  
  # C====
  # Total # of Departures & Arrivals (Airlines)
  output$OhareAirlineArrDep <- renderPlot({
    
    monthNum <- month(mdy(getDate()))
    #oneMonth <- 6
    #message(oneMonth)
    
    oneMonth <- filter(allOnTimeFlights, month(FL_DATE) == monthNum)
    # Select Proper Month:
    #oneMonth <- oneMonthReactive()
    
    # Filter Arrivals and Departures:
    arrivals <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
      group_by(CARRIER_NAME) %>%
      summarise(freq = n())
    
    departures <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
      group_by(CARRIER_NAME) %>%
      summarise(freq = n())
    
    colnames(arrivals) <- c("Airline", "Arrivals")
    colnames(departures) <- c("Airline", "Departures")
    
    # Combine and Melt:
    airlineArrDep <- join_all(list(airlines, arrivals, departures), by = "Airline", type = "full")
    airlineArrDep[is.na(airlineArrDep)] = 0
    
    airlineArrDepMelt <- melt(airlineArrDep, id.vars = "Airline")
    
    # Plot
    ggplot(data = airlineArrDepMelt, aes(x = Airline,
                                         y = value,
                                         fill = variable)) +
      labs(title = "O'hare Arrivals vs. Departures", x = "Airline", y = "Num. of Flights", color = "Legend") +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Set1")
  })
  
  
  output$MidwayAirlineArrDep <- renderPlot({
    # Select Proper Month:
    #oneMonth <- oneMonthReactive()
    monthNum <- month(mdy(getDate()))
    oneMonth <- filter(allOnTimeFlights, month(FL_DATE) == monthNum)
    
    # Filter Arrivals and Departures:
    arrivals <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International") %>%
      group_by(CARRIER_NAME) %>%
      summarise(freq = n())
    
    departures <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>%
      group_by(CARRIER_NAME) %>%
      summarise(freq = n())
    
    colnames(arrivals) <- c("Airline", "Arrivals")
    colnames(departures) <- c("Airline", "Departures")
    
    # Combine and Melt:
    airlineArrDep <- join_all(list(airlines, arrivals, departures), by = "Airline", type = "full")
    airlineArrDep[is.na(airlineArrDep)] = 0
    
    airlineArrDepMelt <- melt(airlineArrDep, id.vars = "Airline")
    
    # Plot
    ggplot(data = airlineArrDepMelt, aes(x = Airline,
                                         y = value,
                                         fill = variable)) +
      labs(title = "Midway Arrivals vs. Departures", x = "Airline", y = "Num. of Flights", color = "Legend") +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Set1")
  })
  
  # Total # of Departures & Arrivals (Hourly)
  output$OhareHourlyArrDep <- renderPlot({
    # Select Proper Month:
    #oneMonth <- oneMonthReactive()
    
    monthNum <- month(mdy(getDate()))
    #oneMonth <- filter(allOnTimeFlights, month(FL_DATE) == monthNum)
    
    # Convert Time Format:
    if (input$HourFormat)
    {
      hourFormat <- hours24
      oneMonth <- filter(allFlights24, month(FL_DATE) == monthNum)
    }
    else
    {
      hourFormat <- hours12
      oneMonth <- filter(allFlights12, month(FL_DATE) == monthNum)
    }
    
    
    # Filter Arrivals and Departures:
    arrivals <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
      group_by(ARR_TIME) %>%
      summarise(freq = n()) %>%
      na.omit()
    
    departures <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
      group_by(DEP_TIME) %>%
      summarise(n()) %>%
      na.omit()
    
    colnames(arrivals) <- c("Hour", "Arrivals")
    colnames(departures) <- c("Hour", "Departures")
    
    # Combine and Melt:
    hourlyArrDep <- join_all(list(hourFormat, arrivals, departures), by = "Hour", type = "full")
    hourlyArrDep[is.na(hourlyArrDep)] = 0
    
    hourlyArrDep$Hour <- ordered(hourlyArrDep$Hour, levels = hourFormat[,])
    
    hourlyArrDepMelt <- melt(hourlyArrDep, id.vars = "Hour")
    
    # Plot
    ggplot(data = hourlyArrDepMelt, aes(x = Hour,
                                        y = value,
                                        group = variable,
                                        color = variable)) +
      labs(title = "O'hare Hourly Arrivals vs. Departures", x = "Time of Day (Hour)", y = "Num. of Flights", color = "Legend") +
      geom_point() +
      geom_line(size = 1.5, alpha = 0.7)
  })
  
  
  
  
}
