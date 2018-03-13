# Peter Hanula
# Dimitar Kirilov
# Tarush Vig

# ---------------------------

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

# # ---------------------------
# 
# statesData <-
#   read.csv(file = 'statesData.csv',
#            header = TRUE)
# 
# states <- geojsonio::geojson_read("states.geojson", what = "sp")
# 
# statesWData <- merge(states, statesData, by = "NAME")
# 
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
#   addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
#                                        position = "bottomright")
# 
# ---------------------------


#Data----
#allOnTimeFlights <- read.csv(file = "2017_ontime_flights.cleaned.csv", header = TRUE)
load("allFlights12.RData")
load("allFlights24.RData")

#Format Date & Time:
#allOnTimeFlights$FL_DATE <- as.Date(allOnTimeFlights$FL_DATE, format = "%m/%d/%Y")

#allOnTimeFlights$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allOnTimeFlights$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
#allOnTimeFlights$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allOnTimeFlights$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')

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


# Define server
server <- function(input, output) {
  # Chicago O\'Hare International
  # Chicago Midway International
  
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
  
  # C====
  # Total # of Departures & Arrivals (Airlines)
  output$OhareAirlineArrDep <- renderPlot({
    # Select Proper Month:
    oneMonth <- oneMonthReactive()
    
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
    oneMonth <- oneMonthReactive()
    
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
    oneMonth <- oneMonthReactive()
    
    # Convert Time Format:
    if (input$HourFormat)
    {
      hourFormat <- hours24
      #oneMonth$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
      #oneMonth$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
    }
    else
    {
      hourFormat <- hours12
      #oneMonth$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
      #oneMonth$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
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
  
  output$MidwayHourlyArrDep <- renderPlot({
    # Select Proper Month:
    oneMonth <- oneMonthReactive()
    
    # Convert Time Format:
    if (input$HourFormat)
    {
      hourFormat <- hours24
      #oneMonth$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
      #oneMonth$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
    }
    else
    {
      hourFormat <- hours12
      #oneMonth$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
      #oneMonth$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
    }
    
    # Filter Arrivals and Departures:
    arrivals <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International") %>%
      group_by(ARR_TIME) %>%
      summarise(freq = n()) %>%
      na.omit()
    
    departures <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>%
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
      labs(title = "Midway Hourly Arrivals vs. Departures", x = "Time of Day (Hour)", y = "Num. of Flights", color = "Legend") +
      geom_point() +
      geom_line(size = 1.5, alpha = 0.7)
  })
  
  
  # Total # of Departures & Arrivals (Weekly)
  output$OhareWeeklyArrDep <- renderPlot({
    # Select Proper Month:
    oneMonth <- oneMonthReactive()
    
    # Filter Arrivals and Departures:
    arrivals <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
      group_by(weekdays(FL_DATE)) %>%
      summarise(freq = n()) %>%
      na.omit()
    
    departures <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
      group_by(weekdays(FL_DATE)) %>%
      summarise(n()) %>%
      na.omit()
    
    colnames(arrivals) <- c("Day", "Arrivals")
    colnames(departures) <- c("Day", "Departures")
    
    # Combine and Melt:
    weeklyArrDep <- join_all(list(arrivals, departures), by = "Day", type = "full")
    weeklyArrDep[is.na(weeklyArrDep)] = 0
    
    weeklyArrDep$Day <- ordered(weeklyArrDep$Day, levels = weekdays[,])
    
    hourlyArrDepMelt <- melt(weeklyArrDep, id.vars = "Day")
    
    # Plot
    ggplot(data = hourlyArrDepMelt, aes(x = Day,
                                        y = value,
                                        group = variable,
                                        color = variable)) +
      labs(title = "O'hare Weekly Arrivals vs. Departures", x = "Day of Week", y = "Num. of Flights", color = "Legend") +
      geom_point() +
      geom_line(size = 1.5, alpha = 0.7)
  })
  
  output$MidwayWeeklyArrDep <- renderPlot({
    # Select Proper Month:
    oneMonth <- oneMonthReactive()
    
    # Filter Arrivals and Departures:
    arrivals <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International") %>%
      group_by(weekdays(FL_DATE)) %>%
      summarise(freq = n()) %>%
      na.omit()
    
    departures <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>%
      group_by(weekdays(FL_DATE)) %>%
      summarise(n()) %>%
      na.omit()
    
    colnames(arrivals) <- c("Day", "Arrivals")
    colnames(departures) <- c("Day", "Departures")
    
    # Combine and Melt:
    weeklyArrDep <- join_all(list(arrivals, departures), by = "Day", type = "full")
    weeklyArrDep[is.na(weeklyArrDep)] = 0
    
    weeklyArrDep$Day <- ordered(weeklyArrDep$Day, levels = weekdays[,])
    
    hourlyArrDepMelt <- melt(weeklyArrDep, id.vars = "Day")
    
    # Plot
    ggplot(data = hourlyArrDepMelt, aes(x = Day,
                                        y = value,
                                        group = variable,
                                        color = variable)) +
      labs(title = "Midway Weekly Arrivals vs. Departures", x = "Day of Week", y = "Num. of Flights", color = "Legend") +
      geom_point() +
      geom_line(size = 1.5, alpha = 0.7)
  })
  
  
  # Total # of Delays (Hourly)
  output$OhareHourlyDelays <- renderPlot({
    # Select Proper Month:
    oneMonth <- oneMonthReactive()
    
    # Convert Time Format:
    if (input$HourFormat)
    {
      hourFormat <- hours24
      #oneMonth$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
      #oneMonth$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
    }
    else
    {
      hourFormat <- hours12
      #oneMonth$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
      #oneMonth$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
    }
    
    # Filter Delays
    delays <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
      select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
      filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
      mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
      group_by(DEP_TIME) %>%
      summarise_each(funs(sum)) %>%
      mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY)
    
    colnames(delays) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
    
    delays <- join_all(list(hourFormat, delays), by = "Hour", type = "full")
    delays[is.na(delays)] = 0
    
    delays$Hour <- ordered(delays$Hour, levels = hourFormat[,])
    
    delaysMelt <- melt(delays, id.vars = "Hour")
    
    ggplot(data = delaysMelt, aes(x = Hour, 
                                  y = value,
                                  group = variable,
                                  color = variable)) + 
      labs(title = "Ohare Delays", x = "Hour", y = "Num. of Delays", color = "Delay Type") +
      geom_point() + 
      geom_line(size = 1.5, alpha = 0.7)
  })
  
  output$MidwayHourlyDelays <- renderPlot({
    # Select Proper Month:
    oneMonth <- oneMonthReactive()
    
    # Convert Time Format:
    if (input$HourFormat)
    {
      hourFormat <- hours24
      #oneMonth$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
      #oneMonth$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
    }
    else
    {
      hourFormat <- hours12
      #oneMonth$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
      #oneMonth$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", oneMonth$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
    }
    
    # Filter Delays
    delays <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") %>%
      select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
      filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
      mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
      group_by(DEP_TIME) %>%
      summarise_each(funs(sum)) %>%
      mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY)
    
    colnames(delays) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
    
    delays <- join_all(list(hourFormat, delays), by = "Hour", type = "full")
    delays[is.na(delays)] = 0
    
    delays$Hour <- ordered(delays$Hour, levels = hourFormat[,])
    
    delaysMelt <- melt(delays, id.vars = "Hour")
    
    ggplot(data = delaysMelt, aes(x = Hour, 
                                  y = value,
                                  group = variable,
                                  color = variable)) + 
      labs(title = "Midway Delays", x = "Hour", y = "Num. of Delays", color = "Delay Type") +
      geom_point() + 
      geom_line(size = 1.5, alpha = 0.7)
  })
  
  
  # Most Common 15 Arrival and Destination Airports
  output$OhareMostCommonArrivalAirports <- renderPlot({
    # Select Proper Month:
    oneMonth <- oneMonthReactive()
    
    # Filter Arrival and Destination Airports
    arrivalAirports <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
      group_by(ORIGIN_AIRPORT) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq)) %>%
      top_n(15)
    
    colnames(arrivalAirports) <- c("Departing Airport", "Flights")
  })
  
  output$OhareMostCommonDestinationAirports <- renderPlot({
    # Select Proper Month:
    oneMonth <- oneMonthReactive()
    
    destinationAirports <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
      group_by(DEST_AIRPORT) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq)) %>%
      top_n(15)
    
    colnames(destinationAirports) <- c("Airport", "Flights")
    
    ggplot(data = destinationAirports, aes(x = reorder(Airport, Flights), 
                                           y = Flights)) + 
      labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
      geom_bar(stat = "identity", fill = "blue") + 
      guides(fill = FALSE) +
      coord_flip()
  })
  
  output$OhareMostCommonArrivalAirports <- renderPlot({
    # Select Proper Month:
    oneMonth <- oneMonthReactive()
    
    arrivalAirports <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
      group_by(ORIGIN_AIRPORT) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq)) %>%
      top_n(15)
    
    colnames(arrivalAirports) <- c("Airport", "Flights")
    
    ggplot(data = arrivalAirports, aes(x = reorder(Airport, Flights), 
                                       y = Flights)) + 
      labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
      geom_bar(stat = "identity", fill = "red") + 
      guides(fill = FALSE) +
      coord_flip()
  })
  
  output$MidwayMostCommonDestinationAirports <- renderPlot({
    # Select Proper Month:
    oneMonth <- oneMonthReactive()
    
    destinationAirports <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>%
      group_by(DEST_AIRPORT) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq)) %>%
      top_n(15)
    
    colnames(destinationAirports) <- c("Airport", "Flights")
    
    ggplot(data = destinationAirports, aes(x = reorder(Airport, Flights), 
                                           y = Flights)) + 
      labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
      geom_bar(stat = "identity", fill = "blue") + 
      guides(fill = FALSE) +
      coord_flip()
  })
  
  output$MidwayMostCommonArrivalAirports <- renderPlot({
    # Select Proper Month:
    oneMonth <- oneMonthReactive()
    
    arrivalAirports <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International") %>%
      group_by(ORIGIN_AIRPORT) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq)) %>%
      top_n(15)
    
    colnames(arrivalAirports) <- c("Airport", "Flights")
    
    ggplot(data = arrivalAirports, aes(x = reorder(Airport, Flights), 
                                       y = Flights)) + 
      labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
      geom_bar(stat = "identity", fill = "red") + 
      guides(fill = FALSE) +
      coord_flip()
  })
  
  # C====
  
  
  # B====
  output$Ohare1YearHourlyArrivals <- renderPlot({
    
    # Convert Time Format:
    if (input$HourFormat)
    {
      hourFormat <- hours24
      allFlights <- allFlights24
      #allFlights$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
      #allFlights$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
    }
    else
    {
      hourFormat <- hours12
      allFlights <- allFlights12
      #allFlights$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
      #allFlights$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
    }
    
    # Filter Arrivals
    arrivals1 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(January_ARR = n()) %>% na.omit()
    arrivals2 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(February_ARR = n()) %>% na.omit()
    arrivals3 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(March_ARR = n()) %>% na.omit()
    arrivals4 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(April_ARR = n()) %>% na.omit()
    arrivals5 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May_ARR = n()) %>% na.omit()
    arrivals6 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(June_ARR = n()) %>% na.omit()
    arrivals7 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(July_ARR = n()) %>% na.omit()
    arrivals8 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(August_ARR = n()) %>% na.omit()
    arrivals9 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(September_ARR = n()) %>% na.omit()
    arrivals10 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(October_ARR = n()) %>% na.omit()
    arrivals11 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(November_ARR = n()) %>% na.omit()
    arrivals12 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(December_ARR = n()) %>% na.omit()
    
    # Combine and Melt
    colnames(hourFormat) <- c("ARR_TIME")
    allArrivals <- join_all(list(hourFormat, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "ARR_TIME", type = "full")
    allArrivals[is.na(allArrivals)] = 0
    
    allArrivalsMelt <- melt(allArrivals)
    
    ggplot(data = allArrivalsMelt, aes(x = variable,
                                       y = ARR_TIME)) +
      geom_tile(aes(fill = allArrivalsMelt$value)) +
      scale_fill_gradient(low = "#85aef2", high = "#001a44") + 
      labs(title = "O'hare Arrivals (2017)", x = "Month", y = "Hour", color = "Legend")
  })
  
  output$Ohare1YearHourlyDepartures <- renderPlot({
    
    # Convert Time Format:
    if (input$HourFormat)
    {
      hourFormat <- hours24
      allFlights <- allFlights24
      #allFlights$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
      #allFlights$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
    }
    else
    {
      hourFormat <- hours12
      allFlights <- allFlights12
      #allFlights$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
      #allFlights$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
    }
    
    # Filter Departures
    departures1 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(January_DEP = n()) %>% na.omit()
    departures2 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(February_DEP = n()) %>% na.omit()
    departures3 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(March_DEP = n()) %>% na.omit()
    departures4 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(April_DEP = n()) %>% na.omit()
    departures5 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May_DEP = n()) %>% na.omit()
    departures6 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(June_DEP = n()) %>% na.omit()
    departures7 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(July_DEP = n()) %>% na.omit()
    departures8 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(August_DEP = n()) %>% na.omit()
    departures9 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(September_DEP = n()) %>% na.omit()
    departures10 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(October_DEP = n()) %>% na.omit()
    departures11 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(November_DEP = n()) %>% na.omit()
    departures12 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(December_DEP = n()) %>% na.omit()
    
    # Combine and Melt
    colnames(hourFormat) <- c("DEP_TIME")
    allDepartures <- join_all(list(hourFormat, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "DEP_TIME", type = "full")
    allDepartures[is.na(allDepartures)] = 0
    
    allDeparturesMelt <- melt(allDepartures)
    
    ggplot(data = allDeparturesMelt, aes(x = variable,
                                         y = DEP_TIME)) +
      geom_tile(aes(fill = allDeparturesMelt$value)) +
      scale_fill_gradient(low = "#7bf7c5", high = "#004f2f") + 
      labs(title = "O'hare Departures (2017)", x = "Month", y = "Hour", color = "Legend")
  })
  
  output$Midway1YearHourlyArrivals <- renderPlot({
    
    # Convert Time Format:
    if (input$HourFormat)
    {
      hourFormat <- hours24
      allFlights <- allFlights24
      #allFlights$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
      #allFlights$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
    }
    else
    {
      hourFormat <- hours12
      allFlights <- allFlights12
      #allFlights$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
      #allFlights$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
    }
    
    # Filter Arrivals
    arrivals1 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(January_ARR = n()) %>% na.omit()
    arrivals2 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(February_ARR = n()) %>% na.omit()
    arrivals3 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(March_ARR = n()) %>% na.omit()
    arrivals4 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(April_ARR = n()) %>% na.omit()
    arrivals5 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May_ARR = n()) %>% na.omit()
    arrivals6 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(June_ARR = n()) %>% na.omit()
    arrivals7 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(July_ARR = n()) %>% na.omit()
    arrivals8 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(August_ARR = n()) %>% na.omit()
    arrivals9 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(September_ARR = n()) %>% na.omit()
    arrivals10 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(October_ARR = n()) %>% na.omit()
    arrivals11 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(November_ARR = n()) %>% na.omit()
    arrivals12 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(December_ARR = n()) %>% na.omit()
    
    # Combine and Melt
    colnames(hourFormat) <- c("ARR_TIME")
    allArrivals <- join_all(list(hourFormat, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "ARR_TIME", type = "full")
    allArrivals[is.na(allArrivals)] = 0
    
    allArrivalsMelt <- melt(allArrivals)
    
    ggplot(data = allArrivalsMelt, aes(x = variable,
                                       y = ARR_TIME)) +
      geom_tile(aes(fill = allArrivalsMelt$value)) +
      scale_fill_gradient(low = "#85aef2", high = "#001a44") + 
      labs(title = "Midway Arrivals (2017)", x = "Month", y = "Hour", color = "Legend")
  })
  
  output$Midway1YearHourlyDepartures <- renderPlot({
    
    # Convert Time Format:
    if (input$HourFormat)
    {
      hourFormat <- hours24
      allFlights <- allFlights24
      #allFlights$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
      #allFlights$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%H:00')
    }
    else
    {
      hourFormat <- hours12
      allFlights <- allFlights12
      #allFlights$DEP_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$DEP_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
      #allFlights$ARR_TIME <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", allFlights$ARR_TIME), format="%H%M"), 12, 16), '%H:%M'), '%I:00 %p')
    }
    
    # Filter Departures
    departures1 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(January_DEP = n()) %>% na.omit()
    departures2 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(February_DEP = n()) %>% na.omit()
    departures3 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(March_DEP = n()) %>% na.omit()
    departures4 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(April_DEP = n()) %>% na.omit()
    departures5 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May_DEP = n()) %>% na.omit()
    departures6 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(June_DEP = n()) %>% na.omit()
    departures7 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(July_DEP = n()) %>% na.omit()
    departures8 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(August_DEP = n()) %>% na.omit()
    departures9 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(September_DEP = n()) %>% na.omit()
    departures10 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(October_DEP = n()) %>% na.omit()
    departures11 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(November_DEP = n()) %>% na.omit()
    departures12 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(December_DEP = n()) %>% na.omit()
    
    # Combine and Melt
    colnames(hourFormat) <- c("DEP_TIME")
    allDepartures <- join_all(list(hourFormat, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "DEP_TIME", type = "full")
    allDepartures[is.na(allDepartures)] = 0
    
    allDeparturesMelt <- melt(allDepartures)
    
    ggplot(data = allDeparturesMelt, aes(x = variable,
                                         y = DEP_TIME)) +
      geom_tile(aes(fill = allDeparturesMelt$value)) +
      scale_fill_gradient(low = "#7bf7c5", high = "#004f2f") + 
      labs(title = "Midway Departures (2017)", x = "Month", y = "Hour", color = "Legend")
  })
  
  
  output$Ohare1YearDelays <- renderStreamgraph({
    
    # delays <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
    #   select(FL_DATE, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
    #   filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
    #   mutate_each(funs(replace(., . > 0, 1)), -FL_DATE) %>%
    #   group_by(month(FL_DATE)) %>%
    #   summarise_each(funs(sum)) %>%
    #   mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY)
    # 
    # delays$FL_DATE <- NULL
    # colnames(delays) <- c("Month", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
    # 
    # delaysMelt <- melt(delays, id.vars = "Month")
    # 
    # ggplot(data = delaysMelt, aes(x = Month,
    #                               y = value,
    #                               group = variable,
    #                               color = variable)) +
    #   labs(title = "O'hare Yearly Delays", x = "Hour", y = "Num. of Delays", color = "Delay Type") +
    #   geom_point() +
    #   geom_line(size = 1.5, alpha = 0.7)
    
    # OR
    
    delays <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
      select(FL_DATE, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
      filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
      mutate_each(funs(replace(., . > 0, 1)), -FL_DATE) %>%
      group_by(month(FL_DATE)) %>%
      summarise_each(funs(sum))
    
    delays$FL_DATE <- NULL
    colnames(delays) <- c("Month", "Carrier", "Weather", "NAS", "Security", "Aircraft")
    
    delaysMelt <- melt(delays, id.vars = "Month")
    
    delaysMelt$Month <- as.POSIXct(sprintf("2017 %d 1", delaysMelt$Month), format = "%Y %m %d")
    
    streamgraph(delaysMelt, key="variable", value="value", date="Month") %>% 
      sg_axis_x(tick_interval = 1, tick_units = "month", tick_format = "%m") %>%
      sg_legend(show = TRUE, label = "variable")
  })
  
  
  output$Ohare1YearlMostCommon <- renderPlot({
    
    topAirports <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
      group_by(DEST_AIRPORT) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq)) %>%
      top_n(15)
    
    destinationAirports <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & DEST_AIRPORT %in% topAirports$DEST_AIRPORT) %>%
      group_by(DEST_AIRPORT, month(FL_DATE)) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq))
    
    colnames(destinationAirports) <- c("Airport", "Month", "Flights")
    
    #destinationAirportsMelt <- melt(destinationAirports, id.vars = "Month")
    
    ggplot(data = destinationAirports, aes(x = Month, 
                                           y = Flights,
                                           group = Airport,
                                           color = Airport)) + 
      labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
      geom_point() +
      geom_line(size = 1.5, alpha = 0.7)
  })
  
  output$Midway1YearlMostCommon <- renderPlot({
    
    topAirports <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>%
      group_by(DEST_AIRPORT) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq)) %>%
      top_n(15)
    
    destinationAirports <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & DEST_AIRPORT %in% topAirports$DEST_AIRPORT) %>%
      group_by(DEST_AIRPORT, month(FL_DATE)) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq))
    
    colnames(destinationAirports) <- c("Airport", "Month", "Flights")
    
    #destinationAirportsMelt <- melt(destinationAirports, id.vars = "Month")
    
    ggplot(data = destinationAirports, aes(x = Month, 
                                           y = Flights,
                                           group = Airport,
                                           color = Airport)) + 
      labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
      geom_point() +
      geom_line(size = 1.5, alpha = 0.7)
  })
  
  
  output$Ohare1YearAirlinesArrivals <- renderPlot({
    
    arrivals1 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(January = n())
    arrivals2 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(February = n())
    arrivals3 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(March = n())
    arrivals4 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(April = n())
    arrivals5 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
    arrivals6 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(June = n())
    arrivals7 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(July = n())
    arrivals8 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(August = n())
    arrivals9 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(September = n())
    arrivals10 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(October = n())
    arrivals11 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(November = n())
    arrivals12 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(December = n())
    
    arrivals <- join_all(list(airlines, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "Airline")
    #arrivals[is.na(arrivals)] = 0
    
    arrivalsMelt <- melt(arrivals)
    
    ggplot(data = arrivalsMelt, aes(x = variable,
                                    y = Airline)) +
      geom_tile(aes(fill = arrivalsMelt$value)) +
      scale_fill_gradient(na.value = "#bfbfbf",low = "#85aef2", high = "#001a44") + 
      labs(title = "O'hare Arrivals by Airline(2017)", x = "Month", y = "Hour", color = "Legend")
  })
  
  output$Ohare1YearAirlinesDepartures <- renderPlot({
    
    departures1 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(January = n())
    departures2 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(February = n())
    departures3 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(March = n())
    departures4 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(April = n())
    departures5 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
    departures6 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(June = n())
    departures7 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(July = n())
    departures8 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(August = n())
    departures9 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(September = n())
    departures10 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(October = n())
    departures11 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(November = n())
    departures12 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(December = n())
    
    departures <- join_all(list(airlines, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "Airline")
    #arrivals[is.na(arrivals)] = 0
    
    departuresMelt <- melt(departures)
    
    ggplot(data = departuresMelt, aes(x = variable,
                                      y = Airline)) +
      geom_tile(aes(fill = departuresMelt$value)) +
      scale_fill_gradient(na.value = "#bfbfbf",low = "#7bf7c5", high = "#004f2f") + 
      labs(title = "O'hare Arrivals by Airline(2017)", x = "Month", y = "Hour", color = "Legend")
  })
  
  output$Midway1YearAirlinesArrivals <- renderPlot({
    
    arrivals1 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(January = n())
    arrivals2 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(February = n())
    arrivals3 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(March = n())
    arrivals4 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(April = n())
    arrivals5 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
    arrivals6 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(June = n())
    arrivals7 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(July = n())
    arrivals8 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(August = n())
    arrivals9 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(September = n())
    arrivals10 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(October = n())
    arrivals11 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(November = n())
    arrivals12 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(December = n())
    
    arrivals <- join_all(list(airlines, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "Airline")
    #arrivals[is.na(arrivals)] = 0
    
    arrivalsMelt <- melt(arrivals)
    
    ggplot(data = arrivalsMelt, aes(x = variable,
                                    y = Airline)) +
      geom_tile(aes(fill = arrivalsMelt$value)) +
      scale_fill_gradient(na.value = "#bfbfbf",low = "#85aef2", high = "#001a44") + 
      labs(title = "O'hare Arrivals by Airline(2017)", x = "Month", y = "Hour", color = "Legend")
  })
  
  output$Midway1YearAirlinesDepartures <- renderPlot({
    
    departures1 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(January = n())
    departures2 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(February = n())
    departures3 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(March = n())
    departures4 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(April = n())
    departures5 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
    departures6 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(June = n())
    departures7 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(July = n())
    departures8 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(August = n())
    departures9 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(September = n())
    departures10 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(October = n())
    departures11 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(November = n())
    departures12 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(December = n())
    
    departures <- join_all(list(airlines, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "Airline")
    #arrivals[is.na(arrivals)] = 0
    
    departuresMelt <- melt(departures)
    
    ggplot(data = departuresMelt, aes(x = variable,
                                      y = Airline)) +
      geom_tile(aes(fill = departuresMelt$value)) +
      scale_fill_gradient(na.value = "#bfbfbf",low = "#7bf7c5", high = "#004f2f") + 
      labs(title = "O'hare Arrivals by Airline(2017)", x = "Month", y = "Hour", color = "Legend")
  })
  # B====
  
  
  # A====
  
  #Choropleth
  output$Choropleth <- renderLeaflet({
    
    allStates <- separate(allFlights24, ORIGIN_CITY_NAME, c("ORIGIN_CITY", "ORIGIN_STATE"), sep = ",")
    
    allStatesPercent <- allStates %>% group_by(ORIGIN_STATE) %>% summarise(n = n()) %>% mutate(freq = (n / sum(n)) * 100)
    
    colnames(allStatesPercent) <- c("state", "flights", "percent")
    
    statesOrder <- c(" MD", " MN", " MT", " ND", " HI", " ID", " WA", " AZ", " CA", " CO", " NV", " NM", 
                     " OR", " WY", " AR", " IA", " KS", " MO", " NE", " OK", " SD", " LA", " TX", " CT", " MA",
                     " NH", " RI", " VT", " AL", " FL", " GA", " MS", " SC", " IL", " IN", " KY", " NC", " OH", 
                     " TN", " WI", " WV", " PR", " NJ", " NY", " PA", " ME", " MI", " AK", " VA", " DC", " UT", " VI")
    
    allStatesPercent$state <- factor(allStatesPercent$state, levels = statesOrder)
    allStatesPercent <- allStatesPercent[order(allStatesPercent$state),]
    
    states <- geojsonio::geojson_read("us_states.json", what = "sp")
    
    map <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    map %>% addPolygons()
    
    bins <- c(0, 50, 100, 500, 1000, 5000, 10000, 25000, Inf)
    pal <- colorBin("YlOrRd", domain = allStatesPercent$flights, na.color = "#808080" , bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g arrivals (%g)",
      allStatesPercent$state, allStatesPercent$flights, allStatesPercent$percent
    ) %>% lapply(htmltools::HTML)
    
    # map <- map %>% addPolygons(
    #   fillColor = ~pal(allStatesPercent$flights),
    #   weight = 2,
    #   opacity = 1,
    #   color = "white",
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
    #     direction = "auto"))
    # 
    
    
    
    leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(allStatesPercent$flights),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright")
  })
  
  # # A====
}
