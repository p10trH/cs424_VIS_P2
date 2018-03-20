# Peter Hanula
# Dimitar Kirilov
# Tarush Vig

# ---------------------------

textSize <- 16
textSize_pix <- '16px'

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


themeTest <- theme(plot.margin= unit(c(1,1,1.5,1), "cm"),
              plot.background = element_rect(fill = "#AAAAAA"),
              legend.background = element_rect(fill = "#AAAAAA"),
              axis.text.x = element_text(angle = 30),
              axis.text = element_text(size = textSize),
              axis.title = element_text(size = textSize),
              legend.text = element_text(size = textSize))



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



# ---------------------------




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
  
  # Overview. Dygraph, total flights + delays
  
  allFlightsCondensed <- data.frame(allFlights24$FL_DATE, allFlights24$ORIGIN_AIRPORT, allFlights24$DEST_AIRPORT)
  colnames (allFlightsCondensed) <- c("FL_DATE", "ORIGIN_AIRPORT", "DEST_AIRPORT")
  
  # ohare
  allFlightsOHare <- filter(allFlightsCondensed, ORIGIN_AIRPORT == "Chicago O'Hare International" | DEST_AIRPORT == "Chicago O'Hare International")
  allFlightsOHare <- allFlightsOHare %>% add_count(FL_DATE)
  allFlightsOHare <- allFlightsOHare[!duplicated(allFlightsOHare$FL_DATE), ]
  
  series1 = xts(x = allFlightsOHare$n, order.by = allFlightsOHare$FL_DATE)
  
  # midway
  allFlightsMidway <- filter(allFlightsCondensed, ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago Midway International")
  allFlightsMidway <- allFlightsMidway %>% add_count(FL_DATE)
  allFlightsMidway <- allFlightsMidway[!duplicated(allFlightsMidway$FL_DATE), ]
  
  series2 = xts(x = allFlightsMidway$n, order.by = allFlightsMidway$FL_DATE)
  

  allData <- cbind(series1, series2)
  colnames (allData) <- c("O\'Hare", "Midway")
  
  
  output$dygraphTotalFlights <- renderDygraph({
    
    dygraph(allData, group = "overview") %>% dyRangeSelector(height = 50, strokeColor = "#484747", fillColor = "#DDDDDD") %>%
      dyOptions(colors = c("#1E5493", "#860800")) %>%
      dyAxis("y", label = "Flights") %>%
      dyOptions(includeZero = TRUE) %>%
      dyOptions(strokeWidth = 3) %>%
      #dyOptions(axisLabelFontSize = 16) %>%
      dyLegend(hideOnMouseOut = FALSE, width = 350, show = "follow")%>% 
      dyRoller(rollPeriod = 1) %>%
      dyShading(from = "2017-1-1", to = "2017-1-31", color = "#AFAFAF") %>%
      dyShading(from = "2017-3-1", to = "2017-3-31", color = "#AFAFAF") %>%
      dyShading(from = "2017-5-1", to = "2017-5-31", color = "#AFAFAF") %>%
      dyShading(from = "2017-7-1", to = "2017-7-31", color = "#AFAFAF") %>%
      dyShading(from = "2017-9-1", to = "2017-9-30", color = "#AFAFAF") %>%
      dyShading(from = "2017-11-1", to = "2017-11-30", color = "#AFAFAF") %>%
      dyEvent("2017-11-23", "Thanksgiving", labelLoc = "bottom") %>%
      dyEvent("2017-7-4", "4th of July", labelLoc = "bottom") %>%
      dyEvent("2017-12-25", "Christmas", labelLoc = "bottom") %>%
      dyEvent("2017-5-29", "Memorial Day", labelLoc = "bottom") %>%
      dyEvent("2017-2-14", "Valentine's Day", labelLoc = "bottom") %>%
      dyEvent("2017-8-28", "UIC Fall Sart", labelLoc = "bottom") %>%
      dyEvent("2017-1-16", "UIC Spring Start", labelLoc = "bottom") %>%
      dyEvent("2017-12-15", "UIC Fall End", labelLoc = "bottom")%>%
      dyEvent("2017-5-5", "UIC Spring End", labelLoc = "bottom")%>%
      dyEvent("2017-9-4", "Labor Day", labelLoc = "bottom")
    
    
    
  })
  
  # Leaflet
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  
  allFlightsMap <- data.frame(FL_DATE = allOnTimeFlights$FL_DATE, ARR_TIMESTAMP = allOnTimeFlights$ARR_TIMESTAMP, DEP_TIMESTAMP = allOnTimeFlights$DEP_TIMESTAMP, ORIGIN_AIRPORT = allOnTimeFlights$ORIGIN_AIRPORT, ORIGIN_STATE_NAME = allOnTimeFlights$ORIGIN_CITY_NAME, DEST_AIRPORT = allOnTimeFlights$DEST_AIRPORT, DEST_STATE_NAME = allOnTimeFlights$DEST_CITY_NAME, AIR_TIME = round(allOnTimeFlights$AIR_TIME / 60.0, 2), DISTANCE = allOnTimeFlights$DISTANCE)
  allFlightsMap$ORIGIN_STATE_NAME <- substrRight(as.character(allFlightsMap$ORIGIN_STATE_NAME), 2)
  allFlightsMap$DEST_STATE_NAME <- substrRight(as.character(allFlightsMap$DEST_STATE_NAME), 2)
  
  #ohare
  ohareArrMap <- filter(allFlightsMap, DEST_AIRPORT == "Chicago O'Hare International")
  ohareDepMap <- filter(allFlightsMap, ORIGIN_AIRPORT == "Chicago O'Hare International")
  

  
  
  #midway
  midwayArrMap <- filter(allFlightsMap, DEST_AIRPORT == "Chicago Midway International")
  midwayDepMap <- filter(allFlightsMap, ORIGIN_AIRPORT == "Chicago Midway International")
  
 
  
  
  
 
  

  
  # Day
  
   # statesData <-
   #   read.csv(file = 'data/statesData.csv',
   #            header = TRUE)
   
  states <- geojsonio::geojson_read("data/states.geojson", what = "sp")
   
  # statesWData <- sp::merge(states, statesData, by = "NAME")
  
  output$leafDay1 <- renderLeaflet({ 
    
    # Ohare Arrivals
    # min = , max = 
    
    # filter dates here before moving on
    
    dayToFilter <- getDate()
    
    #ohareArrMap <- filter(ohareArrMap, month(ymd(FL_DATE)) == month(mdy(dayToFilter)) & day(ymd(FL_DATE)) == day(mdy(dayToFilter)))
    ohareArrMap <- filter(ohareArrMap, ymd(FL_DATE) == mdy(dayToFilter))
    
    
    # filter distance and airtime here if want full resolution data
    ohareArrMap <- filter(ohareArrMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    ohareArrMap <- filter(ohareArrMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    ohareArrMap <- ohareArrMap %>% add_count(ORIGIN_STATE_NAME) # Ready for table output, after this step resolution lost
    
    
    ohareArrMapUnique <- ohareArrMap[!duplicated(ohareArrMap$ORIGIN_STATE_NAME), ]
    colnames(ohareArrMapUnique)[5] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, ohareArrMapUnique, by = "STUSPS")
    
    totalFlights <- sum(ohareArrMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 15, 30, 45, 60, 75, 90, 105, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "O\'Hare",
                         position = "bottomright") %>% syncWith("maps")
    
  })
  
  output$leafDay2 <- renderLeaflet({
    # Midway Arrivals
    # min = 43, max = 7571
    
    # filter dates here before moving on
    
    dayToFilter <- getDate()
    
    midwayArrMap <- filter(midwayArrMap, ymd(FL_DATE) == mdy(dayToFilter))
    

    # filter distance and airtime here if want full resolution data
    midwayArrMap <- filter(midwayArrMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    midwayArrMap <- filter(midwayArrMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    midwayArrMap <- midwayArrMap %>% add_count(ORIGIN_STATE_NAME) # Ready for table output, after this step resolution lost
    
    
    midwayArrMapUnique <- midwayArrMap[!duplicated(midwayArrMap$ORIGIN_STATE_NAME), ]
    colnames(midwayArrMapUnique)[5] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, midwayArrMapUnique, by = "STUSPS")
    
    totalFlights <- sum(midwayArrMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 15, 30, 45, 60, 75, 90, 105, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Midway",
                         position = "bottomright") %>% syncWith("maps")
  })
  
  output$leafDay3 <- renderLeaflet({
    # Ohare Departures
    # min = 34, max = 24382
    
    # filter dates here before moving on
    
    dayToFilter <- getDate()
    
    ohareDepMap <- filter(ohareDepMap, ymd(FL_DATE) == mdy(dayToFilter))
    
    
    # filter distance and airtime here if want full resolution data
    ohareDepMap <- filter(ohareDepMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    ohareDepMap <- filter(ohareDepMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    
    ohareDepMap <- ohareDepMap %>% add_count(DEST_STATE_NAME) # Ready for table output
    
    
    ohareDepMapUnique <- ohareDepMap[!duplicated(ohareDepMap$DEST_STATE_NAME), ]
    colnames(ohareDepMapUnique)[7] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, ohareDepMapUnique, by = "STUSPS")
    
    totalFlights <- sum(ohareDepMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 15, 30, 45, 60, 75, 90, 105, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "O\'Hare",
                         position = "bottomright") %>% syncWith("maps")
  })
  
  output$leafDay4 <- renderLeaflet({
   
    # Midway Departures
    # min = 43, max = 7577
    
    # filter dates here before moving on
    
    dayToFilter <- getDate()
    
    midwayDepMap <- filter(midwayDepMap, ymd(FL_DATE) == mdy(dayToFilter))
    
    # filter distance and airtime here if want full resolution data
    midwayDepMap <- filter(midwayDepMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    midwayDepMap <- filter(midwayDepMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    
    
    
    midwayDepMap <- midwayDepMap %>% add_count(DEST_STATE_NAME) # Ready for table output
    
    midwayDepMapUnique <- midwayDepMap[!duplicated(midwayDepMap$DEST_STATE_NAME), ]
    colnames(midwayDepMapUnique)[7] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, midwayDepMapUnique, by = "STUSPS")
    
    totalFlights <- sum(midwayDepMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 15, 30, 45, 60, 75, 90, 105, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Midway",
                         position = "bottomright") %>% syncWith("maps")
    
  })
  
  output$leafMonth1 <- renderLeaflet({
    
    # Ohare Arrivals
    # min = 34, max = 24477
    
    # filter dates here before moving on
    
    ohareArrMap <- filter(ohareArrMap, month(ymd(FL_DATE)) == as.numeric(substr(input$slider_month, 2, 3)))
    
    
    # filter distance and airtime here if want full resolution data
    ohareArrMap <- filter(ohareArrMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    ohareArrMap <- filter(ohareArrMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    ohareArrMap <- ohareArrMap %>% add_count(ORIGIN_STATE_NAME) # Ready for table output, after this step resolution lost
    
    
    ohareArrMapUnique <- ohareArrMap[!duplicated(ohareArrMap$ORIGIN_STATE_NAME), ]
    colnames(ohareArrMapUnique)[5] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, ohareArrMapUnique, by = "STUSPS")
    
    totalFlights <- sum(ohareArrMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 400, 800, 1200, 1600, 2000, 2400, 2800, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "O\'Hare",
                         position = "bottomright") %>% syncWith("maps")
    
    
  })
  
  output$leafMonth2 <- renderLeaflet({
    
    # Midway Arrivals
    # min = 43, max = 7571
    
    # filter dates here before moving on
    
    midwayArrMap <- filter(midwayArrMap, month(ymd(FL_DATE)) == as.numeric(substr(input$slider_month, 2, 3)))
    
    # filter distance and airtime here if want full resolution data
    midwayArrMap <- filter(midwayArrMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    midwayArrMap <- filter(midwayArrMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    midwayArrMap <- midwayArrMap %>% add_count(ORIGIN_STATE_NAME) # Ready for table output, after this step resolution lost
    
    
    midwayArrMapUnique <- midwayArrMap[!duplicated(midwayArrMap$ORIGIN_STATE_NAME), ]
    colnames(midwayArrMapUnique)[5] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, midwayArrMapUnique, by = "STUSPS")
    
    totalFlights <- sum(midwayArrMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 400, 800, 1200, 1600, 2000, 2400, 2800, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Midway",
                         position = "bottomright") %>% syncWith("maps")
  })
  
  output$leafMonth3 <- renderLeaflet({ 
    # Ohare Departures
    # min = 34, max = 24382
    
    # filter dates here before moving on
    
    ohareDepMap <- filter(ohareDepMap, month(ymd(FL_DATE)) == as.numeric(substr(input$slider_month, 2, 3)))
    
    # filter distance and airtime here if want full resolution data
    ohareDepMap <- filter(ohareDepMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    ohareDepMap <- filter(ohareDepMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    ohareDepMap <- ohareDepMap %>% add_count(DEST_STATE_NAME) # Ready for table output
    
    
    ohareDepMapUnique <- ohareDepMap[!duplicated(ohareDepMap$DEST_STATE_NAME), ]
    colnames(ohareDepMapUnique)[7] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, ohareDepMapUnique, by = "STUSPS")
    
    totalFlights <- sum(ohareDepMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 400, 800, 1200, 1600, 2000, 2400, 2800, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "O\'Hare",
                         position = "bottomright") %>% syncWith("maps")
    
    
    })
  
  output$leafMonth4 <- renderLeaflet({ 
    
    # Midway Departures
    # min = 43, max = 7577
    
    # filter dates here before moving on
    
    midwayDepMap <- filter(midwayDepMap, month(ymd(FL_DATE)) == as.numeric(substr(input$slider_month, 2, 3)))
    
    # filter distance and airtime here if want full resolution data
    midwayDepMap <- filter(midwayDepMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    midwayDepMap <- filter(midwayDepMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    midwayDepMap <- midwayDepMap %>% add_count(DEST_STATE_NAME) # Ready for table output
    
    midwayDepMapUnique <- midwayDepMap[!duplicated(midwayDepMap$DEST_STATE_NAME), ]
    colnames(midwayDepMapUnique)[7] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, midwayDepMapUnique, by = "STUSPS")
    
    totalFlights <- sum(midwayDepMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 400, 800, 1200, 1600, 2000, 2400, 2800, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Midway",
                         position = "bottomright") %>% syncWith("maps")
    
    })
  
  output$leafYear1 <- renderLeaflet({
    
    # Ohare Arrivals
    # min = 34, max = 24477
    
    # filter dates here before moving on
    # filter distance and airtime here if want full resolution data
    ohareArrMap <- filter(ohareArrMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    ohareArrMap <- filter(ohareArrMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    
    ohareArrMap <- ohareArrMap %>% add_count(ORIGIN_STATE_NAME) # Ready for table output, after this step resolution lost
    
    
    ohareArrMapUnique <- ohareArrMap[!duplicated(ohareArrMap$ORIGIN_STATE_NAME), ]
    colnames(ohareArrMapUnique)[5] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, ohareArrMapUnique, by = "STUSPS")
    
    totalFlights <- sum(ohareArrMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 3000, 6000, 9000, 12000, 15000, 18000, 21000, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "O\'Hare",
                         position = "bottomright") %>% syncWith("maps")
    
    
    
    
    # allStatesArrivals <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
    #   separate(ORIGIN_CITY_NAME, c("ORIGIN_CITY", "ORIGIN_STATE"), sep = ",")
    # 
    # statesData <- allStatesArrivals %>% group_by(ORIGIN_STATE) %>% summarise(n = n()) %>% mutate(freq = (n / sum(n)) * 100)
    # 
    # colnames(statesData) <- c("STUSPS", "value")
    # 
    # statesWData <- sp::merge(states, statesData, by = "STUSPS")
    # 
    # # # try CartoDB.Positron
    # m <- leaflet(statesWData) %>%
    #   setView(-96, 37.8, 4) %>%
    #   addProviderTiles(providers$Stamen.TonerLite,
    #                    options = providerTileOptions(noWrap = TRUE))
    # 
    # bins <- c(0, 50, 100, 500, 1000, 5000, 10000, 25000, Inf)
    # pal <- colorBin("YlOrRd", domain = statesWData$Flights, bins = bins)
    # 
    # labels <- sprintf(
    #   "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
    #   statesWData$NAME, statesWData$Flights
    # ) %>% lapply(htmltools::HTML)
    # 
    # m <- m %>% addPolygons(
    #   fillColor = ~pal(statesWData$Flights),
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
    #   leaflet::addLegend(pal = pal, values = ~Flights, opacity = 0.7, title = NULL,
    #                      position = "bottomright") %>% syncWith("maps")
    })
  
  output$leafYear2 <- renderLeaflet({ 
    
    
    # Midway Arrivals
    # min = 43, max = 7571
    
    # filter dates here before moving on
    # filter distance and airtime here if want full resolution data
    midwayArrMap <- filter(midwayArrMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    midwayArrMap <- filter(midwayArrMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    midwayArrMap <- midwayArrMap %>% add_count(ORIGIN_STATE_NAME) # Ready for table output, after this step resolution lost
    
    
    midwayArrMapUnique <- midwayArrMap[!duplicated(midwayArrMap$ORIGIN_STATE_NAME), ]
    colnames(midwayArrMapUnique)[5] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, midwayArrMapUnique, by = "STUSPS")
    
    totalFlights <- sum(midwayArrMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 3000, 6000, 9000, 12000, 15000, 18000, 21000, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Midway",
                         position = "bottomright") %>% syncWith("maps")
    
    
    })
  
  output$leafYear3 <- renderLeaflet({
    
    # Ohare Departures
    # min = 34, max = 24382
    
    # filter dates here before moving on
    # filter distance and airtime here if want full resolution data
    ohareDepMap <- filter(ohareDepMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    ohareDepMap <- filter(ohareDepMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    ohareDepMap <- ohareDepMap %>% add_count(DEST_STATE_NAME) # Ready for table output
    
    
    ohareDepMapUnique <- ohareDepMap[!duplicated(ohareDepMap$DEST_STATE_NAME), ]
    colnames(ohareDepMapUnique)[7] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, ohareDepMapUnique, by = "STUSPS")
    
    totalFlights <- sum(ohareDepMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 3000, 6000, 9000, 12000, 15000, 18000, 21000, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "O\'Hare",
                         position = "bottomright") %>% syncWith("maps")
    
  })
  
  output$leafYear4 <- renderLeaflet({ 
    
    # Midway Departures
    # min = 43, max = 7577
    
    # filter dates here before moving on
    # filter distance and airtime here if want full resolution data
    midwayDepMap <- filter(midwayDepMap, DISTANCE >= input$slider_Distance[1] & DISTANCE <= input$slider_Distance[2])
    midwayDepMap <- filter(midwayDepMap, AIR_TIME >= input$slider_AirTime[1] & AIR_TIME <= input$slider_AirTime[2])
    
    
    
    midwayDepMap <- midwayDepMap %>% add_count(DEST_STATE_NAME) # Ready for table output
    
    midwayDepMapUnique <- midwayDepMap[!duplicated(midwayDepMap$DEST_STATE_NAME), ]
    colnames(midwayDepMapUnique)[7] <- "STUSPS"
    
    statesWDataMerge <- sp::merge(states, midwayDepMapUnique, by = "STUSPS")
    
    totalFlights <- sum(midwayDepMapUnique$n)
    
    
    
    # # try CartoDB.Positron
    m <- leaflet(statesWDataMerge) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
    bins <- c(0, 3000, 6000, 9000, 12000, 15000, 18000, 21000, Inf)
    pal <- colorBin("YlOrRd", domain = statesWDataMerge$n, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g flights<br/>%g percent",
      statesWDataMerge$NAME, statesWDataMerge$n, round((statesWDataMerge$n / totalFlights * 100), digits = 2)
    ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(statesWDataMerge$n),
      weight = 2,
      opacity = 1,
      color = "black",
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
      leaflet::addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Midway",
                         position = "bottomright") %>% syncWith("maps")
    
    
    })
  

  ######
  
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
      #curMonth <- month(mdy(getDate()))
      filter(allFlights24, month(FL_DATE) == as.numeric(substr(input$slider_month, 2, 3)))
    }
    else
    {
      #curMonth <- month(mdy(getDate()))
      filter(allFlights12, month(FL_DATE) == as.numeric(substr(input$slider_month, 2, 3)))
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
      curDate <- mdy(getDate())
      filter(allFlights24, date(FL_DATE) == curDate)
    }
    else
    {
      curDate <- mdy(getDate())
      filter(allFlights12, date(FL_DATE) == curDate)
    }
  })
  oneWeekdayReactive <- reactive({
    if (input$HourFormat)
    {
      curWeekday <- weekdays(mdy(getDate()))
      filter(allFlights24, weekdays(FL_DATE) == curWeekday)
    }
    else
    {
      curWeekday <- weekdays(mdy(getDate()))
      filter(allFlights12, weekdays(FL_DATE) == curWeekday)
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
  
  #Plots----
  
    # C====
    # C1: Total # of Departures & Arrivals (Airlines)
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
      
      # Common Scale
      if (input$checkbox_scale)
      {
        arrivals2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International") %>%
          group_by(CARRIER_NAME) %>%
          summarise(freq = n())
        
        departures2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>%
          group_by(CARRIER_NAME) %>%
          summarise(freq = n())
        
        maxY <- max(arrivals$Arrivals, arrivals2$freq, departures$Departures, departures2$freq)
      }
      else
      {
        maxY <- max(arrivals$Arrivals, departures$Departures)
      }
      
      
      # Plot
      ggplot(data = airlineArrDepMelt, aes(x = Airline,
                                           y = value,
                                           fill = variable,
                                           text = value)) +
        labs(title = "O'hare Arrivals vs. Departures", x = "Airline", y = "Num. of Flights", fill = "") +
        geom_bar(stat = "identity", position = "dodge") +
        expand_limits(0, 0) + 
        scale_y_continuous(expand = c(0, 0)) +
        scale_fill_brewer(palette = "Set1") + 
        ylim(0, maxY) +
        coord_flip() +
        themeTest
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
      
      # Common Scale
      if (input$checkbox_scale)
      {
        arrivals2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(CARRIER_NAME) %>%
          summarise(freq = n())
        
        departures2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(CARRIER_NAME) %>%
          summarise(freq = n())
        
        maxY <- max(arrivals$Arrivals, arrivals2$freq, departures$Departures, departures2$freq)
      }
      else
      {
        maxY <- max(arrivals$Arrivals, departures$Departures)
      }
      
      
      # Plot
      ggplot(data = airlineArrDepMelt, aes(x = Airline,
                                           y = value,
                                           fill = variable,
                                           text = value)) +
        labs(title = "Midway Arrivals vs. Departures", x = "Airline", y = "Num. of Flights", fill = "") +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_brewer(palette = "Set1") + 
        ylim(0, maxY) + 
        coord_flip() +
        themeTest
    })
    
    # C2: Total # of Departures & Arrivals (Hourly)
    output$OhareHourlyArrDep <- renderPlotly({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      #Add Delays Line:
      delays <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago O\'Hare International") %>%
        select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
        group_by(DEP_TIME) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
        na.omit()
      
      colnames(delays) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
      
      delays <- join_all(list(hourFormat, delays), by = "Hour", type = "full")
      delays[is.na(delays)] = 0

      
      # Combine and Melt:
      hourlyArrDep <- join_all(list(hourFormat, arrivals, departures), by = "Hour", type = "full")
      hourlyArrDep[is.na(hourlyArrDep)] = 0

      hourlyArrDep$Delays <- delays$Total
      
      hourlyArrDep$Hour <- ordered(hourlyArrDep$Hour, levels = hourFormat[,])
      
      hourlyArrDepMelt <- melt(hourlyArrDep, id.vars = "Hour")
      

      
      # Common Scale
      if (input$checkbox_scale)
      {
        arrivals2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International") %>%
          group_by(ARR_TIME) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        departures2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>%
          group_by(DEP_TIME) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        maxY <- max(arrivals$Arrivals, arrivals2$freq, departures$Departures, departures2$freq, delays$Total)
      }
      else
      {
        maxY <- max(arrivals$Arrivals, departures$Departures, delays$Total)
      }
      
      # Plot
      ggplotly(ggplot(data = hourlyArrDepMelt, aes(x = Hour,
                                          y = value,
                                          group = variable,
                                          color = variable,
                                          text = value)) +
        labs(title = "O'hare Hourly Arrivals vs. Departures", x = "Time of Day (Hour)", y = "Num. of Flights", color = "") +
        geom_point() +
          
          themeTest +
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY), tooltip = c("x", "text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$MidwayHourlyArrDep <- renderPlotly({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      #Add Delays Line:
      delays <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago Midway International") %>%
        select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
        group_by(DEP_TIME) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
        na.omit()
      
      colnames(delays) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
      
      delays <- join_all(list(hourFormat, delays), by = "Hour", type = "full")
      delays[is.na(delays)] = 0
      
      # Combine and Melt:
      hourlyArrDep <- join_all(list(hourFormat, arrivals, departures), by = "Hour", type = "full")
      hourlyArrDep[is.na(hourlyArrDep)] = 0
      
      hourlyArrDep$Delays <- delays$Total
      
      hourlyArrDep$Hour <- ordered(hourlyArrDep$Hour, levels = hourFormat[,])
      
      hourlyArrDepMelt <- melt(hourlyArrDep, id.vars = "Hour")
      
      # Common Scale
      if (input$checkbox_scale)
      {
        arrivals2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(ARR_TIME) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        departures2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(DEP_TIME) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        maxY <- max(arrivals$Arrivals, arrivals2$freq, departures$Departures, departures2$freq, delays$Total)
      }
      else
      {
        maxY <- max(arrivals$Arrivals, departures$Departures, delays$Total)
      }
      
      # Plot
      ggplotly(ggplot(data = hourlyArrDepMelt, aes(x = Hour,
                                          y = value,
                                          group = variable,
                                          color = variable,
                                          text = value)) +
        labs(title = "Midway Hourly Arrivals vs. Departures", x = "Time of Day (Hour)", y = "Num. of Flights", color = "") +
        geom_point() +
          themeTest +
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY), tooltip = c("x", "text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    
    # C3: Total # of Departures & Arrivals (Weekly)
    output$OhareWeeklyArrDep <- renderPlotly({
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
      
      # Common Scale
      if (input$checkbox_scale)
      {
        arrivals2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International") %>%
          group_by(weekdays(FL_DATE)) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        departures2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>%
          group_by(weekdays(FL_DATE)) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        maxY <- max(weeklyArrDep$Arrivals, arrivals2$freq, weeklyArrDep$Departures, departures2$freq)
      }
      else
      {
        maxY <- max(arrivals$Arrivals, departures$Departures)
      }
      
      # Plot
      ggplotly(ggplot(data = hourlyArrDepMelt, aes(x = Day,
                                          y = value,
                                          group = variable,
                                          color = variable,
                                          text = value)) +
        labs(title = "O'hare Weekly Arrivals vs. Departures", x = "Day of Week", y = "Num. of Flights", color = "") +
        geom_point() +
          themeTest +
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY), tooltip = c("x", "text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$MidwayWeeklyArrDep <- renderPlotly({
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
      
      # Common Scale
      if (input$checkbox_scale)
      {
        arrivals2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(weekdays(FL_DATE)) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        departures2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(weekdays(FL_DATE)) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        maxY <- max(weeklyArrDep$Arrivals, arrivals2$freq, weeklyArrDep$Departures, departures2$freq)
      }
      else
      {
        maxY <- max(arrivals$Arrivals, departures$Departures)
      }
      
      # Plot
      ggplotly(ggplot(data = hourlyArrDepMelt, aes(x = Day,
                                          y = value,
                                          group = variable,
                                          color = variable,
                                          text = value)) +
        labs(title = "Midway Weekly Arrivals vs. Departures", x = "Day of Week", y = "Num. of Flights", color = "") +
        geom_point() +
          themeTest +
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY), tooltip = c("x", "text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    
    # C4: Total # of Delays (Hourly)
    output$OhareHourlyDelays <- renderPlotly({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      # Common Scale
      if (input$checkbox_scale)
      {
        delays2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") %>%
          select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
          filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
          mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
          group_by(DEP_TIME) %>%
          summarise_each(funs(sum)) %>%
          mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY)
        
        colnames(delays2) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
        
        maxY <- max(delays$Carrier, delays$Weather, delays$NAS, delays$Security, delays$Aircraft, delays$Total, 
                    delays2$Carrier, delays2$Weather, delays2$NAS, delays2$Security, delays2$Aircraft, delays2$Total)
      }
      else
      {
        maxY <- max(delays$Carrier, delays$Weather, delays$NAS, delays$Security, delays$Aircraft, delays$Total)
      }
      
      # Plot
      ggplotly(ggplot(data = delaysMelt, aes(x = Hour, 
                                    y = value,
                                    group = variable,
                                    color = variable,
                                    text = value)) + 
        labs(title = "Ohare Delays", x = "Hour", y = "Num. of Delays", color = "Delay Type") +
        geom_point() +
          themeTest + 
        geom_line(size = 1.5, alpha = 0.7) +
        ylim(0, maxY), tooltip = c("x", "text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$MidwayHourlyDelays <- renderPlotly({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      # Common Scale
      if (input$checkbox_scale)
      {
        delays2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
          select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
          filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
          mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
          group_by(DEP_TIME) %>%
          summarise_each(funs(sum)) %>%
          mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY)
        
        colnames(delays2) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
        
        maxY <- max(delays$Carrier, delays$Weather, delays$NAS, delays$Security, delays$Aircraft, delays$Total, 
                    delays2$Carrier, delays2$Weather, delays2$NAS, delays2$Security, delays2$Aircraft, delays2$Total)
      }
      else
      {
        maxY <- max(delays$Carrier, delays$Weather, delays$NAS, delays$Security, delays$Aircraft, delays$Total)
      }
      
      # Plot
      ggplotly(ggplot(data = delaysMelt, aes(x = Hour, 
                                    y = value,
                                    group = variable,
                                    color = variable,
                                    text = value)) + 
        labs(title = "Midway Delays", x = "Hour", y = "Num. of Delays", color = "Delay Type") +
        geom_point() +
          themeTest + 
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY), tooltip = c("x", "text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    
    # C5: Total # of Flights (Top 15 Arrival/Destination Airports)
    output$OhareMostCommonArrivalAirports <- renderPlot({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      arrivalAirports <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
        group_by(ORIGIN_AIRPORT) %>%
        summarise(freq = n()) %>%
        arrange(desc(freq)) %>%
        top_n(15)
      
      colnames(arrivalAirports) <- c("Airport", "Flights")
      
      if (input$checkbox_scale)
      {
        arrivalAirports2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International") %>%
          group_by(ORIGIN_AIRPORT) %>%
          summarise(freq = n()) %>%
          arrange(desc(freq)) %>%
          top_n(15)
        
        colnames(arrivalAirports2) <- c("Airport", "Flights")
        
        maxY <- max(arrivalAirports$Flights, arrivalAirports2$Flights)
      }
      else
      {
        maxY <- max(arrivalAirports$Flights)
      }
      
      # Plot
      ggplot(data = arrivalAirports, aes(x = reorder(Airport, Flights), 
                                         y = Flights)) + 
        labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
        geom_bar(stat = "identity", fill = "red") + 
        guides(fill = FALSE) +
        coord_flip() + 
        ylim(0, maxY) +
        themeTest
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
      
      if (input$checkbox_scale)
      {
        destinationAirports2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>%
          group_by(DEST_AIRPORT) %>%
          summarise(freq = n()) %>%
          arrange(desc(freq)) %>%
          top_n(15)
        
        colnames(destinationAirports2) <- c("Airport", "Flights")
        
        maxY <- max(destinationAirports$Flights, destinationAirports2$Flights)
      }
      else
      {
        maxY <- max(destinationAirports$Flights)
      }
      
      # Plot
      ggplot(data = destinationAirports, aes(x = reorder(Airport, Flights), 
                                             y = Flights)) + 
        labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
        geom_bar(stat = "identity", fill = "blue") + 
        guides(fill = FALSE) +
        coord_flip() +
        ylim(0, maxY) +
        themeTest
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
      
      if (input$checkbox_scale)
      {
        arrivalAirports2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(ORIGIN_AIRPORT) %>%
          summarise(freq = n()) %>%
          arrange(desc(freq)) %>%
          top_n(15)
        
        colnames(arrivalAirports2) <- c("Airport", "Flights")
        
        maxY <- max(arrivalAirports$Flights, arrivalAirports2$Flights)
      }
      else
      {
        maxY <- max(arrivalAirports$Flights)
      }
      
      # Plot
      ggplot(data = arrivalAirports, aes(x = reorder(Airport, Flights), 
                                         y = Flights)) + 
        labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
        geom_bar(stat = "identity", fill = "red") + 
        guides(fill = FALSE) +
        coord_flip() +
        ylim(0, maxY) +
        themeTest
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
      
      if (input$checkbox_scale)
      {
        destinationAirports2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(DEST_AIRPORT) %>%
          summarise(freq = n()) %>%
          arrange(desc(freq)) %>%
          top_n(15)
        
        colnames(destinationAirports2) <- c("Airport", "Flights")
        
        maxY <- max(destinationAirports$Flights, destinationAirports2$Flights)
      }
      else
      {
        maxY <- max(destinationAirports$Flights)
      }
      
      # Plot
      ggplot(data = destinationAirports, aes(x = reorder(Airport, Flights), 
                                             y = Flights)) + 
        labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
        geom_bar(stat = "identity", fill = "blue") + 
        guides(fill = FALSE) +
        coord_flip() +
        ylim(0, maxY) +
        themeTest
    })
    
    
    
    
    
    
    # B====
    
    # B1: Total # Departures/Arrivals per Airline (Monthly)
    output$Ohare1YearAirlinesArrivals <- renderPlotly({
      
      arrivals1 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jan = n())
      arrivals2 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Feb = n())
      arrivals3 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Mar = n())
      arrivals4 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Apr = n())
      arrivals5 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
      arrivals6 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jun = n())
      arrivals7 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jul = n())
      arrivals8 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Aug = n())
      arrivals9 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Sep = n())
      arrivals10 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Oct = n())
      arrivals11 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Nov = n())
      arrivals12 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Dec = n())
      
      arrivals <- join_all(list(airlines, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "Airline")
      arrivals[is.na(arrivals)] = 0
      
      arrivalsMelt <- melt(arrivals)
      
      ggplotly(ggplot(data = arrivalsMelt, aes(x = variable,
                                      y = Airline,
                                      text = value)) +
        geom_tile(aes(fill = arrivalsMelt$value)) +
          themeTest +
        # scale_fill_gradient(na.value = "#bfbfbf",low = "#85aef2", high = "#001a44") + 
          scale_fill_gradient(low = "#66ff88", high = "#006616") +
        labs(title = "O'hare Arrivals by Airline(2017)", x = "Month", y = "Hour", color = ""), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Ohare1YearAirlinesDepartures <- renderPlotly({
      
      departures1 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jan = n())
      departures2 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Feb = n())
      departures3 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Mar = n())
      departures4 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Apr = n())
      departures5 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
      departures6 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jun = n())
      departures7 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jul = n())
      departures8 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Aug = n())
      departures9 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Sep = n())
      departures10 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Oct = n())
      departures11 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Nov = n())
      departures12 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Dec = n())
      
      departures <- join_all(list(airlines, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "Airline")
      departures[is.na(departures)] = 0
      
      departuresMelt <- melt(departures)
      
      ggplotly(ggplot(data = departuresMelt, aes(x = variable,
                                        y = Airline,
                                        text = value)) +
        geom_tile(aes(fill = departuresMelt$value)) +
          themeTest +
        # scale_fill_gradient(na.value = "#bfbfbf",low = "#7bf7c5", high = "#004f2f") + 
          scale_fill_gradient(low = "#66ff88", high = "#006616")+
        labs(title = "O'hare Departures by Airline(2017)", x = "Month", y = "Hour", color = ""), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Midway1YearAirlinesArrivals <- renderPlotly({
      
      arrivals1 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jan = n())
      arrivals2 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Feb = n())
      arrivals3 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Mar = n())
      arrivals4 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Apr = n())
      arrivals5 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
      arrivals6 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jun = n())
      arrivals7 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jul = n())
      arrivals8 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Aug = n())
      arrivals9 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Sep = n())
      arrivals10 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Oct = n())
      arrivals11 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Nov = n())
      arrivals12 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Dec = n())
      
      arrivals <- join_all(list(airlines, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "Airline")
      arrivals[is.na(arrivals)] = 0
      
      arrivalsMelt <- melt(arrivals)
      
      #Common Scale
      if (input$checkbox_scale)
      {
        maxY <- 8190
      }
      else
      {
        maxY <- 7747
      }
      
      ggplotly(ggplot(data = arrivalsMelt, aes(x = variable,
                                      y = Airline,
                                      text = value)) +
        geom_tile(aes(fill = arrivalsMelt$value)) +
          themeTest +
        labs(title = "Midway Arrivals by Airline(2017)", x = "Month", y = "Hour", color = "") +
          
        scale_fill_gradient(limits = c(0, maxY), na.value = "#bfbfbf",low = "#66ff88", high = "#006616"), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Midway1YearAirlinesDepartures <- renderPlotly({
      
      departures1 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jan = n())
      departures2 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Feb = n())
      departures3 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Mar = n())
      departures4 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Apr = n())
      departures5 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
      departures6 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jun = n())
      departures7 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jul = n())
      departures8 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Aug = n())
      departures9 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Sep = n())
      departures10 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Oct = n())
      departures11 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Nov = n())
      departures12 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Dec = n())
      
      departures <- join_all(list(airlines, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "Airline")
      departures[is.na(departures)] = 0
      
      departuresMelt <- melt(departures)
      
      #Common Scale
      if (input$checkbox_scale)
      {
        maxY <- 8183
      }
      else
      {
        maxY <- 7747
      }
      
      ggplotly(ggplot(data = departuresMelt, aes(x = variable,
                                        y = Airline,
                                        text = value)) +
        geom_tile(aes(fill = departuresMelt$value)) +
          themeTest +
        scale_fill_gradient(limits = c(0, maxY), na.value = "#bfbfbf",low = "#7bf7c5", high = "#004f2f") + 
        labs(title = "Midway Departures by Airline(2017)", x = "Month", y = "Hour", color = ""), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    
    # B2: Total # Departures/Arrivals (Hourly + Monthly)
    output$Ohare1YearHourlyArrivals <- renderPlotly({
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
        allFlights <- allFlights24
      }
      else
      {
        hourFormat <- hours12
        allFlights <- allFlights12
      }
      
      # Filter Arrivals
      arrivals1 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(Jan = n()) %>% na.omit()
      arrivals2 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(Feb = n()) %>% na.omit()
      arrivals3 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(Mar = n()) %>% na.omit()
      arrivals4 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(Apr = n()) %>% na.omit()
      arrivals5 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
      arrivals6 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(Jun = n()) %>% na.omit()
      arrivals7 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(Jul = n()) %>% na.omit()
      arrivals8 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(Aug = n()) %>% na.omit()
      arrivals9 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(Sep = n()) %>% na.omit()
      arrivals10 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(Oct = n()) %>% na.omit()
      arrivals11 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(Nov = n()) %>% na.omit()
      arrivals12 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("ARR_TIME")
      allArrivals <- join_all(list(hourFormat, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "ARR_TIME", type = "full")
      allArrivals[is.na(allArrivals)] = 0
      
      allArrivals$ARR_TIME <- ordered(allArrivals$ARR_TIME, levels = hourFormat[,])
      
      allArrivalsMelt <- melt(allArrivals)
      
      if (input$checkbox_scale)
      {
        maxY <- 2076
      }
      else
      {
        maxY <- 1777
      }
      
      ggplotly(ggplot(data = allArrivalsMelt, aes(x = variable,
                                         y = ARR_TIME,
                                         text = value)) +
        geom_tile(aes(fill = allArrivalsMelt$value)) +
          themeTest +
        scale_fill_gradient(limits = c(0, maxY), low = "#66ff88", high = "#006616") + 
        labs(title = "O'hare Arrivals (2017)", x = "Month", y = "Hour", fill = ""), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Ohare1YearHourlyDepartures <- renderPlotly({
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
        allFlights <- allFlights24
      }
      else
      {
        hourFormat <- hours12
        allFlights <- allFlights12
      }
      
      # Filter Departures
      departures1 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(Jan = n()) %>% na.omit()
      departures2 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(Feb = n()) %>% na.omit()
      departures3 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(Mar = n()) %>% na.omit()
      departures4 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(Apr = n()) %>% na.omit()
      departures5 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
      departures6 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(Jun = n()) %>% na.omit()
      departures7 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(Jul = n()) %>% na.omit()
      departures8 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(Aug = n()) %>% na.omit()
      departures9 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(Sep = n()) %>% na.omit()
      departures10 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(Oct = n()) %>% na.omit()
      departures11 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(Nov = n()) %>% na.omit()
      departures12 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("DEP_TIME")
      allDepartures <- join_all(list(hourFormat, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "DEP_TIME", type = "full")
      allDepartures[is.na(allDepartures)] = 0
      
      allDepartures$DEP_TIME <- ordered(allDepartures$DEP_TIME, levels = hourFormat[,])
      
      allDeparturesMelt <- melt(allDepartures)
      
      ggplotly(ggplot(data = allDeparturesMelt, aes(x = variable,
                                           y = DEP_TIME,
                                           text = value)) +
        geom_tile(aes(fill = allDeparturesMelt$value)) +
          themeTest +
        scale_fill_gradient(low = "#85aef2", high = "#001a44") + 
        labs(title = "O'hare Departures (2017)", x = "Month", y = "Hour", fill = ""), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Midway1YearHourlyArrivals <- renderPlotly({
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
        allFlights <- allFlights24
      }
      else
      {
        hourFormat <- hours12
        allFlights <- allFlights12
      }
      
      # Filter Arrivals
      arrivals1 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(Jan = n()) %>% na.omit()
      arrivals2 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(Feb = n()) %>% na.omit()
      arrivals3 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(Mar = n()) %>% na.omit()
      arrivals4 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(Apr = n()) %>% na.omit()
      arrivals5 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
      arrivals6 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(Jun = n()) %>% na.omit()
      arrivals7 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(Jul = n()) %>% na.omit()
      arrivals8 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(Aug = n()) %>% na.omit()
      arrivals9 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(Sep = n()) %>% na.omit()
      arrivals10 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(Oct = n()) %>% na.omit()
      arrivals11 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(Nov = n()) %>% na.omit()
      arrivals12 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("ARR_TIME")
      allArrivals <- join_all(list(hourFormat, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "ARR_TIME", type = "full")
      allArrivals[is.na(allArrivals)] = 0
      
      allArrivals$ARR_TIME <- ordered(allArrivals$ARR_TIME, levels = hourFormat[,])
      
      allArrivalsMelt <- melt(allArrivals)
      
      if (input$checkbox_scale)
      {
        maxY <- 2076
      }
      else
      {
        maxY <- 675
      }
      
      ggplotly(ggplot(data = allArrivalsMelt, aes(x = variable,
                                         y = ARR_TIME,
                                         text = value)) +
        geom_tile(aes(fill = allArrivalsMelt$value)) +
          themeTest +
        scale_fill_gradient(limits = c(0, maxY), low = "#66ff88", high = "#006616") + 
        labs(title = "Midway Arrivals (2017)", x = "Month", y = "Hour", fill = ""), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Midway1YearHourlyDepartures <- renderPlotly({
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
        allFlights <- allFlights24
      }
      else
      {
        hourFormat <- hours12
        allFlights <- allFlights12
      }
      
      # Filter Departures
      departures1 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(Jan = n()) %>% na.omit()
      departures2 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(Feb = n()) %>% na.omit()
      departures3 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(Mar = n()) %>% na.omit()
      departures4 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(Apr = n()) %>% na.omit()
      departures5 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
      departures6 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(Jun = n()) %>% na.omit()
      departures7 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(Jul = n()) %>% na.omit()
      departures8 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(Aug = n()) %>% na.omit()
      departures9 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(Sep = n()) %>% na.omit()
      departures10 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(Oct = n()) %>% na.omit()
      departures11 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(Nov = n()) %>% na.omit()
      departures12 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("DEP_TIME")
      allDepartures <- join_all(list(hourFormat, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "DEP_TIME", type = "full")
      allDepartures[is.na(allDepartures)] = 0
      
      allDepartures$DEP_TIME <- ordered(allDepartures$DEP_TIME, levels = hourFormat[,])
      
      allDeparturesMelt <- melt(allDepartures)
      
      if (input$checkbox_scale)
      {
        maxY <- 2076
      }
      else
      {
        maxY <- 756
      }
      
      ggplotly(ggplot(data = allDeparturesMelt, aes(x = variable,
                                           y = DEP_TIME,
                                           text = value)) +
        geom_tile(aes(fill = allDeparturesMelt$value)) +
          themeTest +
        scale_fill_gradient(limits = c(0, maxY), low = "#7bf7c5", high = "#004f2f") + 
        labs(title = "Midway Departures (2017)", x = "", y = "", fill = ""), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    
    # B3: Total # Flights for Top 15 Destinations (Monthly)
    output$Ohare1YearlMostCommon <- renderPlotly({
      
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
      
      ggplotly(ggplot(data = destinationAirports, aes(x = Month, 
                                             y = Flights,
                                             group = Airport,
                                             color = Airport,
                                             text = Flights)) + 
        labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
        geom_point() +
          themeTest +
        geom_line(size = 1.5, alpha = 0.7), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Midway1YearlMostCommon <- renderPlotly({
      
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
      
      if (input$checkbox_scale)
      {
        maxY <- 984 
      }
      else
      {
        maxY <- 426 
      }
      
      ggplotly(ggplot(data = destinationAirports, aes(x = Month, 
                                             y = Flights,
                                             group = Airport,
                                             color = Airport,
                                             text = Flights)) + 
        labs(title = "Most Common Arrival Airports", x = "Airport", y = "Flights") +
        geom_point()  +
          themeTest+
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    
    # B4: Total # Delays (Monthly)
    output$Ohare1YearDelays <- renderPlotly({
      
      delays <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
        select(FL_DATE, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -FL_DATE) %>%
        group_by(month(FL_DATE)) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY)

      delays$FL_DATE <- NULL
      colnames(delays) <- c("Month", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")

      delaysMelt <- melt(delays, id.vars = "Month")

      if (input$checkbox_scale)
      {
        delays2 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") %>%
          select(FL_DATE, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
          filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
          mutate_each(funs(replace(., . > 0, 1)), -FL_DATE) %>%
          group_by(month(FL_DATE)) %>%
          summarise_each(funs(sum)) %>%
          mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY)
        
        delays2$FL_DATE <- NULL
        colnames(delays2) <- c("Month", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
        
        delaysMelt2 <- melt(delays2, id.vars = "Month")
        
        maxY <- max(delaysMelt$value, delaysMelt2$value)
      }
      else
      {
        maxY <- max(delaysMelt$value)
      }
      
      ggplotly(ggplot(data = delaysMelt, aes(x = Month,
                                    y = value,
                                    group = variable,
                                    color = variable,
                                    text = value)) +
        labs(title = "O'hare Yearly Delays", x = "", y = "Num. of Delays", color = "Delay Type") +
        geom_point() +
          themeTest +
        geom_line(size = 1.5, alpha = 0.7) + 
        scale_x_continuous(breaks = round(seq(1, 12, by = 1),1)) +
        ylim(0, maxY), tooltip = c("x", "text")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))
        
      
      # OR
      
      # delays <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
      #   select(FL_DATE, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
      #   filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
      #   mutate_each(funs(replace(., . > 0, 1)), -FL_DATE) %>%
      #   group_by(month(FL_DATE)) %>%
      #   summarise_each(funs(sum))
      # 
      # delays$FL_DATE <- NULL
      # colnames(delays) <- c("Month", "Carrier", "Weather", "NAS", "Security", "Aircraft")
      # 
      # delaysMelt <- melt(delays, id.vars = "Month")
      # 
      # delaysMelt$Month <- as.POSIXct(sprintf("2017 %d 1", delaysMelt$Month), format = "%Y %m %d")
      # 
      # streamgraph(delaysMelt, key="variable", value="value", date="Month") %>% 
      #   sg_axis_x(tick_interval = 1, tick_units = "month", tick_format = "%m") %>%
      #   sg_legend(show = TRUE, label = "variable")
    })
    
    output$Midway1YearDelays <- renderPlotly({
      
      delays <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") %>%
        select(FL_DATE, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -FL_DATE) %>%
        group_by(month(FL_DATE)) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY)
      
      delays$FL_DATE <- NULL
      colnames(delays) <- c("Month", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
      
      delaysMelt <- melt(delays, id.vars = "Month")
      
      if (input$checkbox_scale)
      {
        delays2 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
          select(FL_DATE, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
          filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
          mutate_each(funs(replace(., . > 0, 1)), -FL_DATE) %>%
          group_by(month(FL_DATE)) %>%
          summarise_each(funs(sum)) %>%
          mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY)
        
        delays2$FL_DATE <- NULL
        colnames(delays2) <- c("Month", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
        
        delaysMelt2 <- melt(delays2, id.vars = "Month")
        
        maxY <- max(delaysMelt$value, delaysMelt2$value)
      }
      else
      {
        maxY <- max(delaysMelt$value)
      }
      
      ggplotly(ggplot(data = delaysMelt, aes(x = Month,
                                    y = value,
                                    group = variable,
                                    color = variable,
                                    text = value)) +
        labs(title = "Midway Yearly Delays", x = "", y = "Num. of Delays", color = "Delay Type") +
        geom_point() +
          themeTest +
        geom_line(size = 1.5, alpha = 0.7) + 
        scale_x_continuous(breaks = round(seq(1, 12, by = 1),1)) +
        ylim(0, maxY), tooltip = c("x", "text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
      
      # OR
      
    #   delays <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") %>%
    #     select(FL_DATE, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
    #     filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
    #     mutate_each(funs(replace(., . > 0, 1)), -FL_DATE) %>%
    #     group_by(month(FL_DATE)) %>%
    #     summarise_each(funs(sum))
    #   
    #   delays$FL_DATE <- NULL
    #   colnames(delays) <- c("Month", "Carrier", "Weather", "NAS", "Security", "Aircraft")
    #   
    #   delaysMelt <- melt(delays, id.vars = "Month")
    #   
    #   delaysMelt$Month <- as.POSIXct(sprintf("2017 %d 1", delaysMelt$Month), format = "%Y %m %d")
    #   
    #   streamgraph(delaysMelt, key="variable", value="value", date="Month") %>% 
    #     sg_axis_x(tick_interval = 1, tick_units = "month", tick_format = "%m") %>%
    #     sg_legend(show = TRUE, label = "variable")
    })
    
    
    
    # A====
    
    # A1: TARUSH VIG!?!?!
    
    
    # A2: 1 Day Departures/Arrivals per Airline (Monthly)
    output$Ohare1YearAirlineHourlyArrivals <- renderPlotly({
      
      allFlights <- oneAirlineReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
      }
      
      # Filter Arrivals
      arrivals1 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(Jan = n()) %>% na.omit()
      arrivals2 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(Feb = n()) %>% na.omit()
      arrivals3 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(Mar = n()) %>% na.omit()
      arrivals4 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(Apr = n()) %>% na.omit()
      arrivals5 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
      arrivals6 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(Jun = n()) %>% na.omit()
      arrivals7 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(Jul = n()) %>% na.omit()
      arrivals8 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(Aug = n()) %>% na.omit()
      arrivals9 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(Sep = n()) %>% na.omit()
      arrivals10 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(Oct = n()) %>% na.omit()
      arrivals11 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(Nov = n()) %>% na.omit()
      arrivals12 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("ARR_TIME")
      allArrivals <- join_all(list(hourFormat, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "ARR_TIME", type = "full")
      allArrivals[is.na(allArrivals)] = 0
      
      allArrivals$ARR_TIME <- ordered(allArrivals$ARR_TIME, levels = hourFormat[,])
      
      allArrivalsMelt <- melt(allArrivals)
      
      if (input$checkbox_scale)
      {
        arrivals1a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(January = n()) %>% na.omit()
        arrivals2a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(February = n()) %>% na.omit()
        arrivals3a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(March = n()) %>% na.omit()
        arrivals4a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(April = n()) %>% na.omit()
        arrivals5a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
        arrivals6a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(June = n()) %>% na.omit()
        arrivals7a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(July = n()) %>% na.omit()
        arrivals8a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(August = n()) %>% na.omit()
        arrivals9a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(September = n()) %>% na.omit()
        arrivals10a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(October = n()) %>% na.omit()
        arrivals11a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(November = n()) %>% na.omit()
        arrivals12a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(December = n()) %>% na.omit()
        
        arrivals1b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(January = n()) %>% na.omit()
        arrivals2b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(February = n()) %>% na.omit()
        arrivals3b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(March = n()) %>% na.omit()
        arrivals4b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(April = n()) %>% na.omit()
        arrivals5b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
        arrivals6b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(June = n()) %>% na.omit()
        arrivals7b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(July = n()) %>% na.omit()
        arrivals8b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(August = n()) %>% na.omit()
        arrivals9b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(September = n()) %>% na.omit()
        arrivals10b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(October = n()) %>% na.omit()
        arrivals11b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(November = n()) %>% na.omit()
        arrivals12b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(December = n()) %>% na.omit()
        
        maxY <- max(allArrivalsMelt$value, arrivals1a$January, arrivals2a$February, arrivals3a$March, arrivals4a$April, arrivals5a$May,
                    arrivals6a$June, arrivals7a$July, arrivals8a$August, arrivals9a$September, arrivals10a$October, arrivals11a$November, arrivals12a$December,
                    arrivals1b$January, arrivals2b$February, arrivals3b$March, arrivals4b$April, arrivals5b$May,
                    arrivals6b$June, arrivals7b$July, arrivals8b$August, arrivals9b$September, arrivals10b$October, arrivals11b$November, arrivals12b$December)
      }
      else
      {
        maxY <- max(allArrivalsMelt$value)
      }

      ggplotly(ggplot(data = allArrivalsMelt, aes(x = variable,
                                         y = ARR_TIME,
                                         text = value)) +
        geom_tile(aes(fill = allArrivalsMelt$value)) +
          themeTest +
        scale_fill_gradient(limits = c(0, maxY), low = "#66ff88", high = "#006616") + 
        labs(title = "O'hare Arrivals (2017)", x = "", y = "", fill = ""), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Ohare1YearAirlineHourlyDepartures <- renderPlotly({
      
      allFlights <- oneAirlineReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
      }
      
      # Filter Departures
      departures1 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(Jan = n()) %>% na.omit()
      departures2 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(Feb = n()) %>% na.omit()
      departures3 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(Mar = n()) %>% na.omit()
      departures4 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(Apr = n()) %>% na.omit()
      departures5 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
      departures6 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(Jun = n()) %>% na.omit()
      departures7 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(Jul = n()) %>% na.omit()
      departures8 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(Aug = n()) %>% na.omit()
      departures9 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(Sep = n()) %>% na.omit()
      departures10 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(Oct = n()) %>% na.omit()
      departures11 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(Nov = n()) %>% na.omit()
      departures12 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("DEP_TIME")
      allDepartures <- join_all(list(hourFormat, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "DEP_TIME", type = "full")
      allDepartures[is.na(allDepartures)] = 0
      
      allDepartures$DEP_TIME <- ordered(allDepartures$DEP_TIME, levels = hourFormat[,])
      
      allDeparturesMelt <- melt(allDepartures)
      
      if (input$checkbox_scale)
      {
        arrivals1a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(January = n()) %>% na.omit()
        arrivals2a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(February = n()) %>% na.omit()
        arrivals3a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(March = n()) %>% na.omit()
        arrivals4a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(April = n()) %>% na.omit()
        arrivals5a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
        arrivals6a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(June = n()) %>% na.omit()
        arrivals7a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(July = n()) %>% na.omit()
        arrivals8a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(August = n()) %>% na.omit()
        arrivals9a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(September = n()) %>% na.omit()
        arrivals10a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(October = n()) %>% na.omit()
        arrivals11a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(November = n()) %>% na.omit()
        arrivals12a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(December = n()) %>% na.omit()
        
        arrivals1b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(January = n()) %>% na.omit()
        arrivals2b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(February = n()) %>% na.omit()
        arrivals3b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(March = n()) %>% na.omit()
        arrivals4b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(April = n()) %>% na.omit()
        arrivals5b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
        arrivals6b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(June = n()) %>% na.omit()
        arrivals7b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(July = n()) %>% na.omit()
        arrivals8b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(August = n()) %>% na.omit()
        arrivals9b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(September = n()) %>% na.omit()
        arrivals10b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(October = n()) %>% na.omit()
        arrivals11b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(November = n()) %>% na.omit()
        arrivals12b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(December = n()) %>% na.omit()
        
        maxY <- max(allDeparturesMelt$value, arrivals1a$January, arrivals2a$February, arrivals3a$March, arrivals4a$April, arrivals5a$May,
                    arrivals6a$June, arrivals7a$July, arrivals8a$August, arrivals9a$September, arrivals10a$October, arrivals11a$November, arrivals12a$December,
                    arrivals1b$January, arrivals2b$February, arrivals3b$March, arrivals4b$April, arrivals5b$May,
                    arrivals6b$June, arrivals7b$July, arrivals8b$August, arrivals9b$September, arrivals10b$October, arrivals11b$November, arrivals12b$December)
      }
      else
      {
        maxY <- max(allDeparturesMelt$value)
      }
      
      ggplotly(ggplot(data = allDeparturesMelt, aes(x = variable,
                                           y = DEP_TIME,
                                           text = value)) +
        geom_tile(aes(fill = allDeparturesMelt$value)) +
          themeTest +
        scale_fill_gradient(limits = c(0, maxY), low = "#85aef2", high = "#001a44") + 
        labs(title = "O'hare Departures (2017)", x = "", y = "", fill = ""), tooltip = c("text")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Midway1YearAirlineHourlyArrivals <- renderPlotly({
      
      allFlights <- oneAirlineReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
      }
      
      # Filter Arrivals
      arrivals1 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(Jan= n()) %>% na.omit()
      arrivals2 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(Feb = n()) %>% na.omit()
      arrivals3 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(Mar= n()) %>% na.omit()
      arrivals4 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(Apr = n()) %>% na.omit()
      arrivals5 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
      arrivals6 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(Jun = n()) %>% na.omit()
      arrivals7 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(Jul = n()) %>% na.omit()
      arrivals8 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(Aug = n()) %>% na.omit()
      arrivals9 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(Sep = n()) %>% na.omit()
      arrivals10 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(Oct = n()) %>% na.omit()
      arrivals11 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(Nov = n()) %>% na.omit()
      arrivals12 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("ARR_TIME")
      allArrivals <- join_all(list(hourFormat, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "ARR_TIME", type = "full")
      allArrivals[is.na(allArrivals)] = 0
      
      allArrivals$ARR_TIME <- ordered(allArrivals$ARR_TIME, levels = hourFormat[,])
      
      allArrivalsMelt <- melt(allArrivals)
      
      if (input$checkbox_scale)
      {
        arrivals1a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(January = n()) %>% na.omit()
        arrivals2a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(February = n()) %>% na.omit()
        arrivals3a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(March = n()) %>% na.omit()
        arrivals4a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(April = n()) %>% na.omit()
        arrivals5a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
        arrivals6a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(June = n()) %>% na.omit()
        arrivals7a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(July = n()) %>% na.omit()
        arrivals8a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(August = n()) %>% na.omit()
        arrivals9a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(September = n()) %>% na.omit()
        arrivals10a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(October = n()) %>% na.omit()
        arrivals11a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(November = n()) %>% na.omit()
        arrivals12a <- allFlights %>% filter((DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International" | DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(December = n()) %>% na.omit()
        
        arrivals1b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(January = n()) %>% na.omit()
        arrivals2b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(February = n()) %>% na.omit()
        arrivals3b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(March = n()) %>% na.omit()
        arrivals4b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(April = n()) %>% na.omit()
        arrivals5b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
        arrivals6b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(June = n()) %>% na.omit()
        arrivals7b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(July = n()) %>% na.omit()
        arrivals8b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(August = n()) %>% na.omit()
        arrivals9b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(September = n()) %>% na.omit()
        arrivals10b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(October = n()) %>% na.omit()
        arrivals11b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(November = n()) %>% na.omit()
        arrivals12b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(December = n()) %>% na.omit()
        
        maxY <- max(allArrivalsMelt$value, arrivals1a$January, arrivals2a$February, arrivals3a$March, arrivals4a$April, arrivals5a$May,
                    arrivals6a$June, arrivals7a$July, arrivals8a$August, arrivals9a$September, arrivals10a$October, arrivals11a$November, arrivals12a$December,
                    arrivals1b$January, arrivals2b$February, arrivals3b$March, arrivals4b$April, arrivals5b$May,
                    arrivals6b$June, arrivals7b$July, arrivals8b$August, arrivals9b$September, arrivals10b$October, arrivals11b$November, arrivals12b$December)
      }
      else
      {
        maxY <- max(allArrivalsMelt$value)
      }
      
      ggplotly(ggplot(data = allArrivalsMelt, aes(x = variable,
                                         y = ARR_TIME,
                                         text = value)) +
        geom_tile(aes(fill = allArrivalsMelt$value)) +
          themeTest +
        scale_fill_gradient(limits = c(0, maxY), low = "#66ff88", high = "#006616") + 
        labs(title = "Midway Arrivals (2017)", x = "", y = "", fill = ""), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Midway1YearAirlineHourlyDepartures <- renderPlotly({
      
      allFlights <- oneAirlineReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
      }
      
      # Filter Departures
      departures1 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(Jan = n()) %>% na.omit()
      departures2 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(Feb = n()) %>% na.omit()
      departures3 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(Mar = n()) %>% na.omit()
      departures4 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(Apr = n()) %>% na.omit()
      departures5 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
      departures6 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(Jun = n()) %>% na.omit()
      departures7 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(Jul = n()) %>% na.omit()
      departures8 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(Aug = n()) %>% na.omit()
      departures9 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(Sep = n()) %>% na.omit()
      departures10 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(Oct = n()) %>% na.omit()
      departures11 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(Nov = n()) %>% na.omit()
      departures12 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("DEP_TIME")
      allDepartures <- join_all(list(hourFormat, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "DEP_TIME", type = "full")
      allDepartures[is.na(allDepartures)] = 0
      
      allDepartures$DEP_TIME <- ordered(allDepartures$DEP_TIME, levels = hourFormat[,])
      
      allDeparturesMelt <- melt(allDepartures)
      
      if (input$checkbox_scale)
      {
        arrivals1a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(January = n()) %>% na.omit()
        arrivals2a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(February = n()) %>% na.omit()
        arrivals3a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(March = n()) %>% na.omit()
        arrivals4a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(April = n()) %>% na.omit()
        arrivals5a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
        arrivals6a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(June = n()) %>% na.omit()
        arrivals7a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(July = n()) %>% na.omit()
        arrivals8a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(August = n()) %>% na.omit()
        arrivals9a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(September = n()) %>% na.omit()
        arrivals10a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(October = n()) %>% na.omit()
        arrivals11a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(November = n()) %>% na.omit()
        arrivals12a <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(December = n()) %>% na.omit()
        
        arrivals1b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(January = n()) %>% na.omit()
        arrivals2b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(February = n()) %>% na.omit()
        arrivals3b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(March = n()) %>% na.omit()
        arrivals4b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(April = n()) %>% na.omit()
        arrivals5b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
        arrivals6b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(June = n()) %>% na.omit()
        arrivals7b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(July = n()) %>% na.omit()
        arrivals8b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(August = n()) %>% na.omit()
        arrivals9b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(September = n()) %>% na.omit()
        arrivals10b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(October = n()) %>% na.omit()
        arrivals11b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(November = n()) %>% na.omit()
        arrivals12b <- allFlights %>% filter((DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International" | DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(December = n()) %>% na.omit()
        
        maxY <- max(allDeparturesMelt$value, arrivals1a$January, arrivals2a$February, arrivals3a$March, arrivals4a$April, arrivals5a$May,
                    arrivals6a$June, arrivals7a$July, arrivals8a$August, arrivals9a$September, arrivals10a$October, arrivals11a$November, arrivals12a$December,
                    arrivals1b$January, arrivals2b$February, arrivals3b$March, arrivals4b$April, arrivals5b$May,
                    arrivals6b$June, arrivals7b$July, arrivals8b$August, arrivals9b$September, arrivals10b$October, arrivals11b$November, arrivals12b$December)
      }
      else
      {
        maxY <- max(allDeparturesMelt$value)
      }
      
      ggplotly(ggplot(data = allDeparturesMelt, aes(x = variable,
                                           y = DEP_TIME,
                                           text = value)) +
        geom_tile(aes(fill = allDeparturesMelt$value)) +
          themeTest +
        scale_fill_gradient(limits = c(0, maxY), low = "#7bf7c5", high = "#004f2f") + 
        labs(title = "Midway Departures (2017)", x = "", y = "", fill = ""), tooltip = c("text")) %>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    
    
    
    
    
    
    # A3: 1 Day Departures/Arrivals (Hourly) + Delays?
    output$Ohare1DayHourlyArrDep <- renderPlotly({
      # Select Proper Month:
      oneMonth <- oneDayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      delays <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
        select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
        group_by(DEP_TIME) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
        na.omit()
      
      colnames(delays) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
      
      delays <- join_all(list(hourFormat, delays), by = "Hour", type = "full")
      delays[is.na(delays)] = 0
      
      hourlyArrDep <- join_all(list(hourFormat, arrivals, departures), by = "Hour", type = "full")
      hourlyArrDep[is.na(hourlyArrDep)] = 0
      
      hourlyArrDep$Hour <- ordered(hourlyArrDep$Hour, levels = hourFormat[,])
      
      hourlyArrDep$Total <- delays$Total
      
      hourlyArrDepMelt <- melt(hourlyArrDep, id.vars = "Hour")
      
      #Common Scale
      if (input$checkbox_scale)
      {
        arrivals2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International") %>%
          group_by(ARR_TIME) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        departures2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>%
          group_by(DEP_TIME) %>%
          summarise(n()) %>%
          na.omit()
        
        colnames(arrivals2) <- c("Hour", "Arrivals")
        colnames(departures2) <- c("Hour", "Departures")
        
        # Combine and Melt:
        hourlyArrDep2 <- join_all(list(hourFormat, arrivals2, departures2), by = "Hour", type = "full")
        hourlyArrDep2[is.na(hourlyArrDep2)] = 0
        
        hourlyArrDepMelt2 <- melt(hourlyArrDep2, id.vars = "Hour")
        
        maxY <- max(hourlyArrDepMelt2$value, hourlyArrDepMelt$value, delays$Total)
      }
      else
      {
        maxY <- max(hourlyArrDepMelt$value, delays$Total)
      }
      
      # Plot
      ggplotly(ggplot(data = hourlyArrDepMelt, aes(x = Hour,
                                          y = value,
                                          group = variable,
                                          color = variable,
                                          text = value)) +
        labs(title = "O'hare Hourly Arrivals vs. Departures", x = "", y = "Num. of Flights", color = "") +
        geom_point() +
          themeTest +
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY), tooltip = c("x", "text"))%>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Ohare1DayHourlyDelays <- renderPlotly({
      # Select Proper Month:
      oneMonth <- oneDayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
      }
      
      # Filter Delays
      delays <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
        select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
        group_by(DEP_TIME) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
        na.omit()
      
      colnames(delays) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
      
      delays <- join_all(list(hourFormat, delays), by = "Hour", type = "full")
      delays[is.na(delays)] = 0
      
      delays$Hour <- ordered(delays$Hour, levels = hourFormat[,])
      
      delaysMelt <- melt(delays, id.vars = "Hour")
      
      if (input$checkbox_scale)
      {
        delays2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") %>%
          select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
          filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
          mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
          group_by(DEP_TIME) %>%
          summarise_each(funs(sum)) %>%
          mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY)
        
        colnames(delays2) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
        
        delays2 <- join_all(list(hourFormat, delays2), by = "Hour", type = "full")
        delays2[is.na(delays2)] = 0
        
        delays2$Hour <- ordered(delays2$Hour, levels = hourFormat[,])
        
        delaysMelt2 <- melt(delays2, id.vars = "Hour")
        
        maxY <- max(delaysMelt$value, delaysMelt2$value)
      }
      else
      {
        maxY <- max(delaysMelt$value)
      }
      
      ggplotly(ggplot(data = delaysMelt, aes(x = Hour, 
                                    y = value,
                                    group = variable,
                                    color = variable,
                                    text = value)) + 
        labs(title = "Ohare Delays", x = "", y = "Num. of Delays", color = "Delay Type") +
        geom_point() +
          themeTest + 
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY), tooltip = c("x", "text"))%>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Midway1DayHourlyArrDep <- renderPlotly({
      # Select Proper Month:
      oneMonth <- oneDayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      delays <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
        select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
        group_by(DEP_TIME) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
        na.omit()
      
      colnames(delays) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
      
      delays <- join_all(list(hourFormat, delays), by = "Hour", type = "full")
      delays[is.na(delays)] = 0
      
      hourlyArrDep <- join_all(list(hourFormat, arrivals, departures), by = "Hour", type = "full")
      hourlyArrDep[is.na(hourlyArrDep)] = 0
      
      hourlyArrDep$Hour <- ordered(hourlyArrDep$Hour, levels = hourFormat[,])
      
      hourlyArrDep$Total <- delays$Total
      
      hourlyArrDepMelt <- melt(hourlyArrDep, id.vars = "Hour")
      
      #Common Scale
      if (input$checkbox_scale)
      {
        arrivals2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(ARR_TIME) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        departures2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(DEP_TIME) %>%
          summarise(n()) %>%
          na.omit()
        
        colnames(arrivals2) <- c("Hour", "Arrivals")
        colnames(departures2) <- c("Hour", "Departures")
        
        # Combine and Melt:
        hourlyArrDep2 <- join_all(list(hourFormat, arrivals2, departures2), by = "Hour", type = "full")
        hourlyArrDep2[is.na(hourlyArrDep2)] = 0
        
        hourlyArrDepMelt2 <- melt(hourlyArrDep2, id.vars = "Hour")
        
        maxY <- max(hourlyArrDepMelt2$value, hourlyArrDepMelt$value, delays$Total)
      }
      else
      {
        maxY <- max(hourlyArrDepMelt$value, delays$Total)
      }
      
      # Plot
      ggplotly(ggplot(data = hourlyArrDepMelt, aes(x = Hour,
                                          y = value,
                                          group = variable,
                                          color = variable,
                                          text = value)) +
        labs(title = "Midway Hourly Arrivals vs. Departures", x = "", y = "Num. of Flights", color = "") +
        geom_point() +
          themeTest +
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY), tooltip = c("x", "text"))%>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    output$Midway1DayHourlyDelays <- renderPlotly({
      # Select Proper Month:
      oneMonth <- oneDayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      if (input$checkbox_scale)
      {
        delays2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
          select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
          filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
          mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
          group_by(DEP_TIME) %>%
          summarise_each(funs(sum)) %>%
          mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY)
        
        colnames(delays2) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
        
        delays2 <- join_all(list(hourFormat, delays2), by = "Hour", type = "full")
        delays2[is.na(delays2)] = 0
        
        delays2$Hour <- ordered(delays2$Hour, levels = hourFormat[,])
        
        delaysMelt2 <- melt(delays2, id.vars = "Hour")
        
        maxY <- max(delaysMelt$value, delaysMelt2$value)
      }
      else
      {
        maxY <- max(delaysMelt$value)
      }
      
      ggplotly(ggplot(data = delaysMelt, aes(x = Hour, 
                                    y = value,
                                    group = variable,
                                    color = variable,
                                    text = value)) + 
        labs(title = "Midway Delays", x = "", y = "Num. of Delays", color = "Delay Type") +
        geom_point() +
          themeTest + 
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY), tooltip = c("x", "text"))%>%
        config(staticPlot = FALSE, displayModeBar = FALSE) %>%
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(xaxis = list(fixedrange = TRUE))
    })
    
    
    
    
    
    
    
    
    # A4: 1 Weekday Departures/Arrivals (Hourly) + Delays?
    output$Ohare1WeekdayHourlyArrDep <- renderPlot({
      # Select Proper Month:
      oneMonth <- oneWeekdayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      delays <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
        select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
        group_by(DEP_TIME) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
        na.omit()
      
      colnames(delays) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
      
      delays <- join_all(list(hourFormat, delays), by = "Hour", type = "full")
      delays[is.na(delays)] = 0
      
      hourlyArrDep <- join_all(list(hourFormat, arrivals, departures), by = "Hour", type = "full")
      hourlyArrDep[is.na(hourlyArrDep)] = 0
      
      hourlyArrDep$Hour <- ordered(hourlyArrDep$Hour, levels = hourFormat[,])
      
      hourlyArrDep$Total <- delays$Total
      
      hourlyArrDepMelt <- melt(hourlyArrDep, id.vars = "Hour")
      
      if(input$checkbox_scale)
      {
        arrivals2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(ARR_TIME) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        departures2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(DEP_TIME) %>%
          summarise(n()) %>%
          na.omit()
        
        colnames(arrivals2) <- c("Hour", "Arrivals")
        colnames(departures2) <- c("Hour", "Departures")
        
        # Combine and Melt:
        delays2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") %>%
          select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
          filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
          mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
          group_by(DEP_TIME) %>%
          summarise_each(funs(sum)) %>%
          mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
          na.omit()
        
        colnames(delays2) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
        
        delays2 <- join_all(list(hourFormat, delays2), by = "Hour", type = "full")
        delays2[is.na(delays2)] = 0
        
        hourlyArrDep2 <- join_all(list(hourFormat, arrivals2, departures2), by = "Hour", type = "full")
        hourlyArrDep2[is.na(hourlyArrDep2)] = 0
        
        hourlyArrDep2$Hour <- ordered(hourlyArrDep2$Hour, levels = hourFormat[,])
        
        hourlyArrDep2$Total <- delays2$Total
        
        hourlyArrDepMelt2 <- melt(hourlyArrDep2, id.vars = "Hour")
        
        maxY <- max(hourlyArrDepMelt$value, hourlyArrDepMelt2$value)
      }
      else
      {
        maxY <- max(hourlyArrDepMelt$value)
      }
      
      
      # Plot
      ggplot(data = hourlyArrDepMelt, aes(x = Hour,
                                          y = value,
                                          group = variable,
                                          color = variable)) +
        labs(title = "O'hare Hourly Arrivals vs. Departures", x = "", y = "Num. of Flights", color = "") +
        geom_point() +
        themeTest +
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY)
    })
    
    output$Ohare1WeekdayHourlyDelays <- renderPlot({
      # Select Proper Month:
      oneMonth <- oneWeekdayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
        labs(title = "Ohare Delays", x = "", y = "Num. of Delays", color = "Delay Type") +
        geom_point() +
        themeTest + 
        geom_line(size = 1.5, alpha = 0.7)
    })
    
    output$Midway1WeekdayHourlyArrDep <- renderPlot({
      # Select Proper Month:
      oneMonth <- oneWeekdayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      delays <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") %>%
        select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
        group_by(DEP_TIME) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
        na.omit()
      
      colnames(delays) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
      
      delays <- join_all(list(hourFormat, delays), by = "Hour", type = "full")
      delays[is.na(delays)] = 0
      
      hourlyArrDep <- join_all(list(hourFormat, arrivals, departures), by = "Hour", type = "full")
      hourlyArrDep[is.na(hourlyArrDep)] = 0
      
      hourlyArrDep$Hour <- ordered(hourlyArrDep$Hour, levels = hourFormat[,])
      
      hourlyArrDep$Total <- delays$Total
      
      hourlyArrDepMelt <- melt(hourlyArrDep, id.vars = "Hour")
      
      if(input$checkbox_scale)
      {
        arrivals2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(ARR_TIME) %>%
          summarise(freq = n()) %>%
          na.omit()
        
        departures2 <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
          group_by(DEP_TIME) %>%
          summarise(n()) %>%
          na.omit()
        
        colnames(arrivals2) <- c("Hour", "Arrivals")
        colnames(departures2) <- c("Hour", "Departures")
        
        # Combine and Melt:
        delays2 <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
          select(DEP_TIME, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
          filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
          mutate_each(funs(replace(., . > 0, 1)), -DEP_TIME) %>%
          group_by(DEP_TIME) %>%
          summarise_each(funs(sum)) %>%
          mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
          na.omit()
        
        colnames(delays2) <- c("Hour", "Carrier", "Weather", "NAS", "Security", "Aircraft", "Total")
        
        delays2 <- join_all(list(hourFormat, delays2), by = "Hour", type = "full")
        delays2[is.na(delays2)] = 0
        
        hourlyArrDep2 <- join_all(list(hourFormat, arrivals2, departures2), by = "Hour", type = "full")
        hourlyArrDep2[is.na(hourlyArrDep2)] = 0
        
        hourlyArrDep2$Hour <- ordered(hourlyArrDep2$Hour, levels = hourFormat[,])
        
        hourlyArrDep2$Total <- delays2$Total
        
        hourlyArrDepMelt2 <- melt(hourlyArrDep2, id.vars = "Hour")
        
        maxY <- max(hourlyArrDepMelt$value, hourlyArrDepMelt2$value)
      }
      else
      {
        maxY <- max(hourlyArrDepMelt$value)
      }
      
      # Plot
      ggplot(data = hourlyArrDepMelt, aes(x = Hour,
                                          y = value,
                                          group = variable,
                                          color = variable)) +
        labs(title = "Midway Hourly Arrivals vs. Departures", x = "", y = "Num. of Flights", color = "") +
        geom_point() +
        themeTest +
        geom_line(size = 1.5, alpha = 0.7) + 
        ylim(0, maxY)
    })
    
    output$Midway1WeekdayHourlyDelays <- renderPlot({
      # Select Proper Month:
      oneMonth <- oneWeekdayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
        labs(title = "Midway Delays", x = "", y = "Num. of Delays", color = "Delay Type") +
        geom_point() +
        themeTest + 
        geom_line(size = 1.5, alpha = 0.7)
    })
  
    
    
    
    
    
    # A1 Dygraph
    output$dygraphAirport <- renderDygraph({
      airport <- input$Airport
      
      allFlightsOhareToFromAirport <- filter(allOnTimeFlights, (ORIGIN_AIRPORT == "Chicago O\'Hare International" & DEST_AIRPORT == airport) | (ORIGIN_AIRPORT == airport & DEST_AIRPORT == "Chicago O\'Hare International")) %>%
        group_by(FL_DATE) %>%
        summarise(Flights = n())
      
      allFlightsMidwayToFromAirport <- filter(allOnTimeFlights, (ORIGIN_AIRPORT == "Chicago Midway International" & DEST_AIRPORT == airport) | (ORIGIN_AIRPORT == airport & DEST_AIRPORT == "Chicago Midway International")) %>%
        group_by(FL_DATE) %>%
        summarise(Flights = n())
      
      series1 = xts(x = allFlightsOhareToFromAirport$Flights, order.by = allFlightsOhareToFromAirport$FL_DATE)
      colnames(series1) <- c(airport)
      
      series2 = xts(x = allFlightsMidwayToFromAirport$Flights, order.by = allFlightsMidwayToFromAirport$FL_DATE)
      colnames(series1) <- c(airport)
      
      allData <- cbind(series1, series2)
      colnames(allData) <- c("O\'Hare", "Midway")
      
      dygraph(allData, group = "overview") %>% dyRangeSelector(height = 50, strokeColor = "#484747", fillColor = "#DDDDDD") %>%
        dyOptions(colors = c("#1E5493", "#860800")) %>%
        dyAxis("y", label = "Flights") %>%
        dyOptions(includeZero = TRUE) %>%
        dyOptions(strokeWidth = 3) %>%
        #dyOptions(axisLabelFontSize = 16) %>%
        dyLegend(hideOnMouseOut = FALSE, width = 350, show = "follow")%>% 
        dyRoller(rollPeriod = 1) %>%
        dyShading(from = "2017-1-1", to = "2017-1-31", color = "#AFAFAF") %>%
        dyShading(from = "2017-3-1", to = "2017-3-31", color = "#AFAFAF") %>%
        dyShading(from = "2017-5-1", to = "2017-5-31", color = "#AFAFAF") %>%
        dyShading(from = "2017-7-1", to = "2017-7-31", color = "#AFAFAF") %>%
        dyShading(from = "2017-9-1", to = "2017-9-30", color = "#AFAFAF") %>%
        dyShading(from = "2017-11-1", to = "2017-11-30", color = "#AFAFAF")
      
    })
    
    # Other
    output$OhareDelays1YearAll <- renderPlotly({
      
      # Select Delays
      delays <- allOnTimeFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
        select(ARR_TIMESTAMP, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -ARR_TIMESTAMP) %>%
        group_by(month(ARR_TIMESTAMP), hour(ARR_TIMESTAMP)) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
        na.omit()
      
      # Fix Month
      delays$ARR_TIMESTAMP = NULL
      colnames(delays) <- c("Month", "Hour", "Carrier Delay", "Weather Delay", "NAS Delay", "Security Delay", "Late Aircraft Delay", "Total")
      
      delays$Month <- month.abb[delays$Month]
      
      
      # Fix Hour
      if (input$HourFormat)
      {
        delays$Hour <- format(strptime(delays$Hour, format="%H"), format = "%H:00")
        hourFormat <- hours24
      }
      else
      {
        delays$Hour <- format(strptime(delays$Hour, format="%H"), format = "%I:00 %p")
        hourFormat <- hours12
      }
      
      # Fix Missing Values
      allHoursMonths <- expand.grid(Hour = hourFormat[,], Month = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
      
      allDelays <- left_join(allHoursMonths, delays, by = c("Hour" = "Hour", "Month" = "Month"))
      allDelays[is.na(allDelays)] <- 0
      
      # Fix Ordering
      allDelays$Hour <- ordered(allDelays$Hour, levels = hourFormat[,])
      allDelays$Month <- ordered(allDelays$Month, levels = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
      
      if (input$Delays == "All")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = Total)) +
                   geom_tile(aes(fill = allDelays$Total)) +
                   themeTest +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare Total Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "NAS")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = allDelays$`NAS Delay`)) +
                   geom_tile(aes(fill = allDelays$`NAS Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare NAS Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Security")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = allDelays$`Security Delay`)) +
                   geom_tile(aes(fill = allDelays$`Security Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare Security Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Weather")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = allDelays$`Weather Delay`)) +
                   geom_tile(aes(fill = allDelays$`Weather Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare Weather Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Carrier")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = allDelays$`Carrier Delay`)) +
                   geom_tile(aes(fill = allDelays$`Carrier Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare Carrier Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Late Aircraft")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = allDelays$`Late Aircraft Delay`)) +
                   geom_tile(aes(fill = allDelays$`Late Aircraft Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare Late Aircraft Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
    })
    
    output$MidwayDelays1YearAll <- renderPlotly({
      
      # Select Delays
      delays <- allOnTimeFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") %>%
        select(ARR_TIMESTAMP, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -ARR_TIMESTAMP) %>%
        group_by(month(ARR_TIMESTAMP), hour(ARR_TIMESTAMP)) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
        na.omit()
      
      # Fix Month
      delays$ARR_TIMESTAMP = NULL
      colnames(delays) <- c("Month", "Hour", "Carrier Delay", "Weather Delay", "NAS Delay", "Security Delay", "Late Aircraft Delay", "Total")
      
      delays$Month <- month.abb[delays$Month]
      
      
      # Fix Hour
      if (input$HourFormat)
      {
        delays$Hour <- format(strptime(delays$Hour, format="%H"), format = "%H:00")
        hourFormat <- hours24
      }
      else
      {
        delays$Hour <- format(strptime(delays$Hour, format="%H"), format = "%I:00 %p")
        hourFormat <- hours12
      }
      
      # Fix Missing Values
      allHoursMonths <- expand.grid(Hour = hourFormat[,], Month = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
      
      allDelays <- left_join(allHoursMonths, delays, by = c("Hour" = "Hour", "Month" = "Month"))
      allDelays[is.na(allDelays)] <- 0
      
      # Fix Ordering
      allDelays$Hour <- ordered(allDelays$Hour, levels = hourFormat[,])
      allDelays$Month <- ordered(allDelays$Month, levels = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
      
      if (input$Delays == "All")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = Total)) +
                   geom_tile(aes(fill = allDelays$Total)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway Total Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "NAS")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = allDelays$`NAS Delay`)) +
                   geom_tile(aes(fill = allDelays$`NAS Delay`)) +
                   themeTest +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway NAS Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Security")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = allDelays$`Security Delay`)) +
                   geom_tile(aes(fill = allDelays$`Security Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway Security Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Weather")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = allDelays$`Weather Delay`)) +
                   geom_tile(aes(fill = allDelays$`Weather Delay`)) +
                   themeTest +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway Weather Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Carrier")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = allDelays$`Carrier Delay`)) +
                   geom_tile(aes(fill = allDelays$`Carrier Delay`)) +
                   themeTest +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway Carrier Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Late Aircraft")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Hour,
                                              text = allDelays$`Late Aircraft Delay`)) +
                   geom_tile(aes(fill = allDelays$`Late Aircraft Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway Late Aircraft Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
    })
    
    output$OhareDelays1YearAllWeekday <- renderPlotly({
      
      # Select Delays
      delays <- allOnTimeFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" | ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
        select(ARR_TIMESTAMP, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -ARR_TIMESTAMP) %>%
        group_by(month(ARR_TIMESTAMP), weekdays(ARR_TIMESTAMP)) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
        na.omit()
      
      # Fix Weekday/Month
      delays$ARR_TIMESTAMP = NULL
      colnames(delays) <- c("Month", "Weekday", "Carrier Delay", "Weather Delay", "NAS Delay", "Security Delay", "Late Aircraft Delay", "Total")
      delays$Month <- month.abb[delays$Month]
      
      # Fix Missing Values
      allHoursWeekdays <- expand.grid(Month = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), Weekday = c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
      
      allDelays <- left_join(allHoursWeekdays, delays, by = c("Month" = "Month", "Weekday" = "Weekday"))
      allDelays[is.na(allDelays)] <- 0
      
      # Fix Ordering
      allDelays$Month <- ordered(allDelays$Month, levels = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
      allDelays$Weekday <- ordered(allDelays$Weekday, levels = c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
      
      if (input$Delays == "All")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = Total)) +
                   geom_tile(aes(fill = allDelays$Total)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare Total Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "NAS")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = allDelays$`NAS Delay`)) +
                   geom_tile(aes(fill = allDelays$`NAS Delay`)) +
                   themeTest +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare NAS Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Security")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = allDelays$`Security Delay`)) +
                   geom_tile(aes(fill = allDelays$`Security Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare Security Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Weather")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = allDelays$`Weather Delay`)) +
                   geom_tile(aes(fill = allDelays$`Weather Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare Weather Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Carrier")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = allDelays$`Carrier Delay`)) +
                   geom_tile(aes(fill = allDelays$`Carrier Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare Carrier Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Late Aircraft")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = allDelays$`Late Aircraft Delay`)) +
                   geom_tile(aes(fill = allDelays$`Late Aircraft Delay`)) +
                   themeTest +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "O'Hare Late Aircraft Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
    })
    
    output$MidwayDelays1YearAllWeekday <- renderPlotly({
      
      # Select Delays
      delays <- allOnTimeFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") %>%
        select(ARR_TIMESTAMP, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -ARR_TIMESTAMP) %>%
        group_by(month(ARR_TIMESTAMP), weekdays(ARR_TIMESTAMP)) %>%
        summarise_each(funs(sum)) %>%
        mutate(Total = CARRIER_DELAY + WEATHER_DELAY + NAS_DELAY + SECURITY_DELAY + LATE_AIRCRAFT_DELAY) %>%
        na.omit()
      
      # Fix Weekday/Month
      delays$ARR_TIMESTAMP = NULL
      colnames(delays) <- c("Month", "Weekday", "Carrier Delay", "Weather Delay", "NAS Delay", "Security Delay", "Late Aircraft Delay", "Total")
      delays$Month <- month.abb[delays$Month]
      
      # Fix Missing Values
      allHoursWeekdays <- expand.grid(Month = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), Weekday = c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
      
      allDelays <- left_join(allHoursWeekdays, delays, by = c("Month" = "Month", "Weekday" = "Weekday"))
      allDelays[is.na(allDelays)] <- 0
      
      # Fix Ordering
      allDelays$Month <- ordered(allDelays$Month, levels = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
      allDelays$Weekday <- ordered(allDelays$Weekday, levels = c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
      
      if (input$Delays == "All")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = Total)) +
                   geom_tile(aes(fill = allDelays$Total)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway Total Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "NAS")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = allDelays$`NAS Delay`)) +
                   geom_tile(aes(fill = allDelays$`NAS Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway NAS Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Security")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = allDelays$`Security Delay`)) +
                   geom_tile(aes(fill = allDelays$`Security Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway Security Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Weather")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = allDelays$`Weather Delay`)) +
                   geom_tile(aes(fill = allDelays$`Weather Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway Weather Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Carrier")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = allDelays$`Carrier Delay`)) +
                   geom_tile(aes(fill = allDelays$`Carrier Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway Carrier Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
      else if (input$Delays == "Late Aircraft")
      {
        ggplotly(ggplot(data = allDelays, aes(x = Month,
                                              y = Weekday,
                                              text = allDelays$`Late Aircraft Delay`)) +
                   geom_tile(aes(fill = allDelays$`Late Aircraft Delay`)) +
                   themeTest  +
                   scale_fill_gradient(low = "#66ff88", high = "#006616") +
                   labs(title = "Midway Late Aircraft Delays", x = "", y = "", fill = ""), tooltip = "text")%>%
          config(staticPlot = FALSE, displayModeBar = FALSE) %>%
          layout(yaxis = list(fixedrange = TRUE)) %>%
          layout(xaxis = list(fixedrange = TRUE))
      }
    })
    
  #Tables----
    # C====
    # C1: Total # of Departures & Arrivals (Airlines)
    output$OhareAirlineArrDepTable <- renderDT({
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
      
      # Table
      datatable(airlineArrDep, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$MidwayAirlineArrDepTable <- renderDT({
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
      
      # Table
      datatable(airlineArrDep, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    
    # C2: Total # of Departures & Arrivals (Hourly)
    output$OhareHourlyArrDepTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
        #oneMonth <- filter(allFlights24, month(FL_DATE) == monthNum)
      }
      else
      {
        hourFormat <- hours12
        #oneMonth <- filter(allFlights12, month(FL_DATE) == monthNum)
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
      
      # Table
      datatable(hourlyArrDep, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$MidwayHourlyArrDepTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      # Table
      datatable(hourlyArrDep, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    
    # C3: Total # of Departures & Arrivals (Weekly)
    output$OhareWeeklyArrDepTable <- renderDT({
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
      
      # Table
      datatable(weeklyArrDep, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$MidwayWeeklyArrDepTable <- renderDT({
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
      
      # Table
      datatable(weeklyArrDep, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    
    # C4: Total # of Delays (Hourly)
    output$OhareHourlyDelaysTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      # Table
      datatable(delays, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$MidwayHourlyDelaysTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      # Table
      datatable(delays, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    
    # C5: Total # of Flights (Top 15 Arrival/Destination Airports)
    output$OhareMostCommonArrivalAirportsTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      arrivalAirports <- oneMonth %>% filter(DEST_AIRPORT == "Chicago O\'Hare International") %>%
        group_by(ORIGIN_AIRPORT) %>%
        summarise(freq = n()) %>%
        arrange(desc(freq)) %>%
        top_n(15)
      
      colnames(arrivalAirports) <- c("Airport", "Flights")
      
      # Table
      datatable(arrivalAirports, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$OhareMostCommonDestinationAirportsTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      destinationAirports <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International") %>%
        group_by(DEST_AIRPORT) %>%
        summarise(freq = n()) %>%
        arrange(desc(freq)) %>%
        top_n(15)
      
      colnames(destinationAirports) <- c("Airport", "Flights")
      
      # Table
      datatable(destinationAirports, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$MidwayMostCommonArrivalAirportsTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      arrivalAirports <- oneMonth %>% filter(DEST_AIRPORT == "Chicago Midway International") %>%
        group_by(ORIGIN_AIRPORT) %>%
        summarise(freq = n()) %>%
        arrange(desc(freq)) %>%
        top_n(15)
      
      colnames(arrivalAirports) <- c("Airport", "Flights")
      
      # Table
      datatable(arrivalAirports, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$MidwayMostCommonDestinationAirportsTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneMonthReactive()
      
      destinationAirports <- oneMonth %>% filter(ORIGIN_AIRPORT == "Chicago Midway International") %>%
        group_by(DEST_AIRPORT) %>%
        summarise(freq = n()) %>%
        arrange(desc(freq)) %>%
        top_n(15)
      
      colnames(destinationAirports) <- c("Airport", "Flights")
      
      # Table
      datatable(destinationAirports, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    
    
    
    
    
    
    # B====
    
    # B1: Total # Departures/Arrivals per Airline (MOnthly)
    output$Ohare1YearAirlinesArrivalsTable <- renderDT({
      
      arrivals1 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jan = n())
      arrivals2 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Feb = n())
      arrivals3 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Mar = n())
      arrivals4 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Apr = n())
      arrivals5 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
      arrivals6 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jun = n())
      arrivals7 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jul = n())
      arrivals8 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Aug = n())
      arrivals9 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Sep = n())
      arrivals10 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Oct = n())
      arrivals11 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Nov = n())
      arrivals12 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Dec = n())
      
      arrivals <- join_all(list(airlines, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "Airline")
      arrivals[is.na(arrivals)] = 0
      
      datatable(arrivals, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Ohare1YearAirlinesDeparturesTable <- renderDT({
      
      departures1 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jan = n())
      departures2 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Feb = n())
      departures3 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Mar = n())
      departures4 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Apr = n())
      departures5 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
      departures6 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jun = n())
      departures7 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jul = n())
      departures8 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Aug = n())
      departures9 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Sep = n())
      departures10 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Oct = n())
      departures11 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Nov = n())
      departures12 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Dec = n())
      
      departures <- join_all(list(airlines, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "Airline")
      departures[is.na(departures)] = 0
      
      departures$Airline = NULL
      
      datatable(departures, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1YearAirlinesArrivalsTable <- renderDT({
      
      arrivals1 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jan= n())
      arrivals2 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Feb = n())
      arrivals3 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Mar = n())
      arrivals4 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Apr = n())
      arrivals5 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
      arrivals6 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jun = n())
      arrivals7 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jul = n())
      arrivals8 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Aug = n())
      arrivals9 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Sep = n())
      arrivals10 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Oct = n())
      arrivals11 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Nov = n())
      arrivals12 <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Dec = n())
      
      arrivals <- join_all(list(airlines, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "Airline")
      arrivals[is.na(arrivals)] = 0
      
      arrivals$Airline = NULL
      
      datatable(arrivals, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1YearAirlinesDeparturesTable <- renderDT({
      
      departures1 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jan = n())
      departures2 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Feb = n())
      departures3 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Mar = n())
      departures4 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Apr = n())
      departures5 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(Airline = CARRIER_NAME) %>% summarise(May = n())
      departures6 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jun = n())
      departures7 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Jul = n())
      departures8 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Aug = n())
      departures9 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Sep = n())
      departures10 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Oct = n())
      departures11 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Nov = n())
      departures12 <- allFlights24 %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(Airline = CARRIER_NAME) %>% summarise(Dec = n())
      
      departures <- join_all(list(airlines, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "Airline")
      departures[is.na(departures)] = 0
      
      departures$Airline = NULL
      
      datatable(departures, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    # B2: Total # Departures/Arrivals (Hourly + Monthly)
    output$Ohare1YearHourlyArrivalsTable <- renderDT({
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
        allFlights <- allFlights24
      }
      else
      {
        hourFormat <- hours12
        allFlights <- allFlights12
      }
      
      # Filter Arrivals
      arrivals1 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(Jan = n()) %>% na.omit()
      arrivals2 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(Feb = n()) %>% na.omit()
      arrivals3 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(Mar= n()) %>% na.omit()
      arrivals4 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(Apr = n()) %>% na.omit()
      arrivals5 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
      arrivals6 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(Jun = n()) %>% na.omit()
      arrivals7 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(Jul = n()) %>% na.omit()
      arrivals8 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(Aug = n()) %>% na.omit()
      arrivals9 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(Sep = n()) %>% na.omit()
      arrivals10 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(Oct = n()) %>% na.omit()
      arrivals11 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(Nov = n()) %>% na.omit()
      arrivals12 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("ARR_TIME")
      allArrivals <- join_all(list(hourFormat, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "ARR_TIME", type = "full")
      allArrivals[is.na(allArrivals)] = 0
      
      allArrivals$ARR_TIME <- ordered(allArrivals$ARR_TIME, levels = hourFormat[,])
      
      datatable(allArrivals, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Ohare1YearHourlyDeparturesTable <- renderDT({
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
        allFlights <- allFlights24
      }
      else
      {
        hourFormat <- hours12
        allFlights <- allFlights12
      }
      
      # Filter Departures
      departures1 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(Jan = n()) %>% na.omit()
      departures2 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(Feb = n()) %>% na.omit()
      departures3 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(Mar = n()) %>% na.omit()
      departures4 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(Apr = n()) %>% na.omit()
      departures5 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
      departures6 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(Jun = n()) %>% na.omit()
      departures7 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(Jul = n()) %>% na.omit()
      departures8 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(Aug = n()) %>% na.omit()
      departures9 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(Sep = n()) %>% na.omit()
      departures10 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(Oct = n()) %>% na.omit()
      departures11 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(Nov = n()) %>% na.omit()
      departures12 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("DEP_TIME")
      allDepartures <- join_all(list(hourFormat, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "DEP_TIME", type = "full")
      allDepartures[is.na(allDepartures)] = 0
      
      allDepartures$DEP_TIME <- ordered(allDepartures$DEP_TIME, levels = hourFormat[,])
      
      datatable(allDepartures, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1YearHourlyArrivalsTable <- renderDT({
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
        allFlights <- allFlights24
      }
      else
      {
        hourFormat <- hours12
        allFlights <- allFlights12
      }
      
      # Filter Arrivals
      arrivals1 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(Jan = n()) %>% na.omit()
      arrivals2 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(Feb = n()) %>% na.omit()
      arrivals3 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(Mar = n()) %>% na.omit()
      arrivals4 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(Apr = n()) %>% na.omit()
      arrivals5 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
      arrivals6 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(Jun = n()) %>% na.omit()
      arrivals7 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(Jul = n()) %>% na.omit()
      arrivals8 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(Aug = n()) %>% na.omit()
      arrivals9 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(Sep = n()) %>% na.omit()
      arrivals10 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(Oct = n()) %>% na.omit()
      arrivals11 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(Nov = n()) %>% na.omit()
      arrivals12 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("ARR_TIME")
      allArrivals <- join_all(list(hourFormat, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "ARR_TIME", type = "full")
      allArrivals[is.na(allArrivals)] = 0
      
      allArrivals$ARR_TIME <- ordered(allArrivals$ARR_TIME, levels = hourFormat[,])
      
      datatable(allArrivals, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1YearHourlyDeparturesTable <- renderDT({
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
        allFlights <- allFlights24
      }
      else
      {
        hourFormat <- hours12
        allFlights <- allFlights12
      }
      
      # Filter Departures
      departures1 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(Jan = n()) %>% na.omit()
      departures2 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(Feb = n()) %>% na.omit()
      departures3 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(Mar = n()) %>% na.omit()
      departures4 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(Apr = n()) %>% na.omit()
      departures5 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
      departures6 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(Jun = n()) %>% na.omit()
      departures7 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(Jul = n()) %>% na.omit()
      departures8 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(Aug = n()) %>% na.omit()
      departures9 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(Sep = n()) %>% na.omit()
      departures10 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(Oct= n()) %>% na.omit()
      departures11 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(Nov = n()) %>% na.omit()
      departures12 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("DEP_TIME")
      allDepartures <- join_all(list(hourFormat, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "DEP_TIME", type = "full")
      allDepartures[is.na(allDepartures)] = 0
      
      allDepartures$DEP_TIME <- ordered(allDepartures$DEP_TIME, levels = hourFormat[,])
      
      datatable(allDepartures, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    
    # B3: Total # Flights for Top 15 Destinations (Monthly)
    output$Ohare1YearlMostCommonTable <- renderDT({
      
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
      
      datatable(destinationAirports, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1YearlMostCommonTable <- renderDT({
      
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
      
      datatable(destinationAirports, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    
    # B4: Total # Delays (Monthly)
    output$Ohare1YearDelaysTable <- renderDT({
      
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
      
      datatable(delays, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1YearDelaysTable <- renderDT({
      
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
      
      delays <- allFlights24 %>% filter(DEST_AIRPORT == "Chicago Midway International" | ORIGIN_AIRPORT == "Chicago Midway International") %>%
        select(FL_DATE, CARRIER_DELAY : LATE_AIRCRAFT_DELAY) %>%
        filter(CARRIER_DELAY > 0 | WEATHER_DELAY > 0 | NAS_DELAY > 0 | SECURITY_DELAY > 0 | LATE_AIRCRAFT_DELAY > 0) %>%
        mutate_each(funs(replace(., . > 0, 1)), -FL_DATE) %>%
        group_by(month(FL_DATE)) %>%
        summarise_each(funs(sum))
      
      delays$FL_DATE <- NULL
      colnames(delays) <- c("Month", "Carrier", "Weather", "NAS", "Security", "Aircraft")
      
      datatable(delays, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    
    
    # A====
    
    
    
    
    # A2: 1 Day Departures/Arrivals per Airline (Monthly)
    output$Ohare1YearAirlineHourlyArrivalsTable <- renderDT({
      
      allFlights <- oneAirlineReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
      }
      
      # Filter Arrivals
      arrivals1 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(Jan = n()) %>% na.omit()
      arrivals2 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(Feb = n()) %>% na.omit()
      arrivals3 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(Mar = n()) %>% na.omit()
      arrivals4 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(Apr = n()) %>% na.omit()
      arrivals5 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
      arrivals6 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(Jun = n()) %>% na.omit()
      arrivals7 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(Jul = n()) %>% na.omit()
      arrivals8 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(Aug = n()) %>% na.omit()
      arrivals9 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(Sep = n()) %>% na.omit()
      arrivals10 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(Oct = n()) %>% na.omit()
      arrivals11 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(Nov = n()) %>% na.omit()
      arrivals12 <- allFlights %>% filter(DEST_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("ARR_TIME")
      allArrivals <- join_all(list(hourFormat, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "ARR_TIME", type = "full")
      allArrivals[is.na(allArrivals)] = 0
      
      allArrivals$ARR_TIME <- ordered(allArrivals$ARR_TIME, levels = hourFormat[,])
      
      datatable(allArrivals, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Ohare1YearAirlineHourlyDeparturesTable <- renderDT({
      
      allFlights <- oneAirlineReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
      }
      
      # Filter Departures
      departures1 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(Jan = n()) %>% na.omit()
      departures2 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(Feb = n()) %>% na.omit()
      departures3 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(Mar = n()) %>% na.omit()
      departures4 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(Apr = n()) %>% na.omit()
      departures5 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
      departures6 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(Jun = n()) %>% na.omit()
      departures7 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(Jul = n()) %>% na.omit()
      departures8 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(August = n()) %>% na.omit()
      departures9 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(Sep = n()) %>% na.omit()
      departures10 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(Oct = n()) %>% na.omit()
      departures11 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(Nov = n()) %>% na.omit()
      departures12 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago O\'Hare International" & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("DEP_TIME")
      allDepartures <- join_all(list(hourFormat, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "DEP_TIME", type = "full")
      allDepartures[is.na(allDepartures)] = 0
      
      allDepartures$DEP_TIME <- ordered(allDepartures$DEP_TIME, levels = hourFormat[,])
      
      datatable(allDepartures, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1YearAirlineHourlyArrivalsTable <- renderDT({
      
      allFlights <- oneAirlineReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
      }
      
      # Filter Arrivals
      arrivals1 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(ARR_TIME) %>% summarise(Jan = n()) %>% na.omit()
      arrivals2 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(ARR_TIME) %>% summarise(Feb = n()) %>% na.omit()
      arrivals3 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(ARR_TIME) %>% summarise(Mar = n()) %>% na.omit()
      arrivals4 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(ARR_TIME) %>% summarise(Apr = n()) %>% na.omit()
      arrivals5 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(ARR_TIME) %>% summarise(May = n()) %>% na.omit()
      arrivals6 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(ARR_TIME) %>% summarise(Jun = n()) %>% na.omit()
      arrivals7 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(ARR_TIME) %>% summarise(Jul = n()) %>% na.omit()
      arrivals8 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(ARR_TIME) %>% summarise(Aug = n()) %>% na.omit()
      arrivals9 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(ARR_TIME) %>% summarise(Sep = n()) %>% na.omit()
      arrivals10 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(ARR_TIME) %>% summarise(Oct = n()) %>% na.omit()
      arrivals11 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(ARR_TIME) %>% summarise(Nov = n()) %>% na.omit()
      arrivals12 <- allFlights %>% filter(DEST_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(ARR_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("ARR_TIME")
      allArrivals <- join_all(list(hourFormat, arrivals1, arrivals2, arrivals3, arrivals4, arrivals5, arrivals6, arrivals7, arrivals8, arrivals9, arrivals10, arrivals11, arrivals12), by = "ARR_TIME", type = "full")
      allArrivals[is.na(allArrivals)] = 0
      
      allArrivals$ARR_TIME <- ordered(allArrivals$ARR_TIME, levels = hourFormat[,])
      
      datatable(allArrivals, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1YearAirlineHourlyDeparturesTable <- renderDT({
      
      allFlights <- oneAirlineReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
      }
      
      # Filter Departures
      departures1 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 1) %>% group_by(DEP_TIME) %>% summarise(Jan = n()) %>% na.omit()
      departures2 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 2) %>% group_by(DEP_TIME) %>% summarise(Feb = n()) %>% na.omit()
      departures3 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 3) %>% group_by(DEP_TIME) %>% summarise(Mar = n()) %>% na.omit()
      departures4 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 4) %>% group_by(DEP_TIME) %>% summarise(Apr = n()) %>% na.omit()
      departures5 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 5) %>% group_by(DEP_TIME) %>% summarise(May = n()) %>% na.omit()
      departures6 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 6) %>% group_by(DEP_TIME) %>% summarise(Jun = n()) %>% na.omit()
      departures7 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 7) %>% group_by(DEP_TIME) %>% summarise(Jul = n()) %>% na.omit()
      departures8 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 8) %>% group_by(DEP_TIME) %>% summarise(Aug = n()) %>% na.omit()
      departures9 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 9) %>% group_by(DEP_TIME) %>% summarise(Sep = n()) %>% na.omit()
      departures10 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 10) %>% group_by(DEP_TIME) %>% summarise(Oct = n()) %>% na.omit()
      departures11 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 11) %>% group_by(DEP_TIME) %>% summarise(Nov = n()) %>% na.omit()
      departures12 <- allFlights %>% filter(ORIGIN_AIRPORT == "Chicago Midway International" & month(FL_DATE) == 12) %>% group_by(DEP_TIME) %>% summarise(Dec = n()) %>% na.omit()
      
      # Combine and Melt
      colnames(hourFormat) <- c("DEP_TIME")
      allDepartures <- join_all(list(hourFormat, departures1, departures2, departures3, departures4, departures5, departures6, departures7, departures8, departures9, departures10, departures11, departures12), by = "DEP_TIME", type = "full")
      allDepartures[is.na(allDepartures)] = 0
      
      allDepartures$DEP_TIME <- ordered(allDepartures$DEP_TIME, levels = hourFormat[,])
      
      datatable(allDepartures, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    
    # A3: 1 Day Departures/Arrivals (Hourly) + Delays?
    output$Ohare1DayHourlyArrDepTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneDayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      datatable(hourlyArrDep, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Ohare1DayHourlyDelaysTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneDayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      datatable(delays, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1DayHourlyArrDepTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneDayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      datatable(hourlyArrDep, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1DayHourlyDelaysTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneDayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      datatable(delays, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    
    # A4: 1 Weekday Departures/Arrivals (Hourly) + Delays?
    output$Ohare1WeekdayHourlyArrDepTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneWeekdayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      datatable(hourlyArrDep, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Ohare1WeekdayHourlyDelaysTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneWeekdayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      datatable(delays, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1WeekdayHourlyArrDepTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneWeekdayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      datatable(hourlyArrDep, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$Midway1WeekdayHourlyDelaysTable <- renderDT({
      # Select Proper Month:
      oneMonth <- oneWeekdayReactive()
      
      # Convert Time Format:
      if (input$HourFormat)
      {
        hourFormat <- hours24
      }
      else
      {
        hourFormat <- hours12
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
      
      datatable(delays, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    
    
    # Other Table
    # Interesting Things
    output$InterestingThingsTable <- renderDT({
      interestingTable <- matrix(c(
        "Thursday, November 23rd (2017-11-23)", "Thanksgiving", "description1",
        "Tuesday, July 4th (2017-7-4)", "4th of July", "description2",
        "Monday, December 25th (2017-12-25)", "Christmas", "description2",
        "Friday, May 5th (2017-5-29)", "Memorial Day", "description2",
        "Tuesday, February 14th (2017-2-14)", "Valentine's Day", "description2",
        "Tuesday, August 8th (2017-8-28)", "UIC Fall Sart", "description2",
        "Monday, January 16th (2017-1-16)", "UIC Spring Start", "description2",
        "Friday, December 15th (2017-12-15)", "UIC Fall End", "description2",
        "Friday, May 5th (2017-5-5)", "UIC Spring End", "description2",
        "Monday, September 4th (2017-9-4)", "Labor Day", "description2"), 
        ncol = 3, byrow = TRUE)
      colnames(interestingTable) <- c("Date", "Event", "Description")
      
      interestingTable <- as.data.frame(interestingTable)
      
      datatable(interestingTable, options = list(
        searching = FALSE,
        pageLength = 10,
        dom = "t",
        ordering = T,
        lengthChange = FALSE),
        rownames = FALSE
      )
    })

}
