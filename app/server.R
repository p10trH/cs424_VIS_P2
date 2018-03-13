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

# ---------------------------

statesData <-
  read.csv(file = 'statesData.csv',
           header = TRUE)

states <- geojsonio::geojson_read("states.geojson", what = "sp")

statesWData <- merge(states, statesData, by = "NAME")

m <- leaflet(statesWData) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE))

bins <- c(0, 3, 6, 9, 12, 15, 18, 21, Inf)
pal <- colorBin("YlOrRd", domain = statesWData$value, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  statesWData$NAME, statesWData$value
) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
  fillColor = ~pal(statesWData$value),
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
  addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
                                       position = "bottomright")

# ---------------------------



# Define server
server <- function(input, output) {
  
  # output$hourlyArrDep <- renderPlot({
  #   ggplot(data = hourlyOhareMelt, aes(x = Hour, 
  #                                      y = value,
  #                                      group = variable,
  #                                      color = variable)) + 
  #     labs(title = "O'Hare", x = "Time of Day (Hour)", y = "Num. of Flights", color = "Legend") +
  #     geom_point() + 
  #     geom_line(size = 1.5, alpha = 0.7)
  # })
  # 
  # output$hourlyArrDep2 <- renderPlot({
  #   ggplot(data = hourlyMidwayMelt, aes(x = Hour, 
  #                                       y = value,
  #                                       group = variable,
  #                                       color = variable)) + 
  #     labs(title = "Midway", x = "Time of Day (Hour)", y = "Num. of Flights", color = "Legend") +
  #     geom_point() + 
  #     geom_line(size = 1.5, alpha = 0.7)
  # })
  # 
  # output$hourlyArrDepTable <- DT::renderDT(DT::datatable(hourlyOhare))
  # 
  # output$hourlyArrDep2Table <- DT::renderDT(DT::datatable(hourlyMidway))
   
}
