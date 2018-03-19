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
library(leaflet.minicharts)
library(zoo)
library(xts)
library(geojsonio)

library(tidyr)
library(streamgraph)
library(plotly)
library(dygraphs)




# month, day, year ("%m-%d-%Y")
dateToShow <- "6-9-2017" # don't change

# 2017 only
choices_month <- format(seq.Date(from = as.Date('1-1-2017', "%m-%d-%Y"), to = as.Date('12-31-2017', "%m-%d-%Y"), by="month"), "(%m)  %b")
# depends on what date we want to show
choices_day <- format(seq.Date(from = as.Date(paste(month(mdy(dateToShow)), '1-2017', sep = "-"), "%m-%d-%Y"), length.out = as.numeric(days_in_month(as.Date(dateToShow, "%m-%d-%Y"))), by="day"), "(%d)  %a")



