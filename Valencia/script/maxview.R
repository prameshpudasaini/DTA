library(data.table)
library(readxl)
library(leaflet)
library(RODBC)

device <- read_excel("Valencia/ignore/DeviceID_Tucson.xlsx", sheet = 'IntersectionID')

device <- as.data.table(device)[grepl('Valencia', Name), .(ID, Name, Latitude, Longitude)]
device[, NameID := paste0(ID, ': ', Name)]

leaflet() |> 
  addTiles() |> 
  addMarkers(lng = ~Longitude, lat = ~Latitude, data = device, popup = ~NameID)


# Collect data from SQL

source('ignore/keys.R')

db_phase <- '[Maxview_Data_2017].[dbo].[ASC_PhasePed_Events_09-13-2021]'

query <- paste0(
    "SELECT * FROM ", db_phase, 
    " WHERE DeviceID = 681 AND EventId IN (1, 7) AND Parameter IN (2, 4, 6, 8) AND DATEPART(w, TimeStamp) IN (3, 4, 5) AND DATEPART(hh, TimeStamp) IN (6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)"
)

options(digits.secs = 3L)

DT <- as.data.table(sqlQuery(getSQLConnection('STL4'), query))
DT[, EventId := as.factor(EventId)]
DT[, Parameter := as.factor(Parameter)]
DT[, Day := wday(TimeStamp)]

listDT <- split(DT, by = 'Day')

for (i in seq_along(listDT)) {
    
    data <- listDT[[i]]
    dat <- split(data, by = 'Parameter')
    
    for (j in seq_along(dat)) {
        dt <- dat[[j]]
        
        greenStart <- dt$TimeStamp[min(which(dt$EventId == 1L))]
        greenEnd <- dt$TimeStamp[max(which(dt$EventId == 7L))]
        
        data1 <- dt[between(TimeStamp, greenStart, greenEnd), ]
        
        event1 <- data1[EventId == 1, ]$TimeStamp
        event7 <- data1[EventId == 7, ]$TimeStamp
        greenTime <- as.numeric(event7) - as.numeric(event1)
    }
    
    greenStart <- data$TimeStamp[min(which(data$EventId == 1L))]
    greenEnd <- data$TimeStamp[max(which(data$EventId == 7L))]
    
    data <- data[between(TimeStamp, greenStart, greenEnd), ]
    
    event1 <- data[EventId == 1, ]$TimeStamp
    event7 <- data[EventId == 7, ]$TimeStamp
    greenTime <- as.numeric(event7) - as.numeric(event1)
}
