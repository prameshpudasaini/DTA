library(RODBC)
library(data.table)
library(readr)
library(plotly)

source("Valencia/ignore/keys.R")

query_file <- read_file("Valencia/script/inrix_valencia.sql")

# DT <- as.data.table(sqlQuery(getSQLConnection('STL5'), query_file))

# fwrite(DT, "Valencia/data/inrix_valencia_2021.txt")

DT <- fread("Valencia/data/inrix_valencia_2021.txt")

DT[, time_mst := time_mst - 7*3600]
DT[, minute := hour(time_mst) * 60L + minute(time_mst)]

DT <- DT[, .(mean_seg_tt = round(mean(travelTimeMinutes), 2)), by = .(minute, SegmentID, Bearing)]
DT <- DT[, .(total_route_tt = sum(mean_seg_tt)), by = .(minute, Bearing)][order(minute)]
DT[, time := as.ITime(minute * 60)]

plot_ly(
    DT,
    type = 'scatter',
    x = ~time,
    y = ~total_route_tt,
    color = ~Bearing,
    mode = 'lines'
)