library(data.table)
library(leaflet)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GTFS Shapefiles --------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# routes

routes <- fread("Valencia/data/GTFS_SunTran/routes.txt")
length(unique(routes$route_id))

# shapes

shapes <- fread("Valencia/data/GTFS_SunTran/shapes.txt")
length(unique(shapes$shape_id))

# leaflet() |> 
#     addTiles() |> 
#     addCircleMarkers(data = shapes, lng = ~shape_pt_lon, lat = ~shape_pt_lat, radius = 0.1)

# stop times

stop_times <- fread("Valencia/data/GTFS_SunTran/stop_times.txt")
length(unique(stop_times$trip_id))
stop_times$stop_headsign <- NULL
stop_times$pickup_type <- NULL
stop_times$drop_off_type <- NULL

# stops

stops <- fread("Valencia/data/GTFS_SunTran/stops.txt")
length(unique(stops$stop_id))
stops <- stops[, .(stop_id, stop_name, stop_lat, stop_lon)]

# trips

trips <- fread("Valencia/data/GTFS_SunTran/trips.txt")
trips$trip_short_name <- NULL
trips$wheelchair_accessible <- NULL
trips$bikes_allowed <- NULL

# calendar

calendar <- fread("Valencia/data/GTFS_SunTran/calendar.txt")
calendar_dates <- fread("Valencia/data/GTFS_SunTran/calendar_dates.txt")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transit in Study Area --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# select service id from calendar
select_service_id <- calendar$service_id[1] # check expcetion in calendar dates

# select study routes id for Valencia and for bus
routes[, .N, by = route_type]
select_routes_type <- 3
select_routes_id <- routes[grepl('Valencia', route_long_name) & route_type %in% select_routes_type, ]$route_id

# select trips based on study service id and route id
select_trips <- trips[route_id %in% select_routes_id & service_id %in% select_service_id, ]
select_trips_id <- unique(select_trips$trip_id)

# select stops based on trips id
select_stop_times <- stop_times[trip_id %in% select_trips_id, ]

select_stop_times[, arrival_time := as.ITime(arrival_time, format = '%H:%M:%S')]
select_stop_times[, departure_time := as.ITime(departure_time, format = '%H:%M:%S')]

select_stop_times[, .N, by = stop_sequence] # 39 stops for each trip

# select trips excluding night trips
select_stop_times2 <- select_stop_times[hour(departure_time) < 19L, ]

# select stops based on stop id
select_stops_id <- unique(select_stop_times2$stop_id)
select_stops <- stops[stop_id %in% select_stops_id, ]

leaflet() |> 
    addTiles() |> 
    addCircleMarkers(lng = ~stop_lon, lat = ~stop_lat, data = select_stops, radius = 5, color = 'black')

# get stop id sequence based on stop sequnce
