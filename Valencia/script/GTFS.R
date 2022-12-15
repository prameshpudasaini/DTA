library(data.table)
library(leaflet)
library(htmlwidgets)

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

stop_times[, arrival_time := as.ITime(arrival_time, format = '%H:%M:%S')]
stop_times[, departure_time := as.ITime(departure_time, format = '%H:%M:%S')]

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
# Transit in Study Area: Data Processing -------------------------------------
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

# get trips id for each direction
trips_id_dir0 <- select_trips[direction_id == 0L, ]$trip_id
trips_id_dir1 <- select_trips[direction_id == 1L, ]$trip_id

# select stops based on trips id
select_stop_times_dir0 <- stop_times[trip_id %in% trips_id_dir0, ]
select_stop_times_dir1 <- stop_times[trip_id %in% trips_id_dir1, ]

select_stop_times_dir0[, .N, by = stop_sequence] # 39 stops for each of 30 trips
select_stop_times_dir1[, .N, by = stop_sequence] # 39 stops for each of 31 trips

# select trips excluding night trips
select_stop_times_dir0 <- select_stop_times_dir0[hour(departure_time) < 19L, ]
select_stop_times_dir1 <- select_stop_times_dir1[hour(departure_time) < 19L, ]

# get stops id
select_stops_id_dir0 <- unique(select_stop_times_dir0$stop_id)
select_stops_id_dir1 <- unique(select_stop_times_dir1$stop_id)

length(select_stops_id_dir0)
length(select_stops_id_dir1)

# select stops based on stop id
select_stops_dir0 <- stops[stop_id %in% select_stops_id_dir0, ]
select_stops_dir1 <- stops[stop_id %in% select_stops_id_dir1, ]

leaflet() |> 
    addTiles() |> 
    addCircleMarkers(lng = ~stop_lon, lat = ~stop_lat, data = select_stops_dir0, radius = 5, color = 'black')

leaflet() |> 
    addTiles() |> 
    addCircleMarkers(lng = ~stop_lon, lat = ~stop_lat, data = select_stops_dir1, radius = 5, color = 'black')

# check stop sequence for each trip
split_stops_dir0 <- select_stop_times_dir0[, .(trip_id, stop_id, stop_sequence)] |> 
    split(by = 'trip_id', keep.by = FALSE)

split_stops_dir1 <- select_stop_times_dir1[, .(trip_id, stop_id, stop_sequence)] |> 
    split(by = 'trip_id', keep.by = FALSE)

check_identical <- function(x, y) {
    if (identical(x, y)) x
    else FALSE
}

Reduce(check_identical, split_stops_dir0)
Reduce(check_identical, split_stops_dir1)

# get stop id sequence based on stop sequence
stops_id_seq_dir0 <- split_stops_dir0[[1]]$stop_id
stops_id_seq_dir1 <- split_stops_dir1[[1]]$stop_id

rev(stops_id_seq_dir1) == stops_id_seq_dir0

# update selected stops
select_stops_dir0 <- select_stops_dir0[split_stops_dir0[[1]], on = 'stop_id']
select_stops_dir1 <- select_stops_dir1[split_stops_dir1[[1]], on = 'stop_id']

stops_dir0 <- leaflet() |> 
    addTiles() |> 
    addCircleMarkers(lng = ~stop_lon, lat = ~stop_lat, data = select_stops_dir0, radius = 5, color = 'black',
                     popup = paste0('ID: ', select_stops_dir0$stop_id, ', ',
                                    'SEQ: ', select_stops_dir0$stop_sequence, ', ', 
                                    'LOC: ', select_stops_dir0$stop_name))

stops_dir1 <- leaflet() |> 
    addTiles() |> 
    addCircleMarkers(lng = ~stop_lon, lat = ~stop_lat, data = select_stops_dir1, radius = 5, color = 'black',
                     popup = paste0('ID: ', select_stops_dir1$stop_id, ', ',
                                    'SEQ: ', select_stops_dir1$stop_sequence, ', ', 
                                    'LOC: ', select_stops_dir1$stop_name))

# save stops as HTML files
stops_dir0 # Laos Transit Center to Casino Del Sol
stops_dir1 # Casino Del Sol to Laos Transit Center

# saveWidget(stops_dir0, file = "Valencia/output/stops_LTC_CDS.html")
# saveWidget(stops_dir1, file = "Valencia/output/stops_CDS_LTC.html")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transit Route Schedule -------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# A: get departure times for each trip

getDepTimes <- function(data) {
    
    # ignore departure time for last stop sequence
    len_stop_seq <- max(data$stop_sequence)
    
    DT <- copy(data)[stop_sequence != len_stop_seq, .(trip_id, stop_sequence, departure_time)]
    DT[, dynust_format := paste0(hour(departure_time), '.', sprintf('%02d', minute(departure_time)))]
    DT$departure_time <- NULL
    
    DT <- dcast(DT, trip_id ~ stop_sequence, value.var = 'dynust_format')
    DT <- DT[order(as.numeric(`1`))][, trip_id := substr(as.character(trip_id), 4, 7)]
    
    return (DT)
}

split_trips_dir0 <- getDepTimes(select_stop_times_dir0) # LTC to CDS
split_trips_dir1 <- getDepTimes(select_stop_times_dir1) # CDS to LTC

# fwrite(split_trips_dir0, 'Valencia/output/2021_GTFS_trip_departure_LTC_CDS.txt', sep = '\t')
# fwrite(split_trips_dir1, 'Valencia/output/2021_GTFS_trip_departure_CDS_LTC.txt', sep = '\t')

# B: distance along bus stops in each route

ex_trip_id_dir0 <- select_stop_times_dir0$trip_id[1]
ex_trip_id_dir1 <- select_stop_times_dir1$trip_id[1]

getStopDist <- function(x, ex_trip_id) {
    y <- copy(x)[trip_id == ex_trip_id, .(stop_sequence, shape_dist_traveled)]
    y[, dist_feet := shape_dist_traveled * 3280.84]
    
    y$dist_feet[1] <- 0
    y[, dist_feet := round(shift(dist_feet, type = 'lead') - dist_feet, 0L)]
}

stop_dist_dir0 <- getStopDist(select_stop_times_dir0, ex_trip_id_dir0)
stop_dist_dir1 <- getStopDist(select_stop_times_dir1, ex_trip_id_dir1)

stop_dist <- data.table(
    stop_seq = seq(1, nrow(stop_dist_dir0), 1),
    CDS_LTC = stop_dist_dir1$dist_feet,
    LTC_CDS = stop_dist_dir0$dist_feet
) |> head(-1L)

# fwrite(stop_dist, 'Valencia/output/2021_GTFS_stop_dist.csv')

# C: create route data in DynusT format

dynust_bus_route <- fread("Valencia/data/DynusT_Valencia_BusStop_Distance.csv")

aNode_CL <- dynust_bus_route$aNode1
bNode_CL <- dynust_bus_route$bNode1

aNode_LC <- dynust_bus_route$aNode2
bNode_LC <- dynust_bus_route$bNode2

dynust_stop_dist_CL <- dynust_bus_route$`CDS-LTC`
dynust_stop_dist_LC <- dynust_bus_route$`LTC-CDS`

getDynustRoute <- function(aNode, bNode, stop_dist, stop_seq_start) {
    
    num_row <- length(aNode)
    rep_neg1 <- rep(-1L, num_row)
    rep_zero <- rep(0L, num_row)
    
    dt <- data.table(
        aNode = aNode,
        bNode = bNode,
        num_stop = sprintf('%.1f', rep(1L, num_row)),
        stop_seq = seq(stop_seq_start, stop_seq_start + num_row - 1L, 1L),
        col1 = rep_neg1,
        col2 = rep_neg1,
        col3 = rep_neg1,
        dist = stop_dist,
        col4 = rep_neg1,
        col5 = rep_neg1,
        col6 = rep_neg1,
        col7 = rep_zero,
        col8 = rep_zero,
        col9 = rep_zero,
        col10 = rep_zero
    )
    
    return(dt)
}

dynust_route_CL <- getDynustRoute(aNode_CL, bNode_CL, dynust_stop_dist_CL, 101L)
dynust_route_LC <- getDynustRoute(aNode_LC, bNode_LC, dynust_stop_dist_LC, 201L)

# fwrite(dynust_route_LC, 'Valencia/output/2021_GTFS_dynust_route_LTC_CDS.txt', sep = '\t')
# fwrite(dynust_route_CL, 'Valencia/output/2021_GTFS_dynust_route_CDS_LTC.txt', sep = '\t')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transit Dwell Time -----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dwell_time <- 0.15

getDwellTime <- function(trips_dep_df) {
    
    num_trips <- nrow(trips_dep_df)
    num_stops <- length(trips_dep_df) - 1L
    
    dt <- cbind(as.data.table(trips_dep_df$trip_id),
                as.data.table(matrix(dwell_time, num_trips, num_stops)))
    
    return(dt)
}

dwell_time_CL <- getDwellTime(split_trips_dir1)
dwell_time_LC <- getDwellTime(split_trips_dir0)

# fwrite(dwell_time_LC, 'Valencia/output/2021_GTFS_dwell_time_LTC_CDS.txt', sep = '\t')
# fwrite(dwell_time_CL, 'Valencia/output/2021_GTFS_dwell_time_CDS_LTC.txt', sep = '\t')
