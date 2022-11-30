library(data.table)
library(leaflet)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GTFS Shapefiles --------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# routes

routes <- fread("Valencia/ignore/GTFS/gtfs_SunTran/routes.txt")
length(unique(routes$route_id))
routes <- routes[, .(route_id, route_long_name)]

# shapes

shapes <- fread("Valencia/ignore/GTFS/gtfs_SunTran/shapes.txt")
length(unique(shapes$shape_id))

leaflet() |> 
    addTiles() |> 
    addCircleMarkers(data = shapes, lng = ~shape_pt_lon, lat = ~shape_pt_lat, radius = 0.1)

# stop times

stop_times <- fread("Valencia/ignore/GTFS/gtfs_SunTran/stop_times.txt")
length(unique(stop_times$trip_id))

# stops

stops <- fread("Valencia/ignore/GTFS/gtfs_SunTran/stops.txt")
length(unique(stops$stop_id))
stops <- stops[, .(stop_id, stop_name, stop_desc, stop_lat, stop_lon)]

# trips

trips <- fread("Valencia/ignore/GTFS/gtfs_SunTran/trips.txt")
