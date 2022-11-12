library(sf)
library(leaflet)
library(data.table)

path <- "Valencia/data/ShapeFiles_Study_Area/"

get_path <- function(file_name) {
    paste0(path, file_name, "/", file_name, ".shp")
}

crs <- "+proj=longlat +datum=WGS84"

# Study region

study_region <- st_transform(st_read(get_path('DTA_Study_Region')), crs = crs)

study_region |> 
    leaflet() |> 
    addTiles() |> 
    addPolygons()

# Intersections

intersections <- st_transform(st_read(get_path('Intersections')), crs = crs)

intersections |> 
    leaflet() |> 
    addTiles() |> 
    addCircleMarkers(popup = ~STREET_ALL, radius = 1)

# Roads

roads <- st_transform(st_read(get_path('Roads')), crs = crs)

roads |> 
    leaflet() |> 
    addTiles() |> 
    addPolylines(popup = ~STREET)

as.data.table(roads)[, .N, by = ROADCAT_DS]

major_roads <- roads[roads$ROADCAT_DS != 'Minor local road', ]

major_roads |>
    leaflet() |> 
    addTiles() |> 
    addPolylines(popup = ~STREET)

# Signals

signals <- st_transform(st_read(get_path('Signals')), crs = crs)

signals |> 
    leaflet() |> 
    addTiles() |> 
    addCircleMarkers(popup = paste0(signals$RouteId, " : ", signals$SIG_SE), radius = 3)

unique(signals$SIG_SE)

# Signs

signs <- st_transform(st_read(get_path('Signs')), crs = crs)

signs |> 
    leaflet() |> 
    addTiles() |> 
    addCircleMarkers(popup = ~RouteId, radius = 1)

unique(signs$SI_Count)
unique(signs$MXFULLDESC)
