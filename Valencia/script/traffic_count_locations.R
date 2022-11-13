library(googlesheets4)
library(data.table)
library(leaflet)
library(leaflegend)

url <- "https://docs.google.com/spreadsheets/d/1W8KRpJS6o5fqMzp_u9zUzhWbxWpN8M0ehl8WRSG8DzM/edit?usp=sharing"
cols <- c('StreetLight', 'LocationID', 'Route', 'Count_Type', 'From', 'To', 'Latitude',
          'Longitude', 'AADT', 'Direction', 'Perc_Truck_Revised', 'Perc_Truck_StL')

DT <- as.data.table(read_sheet(url, sheet = 'AADT'))

DT <- DT[, .SD, .SDcols = cols]
DT[, LocationID := as.character(LocationID)]

get_data_text <- function(x, count_type, stl_only = FALSE) {
    data <- copy(x)[Count_Type == count_type, ]
    
    text <- lapply(seq(nrow(data)), function(i) {
        
        if (stl_only == FALSE) {
            truck_text <- data[i, 'Perc_Truck_Revised']
        } else {
            truck_text <- data[i, 'Perc_Truck_StL']
        }
        
        return(paste0('<p>', 'Route: ', data[i, 'Route'], ' (', data[i, 'Direction'], ') ', '<p></p>', 
                      'ID: ', data[i, 'LocationID'], '<p></p>', 
                      'From ', data[i, 'From'], ' to ', data[i, 'To'], '<p></p>',
                      'AADT: ', data[i, 'AADT'], '<p></p>',
                      'Truck AADT: ', truck_text, '%', '</p>'))
    })
    
    return(list(data = data, text = text))
}

DT_perm <- get_data_text(DT, 'Permanent')$data
DT_perm_text <- get_data_text(DT, 'Permanent')$text

DT_short <- get_data_text(DT, 'Short')$data
DT_short_text <- get_data_text(DT, 'Short')$text

DT |> 
    leaflet() |> 
    addTiles() |> 
    addCircleMarkers(lng = ~Longitude, lat = ~Latitude, data = DT_perm,
                     color = 'red', radius = 10, 
                     popup = DT_perm_text, group = 'Permanent') |> 
    addCircleMarkers(lng = ~Longitude, lat = ~Latitude, data = DT_short,
                     color = 'blue', radius = 8, 
                     popup = DT_short_text, group = 'Short') |> 
    addLayersControl(overlayGroups = c('Permanent', 'Short'),
                     options = layersControlOptions(position = 'bottomright'))


# Truck volume as % of AADT

dt <- copy(DT)[StreetLight == 1, ]

dt_perm <- get_data_text(dt, 'Permanent', TRUE)$data
dt_perm_text <- get_data_text(dt, 'Permanent', TRUE)$text

dt_short <- get_data_text(dt, 'Short', TRUE)$data
dt_short_text <- get_data_text(dt, 'Short', TRUE)$text

dt |> 
    leaflet() |> 
    addTiles() |> 
    addProviderTiles(providers$CartoDB.Positron, group = 'Toner') |> 
    addCircleMarkers(lng = ~Longitude, lat = ~Latitude, data = dt_perm,
               color = 'black', radius = ~Perc_Truck_StL, 
               popup = dt_perm_text, group = 'Permanent') |> 
    addCircleMarkers(lng = ~Longitude, lat = ~Latitude, data = dt_short,
                     color = 'black', radius = ~Perc_Truck_StL, 
                     popup = dt_short_text, group = 'Short') |> 
    addLayersControl(overlayGroups = c('Permanent', 'Short'),
                     options = layersControlOptions(position = 'bottomright')) |> 
    addLegendSize(
        values = dt$Perc_Truck_StL,
        color = 'black',
        fillColor = 'black',
        opacity = .5,
        title = 'Truck %',
        shape = 'circle',
        orientation = 'horizontal',
        breaks = 4)
