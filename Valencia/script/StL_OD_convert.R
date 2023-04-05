library(data.table)
library(ggplot2)

num_zone <- 1104

# create empty data set of required zones and hours
OD <- data.table(
    OZoneID = sort(rep.int(1:num_zone, num_zone * 24)),
    DZoneID = rep.int(sort(rep.int(1:num_zone, 24)), num_zone),
    Hour = rep.int(0:23, num_zone * num_zone) * 60
)

input_dir <- "Valencia/ignore/StreetLight/2021_ODD_v2/"
input_file_vehicles <- "vehicles"
input_file_truck <- "trucks"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get OD demand from StreetLight analysis -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getOD <- function(type, input_file) {
  
  path <- paste0(input_dir, input_file, ".csv")
  DT <- fread(path)
  
  # rename variables
  setnames(DT, 'Origin Zone ID', 'OZoneID')
  setnames(DT, 'Destination Zone ID', 'DZoneID')
  setnames(DT, 'Day Part', 'Hour')
  setnames(DT, 'Average Daily O-D Traffic (StL Calibrated Index)', 'Volume')
  
  DT <- DT[`Day Type` == '1: Weekday (M-Th)' & Hour != '00: All Day (12am-12am)', ]
  
  # aggregate demand for trucks
  if (type == 'truck'){
    DT <- DT[, .(Volume = sum(Volume)), by = .(OZoneID, DZoneID, Hour)]
  }
  
  DT <- DT[, .(OZoneID, DZoneID, Hour, Volume)]
  DT[, Hour := (as.integer(substr(Hour, 1, 2)) - 1) * 60]
  
  # merge data sets
  DT <- merge(OD, DT, by = c('OZoneID', 'DZoneID', 'Hour'), all = TRUE)
  DT <- DT[order(Hour, OZoneID, DZoneID)]
  DT[, Volume := fifelse(is.na(Volume), 0, Volume)]
  
  # write demand
  output_file <- paste0(input_dir, input_file, ".DAT")
  fwrite(DT, output_file, sep = '\t')
  
  return(DT)
}

demand_car <- getOD('vehicles', input_file_vehicles)
demand_truck <- getOD('truck', input_file_truck)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Demand distribution ----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dist_car <- copy(demand_car)[, .(Total_Volume = sum(Volume)), by = .(Hour)][, Hour := Hour / 60]
dist_truck <- copy(demand_truck)[, .(Total_Volume = sum(Volume)), by = .(Hour)][, Hour := Hour / 60]

total_car <- round(sum(dist_car$Total_Volume) / 10^6, 2)
total_truck <- round(sum(dist_truck$Total_Volume) / 10^3, 1)

max(dist_car$Total_Volume)
max(dist_truck$Total_Volume)

plot_car <- dist_car |> 
  ggplot() + 
  geom_point(aes(Hour, Total_Volume), size = 3) + 
  geom_line(aes(Hour, Total_Volume)) + 
  scale_x_continuous(breaks = seq(0, 23, 3)) + 
  scale_y_continuous(breaks = seq(0, 200000, 25000)) + 
  annotate("text", x = 12, y = 12500, 
           label = paste0("Total OD traffic in 24 hours = ", total_car, " million"), size = 5) +
  labs(x = "Hour", y = "Total OD Traffic for Car") + 
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

plot_truck <- dist_truck |> 
  ggplot() + 
  geom_point(aes(Hour, Total_Volume), size = 3) + 
  geom_line(aes(Hour, Total_Volume)) + 
  scale_x_continuous(breaks = seq(0, 23, 3)) + 
  scale_y_continuous(breaks = seq(0, 2000, 250)) + 
  annotate("text", x = 10, y = 150, 
           label = paste0("Total OD traffic in 24 hours = ", total_truck, " thousand"), size = 5) +
  labs(x = "Hour", y = "Total OD Traffic for Truck") + 
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

plot_car
plot_truck

output_dir <- "Valencia/output/"

ggsave(paste0(output_dir, "2021_demand_dist_car.png"),
       plot = plot_car,
       units = "cm",
       width = 29.7,
       height = 21,
       dpi = 600)

ggsave(paste0(output_dir, "2021_demand_dist_truck.png"),
       plot = plot_truck,
       units = "cm",
       width = 29.7,
       height = 21,
       dpi = 600)


# Check demand

dem_veh <- fread(paste0(input_dir, input_file_vehicles, ".DAT"))
dem_truck <- fread(paste0(input_dir, input_file_truck, ".DAT"))

total_dem_veh <- sum(dem_veh$Volume)
total_dem_truck <- sum(dem_truck$Volume)

dem_veh[, .(demand = round(sum(Volume) / total_dem_veh * 100, 2)), by = .(Hour)]
dem_truck[, .(demand = round(sum(Volume) / total_dem_truck * 100, 2)), by = .(Hour)]
