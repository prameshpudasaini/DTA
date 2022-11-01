library(data.table)
library(ggplot2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get OD values from StreetLight analysis -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

num_zone <- 1104

OD <- data.table(
  OZoneID = sort(rep.int(1:num_zone, num_zone * 24)),
  DZoneID = rep.int(sort(rep.int(1:num_zone, 24)), num_zone),
  Hour = rep.int(0:23, num_zone * num_zone) * 60
)

getOD <- function(type, folder) {
  
  if (type == 'vehicles'){
    csv_ext <- 'od_all'
  } else {
    csv_ext <- 'od_comm'
  }
  
  path <- paste0("./ignore/Valencia/StreetLight/", folder, "/", folder, "/", folder, "_", csv_ext, ".csv")
  DT <- fread(path)
  
  setnames(DT, "Origin Zone ID", "OZoneID")
  setnames(DT, "Destination Zone ID", "DZoneID")
  setnames(DT, "Day Part", "Hour")
  setnames(DT, "Average Daily O-D Traffic (StL Calibrated Index)", "Volume")
  
  DT <- DT[`Day Type` == "1: Weekday (M-Th)" & Hour != "00: All Day (12am-12am)", ]
  
  if (type == 'truck'){
    DT <- DT[, .(Volume = sum(Volume)), by = .(OZoneID, DZoneID, Hour)]
  }
  
  DT <- DT[, .(OZoneID, DZoneID, Hour, Volume)]
  DT[, Hour := (as.integer(substr(Hour, 1, 2)) - 1) * 60]
  DT <- DT[order(OZoneID, Hour, DZoneID)]
  
  DT <- merge(OD, DT, by = c("OZoneID", "DZoneID", "Hour"), all = TRUE)
  DT[, Volume := fifelse(is.na(Volume), 0, Volume)]
}

stl_vehicles_folder <- "1202750_2021_ODD_Vehicles"
stl_truck_folder <- "1202751_2021_ODD_Truck"

# Write demand to DAT files
demand_car <- getOD('vehicles', stl_vehicles_folder)
fwrite(demand_car, paste0("./ignore/Valencia/StreetLight/", stl_vehicles_folder, ".DAT"), sep = "\t")

demand_truck <- getOD('truck', stl_truck_folder)
fwrite(demand_truck, paste0("./ignore/Valencia/StreetLight/", stl_truck_folder, ".DAT"), sep = "\t")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Demand distribution ----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dist_car <- copy(demand_car)[, .(Total_Volume = sum(Volume)), by = .(Hour)][, Hour := Hour / 60]
dist_truck <- copy(demand_truck)[, .(Total_Volume = sum(Volume)), by = .(Hour)][, Hour := Hour / 60]

sum(dist_car$Total_Volume)
sum(dist_truck$Total_Volume)

max(dist_car$Total_Volume)
max(dist_truck$Total_Volume)

plot_car <- dist_car |> 
  ggplot() + 
  geom_point(aes(Hour, Total_Volume), size = 3) + 
  geom_line(aes(Hour, Total_Volume)) + 
  scale_x_continuous(breaks = seq(0, 23, 3)) + 
  scale_y_continuous(breaks = seq(0, 200000, 25000)) + 
  annotate("text", x = 12, y = 15000, label = "Total volume of cars in 24 hours = 2,332,527", size = 5) +
  labs(x = "Hour", y = "Total Volume of Cars") + 
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

plot_truck <- dist_truck |> 
  ggplot() + 
  geom_point(aes(Hour, Total_Volume), size = 3) + 
  geom_line(aes(Hour, Total_Volume)) + 
  scale_x_continuous(breaks = seq(0, 23, 3)) + 
  scale_y_continuous(breaks = seq(0, 2000, 250)) + 
  annotate("text", x = 10, y = 150, label = "Total volume of trucks in 24 hours = 16,535", size = 5) +
  labs(x = "Hour", y = "Total Volume of Trucks") + 
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

plot_car
plot_truck

ggsave("./Valencia/figures/2021_demand_dist_car.png",
       plot = plot_car,
       units = "cm",
       width = 29.7,
       height = 21,
       dpi = 600)

ggsave("./Valencia/figures/2021_demand_dist_truck.png",
       plot = plot_truck,
       units = "cm",
       width = 29.7,
       height = 21,
       dpi = 600)


# Check demand

dem_veh <- fread("ignore/Valencia/StreetLight/1202750_2021_ODD_Vehicles.DAT")
dem_truck <- fread("ignore/Valencia/StreetLight/1202751_2021_ODD_Truck.DAT")

dem_veh[, .(demand = sum(Volume)), by = .(Hour)]
dem_truck[, .(demand = sum(Volume)), by = .(Hour)]