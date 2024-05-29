library(sf)
library(ggplot2)
library(dplyr)
data101 <- st_read("C:/Users/picha/Downloads/station_ids.csv")
data101 <- data101[, !names(data101) %in% c("nameTH","areaTH")]
data101
str(data101)
data101$lat <- as.numeric(data101$lat)
data101$long <- as.numeric(data101$long)

data99 <- st_read("C:/Users/picha/Desktop/summer/griddd/2021selected/032021r.shp")
data99

#"41t"	"76t"	"86t"	"92t"	"94t"	"95t"	"96t"	"97t"	"98t" is station

# Filter data101 to select rows with specified stationIDs
selected_stationIDs <- c("94t", "76t", "41t", "95t", "86t", "96t", "92t", "98t", "97t")
filtered_data101 <- data101 %>% filter(stationID %in% selected_stationIDs)
# Convert the filtered data101 to an sf object
filtered_data101_sf <- st_as_sf(filtered_data101, coords = c("long", "lat"), crs = 4326)
# Indices of polygons to highlight in light blue
highlight_indices <- c(104, 28, 101, 74, 102, 73, 152, 100)
# Plot the map
ggplot() +
  # Plot all polygons in light grey (map)
  geom_sf(data = data99, fill = 'lightgrey', color = 'black', alpha = 0.5) +
  # Highlight specific polygons in light blue (square)
  geom_sf(data = data99[highlight_indices, ], fill = 'lightblue', color = 'lightblue', alpha = 0.5) +
  # Plot the filtered points in red (station)
  geom_sf(data = filtered_data101_sf, color = 'red', size = 2) +
  theme_minimal()

