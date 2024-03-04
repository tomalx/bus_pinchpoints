

###############################
### load data:
###  - traffic master shapefiles
###  - bus priority
###  - bus routes

setwd("C:\\Users\\tom.alexander1\\OneDrive - West Of England Combined Authority\\Transport\\2.1 Walking, Cycling & Wheeling\\")
library(tidyverse)
library(sf)

####   load trafficmaster   ########################################################
geojson_file_path <- "C:\\Users\\tom.alexander1\\OneDrive - West Of England Combined Authority\\Transport\\7.0 Data\\01 Regional Modelling\\Trafficmaster\\data\\geojson\\"
tm_0700_0800 <- read_sf(paste0(geojson_file_path, "tm_0700_0800.geojson")) %>%
  st_transform(crs = 4326)
tm_0800_0900 <- read_sf(paste0(geojson_file_path, "tm_0800_0900.geojson")) %>%
  st_transform(crs = 4326)
tm_1000_1600 <- read_sf(paste0(geojson_file_path, "tm_1000_1600.geojson")) %>%
  st_transform(crs = 4326)
tm_1600_1700 <- read_sf(paste0(geojson_file_path, "tm_1600_1700.geojson")) %>%
  st_transform(crs = 4326)
tm_1700_1800 <- read_sf(paste0(geojson_file_path, "tm_1700_1800.geojson")) %>%
  st_transform(crs = 4326)




####   load bus priority   ########################################################

## check whether the 'connec' database connection exists in the global environment
## if not then connect to the database using connect_postgreSQL.R

path_to_helper_scripts <- "C:\\Users\\tom.alexander1\\OneDrive - West Of England Combined Authority\\Transport\\7.0 Data\\Rscripts\\"
 
if(!exists("connec")) {
  # source the connect_postgreSQL.R script
  source(paste0(path_to_helper_scripts,"access_postgresql\\access_to_postgresql\\connect_postgreSQL.R"))
}

bcc_bus_lanes <- st_read(connec,query = "SELECT * FROM bcc.bus_lane_centrelines")
bcc_bus_gates <- st_read(connec,query = "SELECT * FROM bcc.bus_gate_lines")


####   load bus routes   ##########################################################

bus_routes <- st_read(connec,query = "SELECT * FROM arup.existingbusservices")
 
