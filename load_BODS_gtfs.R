## load SW bus open data from DfT

## https://data.bus-data.dft.gov.uk/timetable/download/

## use tidytransit package to load the data

## data downloaded and saved in:
## C:\Users\tom.alexander1\OneDrive - West Of England Combined Authority\Transport\2.2 Bus\BODS\gtfs

## script loads gtfs from file then tidies:
##          - converts to sf
##          - clips bus routes to only those that travel in the West of England
##          - add direction_id column in trips table
##          - identifies trips with missing shapes
##          - create linestring for missing shapes from stop_times

## load libraries
library(tidytransit)
library(sf)
library(dplyr)

######### load objects from file #########
# gtfs zip file
sw_gtfs <- tidytransit::read_gtfs("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/2.2 Bus/BODS/gtfs/itm_south_west_gtfs.zip")
# weca boundary line
weca_bdline <- st_read("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/7.0 Data/03 Analysis Projects/accessibility_connectivity/shp/weca_bdline.shp")

######### convert to sf #########
gtfs <- gtfs_as_sf(sw_gtfs, crs = 27700)

######### clip to weca only services #########
weca_gtfs <- filter_feed_by_area(gtfs, weca_bdline)
rm(sw_gtfs, gtfs)

######### add direction_id column in trips table #########
weca_gtfs$trips <- weca_gtfs$trips %>% left_join(weca_gtfs$routes, by="route_id") %>% 
  #filter(route_short_name == 7) %>% 
  group_by(route_id) %>% 
  mutate(direction_id = as.integer(factor(trip_headsign))) %>% 
  ungroup()

######### identify trips with missing shapes #########
# trips with no shape
trips_missing_shapes <- weca_gtfs$trips %>% filter(shape_id == "") %>% pull(trip_id)

# stop times for trips with missing shapes
stop_times_missing_shapes <- weca_gtfs$stop_times %>% 
  filter(trip_id %in% trips_missing_shapes)



shapes <- weca_gtfs$shapes %>% st_transform(4326)
leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  #addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addPolylines(data = shapes, weight = 2, color = "purple", group = "BOD data" ) %>% 
  addPolylines(data = bus_routes, weight = 2, color = "cyan", group = "theo DB data") %>% 
  addLayersControl(baseGroups = c("BOD data", "theo DB data"), options = layersControlOptions(collapsed = FALSE))

route_40s_stops <- weca_gtfs$trips %>% filter(route_short_name %in% c(42,43,44,45)) %>% 
  left_join(weca_gtfs$stop_times, by="trip_id") %>% 
  select(stop_id) %>% 
  distinct() %>% 
  pull(stop_id)


######## connect to DB and load First's Prospective data #########

## IDEA: use a network version of the prospective data to recreate the missing BODS shapes
# databse connection
if(!exists("connec")) {
  # source the connect_postgreSQL.R script
  source("C:\\Users\\tom.alexander1\\OneDrive - West Of England Combined Authority\\Transport\\7.0 Data\\Rscripts\\access_postgresql\\access_to_postgresql\\connect_postgreSQL.R")
}
# sql query (using glue package for syntax formatting)
sql <- glue_sql( "SELECT * FROM prospective.congestion ",
                  "WHERE bounding_start_stop IN ({route_40s_stops*}) ",
                  "AND bounding_end_stop IN ({route_40s_stops*}) ",
                  "AND data_period = '2023 Oct - 2023 Dec' ",
                  #"AND hour_of_day = '08:00:00' " ,
                  #"FETCH FIRST 200 ROWS ONLY", 
                 .con = connec)
# fetch data
route_40s_pros_route <- st_read(connec, query = sql)



####### tidy prospective data ########
route_40s_pros_route_tidy <- route_40s_pros_route %>% 
  group_by(shape, hour_of_day) %>%
 # first(start_stop_name) %>% 
  summarise(delay_per_m = mean(delay_per_m), average_total_occupancy = mean(average_total_occupancy)) %>% 
  st_transform(4326)

####### leaflet plot of prospective data #########
# palette
pal <- colorQuantile(palette = "Blues", domain = route_40s_pros_route_tidy$delay_per_m, 6)


leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  #addProviderTiles(providers$CartoDB.DarkMatter) %>%
  #addPolylines(data = shapes, weight = 2, color = "purple", group = "BOD data" ) %>% 
  #addPolylines(data = bus_routes, weight = 2, color = "cyan", group = "theo DB data") %>% 
  addPolylines(data = route_40s_pros_route_tidy, weight = 4, color = ~pal(route_40s_pros_route_tidy$delay_per_m), group = "prospective data") %>%
  addLayersControl(baseGroups = c("BOD data", "theo DB data", "prospective data"), options = layersControlOptions(collapsed = FALSE))
                              
## next steps                             

# congestion_data_periods <- st_read(connec, query = "SELECT DISTINCT data_period FROM prospective.congestion")

## OP736 = First Bristol, Bath & the West
first_routes <- sw_gtfs$routes %>% filter(agency_id == "OP736")
first_shapes <- sw_gtfs$shapes

# After setting the service patterns, we can summarise each service by the 
# number of trips and stops. We’ll also summarise the total distance covered 
# by all trips in the service, and then check that against the total distance 
# covered by the average route. First, we need to calculate the distance of 
# each part of the route shapes. To do this (and for creating maps later on) 
# we convert stops and shapes to simple features with gtfs_as_sf.

gtfs <- set_servicepattern(sw_gtfs)
gtfs <- gtfs_as_sf(gtfs)
gtfs$shapes$length <- st_length(gtfs$shapes)

shape_lengths <- gtfs$shapes %>% 
  as.data.frame() %>% 
  select(shape_id, length, -geometry)

# subset for only one operator

OP736_routes <- gtfs$routes %>% filter(agency_id == "OP736") %>% pull(route_id)


gtfs$trips <- gtfs$trips %>% filter(route_id %in% OP736_routes)
OP736_shapes <- gtfs$trips %>% pull(shape_id) %>% unique()

gtfs$shapes <- gtfs$shapes %>% filter(shape_id %in% OP736_shapes)

# count trips per route
OP736_trip_summary <- gtfs$trips %>% 
  left_join(gtfs$.$servicepatterns, by="service_id") %>% 
  left_join(shape_lengths, by="shape_id") %>%
  group_by(route_id) %>%
  summarise(#route_short_name = route_short_name,
            total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
            trips = n()) %>%
  left_join(first_routes %>% select(route_id, route_short_name), by="route_id")
           

# Now we’re ready to roll the statistics up to services.


service_pattern_summary <- gtfs$trips %>% 
  left_join(gtfs$.$servicepatterns, by="service_id") %>% 
  left_join(shape_lengths, by="shape_id") %>%
  left_join(gtfs$stop_times, by="trip_id") %>% 
  group_by(servicepattern_id) %>% 
  summarise(
    trips = n(), 
    routes = n_distinct(route_id),
    total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
    route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
    stops=(n_distinct(stop_id)/2))


