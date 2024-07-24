

######### identify trips with missing shapes #########
# trips with no shape
trips_missing_shapes <- weca_gtfs$trips %>% filter(shape_id == "") %>% pull(trip_id)

# stop times for trips with missing shapes
stop_times_missing_shapes <- weca_gtfs$stop_times %>% 
  filter(trip_id %in% trips_missing_shapes)

################################################################################
library(leaflet)
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


route_6or7_stops <- weca_gtfs$trips %>% filter(route_short_name %in% c(6,7)) %>% 
  left_join(weca_gtfs$stop_times, by="trip_id") %>% 
  select(stop_id) %>% 
  distinct() %>% 
  pull(stop_id)

route_75or76_stops <- weca_gtfs$trips %>% filter(route_short_name %in% c(75,76)) %>% 
  left_join(weca_gtfs$stop_times, by="trip_id") %>% 
  select(stop_id) %>% 
  distinct() %>% 
  pull(stop_id)

######## connect to DB and load First's Prospective data #########
library(glue)
## IDEA: use a network version of the prospective data to recreate the missing BODS shapes
# databse connection
if(!exists("connec")) {
  # source the connect_postgreSQL.R script
  source("C:\\Users\\tom.alexander1\\OneDrive - West Of England Combined Authority\\Transport\\7.0 Data\\Rscripts\\access_postgresql\\access_to_postgresql\\connect_postgreSQL.R")
}
# sql query (using glue package for syntax formatting)
sql <- glue_sql( "SELECT * FROM prospective.congestion ",
                 "WHERE bounding_start_stop IN ({route_75or76_stops*}) ",
                 "AND bounding_end_stop IN ({route_75or76_stops*}) ",
                 "AND data_period = '2023 Oct - 2023 Dec' ",
                 #"AND hour_of_day = '08:00:00' " ,
                 #"FETCH FIRST 200 ROWS ONLY", 
                 .con = connec)
# fetch data
route_75or76_pros_route <- st_read(connec, query = sql)


####### tidy prospective data ########
route_75or76_pros_route_tidy <- route_75or76_pros_route %>% 
  group_by(shape, hour_of_day) %>%
  # first(start_stop_name) %>% 
  summarise(delay_per_m = mean(delay_per_m), average_total_occupancy = mean(average_total_occupancy)) %>% 
  st_transform(4326)

####### leaflet plot of prospective data #########
# palette
pal <- colorQuantile(palette = "Blues", domain = route_75or76_pros_route_tidy$delay_per_m, 10)
palOcc <- colorQuantile(palette = "Reds", domain = route_75or76_pros_route_tidy$average_total_occupancy, 10)

leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  #addProviderTiles(providers$CartoDB.DarkMatter) %>%
  #addPolylines(data = shapes, weight = 2, color = "purple", group = "BOD data" ) %>% 
  #addPolylines(data = bus_routes, weight = 2, color = "cyan", group = "theo DB data") %>% 
  addPolylines(data = route_75or76_pros_route_tidy, weight = 4, 
               color = ~pal(route_75or76_pros_route_tidy$delay_per_m),
               popup = ~paste("Average total occupancy: ", delay_per_m),
               highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, weight = 8, sendToBack = FALSE),
               group = "delay per m") %>%
  addPolylines(data = route_75or76_pros_route_tidy, weight = 4, 
               color = ~palOcc(route_75or76_pros_route_tidy$average_total_occupancy),
               popup = ~paste("Average total occupancy: ", average_total_occupancy),
               highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, weight = 8, sendToBack = FALSE),
               group = "total occupancy") %>%
  #add(popup = ~paste("Delay per metre: ", delay_per_m, "<br> Average total occupancy: ", average_total_occupancy), group = "prospective data") %>% 
  addLayersControl(baseGroups = c("delay per m", "total occupancy"), options = layersControlOptions(collapsed = FALSE))

## next steps - split out visulaising prospective data from the BODS data upload.
## turning prospective data interactive with controls for:
##            - directiom (inbound, outbound)
##            - time of day
##            - scale - delay per metre
##            - scale - average total occupancy
##            - use time periods too

# congestion_data_periods <- st_read(connec, query = "SELECT DISTINCT data_period FROM prospective.congestion")

## OP736 = First Bristol, Bath & the West
first_routes <- sw_gtfs$routes %>% filter(agency_id == "OP736")
first_shapes <- sw_gtfs$shapes

# After setting the service patterns, we can summarise each service by the 
# number of trips and stops. Web