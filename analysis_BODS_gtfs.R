
# load weca boundary line from C:\Users\tom.alexander1\OneDrive - West Of England Combined Authority\Transport\7.0 Data\03 Analysis Projects\accessibility_connectivity\shp
weca_bdline <- st_read("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/7.0 Data/03 Analysis Projects/accessibility_connectivity/shp/weca_bdline.shp")

# cut gtfs feed for only weca bus services


# convert gtfs data crs and clip to weca only services
gtfs <- gtfs_as_sf(sw_gtfs, crs = 27700)
weca_gtfs <- filter_feed_by_area(gtfs, weca_bdline)

# bus operators in weca
weca_bus_operators <- weca_gtfs$routes %>% filter(route_type == 3) %>% 
  left_join(gtfs$agency, by="agency_id") %>% group_by(agency_id) %>%
  summarise(agency_name = first(agency_name),
            routes = n_distinct(route_id))

# route frequency using gtfs::get_route_frequency function

weca_routes_freq <- weca_gtfs$trips %>% left_join(weca_gtfs$routes, by="route_id") %>%
  #filter(route_short_name == 7) %>%
  group_by(route_id, route_short_name, trip_headsign) %>%  summarise(trips = n())  # number of trips per route

# add direction_id column with 0 or 1 value (needed for gtfs::get_route_frequency function)
weca_gtfs$trips <- weca_gtfs$trips %>% left_join(weca_gtfs$routes, by="route_id") %>% 
  #filter(route_short_name == 7) %>% 
  group_by(route_id) %>% mutate(direction_id = as.integer(factor(trip_headsign)))

route_freq <- get_route_frequency(weca_gtfs, start_time = "10:00:00", end_time = "15:00:00") %>% 
  left_join(weca_gtfs$routes, by="route_id")

