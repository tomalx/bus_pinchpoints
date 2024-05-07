


# bus operators in weca
weca_bus_operators <- weca_gtfs$routes %>% filter(route_type == 3) %>% 
  left_join(gtfs$agency, by="agency_id") %>% group_by(agency_id) %>%
  summarise(agency_name = first(agency_name),
            routes = n_distinct(route_id))

# route frequency using gtfs::get_route_frequency function

# filter non-bus routes
weca_gtfs$routes <- weca_gtfs$routes %>% filter(route_type == 3)

# get service id for Mon-Fri, Sat and Sun

service_id_MF <- weca_gtfs$calendar %>% filter(monday == 1 & tuesday == 1 & wednesday == 1 & thursday == 1 & friday == 1 &
                                               saturday == 0 & sunday == 0) %>% pull(service_id)
service_id_Sat <- weca_gtfs$calendar %>% filter(monday == 0 & tuesday == 0 & wednesday == 0 & thursday == 0 & friday == 0 &
                                                  saturday == 1 & sunday == 0) %>% pull(service_id)
service_id_Sun <- weca_gtfs$calendar %>% filter(monday == 0 & tuesday == 0 & wednesday == 0 & thursday == 0 & friday == 0 &
                                                  saturday == 1 & sunday == 0) %>% pull(service_id)

weca_routes_freq_MF <- weca_gtfs$trips %>% left_join(weca_gtfs$routes, by="route_id") %>%
  #filter(route_short_name == 7) %>%
  filter(service_id %in% service_id_MF) %>%
  group_by(route_id, route_short_name, trip_headsign) %>%  summarise(trips = n())  # number of trips per route

weca_routes_freq_Sat <- weca_gtfs$trips %>% left_join(weca_gtfs$routes, by="route_id") %>%
  #filter(route_short_name == 7) %>%
  filter(service_id %in% service_id_Sat) %>%
  group_by(route_id, route_short_name, trip_headsign) %>%  summarise(trips = n())  # number of trips per route

weca_routes_freq_Sun <- weca_gtfs$trips %>% left_join(weca_gtfs$routes, by="route_id") %>%
  #filter(route_short_name == 7) %>%
  filter(service_id %in% service_id_Sun) %>%
  group_by(route_id, route_short_name, trip_headsign) %>%  summarise(trips = n())  # number of trips per route



route_freq_MF <- get_route_frequency(weca_gtfs, start_time = "08:00:00", end_time = "10:00:00", service_ids = service_id_MF) %>% 
  left_join(weca_gtfs$routes, by="route_id")

route_freq_Sat <- get_route_frequency(weca_gtfs, start_time = "10:00:00", end_time = "15:00:00", service_ids = service_id_Sat) %>% 
  left_join(weca_gtfs$routes, by="route_id")

route_freq_Sun <- get_route_frequency(weca_gtfs, start_time = "10:00:00", end_time = "15:00:00", service_ids = service_id_Sun) %>% 
  left_join(weca_gtfs$routes, by="route_id")


# plot one or more routes
#trips_sf <- get_trip_geometry(gtfs_duke, c("t_726295_b_19493_tn_41", "t_726295_b_19493_tn_40"))
#plot(trips_sf[1,])



route_sf <- get_route_geometry(weca_gtfs, route_ids = 18502) %>% st_transform(4326)


# investigate missing shapes

u1_trip <- sw_gtfs$trips %>% filter(route_id == 15586) %>% nth(100) %>% pull(trip_id)
sus_stops <- sw_gtfs$stop_times %>% filter(trip_id == u1_trip) %>% pull(stop_id)
sw_gtfs$stops %>% filter(stop_id %in% sus_stops)

sw_gtfs$trips %>% filter(shape_id == "")

# identify trips with missing shapes

# create linestring from stop_times for one trip with missing shape



#

