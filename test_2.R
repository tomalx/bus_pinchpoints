
# get list of stops 
route_40s_stops <- weca_gtfs$trips %>% filter(route_short_name %in% c(42,43,44,45)) %>% 
  left_join(weca_gtfs$stop_times, by="trip_id") %>% 
  filter(direction_id == 1) %>%
  group_by(stop_id, stop_sequence, trip_headsign, route_short_name) %>%
  summarise()
  # select(stop_id) %>% 
  # distinct() %>% 
  # pull(stop_id)

# get df of stop segments (between stops) in sequence
# e.g route_segment = from stop 1 to stop 2, stop 2 to stop 3 etc
# also need end stop of segment naptan reference and stop sequence
# also need to ensure that df has attribute to identify the direction of the route
# e.g. for 40 routes City Centre to Fountain Church Road in common.

route_40s_stops <- weca_gtfs$trips %>% filter(route_short_name %in% c(42,43,44,45)) %>% 
  left_join(weca_gtfs$stop_times, by="trip_id") %>%
  group_by(trip_id) %>% 
  arrange(trip_id, stop_sequence) %>%
  mutate(from_stop_id = lag(stop_id)) %>%
  ungroup() %>%
  select(from_stop_id, to_stop_id = stop_id, stop_sequence, trip_headsign, route_short_name, stop_sequence) 
  
 

route_40s_stops <- route_40s_stops %>% 
  group_by(to_stop_id, from_stop_id) %>% 
  # summarise(from_stop_id = from_stop_id, 
  #           to_stop_id = stop_id,
  #           #stop_sequence = stop_sequence,
  #           trip_headsign = trip_headsign) %>%
            distinct(.keep_all = TRUE)


# get a list of common stop sequence pairs across all routes and directions

  # Define the function
  summarise_strings <- function(strings) {
    if (n_distinct(strings) == 1) {
      return(first(strings))
    } else {
      return(paste(unique(strings), collapse = ", "))
    }
  }
  
  
 

common_stops <- route_40s_stops %>% 
  group_by(from_stop_id, to_stop_id) %>% 
  summarise(n = n(), direction = summarise_strings(trip_headsign), stop_sequence = first(stop_sequence), routes = summarise_strings(route_short_name)) %>% 
  mutate(direction = factor(direction, levels = unique(direction))) %>% 
  filter(direction != "Bristol City Centre") %>% 
  
  arrange(direction, stop_sequence)
  
  filter(n == 4) %>% 
  select(from_stop_id, to_stop_id) %>% 
  distinct()

route_40s_stops <- route_40s_stops %>% 
  group_by(trip_headsign, route_short_name) %>% 
  group_keys()






test <- test %>% st_transform(crs = 4326)

####### leaflet plot of prospective data #########
# palette
pal <- colorQuantile(palette = "Blues", domain = test$delay_per_m, 9)


leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  #addProviderTiles(providers$CartoDB.DarkMatter) %>%
  #addPolylines(data = shapes, weight = 2, color = "purple", group = "BOD data" ) %>% 
  #addPolylines(data = bus_routes, weight = 2, color = "cyan", group = "theo DB data") %>% 
  addPolylines(data = test, weight = 4, 
               color = ~pal(test$delay_per_m),
               popup = ~paste("Average total occupancy: ", delay_per_m),
               highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, weight = 8, sendToBack = FALSE),
               group = "delay per m")
