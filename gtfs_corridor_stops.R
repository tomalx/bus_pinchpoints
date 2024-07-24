
# get df of stop segments (between stops) in sequence
# e.g route_segment = from stop 1 to stop 2, stop 2 to stop 3 etc
# also need end stop of segment naptan reference and stop sequence
# also need to ensure that df has attribute to identify the direction of the route
# e.g. for 40 routes City Centre to Fountain Church Road in common.

# routes_on_corridor <- c(42,43,44,45)   # SET routes to be analysed

corridor_stops <- weca_gtfs$trips %>% filter(route_short_name %in% routes_on_corridor) %>% 
  left_join(weca_gtfs$stop_times, by="trip_id") %>%
  group_by(trip_id) %>% 
  arrange(trip_id, stop_sequence) %>%
  mutate(from_stop_id = lag(stop_id)) %>%
  ungroup() %>%
  select(from_stop_id, to_stop_id = stop_id, stop_sequence, trip_headsign, route_short_name, stop_sequence, direction_id) 


corridor_stops <- corridor_stops %>% 
  group_by(to_stop_id, from_stop_id) %>% 
  # summarise(from_stop_id = from_stop_id, 
  #           to_stop_id = stop_id,
  #           #stop_sequence = stop_sequence,
  #           trip_headsign = trip_headsign) %>%
  distinct(.keep_all = TRUE)


# get a list of common stop sequence pairs across all routes and directions

# Define the function used in summarise for naming directions
summarise_strings <- function(strings) {
  if (n_distinct(strings) == 1) {
    return(first(strings))
  } else {
    return(paste(unique(strings), collapse = ", "))
  }
}

common_stops <- corridor_stops %>%
  arrange(route_short_name) %>% 
  group_by(from_stop_id, to_stop_id) %>% 
  summarise(n = n(), direction = summarise_strings(trip_headsign), 
            stop_sequence = first(stop_sequence), routes = summarise_strings(route_short_name),
            direction_id = first(direction_id)) %>% 
  mutate(direction = factor(direction, levels = unique(direction))) %>% 
  #filter(direction != "Bristol City Centre") %>% 
  arrange(direction, stop_sequence)

common_stops <- common_stops %>% filter(routes == paste(routes_on_corridor, collapse = ", "))

route_summary <- weca_gtfs$trips %>% filter(route_short_name %in% routes_on_corridor) %>% 
  group_by(route_short_name) %>% 
  summarise(route_short_name = first(route_short_name), 
            trip_headsign = summarise_strings(trip_headsign))



