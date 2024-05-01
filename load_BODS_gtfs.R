## load SW bus open data from DfT

## https://data.bus-data.dft.gov.uk/timetable/download/

## use tidytransit package to load the data

## data downloaded and saved in:
## C:\Users\tom.alexander1\OneDrive - West Of England Combined Authority\Transport\2.2 Bus\BODS\gtfs

sw_gtfs <- tidytransit::read_gtfs("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/2.2 Bus/BODS/gtfs/itm_south_west_gtfs.zip")

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


