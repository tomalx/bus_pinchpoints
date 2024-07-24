
# find latest file listed in rds folder
path <- "C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/2.2 Bus/BODS/rds/"

latest_rds_file <- list.files(path = path, full.names = T) %>% 
  enframe(name = NULL) %>% 
  bind_cols(pmap_df(., file.info)) %>% 
  filter(mtime==max(mtime)) %>% 
  pull(value)


# requires weca_gtfs to be loaded into env
if (!exists("weca_gtfs")) {
  print("weca_gtfs object not found in environment, attempting to load from file...")
  # load from direcotry
  weca_gtfs <- readRDS(latest_rds_file)
} else {
  print("weca_gtfs object found in environment")
}

######## get df of routes by stop ########
# e,g, city lines Church Rd services
route_40s_stops <- weca_gtfs$trips %>% 
  filter(route_short_name %in% c(42,43,44,45)) %>% 
  #filter(route_id == 6515, direction_id == 2) %>% 
  left_join(weca_gtfs$stop_times, by="trip_id") %>% 
  group_by(trip_headsign, route_short_name, stop_sequence, direction_id) %>%
  arrange(desc(arrival_time)) %>%    # ordering and filtering
  filter(row_number() == 1) %>%      # by row number catches
  summarise(first = first(trip_id), stop_id = stop_id) 

# convert stop points to route linestring
route_40s_routes <- route_40s_stops %>%
  left_join(weca_gtfs$stops %>% select(stop_id, geometry), by="stop_id") %>%
  st_as_sf(crs = 27700) %>%
  group_by(route_short_name, trip_headsign, direction_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

# # leaflet
# pal <- colorFactor(topo.colors(5), route_40s_routes$route_short_name)
# 
# 
# leaflet() %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolylines(data = route_40s_routes %>% st_transform(crs = 4326), weight = 2, color = ~pal(route_short_name), group = "routes") %>% 
#   addLayersControl(baseGroups = c("routes"), options = layersControlOptions(collapsed = FALSE))

####### leaflet plot of prospective data #########
# palette
palDly <- colorQuantile(palette = "Blues", domain = route_40s_pros_route_tidy$delay_per_m, 10)
palOcp <- colorQuantile(palette = "Reds", domain = route_40s_pros_route_tidy$average_total_occupancy, 10)

leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  #addProviderTiles(providers$CartoDB.DarkMatter) %>%
  #addPolylines(data = shapes, weight = 2, color = "purple", group = "BOD data" ) %>% 
  #addPolylines(data = bus_routes, weight = 2, color = "cyan", group = "theo DB data") %>% 
  addPolylines(data = route_40s_pros_route_tidy, weight = 4, 
               color = ~palDly(route_40s_pros_route_tidy$delay_per_m),
               popup = ~paste("Average total occupancy: ", delay_per_m),
               highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, weight = 8, sendToBack = FALSE),
               group = "delay per m") %>%
  addPolylines(data = route_40s_pros_route_tidy, weight = 4, 
               color = ~palOcp(route_40s_pros_route_tidy$average_total_occupancy),
               popup = ~paste("Average total occupancy: ", average_total_occupancy),
               highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, weight = 8, sendToBack = FALSE),
               group = "total occupancy") %>%
  #add(popup = ~paste("Delay per metre: ", delay_per_m, "<br> Average total occupancy: ", average_total_occupancy), group = "prospective data") %>% 
  addLayersControl(baseGroups = c("delay per m", "total occupancy"), options = layersControlOptions(collapsed = FALSE))






 