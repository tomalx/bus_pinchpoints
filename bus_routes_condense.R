

### function to merge and reduce size of bus routes file ########

### assumes bus_routes file already loaded into R,
### if not,
### can load data from GIS database using
### bus_routes <- st_read(connec,query = "SELECT * FROM arup.existingbusservices") %>%st_transform(crs = 4326)

# bus_routes <- st_read(connec,query = "SELECT * FROM arup.existingbusservices") %>%st_transform(crs = 4326)

#bus_routes <- bus_routes %>% filter(line_name %in% c("48", "48A", "49", "49A"))

# filter bus routes
bus_routes <- bus_routes %>% filter(line_name != "FALCON" & freqency > 0)


#plot(st_geometry(bus_routes))
#plot(bus_routes %>% select(diva_line))

#bus_routes <- st_multilinestring(unique(st_geometry(bus_routes)))

#frequency <- 2   # only show routes with frequency of x or more

bus_routes_size <- as.numeric(round(object.size(bus_routes)/1024, 0))

bus_routes_merged <- function(bus_routes_sf){
  
  comp <- bus_routes_sf %>% st_geometry() %>%  st_touches() %>% 
    graph_from_adj_list() %>% components() %>% .$membership
  
  bus_routes_sf <- bus_routes_sf %>% group_by(section = as.character(comp)) %>%
    summarise()
  
  return(bus_routes_sf)
  
}

bus_routes_touch <- bus_routes_merged(bus_routes) 

bus_routes_touch_size <- as.numeric(round(object.size(bus_routes_touch_size)/1024, 0))

#plot(bus_routes_touch)
