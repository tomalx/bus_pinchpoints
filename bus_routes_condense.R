

### function to merge and reduce size of bus routes file ########

### assumes bus_routes file already loaded into R,
### if not,
### can load data from GIS database using
### bus_routes <- st_read(connec,query = "SELECT * FROM arup.existingbusservices") %>%st_transform(crs = 4326)

bus_routes <- st_read(connec,query = "SELECT * FROM arup.existingbusservices") %>%st_transform(crs = 4326)

bus_routes <- bus_routes %>% filter(line_name %in% c("48", "48A", "49", "49A"))

comp <- components(graph.adjlist(st_touches(st_geometry(bus_routes))))
bus_routes$group <- comp$membership

#summarise by group
bus_routes <- bus_routes %>% group_by(group) %>% summarise()



ggplot() + geom_sf(data = bus_routes)
ggplot() + geom_sf(data = bus_routes_touch)

# filter bus routes
bus_routes <- bus_routes %>% filter(line_name != "FALCON" & freqency > 0)


#plot(st_geometry(bus_routes))
#plot(bus_routes %>% select(diva_line))

#bus_routes <- st_multilinestring(unique(st_geometry(bus_routes)))

#frequency <- 2   # only show routes with frequency of x or more

bus_routes_size <- as.numeric(round(object.size(bus_routes)/1024, 0))

# bus_routes_merged <- function(bus_routes_sf){
#   
#   comp <- bus_routes_sf %>% st_geometry() %>%  st_touches() %>% 
#     graph_from_adj_list() %>% components() %>% .$membership
#   
#   bus_routes_sf <- bus_routes_sf %>% group_by(section = as.character(comp)) %>%
#     summarise()
#   
#   return(bus_routes_sf)
#   
# }
# 
# bus_routes_touch <- bus_routes_merged(bus_routes) 
# 


bus_routes_touch <-  st_intersection(st_geometry(bus_routes))
bus_routes_touch <- graph_from_adj_list(bus_routes_touch)
components <- components(bus_routes_touch)$membership
bus_routes_touch <- bus_routes %>% group_by(op_code) %>% 
  summarise() %>% st_buffer(0)


bus_routes_touch_size <- as.numeric(round(object.size(bus_routes_touch)/1024, 0))












library(sf)
road_network <- st_sfc(list(st_linestring(rbind( c(0, 0), c(2,3), c(4, 5), c(8,7), c(10,10) )), 
                            st_linestring(rbind( c(10,0), c(7,2), c(4,5) )),
                            st_linestring(rbind( c(10,10), c(9,6), c(8, 5), c(10,0) )),
                            st_linestring(rbind( c(0,10), c(2,9), c(5, 10), c(8,9), c(10,10) ))
                )) 

lines <- st_sfc(list(st_linestring(rbind( c(0,0), c(5, 5), c(10,10) )), 
                     st_linestring(rbind( c(0, 0), c(4, 5), c(10,9) )), 
                     st_linestring(rbind( c(10,0), c(4,5), c(10,10) )),
                     st_linestring(rbind( c(0,10), c(5, 10), c(10,9) )),
                     st_linestring(rbind( c(0,9), c(5, 9), c(10,10) ))
                     )) 
                    
simpler_lines <- st_sfc(list(st_linestring(rbind( c(0, 0), c(4, 5), c(10,9) )), 
                     st_linestring(rbind( c(10,0), c(4,5) )),
                     st_linestring(rbind( c(0,10), c(5, 10), c(10,9) ))
                    )) 

plot(lines)
plot(simpler_lines)
plot(road_network, lwd = 5, col = "grey")
plot(lines, add = TRUE)

plot(st_snap(lines, road_network, tolerance = 0.8), add = TRUE, col = "red")

