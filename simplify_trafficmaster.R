
### use  cut 
all_tm_wide <- all_tm %>% filter(time_period == "0700_0800") %>% st_zm() %>%
  mutate(delay_level = cut(delay, breaks = c(-Inf,0,2,5,Inf), labels = c(0,1,2,3))) %>%
  mutate(delay_level = ifelse(is.na(delay_level), 0, delay_level)) %>% 
  group_by(delay_level) %>%  # ".x" is refers to the current group:
  group_modify(~ st_union(.x) %>% as_tibble()) %>%
  ungroup() %>%
  st_as_sf() %>%  # convert back to "sf" object since this 
  # information is lost during group_by()
  st_simplify(dTolerance = 1, preserveTopology = TRUE)

### use st_union and group_by to reduce the size of the sf object
tm_7_8_union <- tm_7_8 %>% st_zm() %>%
  mutate(delay_level = cut(delay, breaks = c(-Inf,0,2,5,Inf), labels = c(0,1,2,3))) %>%
  mutate(delay_level = ifelse(is.na(delay_level), 0, delay_level)) %>% 
  group_by(delay_level) %>%  # ".x" is refers to the current group:
  group_modify(~ st_union(.x) %>% as_tibble()) %>%
  ungroup() %>%
  st_as_sf() %>%  # convert back to "sf" object since this 
                  # information is lost during group_by()
  st_simplify(dTolerance = 5, preserveTopology = TRUE)

tm_union_size <- as.numeric(round(object.size(tm_7_8_union)/1024, 0))
all_tm <- all_tm %>% st_zm() %>% st_simplify(dTolerance = 1, preserveTopology = TRUE)
all_tm_size <- as.numeric(round(object.size(all_tm)/1024, 0))

# filter for each time period, drop geometry, select delay attribute and join back together on fid
all_tm_geom <- all_tm %>% filter(time_period == "0700_0800") %>% select(fid, geometry) %>% st_zm()

all_tm_wide <- all_tm %>% st_drop_geometry() %>% 
  mutate(delay_level = cut(delay, breaks = c(-Inf,0,2,5,Inf), labels = c(0,1,2,3))) %>%
  mutate(delay_level = ifelse(is.na(delay_level), 0, delay_level)) %>% 
  
  # group_by(delay_level) %>%  # ".x" is refers to the current group:
  # group_modify(~ st_union(.x) %>% 
  #                as_tibble()) %>%
  # ungroup() # 
  
  select(fid, delay_level, time_period) %>% 
  pivot_wider(names_from = time_period, values_from = delay_level) %>%
  left_join(all_tm_geom, by = "fid") %>%
  st_as_sf() %>% 
  #st_zm() %>% 
  st_simplify(dTolerance = 10, preserveTopology = TRUE) %>% 
  st_cast("MULTILINESTRING")


# all_tm_wide <- all_tm %>% 
#   st_drop_geometry() %>% 
#   select(fid, delay, time_period) %>% 
#   pivot_wider(names_from = time_period, values_from = delay) %>% 
  
  # left_join(all_tm_geom, by = "fid") %>%
  # st_as_sf() %>% 
  # #st_cast("MULTILINESTRING") %>% 
  # #st_zm() %>% 
  # st_simplify(dTolerance = 5, preserveTopology = TRUE)


rm(all_tm_geom)
all_tm_wide_size <- as.numeric(round(object.size(all_tm_wide)/1024, 0))

pal <- colorNumeric(
  palette = c(brewer.pal(9, "Greens")[4], brewer.pal(9, "YlOrBr")[4], brewer.pal(9, "Reds")[6], brewer.pal(9, "Reds")[8], brewer.pal(9, "Greys")[9] ),
  domain = all_tm_wide$'0700_0800')

leaflet() %>%  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(data = all_tm_wide, 
               color = ~pal(`0700_0800`), 
               weight = 3)

leaflet() %>%  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(data = all_tm_wide, weight = 2)

#######################

# new approach:
# simplifying file of this size still results in huge leaflet html so...
# filter for the worst delays where traffic speeds are reduced by x percent?
# display this sf object in leaflet

delay_colour_domain <- all_tm %>% filter(delay >= 5 & delay <= 10)

# set colour palette for delays (over x minutes)
pal <- colorNumeric(
    palette = c( "#00FFDD", "#2FA4FF","#0E185F"),
    domain = delay_colour_domain$delay)
  
# filter for the worst delays where traffic speeds are reduced by x percent?

all_tm_slowest <- all_tm %>% filter(delay >= 10) %>% st_zm() %>% 
   group_by(time_period) %>%  # ".x" is refers to the current group:
   group_modify(~ st_union(.x) %>% 
                  as_tibble()) %>%
   ungroup() %>%  # 
   st_as_sf()  # convert back to "sf" object since this 
                   # information is lost during group_by()

all_tm_slowest_size <- as.numeric(round(object.size(all_tm_slowest)/1024, 0))

leaflet() %>%  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(data = all_tm_slowest %>% filter(time_period == "0700_0800"), color = "#0E185F",group = "seven", weight = 2) %>% 
  addPolylines(data = all_tm_slowest %>% filter(time_period == "0800_0900"), color = "#0E185F", group = "eight", weight = 2) %>% 
  addPolylines(data = all_tm_slowest %>% filter(time_period == "1000_1600"), color = "#0E185F", group = "IP", weight = 2) %>%

  addLayersControl(
    baseGroups = c("seven", "eight", "IP"),
    #overlayGroups = c("seven", "eight", "IP"),
    options = layersControlOptions(collapsed = FALSE))


########################

fruit <- c("apple", "banana", "cherry", "date", "elderberry")
fruit <- rep(fruit, c(5, 5, 5, 5, 5))
fruit <- tibble(id = rep(1:5, 5), fruit = fruit, value = sample(1:100, 25, replace = TRUE))

# use pivot wider to make fruit wider
fruit_wide <- fruit %>% pivot_wider(names_from = fruit, values_from = value)

tm_7_8 <- all_tm %>% filter(time_period == "0700_0800") %>% st_zm()
tm_7_8_light <- all_tm %>% filter(time_period == "0700_0800") %>% st_zm() %>% st_simplify(dTolerance = 20, preserveTopology = TRUE)


ggplot(tm_7_8) + geom_sf()

ggplot(tm_7_8) + geom_sf() + coord_sf(xlim = c(-2.7, -2.5), ylim = c(51.4, 51.5))
leaflet() %>% addTiles() %>% addPolylines(data = tm_7_8)

ggplot(tm_7_8_light) + geom_sf()

ggplot(tm_7_8_light) + geom_sf() + coord_sf(xlim = c(-2.7, -2.5), ylim = c(51.4, 51.5))
leaflet() %>% addTiles() %>% addPolylines(data = tm_7_8_light)

round(c(object.size(tm_7_8)/1024, object.size(tm_7_8_light)/1024), 0)
round(c(object.size(tm_7_8), object.size(tm_7_8_light)) /1024)


tm_size <- as.numeric(round(object.size(tm_7_8)/1024, 0))
tm_light_size <- as.numeric(round(object.size(tm_7_8_light)/1024, 0))
tm_light_union_size <- as.numeric(round(object.size(tm_7_8_light_union)/1024, 0))

# percentage difference in size
(tm_size - tm_light_size) / tm_size * 100

###################################################################
#### simplify bus routes  #########################################

bus_routes <- st_read(connec,query = "SELECT * FROM arup.existingbusservices") %>%st_transform(crs = 4326)

bus_routes <- bus_routes %>% filter(line_name %in% c("48", "48A", "49", "49A"))

plot(st_geometry(bus_routes))
plot(bus_routes %>% select(diva_line))

bus_routes <- st_multilinestring(unique(st_geometry(bus_routes)))

frequency <- 2   # only show routes with frequency of x or more

bus_routes_size <- as.numeric(round(object.size(bus_routes)/1024, 0))




bus_routes_touch <-  st_touches(st_geometry(bus_routes))
bus_routes_touch <- graph_from_adj_list(bus_routes_touch)
components <- components(bus_routes_touch)$membership
bus_routes_touch <- bus_routes %>% group_by(section = as.character({{components}})) %>% 
  summarise()


bus_routes_touch_size <- as.numeric(round(object.size(bus_routes_touch_size)/1024, 0))




 