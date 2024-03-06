

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

# percentage difference in size
(tm_size - tm_light_size) / tm_size * 100


### use st_union and group_by to reduce the size of the sf object
tm_7_8_light_union <- tm_7_8_light %>% st_zm() %>% 
  mutate(delay_level = cut(delay, breaks = c(-Inf,0,2,5,Inf), labels = c(0,1,2,3)))









