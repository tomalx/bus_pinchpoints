



#######       create leaflet map of traffic master data with bus gates/lanes and bus routes       #####

library(leaflet)
library(RColorBrewer)


### source simplify and reduce bus_routes function
source("visualisation/bus_pinchpoints/bus_routes_condense.R")


### filter all_tm for the worst delays where traffic speeds are reduced by x percent?
# remove NA values
top_x_delay <- 5   # percentage value 
top_x_delay <- (100 - top_x_delay) / 100
tm_most_delayed <- all_tm %>% filter(delay_per_km >= as.numeric(quantile(all_tm$delay_per_km, top_x_delay)))

# filter for the worst delays where traffic speeds are reduced by x percent?

tm_most_delayed <- tm_most_delayed %>% st_zm() %>% 
  group_by(time_period) %>%  # ".x" is refers to the current group:
  group_modify(~ st_union(.x) %>% 
                 as_tibble()) %>%
  ungroup() %>%  # 
  st_as_sf()  # convert back to "sf" object since this 
# information is lost during group_by()

tm_most_delayed_size <- as.numeric(round(object.size(tm_most_delayed)/1024, 0))

leaflet() %>%  addProviderTiles(providers$CartoDB.Positron) %>% 
  
  addPolylines(data = bcc_bus_lanes, color = "#118DF0", group = "bus lanes", weight =3) %>%
  addPolylines(data = bcc_bus_gates, color = "#118DF0", group = "bus gates", weight = 5) %>%
  addPolylines(data = bus_routes, color = "#0E2F56", group = "bus routes", opacity = 0.5, weight = 0.5) %>%
  
  addPolylines(data = tm_most_delayed %>% filter(time_period == "0700_0800"), color = "#FF304F",group = "7am-8am", weight = 2) %>% 
  addPolylines(data = tm_most_delayed %>% filter(time_period == "0800_0900"), color = "#FF304F", group = "8am-9am", weight = 2) %>% 
  addPolylines(data = tm_most_delayed %>% filter(time_period == "1000_1600"), color = "#FF304F", group = "10am-4pm", weight = 2) %>%
  addPolylines(data = tm_most_delayed %>% filter(time_period == "1600_1700"), color = "#FF304F", group = "4pm-5pm", weight = 2) %>%
  addPolylines(data = tm_most_delayed %>% filter(time_period == "1700_1800"), color = "#FF304F", group = "5pm-6pm", weight = 2) %>%
  
  
  
  addLayersControl(
    baseGroups = c("7am-8am", "8am-9am", "10am-4pm", "4pm-5pm", "5pm-6pm"),
    overlayGroups = c("bus lanes", "bus gates", "bus routes"),
    options = layersControlOptions(collapsed = FALSE))





##############               select colour palettes          #############
display.brewer.all(type = "seq") 
pal <- brewer.pal(12, "Paired")
bluPal <- brewer.pal(9, "Blues")
redPal <- brewer.pal(9, "Reds")



###############    create leaflet map of traffic master data with bus gates/lanes and bus routes    #############
leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  
  addPolylines(data = bcc_bus_lanes) %>%
  # addPolylines(data = all_tm %>% filter(time_period == "0700_0800") %>% st_zm(),  #  st_zm() removes the z and m dimensions
  #                                                                                 #  for plotting in leaflet
  #              color = redPal[4],weight = 1, opacity = 1, group = "7am to 8am", label = all_tm$delay) %>%
  addPolylines(data = tm_7_8,
               color = redPal[4],weight = 1, opacity = 1, group = "7am to 8am", label = all_tm$delay) %>%
  setView(lng = -2.5, lat = 51.5, zoom = 10) # %>%
  
  # addLayersControl(
  #   overlayGroups = c("Committed Schemes", "Education Sites",  "West of England CA Boundary"),
  #   options = layersControlOptions(collapsed = FALSE)) %>% 
  # 
 # addLegend("bottomright",
  #          colors = c(redPal[5], bluPal[5], bluPal[8]),
  #          labels = c("Committed Cycling Infrastructure", "Education - Primary Schools", "Education - Secondary Schools"),
  #          title = "Committed Cycling Infrastructure and Education Sites")



