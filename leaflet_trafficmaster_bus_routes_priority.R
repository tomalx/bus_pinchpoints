



#######       create leaflet map of traffic master data with bus gates/lanes and bus routes       #####

library(leaflet)
library(RColorBrewer)



##############               select colour palettes          #############
display.brewer.all()
pal <- brewer.pal(12, "Paired")
bluPal <- brewer.pal(9, "Blues")
redPal <- brewer.pal(9, "Reds")



###############    create leaflet map of traffic master data with bus gates/lanes and bus routes    #############
leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  
  addPolylines(data = bcc_bus_lanes) %>%
  addPolylines(data = all_tm[1:10,]) %>% 
 
  setView(lng = -2.5, lat = 51.5, zoom = 10) # %>%
  
  # addLayersControl(
  #   overlayGroups = c("Committed Schemes", "Education Sites",  "West of England CA Boundary"),
  #   options = layersControlOptions(collapsed = FALSE)) %>% 
  # 
 # addLegend("bottomright",
  #          colors = c(redPal[5], bluPal[5], bluPal[8]),
  #          labels = c("Committed Cycling Infrastructure", "Education - Primary Schools", "Education - Secondary Schools"),
  #          title = "Committed Cycling Infrastructure and Education Sites")



