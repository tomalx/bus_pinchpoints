##############
##### Aim:
#####  create input for Shiny App
#####  - shiny app will be able to:
#####    - filter occupancy data by time_period
#####   - filter occupancy data by 

###### fetch GTFS data from file ####

      ## load objects from file ##
      # gtfs zip file
      sw_gtfs <- tidytransit::read_gtfs("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/2.2 Bus/BODS/gtfs/itm_south_west_gtfs.zip")
      # weca boundary line
      weca_bdline <- st_read("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/7.0 Data/03 Analysis Projects/accessibility_connectivity/shp/weca_bdline.shp")
      
      ## convert to sf ##
      gtfs <- gtfs_as_sf(sw_gtfs, crs = 27700)
      
      ## clip to weca only services ##
      weca_gtfs <- filter_feed_by_area(gtfs, weca_bdline)
      #rm(sw_gtfs, gtfs)
      
      ## add direction_id column in trips table ##
      weca_gtfs$trips <- weca_gtfs$trips %>% left_join(weca_gtfs$routes, by="route_id") %>% 
        #filter(route_short_name %in% c(42,44)) %>%     ## filter by route_short_name for testing ##
        group_by(route_id) %>% 
        mutate(direction_id = as.integer(factor(trip_headsign))) %>% 
        ungroup()

      
###### create df of routes and directions ####
      
      weca_stops_by_direction_and_route <- weca_gtfs$trips %>% 
        filter(route_short_name %in% c(43)) %>% 
        #filter(route_id == 6515, direction_id == 2) %>% 
        left_join(weca_gtfs$stop_times, by="trip_id") %>% 
        filter(route_type == 3) %>%  # filter for bus services only (no coaches or ferries)
        group_by(trip_headsign, route_short_name, stop_sequence, direction_id) %>%
        arrange(desc(arrival_time)) %>%    # ordering and filtering
        filter(row_number() == 20) %>%      # by row number catches
        summarise(first = first(trip_id), stop_id = stop_id) 
      
      # convert stop points to route linestring
      weca_routes_by_direction <- weca_stops_by_direction_and_route %>%
        left_join(weca_gtfs$stops %>% select(stop_id, geometry), by="stop_id") %>%
        st_as_sf(crs = 27700) %>%
        group_by(route_short_name, trip_headsign, direction_id) %>%
        summarise(do_union = FALSE) %>%
        st_cast("LINESTRING")
      
      
      
##### create leaflet map ####
      myRoutes <- c(43)
      
      weca_stops_by_direction_and_route <-
        weca_stops_by_direction_and_route %>%
        filter(route_short_name %in% myRoutes) %>%
        left_join(weca_gtfs$stops %>% select(stop_id, geometry), by="stop_id") %>%
        st_as_sf(crs = 27700) %>%
        st_transform(4326)
      
      weca_routes_by_direction <-
        weca_routes_by_direction %>%
        filter(route_short_name %in% myRoutes) %>% 
        st_transform(4326)
      
      
      pal <- colorFactor("Set3", weca_routes_by_direction$trip_headsign)
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        #addProviderTiles(providers$CartoDB.DarkMatter) %>%
        #addPolylines(data = shapes, weight = 2, color = "purple", group = "BOD data" ) %>% 
        #addPolylines(data = bus_routes, weight = 2, color = "cyan", group = "theo DB data") %>% 
        addPolylines(data = weca_routes_by_direction, weight = 4, 
                     color = ~pal(weca_routes_by_direction$trip_headsign),
                     popup = ~paste("route: ", route_short_name, " to ", trip_headsign),
                     highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, weight = 8, sendToBack = FALSE),
                     group = "routes") %>%
        #add stop points
        addCircleMarkers(data = weca_stops_by_direction_and_route,
                         popup = ~paste("stop: ", stop_id),
                         radius = 2, color = "red", group = "stops") %>%
        addLayersControl(overlayGroups = c("routes", "stops"))
      
      