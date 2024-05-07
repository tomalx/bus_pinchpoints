
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
      rm(sw_gtfs, gtfs)
      
      ## add direction_id column in trips table ##
      weca_gtfs$trips <- weca_gtfs$trips %>% left_join(weca_gtfs$routes, by="route_id") %>% 
        filter(route_short_name %in% c(42,44)) %>%     ## filter by route_short_name for testing ##
        group_by(route_id) %>% 
        mutate(direction_id = as.integer(factor(trip_headsign))) %>% 
        ungroup()
      
#### create df of routes and directions ####
      weca_stops_by_direction_and_route <- weca_gtfs$trips %>% 
        #filter(route_short_name %in% c(42,43,44,45)) %>% 
        #filter(route_id == 6515, direction_id == 2) %>% 
        left_join(weca_gtfs$stop_times, by="trip_id") %>%
        filter(route_type == 3) %>%  # filter for bus services only (no coaches or ferries)
        group_by(trip_headsign, route_short_name, stop_sequence, direction_id) %>%
        arrange(desc(arrival_time)) %>%    # ordering and filtering
        filter(row_number() == 1) %>%      # by row number catches
        summarise(first = first(trip_id), stop_id = stop_id) 
     
      # convert stop points to route linestring
      weca_routes_by_direction <- weca_stops_by_direction_and_route %>%
        left_join(weca_gtfs$stops %>% select(stop_id, geometry), by="stop_id") %>%
        st_as_sf(crs = 27700) %>%
        group_by(route_short_name, trip_headsign, direction_id) %>%
        summarise(do_union = FALSE) %>%
        st_cast("LINESTRING") 
      
#### connect to DB ####
      if(!exists("connec")) {
        # source the connect_postgreSQL.R script
        source("C:\\Users\\tom.alexander1\\OneDrive - West Of England Combined Authority\\Transport\\7.0 Data\\Rscripts\\access_postgresql\\access_to_postgresql\\connect_postgreSQL.R")
      }
     


#### function to fetch FirstPro data from DB
      fetch_firstpro_data <- function(route_id, direction_id, start_date, end_date, conn){
        
      weca_routes_by_direction <- st_drop_geometry(weca_routes_by_direction)
        
      
      
        for(i in 1:nrow(weca_routes_by_direction)){  
        route_stops <- 
          weca_gtfs$trips %>% 
          filter(route_short_name == weca_routes_by_direction[[i,1]],
                 direction_id == weca_routes_by_direction[[i,3]]) %>%
          left_join(weca_gtfs$stop_times, by="trip_id") %>% 
          select(stop_id) %>% 
          distinct() %>% 
          pull(stop_id)
        }
      
        # sql query (using glue package for syntax formatting)
        sql <- glue_sql( "SELECT * FROM prospective.congestion ",
                         "WHERE bounding_start_stop IN ({route_stops*}) ",
                         "AND bounding_end_stop IN ({route_stops*}) ",
                         "AND data_period = '2023 Oct - 2023 Dec' ",
                         #"AND hour_of_day = '08:00:00' " ,
                         #"FETCH FIRST 200 ROWS ONLY", 
                         .con = connec)
      
        
        data <- dbGetQuery(connec, sql)
        
        return(data)
      }

# fetch First Pro data using function to iterate over list of routes

