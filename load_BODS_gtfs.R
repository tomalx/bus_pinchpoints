## load SW bus open data from DfT

## https://data.bus-data.dft.gov.uk/timetable/download/

## use tidytransit package to load the data

## data downloaded and saved in:
## C:\Users\tom.alexander1\OneDrive - West Of England Combined Authority\Transport\2.2 Bus\BODS\gtfs

## script loads gtfs from file then tidies:
##          - converts to sf
##          - clips bus routes to only those that travel in the West of England
##          - add direction_id column in trips table
##          - identifies trips with missing shapes
##          - create linestring for missing shapes from stop_times

## load libraries
library(tidytransit)
library(sf)
library(dplyr)
library(glue)

######### get the name and path to the latest zip file in the gtfs folder #########

path <- "C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/2.2 Bus/BODS/gtfs/"

latest_gtfs_zip_file <- list.files(path = path, full.names = T) %>% 
  enframe(name = NULL) %>% 
  bind_cols(pmap_df(., file.info)) %>% 
  filter(mtime==max(mtime)) %>% 
  pull(value)

######### load objects from file #########
# gtfs zip file
sw_gtfs <- tidytransit::read_gtfs(latest_gtfs_zip_file)
# weca boundary line
weca_bdline <- st_read("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/7.0 Data/03 Analysis Projects/accessibility_connectivity/shp/weca_bdline.shp")
rm(path, latest_gtfs_zip_file)


######### convert to sf #########
gtfs <- gtfs_as_sf(sw_gtfs, crs = 27700)

######### clip to weca only services #########
weca_gtfs <- filter_feed_by_area(gtfs, weca_bdline)
rm(sw_gtfs, gtfs)

######### add direction_id column in trips table #########
weca_gtfs$trips <- weca_gtfs$trips %>% left_join(weca_gtfs$routes, by="route_id") %>% 
  #filter(route_short_name == 7) %>% 
  group_by(route_id) %>% 
  mutate(direction_id = as.integer(factor(trip_headsign))) %>% 
  ungroup()

######### save as RDS with today's date in file name #########
date <- format(Sys.Date(), "%Y%m%d")
saveRDS(weca_gtfs, glue("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/2.2 Bus/BODS/rds/weca_gtfs{date}.rds"))
rm(date, weca_bdline, weca_gtfs)
