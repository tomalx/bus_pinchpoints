######## merge trafficmaster data shp files into one data frame #########

## combine all rows of trafficmaster data shp files into one data frame, all_tm
## assumes that trafficmaster data has been pre-loaded
## if not this can be done by calling 
## source("C:\\Users\\tom.alexander1\\OneDrive - West Of England Combined Authority\\Transport\\2.2 Bus\\visualisation\\bus_pinchpoints\\load_data.R")
## The bus_pinchpoints\\load_data.R script loads trafficmaster data from:
## C:\Users\tom.alexander1\OneDrive - West Of England Combined Authority\Transport\7.0 Data\01 Regional Modelling\Trafficmaster\data\geojson

# function to standardize column names and add a time_period column
standardize_tm <- function(tm_df) {
  time_period <- deparse(substitute(tm_df))
  tm_df <- tm_df %>% 
    set_names(c("fid", "delay", "delayed_speed", "freeflow_speed", "delay_per_km", "geometry")) %>% 
    mutate( time_period = str_sub(paste0(time_period), 4) )
    rm(time_period)
  return(tm_df)
}

# use standardize_tm function to standardize column names and add a time_period column
tm_0700_0800 <- standardize_tm(tm_0700_0800)
tm_0800_0900 <- standardize_tm(tm_0800_0900)
tm_1000_1600 <- standardize_tm(tm_1000_1600)
tm_1600_1700 <- standardize_tm(tm_1600_1700)
tm_1700_1800 <- standardize_tm(tm_1700_1800)

# bind all trafficmaster data frames that begin with tm_ into one data frame
all_tm <- rbind(tm_0700_0800, tm_0800_0900, tm_1000_1600, tm_1600_1700, tm_1700_1800)

# # make tm wider by adding a time_period column for each time period
# all_tm <- all_tm %>% 
#   mutate( time_period = case_when(
#     str_detect(time_period, "0700_0800") ~ "0700_0800",
#     str_detect(time_period, "0800_0900") ~ "0800_0900",
#     str_detect(time_period, "1000_1600") ~ "1000_1600",
#     str_detect(time_period, "1600_1700") ~ "1600_1700",
#     str_detect(time_period, "1700_1800") ~ "1700_1800"
#   ))

tm_7_8 <- all_tm %>% filter(time_period == "0700_0800") %>% st_zm()

#all_tm <- all_tm %>% select("fid", "delay", "delayed_speed", "time_period",  "geometry", "time_period")

# remove individual trafficmaster data frames from the global environment
rm(list = ls(pattern = "^tm_"))







