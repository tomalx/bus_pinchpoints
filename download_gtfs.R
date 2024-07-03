# Download SW zipped GTFS file from DfT and save in BODS folder

# https://data.bus-data.dft.gov.uk/timetable/download/gtfs-file/south_west/

setwd("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/2.2 Bus/BODS/gtfs")

date <- as.character(Sys.Date())

download.file("https://data.bus-data.dft.gov.uk/timetable/download/gtfs-file/south_west/",destfile = paste0("gtfs_",date,".zip"))


path <- "C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/2.2 Bus/BODS/gtfs/"

latest_gtfs_zip_file <- list.files(full.names = T) %>% 
                          enframe(name = NULL) %>% 
                          bind_cols(pmap_df(., file.info)) %>% 
                          filter(mtime==min(mtime)) %>% 
                          pull(value) %>% 
                          str_remove("./") 
latest_gtfs_zip_file <- str_c(path, latest_gtfs_zip_file)                         


gtfs_test <- tidytransit::read_gtfs(latest_gtfs_zip_file)
