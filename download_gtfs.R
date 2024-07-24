# Download SW zipped GTFS file from DfT and save in BODS folder

# https://data.bus-data.dft.gov.uk/timetable/download/gtfs-file/south_west/
# https://data.bus-data.dft.gov.uk/timetable/download/gtfs-file/south_west/



setwd("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/2.2 Bus/BODS/gtfs")

date <- as.character(Sys.Date())

download.file("https://data.bus-data.dft.gov.uk/timetable/download/gtfs-file/south_west/",
              destfile = paste0("gtfs_",date,".zip"),
              mode = "wb")



