## Scripts for accessing BOD from Dft

#### download\_gtfs.R

-   Download SW zipped GTFS file from DfT and save in BODS folder  
-   <https://data.bus-data.dft.gov.uk/timetable/download/gtfs-file/south_west/>

#### load\_BODS\_gtfs.R

-   loads gtfs data from file using tidytransit package
-   tidies the gtfs data and isolates for specific routes in the West of
    England

#### analysis\_BODS\_gtfs\_routes\_by\_stop.R

-   leaflet map of selected bus routes.
-   route linestrings are created from stop points.

#### analysis\_BODS\_gtfs.R

-   route frequency using gtfs::get\_route\_frequency function
-   route frequency is calculated for Mon-Fri, Sat and Sun
-   buses per hour are calculated

## Scripts for accessing First’s gps bus data (from WECA postgresql database)
