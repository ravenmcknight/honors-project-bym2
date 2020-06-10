# Goal: 
#     FACT_APC to get observed trips
#     FACT_AVL to get run trips
#     schedule to fill in stops between time points
# This script pulls data from Metro Transit data bases

# Credit to Eric Lind

## packages -----------------------------------------------

# these lines make sure any user has all the necessary packages
packages <- c('data.table', 'DBI', 'odbc', 'lubridate')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)


## the connection -----------------------------------------

# the only part that's not fully reproducible for others
ch_sdw <- dbConnect(odbc(), 'db_SDWarehouse')


## picks --------------------------------------------------

system.time(picks <- dbGetQuery(ch_sdw, "SELECT * from DIM_PICK_DATE"))
# 0.002

## actual trips (FACT_AVL) --------------------------------

avl_query <- paste0("
                    set transaction isolation level read uncommitted; 
                    SELECT DISTINCT date_key, a.trip_tmsk, block_number, trip_number, 
                    line_id, line_direction, service_id, time_bracket_start, DIM_TRIP_TM.ROUTE_PROVIDER
                    FROM FACT_AVL a
                    LEFT JOIN DIM_TRIP_TM
                    on DIM_TRIP_TM.TRIP_TMSK = a.TRIP_TMSK
                    LEFT JOIN DIM_STOP
                    on DIM_STOP.stopsk = a.stopsk
                    WHERE date_key BETWEEN 20150101 AND 20181231
                    ")

system.time(trips <- dbGetQuery(ch_sdw, avl_query))
# 49.602

setDT(trips)
setkey(trips, date_key, line_id, time_bracket_start)
trips <- trips[trip_number > 0]
# trips[, range(date_key)]

# assign pickdate
setDT(picks)
setkey(picks, PICK_START_DATE_KEY, PICK_END_DATE_KEY)
trips <- picks[trips, on = .(PICK_START_DATE_KEY = date_key), roll = T]
setnames(trips, 'PICK_START_DATE_KEY', 'date_key')


## observed trips (FACT_APC) ------------------------------

apc_query <- paste0(" 
                    set transaction isolation level read uncommitted;
                    SELECT a.date_key, a.trip_tmsk, block_number, trip_number, 
                    line_id, site_id, service_id, line_direction, a.board, a.alight, a.apc_msg_time
                    from FACT_APC a
                    LEFT JOIN DIM_TRIP_TM
                    on DIM_TRIP_TM.TRIP_TMSK = a.trip_TMSK
                    LEFT JOIN DIM_STOP s
                    on s.STOPSK = a.STOPSK
                    WHERE a.date_key BETWEEN 20150101 AND 20181231
                    ")

# this is the long one:
#system.time(apc <- dbGetQuery(ch_sdw, apc_query))
#  888.329

setDT(apc)
setkey(apc, date_key, site_id)
apc <- apc[site_id > 0]

saveRDS(apc, 'data/apc.RDS')
saveRDS(trips, 'data/trips.RDS')
saveRDS(picks, 'data/picks.RDS')