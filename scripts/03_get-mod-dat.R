## Goal: clean Metro Transit data from 01 and 02 into usable modeling data

## packages -----------------------------------------------
packages <- c('data.table', 'tigris', 'ggplot2', 'dplyr', 'rgdal', 'sf', 'lubridate')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

options(tigris_use_cache = TRUE)
options(tigris_class = 'sf')

## data ---------------------------------------------------
# Read "raw" data, from "gap-fill.R" script
apc <- readRDS('data/metro-transit/apc-interpolated.RDS')
setDT(apc)

apc[, site_id := as.character(site_id)]

# assign stops to block groups ----------------------------
url <- 'ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_transit_stops/shp_trans_transit_stops.zip'
loc <- file.path(tempdir(), 'stops.zip')
download.file(url, loc)
unzip(loc, exdir = file.path(tempdir(), 'stops'), overwrite = TRUE)
file.remove(loc)
stops <- readOGR(file.path(tempdir(), 'stops'), layer = 'TransitStops', stringsAsFactors = FALSE)
stops <- st_as_sf(stops)
stops <- st_transform(stops, 4326)

# block groups
options(tigris_class = 'sf')
counties <- c('Anoka', 'Carver', 'Dakota', 'Hennepin', 'Ramsey', 'Scott', 'Washington')
bgs <- block_groups('MN', counties, year = 2016)
bgs <- st_transform(bgs, 4326)

apc[, site_id := as.character(site_id)]
apc[, year := year(ymd(date_key))]

# this step takes a while, save "stop_to_bg" and don't rerun
apc_loc <- left_join(stops, apc) # dplyr joins are usually easier with sf objects
rm(apc) # just for space

# intersect stops and block groups
apc_bg <- st_join(apc_loc, bgs, st_intersects)
setDT(apc_bg)

# count bus stops
bc <- apc_bg[rail == 0, .(count_bus_stops = length(unique(site_id))), by = 'GEOID']
rc <- apc_bg[rail == 1, .(count_rail_stops = length(unique(site_id))), by = 'GEOID']

# save stops to block groups
names(apc_bg)
stop_to_bg <- apc_bg[, c('site_id', 'GEOID')]

stop_to_bg <- unique(stop_to_bg)
#saveRDS(stop_to_bg, 'data/metro-transit/stops_to_bgs.RDS')

apc <- stop_to_bg[apc, on = 'site_id']

# exclude rail stations
stops2 <- fread('data/csv_trans_stop_boardings_alightings/TransitStopsBoardingsAndAlightings2018.csv') # year doesn't really matter
stops2 <- stops2[Route == "Blue Line" | Route == "North Star" | Route == "Green Line"]

apc <- apc[!site_id %in% stops2$Site_id]
# double check Green & Blue line are out
apc <- apc[line_id != 902 & line_id != 901]

# exclude things outside of 7 county
apc <- apc[!is.na(GEOID)]


## get aggregations of data -------------------------------
# a little bit overkill but this takes a while to process so do all at once

# total annual by bg
apc_annual_bg <- apc[, .(year_boards = sum(board, na.rm = T), year_alights = sum(alight, na.rm = T), num_interpolated = sum(interpolated), 
                  num_routes = length(unique(line_id)), year_stops = .N), keyby = .(year, GEOID)]

saveRDS(apc_annual_bg, 'data/aggregations/apc_annual_bg.RDS')

# total annual by stop
apc_annual_stop <- apc[, .(year_boards = sum(board, na.rm = T), year_alights = sum(alight, na.rm = T), num_interpolated = sum(interpolated), 
                           num_routes = length(unique(line_id)), year_stops = .N), keyby = .(year, site_id)]

saveRDS(apc_annual_bg, 'data/aggregations/apc_annual_stop.RDS')

# daily across entire region
apc_daily <- apc[, .(daily_boards = sum(board, na.rm = T), daily_alights = sum(alight, na.rm = T), num_interpolated = sum(interpolated), 
                     num_routes = length(unique(line_id)), daily_stops = .N), keyby = .(date_key)]

saveRDS(apc_daily, 'data/aggregations/apc_daily.RDS')

# daily by bg
apc_daily_bg <- apc[, .(daily_boards = sum(board, na.rm = T), daily_alights = sum(alight, na.rm = T), num_interpolated = sum(interpolated), 
                     num_routes = length(unique(line_id)), daily_stops = .N), keyby = .(date_key, GEOID)]

saveRDS(apc_daily_bg, 'data/aggregations/apc_daily_bg.RDS')

# slightly more practical average
apc_avg_daily_bg <- apc_daily_bg[, .(daily_boards = mean(daily_boards, na.rm = T), daily_alights = mean(daily_alights, na.rm = T), 
                                     num_interpolated = mean(num_interpolated, na.rm = T), num_routes = mean(num_routes, na.rm = T), 
                                     daily_stops = mean(daily_stops, na.rm = T)), keyby = .(year, GEOID)]

saveRDS(apc_avg_daily_bg, 'data/aggregations/apc_avg_daily_bg.RDS')

# daily by stop
apc_daily_stop <- apc[, .(daily_boards = sum(board, na.rm = T), daily_alights = sum(alight, na.rm = T), num_interpolated = sum(interpolated), 
                        num_routes = length(unique(line_id)), daily_stops = .N), keyby = .(date_key, site_id)]

saveRDS(apc_daily_stop, 'data/aggregations/apc_daily_stop.RDS')

# slightly more practical average
apc_avg_daily_stop <- apc_daily_stop[, .(daily_boards = mean(daily_boards, na.rm = T), daily_alights = mean(daily_alights, na.rm = T), 
                                     num_interpolated = mean(num_interpolated, na.rm = T), num_routes = mean(num_routes, na.rm = T), 
                                     daily_stops = mean(daily_stops, na.rm = T)), keyby = .(year, site_id)]

saveRDS(apc_avg_daily_stop, 'data/aggregations/apc_avg_daily_stop.RDS')

