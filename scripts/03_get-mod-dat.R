## Goal: clean Metro Transit data from 01 and 02 into usable modeling data

packages <- c('data.table', 'tigris', 'ggplot2', 'dplyr', 'rgdal', 'sf', 'lubridate')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

options(tigris_use_cache = TRUE)
options(tigris_class = 'sf')

# Read "raw" data, from "gap-fill.R" script
apc <- readRDS('data/metro-transit/apc-interpolated.RDS')
setDT(apc)

apc[, site_id := as.character(site_id)]

# assign stops to block groups ------------------
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
# apc_loc <- left_join(stops, apc) # dplyr joins are usually easier with sf objects
# rm(apc) # just for space
# 
# # intersect stops and block groups
# apc_bg <- st_join(apc_loc, bgs, st_intersects)
# setDT(apc_bg)
# 
# # count bus stops
# bc <- apc_bg[rail == 0, .(count_bus_stops = length(unique(site_id))), by = 'GEOID']
# rc <- apc_bg[rail == 1, .(count_rail_stops = length(unique(site_id))), by = 'GEOID']
# 
# # save stops to block groups
# names(apc_bg)
# stop_to_bg <- apc_bg[, c('site_id', 'GEOID')]
# 
# stop_to_bg <- unique(stop_to_bg)
# saveRDS(stop_to_bg, 'data/metro-transit/stops_to_bgs.RDS')

stop_to_bg <- readRDS('data/metro-transit/stops_to_bgs.RDS')

apc <- stop_to_bg[apc, on = 'site_id']

# exclude rail stations
stops2 <- fread('data/csv_trans_stop_boardings_alightings/TransitStopsBoardingsAndAlightings2018.csv') # year doesn't really matter
stops2 <- stops2[Route == "Blue Line" | Route == "North Star" | Route == "Green Line"]

apc <- apc[!site_id %in% stops2$Site_id]
# double check Green & Blue line are out
apc <- apc[line_id != 902 & line_id != 901]

# exclude things outside of 7 county
apc <- apc[!is.na(GEOID)]

apc_ag <- apc[year(ymd(date_key)) == 2017, .(year_boards = sum(board, na.rm = T), year_alights = sum(alight, na.rm = T), num_interpolated = sum(interpolated), 
                  num_routes = length(unique(line_id)), year_stops = .N), keyby = .(GEOID)]

apc_ag[, year_activity := year_boards + year_alights, keyby = .(GEOID)]

saveRDS(apc_ag, 'data/modeling-dat/basic_mod_dat_ann17.RDS')


# apc interp to daily by stop
apc <- readRDS('data/metro-transit/apc-interpolated.RDS')
setDT(apc)
apc17 <- apc[year(ymd(date_key)) == 2017]

, apc_day_stop <- apc[year(ymd(date_key)) == 2017, .(boards = sum(board, na.rm = T), alights = sum(alight, na.rm = TRUE), 
                        num_interp = sum(interpolated)), keyby = .(date_key, site_id)]
saveRDS(apc_day, "data/metro-transit/daily_stop_apc17.RDS")


# daily by region
apc_day <- apc[year(ymd(date_key)) == 2017, .(boards = sum(board, na.rm = T), alights = sum(alight, na.rm = TRUE), 
                   num_interp = sum(interpolated)), keyby = .(date_key)]
saveRDS(apc_day, "data/metro-transit/daily_apc17.RDS")



