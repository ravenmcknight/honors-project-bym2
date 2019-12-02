# Goal: Interpolate boarding & alighting for trips with no APC records
# Reads data from 01 
# This script also uses Metro Transit databases

# Credit to Eric Lind

## packages -----------------------------------------------

packages <- c('data.table', 'DBI', 'odbc')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

## data ---------------------------------------------------

# observed APCs
apc <- readRDS('data/metro-transit/apc.RDS')
apctrips <- apc[, unique(trip_tmsk)]
apctrips <- apc[ROUTE_PROVIDER == "Metro Transit"]

# observed trips
trips <- readRDS('data/metro-transit/trips.RDS')
trips <- trips[ROUTE_PROVIDER == "Metro Transit"]

# picks to get unique trip IDs to gap fill
picks <- readRDS('data/metro-transit/picks.RDS')
setkey(picks, PICK_START_DATE_KEY, PICK_END_DATE_KEY)
apc <- picks[apc, on = .(PICK_START_DATE_KEY = date_key), roll = T]
setnames(apc, 'PICK_START_DATE_KEY', 'date_key')

# observed trips without APC
noAPC_trips <- trips[!trip_tmsk %in% apctrips]
noAPC_trips[, .N, line_id][order(-line_id)][1:20]

# drop Northstar, training & testing routes, others with no APC
noAPC_trips[as.integer(line_id) > 921, paste0(unique(line_id), collapse = "','")]
noAPC_trips <- noAPC_trips[!line_id %in% c('888', '922', '771', '740', '791', '942', '906',
                                           '741', '945','951','953','954','955','961','962','963','952',
                                           '969','941','970','971','972','973')]

# check range of dates before continuing!
range(noAPC_trips$date_key)

## pull schedule for missing trips ------------------------
# get stop-level schedule for trips without APC
ch_schedb <- dbConnect(odbc::odbc(), 'ScheduleDB')

# direction look up table
dirlut <- data.table(Direction = c('South', 'East', 'West', 'North'),
                     Dir = c(1, 2, 3, 4))
setkey(dirlut, Direction)

# function to get stop-level schedule given day-route-trip
pullSched <- function(x) { 
  DateIn = as.Date(as.character(x$date_key), format = '%Y%m%d')
  BlockIn = x$block_number
  RouteIn = x$line_id
  TripIn = x$trip_number
  DirIn <- dirlut[x$line_direction, Dir]
  print(paste(DateIn, BlockIn, RouteIn, TripIn, DirIn))
  q <- paste0("SELECT DISTINCT c.ScheduleDate, b.BlockNumber, tp.RouteID, t.TripNumber,  tps.StopID, 
              tst.PassingTime, v.Booking, v.Scenario, v.ScheduleTypeID, Latitude, Longitude
              FROM Calendars c
              INNER JOIN dbo.VehicleSchedules_Blocks vb
              ON c.VehicleScheduleID = vb.VehicleScheduleID
              AND '", DateIn, "' BETWEEN vb.ValidFrom AND vb.ValidTo
              INNER JOIN dbo.VehicleSchedules v
              ON v.VehicleScheduleID = c.VehicleScheduleID
              INNER JOIN dbo.Blocks b
              ON vb.InternalBlockNumber = b.InternalBlockNumber
              AND '", DateIn, "' BETWEEN b.ValidFrom AND b.ValidTo
              INNER JOIN dbo.TripIndex ti
              ON c.VehicleScheduleID = ti.VehicleScheduleID
              AND b.InternalBlockNumber = ti.InternalBlockNumber
              AND '", DateIn, "' BETWEEN ti.ValidFrom AND ti.ValidTo
              INNER JOIN dbo.Trips t
              ON ti.InternalTripNumber = t.InternalTripNumber
              AND '", DateIn, "' BETWEEN t.ValidFrom AND t.ValidTo
              INNER JOIN dbo.TripPatterns tp
              ON t.RouteID = tp.RouteID
              AND t.TripPatternID = tp.TripPatternID
              AND '", DateIn, "' BETWEEN tp.ValidFrom AND tp.ValidTo
              INNER JOIN dbo.TripStopTimes tst
              ON t.InternalTripNumber = tst.InternalTripNumber
              AND '", DateIn, "' BETWEEN tst.ValidFrom AND tst.ValidTo
              INNER JOIN dbo.TripPatternStops tps
              ON tp.RouteID = tps.RouteID
              AND tp.TripPatternID = tps.TripPatternID
              AND tst.StopSequence = tps.StopSequence
              AND '", DateIn, "' BETWEEN tps.ValidFrom AND tps.ValidTo
              INNER JOIN dbo.Stops st
              ON st.StopID = tps.StopID
              AND '", DateIn, "' BETWEEN st.ValidFrom AND st.ValidTo
              WHERE c.ScheduleDate = '", DateIn, "'
              AND t.TripTypeID = 0
              AND t.RouteID = '", RouteIn, "'
              AND t.TripNumber = ", TripIn, "
              AND tp.DirectionID = ", DirIn, "
              AND b.BlockNumber = ", BlockIn, ";")
  
  out <- as.data.table(dbGetQuery(ch_schedb, q))
  return(out)
}
# execute across all missing trips
system.time(outlist <- lapply(1:nrow(noAPC_trips), function(x) pullSched(noAPC_trips[x])))
gapSched <- rbindlist(outlist)
gapSched <- unique(gapSched)

## Interpolation ------------------------------------------

# Join schedule on date, block, trip
gapSched[, date_key := as.integer(as.character(format(as.Date(ScheduleDate), '%Y%m%d')))]
noAPC_tripStop <- noAPC_trips[gapSched, on = .(date_key, block_number = BlockNumber, line_id = RouteID, trip_number = TripNumber), nomatch = 0L]

noAPC_tripStop[!trip_tmsk %in% apctrips]

# pick, service_id, route, trip, siteid = get expected boards by trip, site_id for that pick
noAPC_tripStop[, .N, .(PICK_START_DATE, service_id, line_id, trip_number, StopID)]

# interpolate random boarding/alighting records from elsewhere in same pick
set.seed(1115)
fillAPC <- apc[, .(board = sample(board, 1), alight = sample(alight, 1)), .(PICK_START_DATE, service_id, line_id, trip_number, StopID = site_id)]

# merge with run trips to get filled observations - mark as interpolated
filledAPC <- fillAPC[noAPC_tripStop[, .(date_key, PICK_START_DATE, service_id, line_id, line_direction, trip_number, StopID, interpolated = TRUE)], on = .(PICK_START_DATE, service_id, line_id, trip_number, StopID)]

nrow(filledAPC[is.na(board)])/nrow(filledAPC) # unclear why so many are still missing?
# I'm not sure if this is my error, or if those trips were unusual and actually shouldn't have a match
# I'm guessing it's the latter and will likely drop na values

# either way, add back to all trips
apcboth <- rbindlist(list(apc[, .(date_key, line_id, line_direction, service_id, site_id, board, alight, interpolated = FALSE)], filledAPC[, .(date_key, line_id, line_direction, service_id, site_id = StopID, board, alight, interpolated)]))

# save all data
saveRDS(apcboth, 'data/metro-transit/apc-interpolated.RDS')

