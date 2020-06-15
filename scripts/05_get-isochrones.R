## goal: get area of walk isochrones for each bg

## packages -------------------------------------
packages <- c('readr', 'sf', 'tigris', 'data.table', 'otptools', 'dplyr', 'ggplot2')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)


options(tigris_class = 'sf')

# first, download population-weighted centroids
url <- "https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG27.txt"
loc <- file.path("/Users/raven/Documents/honors/old-honors/data/covariates", 'centrs.csv')
download.file(url, loc)

centrs <- read_csv('data/covariates/centrs.csv')
setDT(centrs)

counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties, 2016)

centrs <- centrs[COUNTYFP %in% bgs$COUNTYFP]
centrs[, bgid := paste0(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)]

centrs_loc <- centrs[, c("bgid", "LATITUDE", "LONGITUDE")]
setnames(centrs_loc, c("bgid", "LATITUDE", "LONGITUDE"), c("id", "lat", "lon"))

# then, use opentripplanner to get 10min walk isos
isos <- queryIsochrone(location = centrs_loc,
                       otp_params = otp_params(cutoffSec = c(600), mode = "WALK", 
                                               maxWalkDistance = 800, walkSpeed = 5000/3600),
                       host = "localhost", port = 8080)

saveRDS(isos, 'data/covariates/raw_isochrones.RDS')

setDT(isos)

isos[, area := st_area(geometry)]
isos[, max_area := 2.01*10^6] # area of perfect r=800m circle
isos[, walkability := area/max_area] 
isos[, year  := as.character(3)]

bg_isos <- isos[, .(area, max_area, walkability, id, year)]
setnames(bg_isos, "id", 'GEOID')

bg_isos[, walkability := as.numeric(area)]
bg_isos <- bg_isos[, .(walkability, GEOID, year)]

saveRDS(bg_isos, 'data/covariates/walkability.RDS')


