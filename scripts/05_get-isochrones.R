library(readr)
library(sf)
library(tigris)
library(data.table)
library(otptools)
library(dplyr)
library(ggplot2)

options(tigris_class = "sf")

# first, download population-weighted centroids
url <- "https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG27.txt"
loc <- file.path("/Users/mcknigri/Documents/honors/honors-project/data/covariates", 'centrs.csv')
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

bg_isos <- isos[, .(area, max_area, walkability, id)]
setnames(bg_isos, "id", 'GEOID')
saveRDS(bg_isos, 'data/covariates/walkability.RDS')

