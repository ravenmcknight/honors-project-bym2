## Goal: gather some indicator variables to try out

## packages -----------------------------------------------
packages <- c('data.table', 'tigris', 'ggplot2', 'dplyr', 'rgdal', 'sf')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

options(tigris_class = 'sf')

## data ---------------------------------------------------

counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties, 2016)

## gather -------------------------------------------------

## college/university

# read data from: https://hifld-geoplatform.opendata.arcgis.com/datasets/colleges-and-universities/data
url <- "https://opendata.arcgis.com/datasets/0d7bedf9d582472e9ff7a6874589b545_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
loc <- file.path(tempdir(), "cu.zip")
download.file(url, loc)
unzip(loc, exdir = file.path(tempdir(), "cu"), overwrite = TRUE)
file.remove(loc)
cu <- read_sf(file.path(tempdir(), "cu"), stringsAsFactors = FALSE)

# transform
cu <- st_transform(cu, st_crs(bgs))

# just in counties
cubg <- st_intersection(cu, bgs) # wow sf update is way faster
ggplot() + geom_sf(data = bgs) + geom_sf(data = cubg)

  
## light rail

# data from https://gisdata.mn.gov/dataset/us-mn-state-metc-trans-stop-boardings-alightings
lr <- fread('data/csv_trans_stop_boardings_alightings/TransitStopsBoardingsAndAlightings2018.csv') # year doesn't really matter
lr <- lr[Route == "Blue Line" | Route == "North Star" | Route == "Green Line"]
lr <- st_as_sf(lr, coords = c('longitude', 'latitude'), crs = st_crs(bgs))

# just in counties
lrbg <- st_intersection(lr, bgs) 
ggplot() + geom_sf(data = bgs) + geom_sf(data = lrbg)

## airport
abg <- 270539800001

## hospital

# download data from http://gis-hennepin.opendata.arcgis.com/datasets/70e1331d41194f3fa8d48200a2a1af6a_1
url <- "https://opendata.arcgis.com/datasets/70e1331d41194f3fa8d48200a2a1af6a_1.zip?outSR=%7B%22latestWkid%22%3A26915%2C%22wkid%22%3A26915%7D"
loc <- file.path(tempdir(), "hos.zip")
download.file(url, loc)
unzip(loc, exdir = file.path(tempdir(), "hos"), overwrite = TRUE)
file.remove(loc)
hos <- read_sf(file.path(tempdir(), "hos"), stringsAsFactors = FALSE)

# transform
hos <- st_transform(hos, st_crs(bgs))

# in counties
hosbg <- st_intersection(hos, bgs)
ggplot() + geom_sf(data = bgs) + geom_sf(data = hosbg)

## add to covariates --------------------------------------

cov <- readRDS('data/covariates/cleaned/all_covariates_scaled.RDS')
setDT(cov)
cov[, `:=` (airport = 0, college = 0, hospital = 0, lightrail = 0)]

# mark indicators
cov[GEOID %in% cubg$GEOID, college := 1]
cov[GEOID %in% hosbg$GEOID, hospital := 1]
cov[GEOID %in% abg, airport := 1]
cov[GEOID %in% lrbg$GEOID, lightrail := 1]

cov[, `:=` (airport = as.factor(airport), college = as.factor(college),
            hospital = as.factor(hospital), lightrail = as.factor(lightrail))] 

saveRDS(cov, 'data/covariates/cleaned/all_covariates_scaled_ind.RDS')
