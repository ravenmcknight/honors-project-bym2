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

## airport

## hospital

## add to covariates --------------------------------------