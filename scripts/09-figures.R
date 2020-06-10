## goal: create lots of plots!

## packages -----------------------------------------------

# these lines make sure any user has all the necessary packages
packages <- c('ggplot2', 'dplyr', 'sf', 'tigris')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

## data ---------------------------------------------------

counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
options(tigris_class = "sf")
bgs <- block_groups("MN", counties, 2016)

# unscaled covariates for mapping etc
cov <- readRDS("data/covariates/cleaned/all_covariates.RDS")

# sf
sfdat <- dplyr::left_join(bgs, cov)

## functions ----------------------------------------------
source("functions/plotting.R")
