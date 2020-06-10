## Goal: get covariate data

## packages -----------------------------------------------
packages <- c('data.table', 'tigris', 'ggplot2', 'dplyr', 'rgdal', 'sf')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

options(tigris_class = 'sf')

## read all covariates into df together -------------------
# this is very imperfect

file_list <- paste0("data/covariates/", list.files(path = "data/covariates/", pattern = ".RDS"))

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- readRDS(file)
  }
  
  # if the merged dataset does exist, merge to it
  if (exists("dataset")){
    temp_dataset <- readRDS(file)
    dataset <- temp_dataset[dataset, on = .(year, GEOID)]
    rm(temp_dataset)
  }
  
}

setDT(dataset)

# remove duplicates
dataset[, `:=` (i.summary_est = NULL, i.summary_est.1 = NULL, i.genz = NULL, 
                i.millenial = NULL, i.genx = NULL, i.boomer = NULL) ]

dataset[, tract_GEOID := stringr::str_sub(GEOID, end = -2)]

# only available at tract level
tract_list <- paste0("data/covariates/tract/", list.files(path = "data/covariates/tract/", pattern = ".RDS"))

for (file in tract_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("tdataset")){
    tdataset <- readRDS(file)
  }
  
  # if the merged dataset does exist, merge to it
  if (exists("tdataset")){
    temp_tdataset <- readRDS(file)
    tdataset <- temp_tdataset[tdataset, on = .(year, GEOID)]
    rm(temp_tdataset)
  }
  
}

setDT(tdataset)
tdataset[, `:=` (i.perc_transit_comm = NULL, i.perc_wfh = NULL)]
setnames(tdataset, "GEOID", "tract_GEOID")

cov <- tdataset[dataset, on = .(tract_GEOID, year)]

## geographic info ------------------------------

counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- block_groups("MN", counties, 2016)

cov <- left_join(bgs, cov, on = 'GEOID')
setDT(cov)

cov[, sqkm := ALAND/1000000]
cov[, emp_density := w_total_jobs_here/sqkm]
cov[, pop_density := estimate_tot_pop/sqkm]
cov[, c('STATEFP', 'COUNTYFP', 'TRACTCE', 'BLKGRPCE', 'NAMELSAD', 'MTFCC', 'FUNCSTAT', 
            'ALAND', 'AWATER', 'INTPTLAT', 'INTPTLON', 'geometry') := NULL]

# save un-standardized
saveRDS(cov, 'data/covariates/cleaned/all_covariates.RDS')

## standardize ------------------------

# scale
scaled_dat <- lapply(cov[, -c('GEOID', 'year', 'tract_GEOID')], scale)
scaled_dat$GEOID <- cov$GEOID
scaled_dat$year <- cov$year
scaled_dat$tract_GEOID <- cov$tract_GEOID
scaled_dat <- as.data.table(scaled_dat)

saveRDS(scaled_dat, 'data/covariates/cleaned/all_covariates_scaled.RDS')
