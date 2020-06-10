## goal: speed up repetetive plotting

## packages -----------------------------------------------

# these lines make sure any user has all the necessary packages
packages <- c('ggplot2', 'dplyr')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

## functions ----------------------------------------------

# testing stuff to be deleted
options(tigris_class = "sf")
counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
bgs <- tigris::block_groups("MN", counties, 2016)

# use unscaled for clarity
mod_dat <- readRDS("data/covariates/cleaned/all_covariates.RDS")

sfdat <- dplyr::left_join(bgs, mod_dat)

head(sfdat)

testvars <- c('genz', 'millenial', 'genx', 'boomer')
testtitles <- c("Genz", "Millenial", "Genx", "Boomer")
ltitles <- c("perc", "perc", "perc", "perc")

# end deletion

## MAKE HISTOGRAMS ##

makeHist <- function(dat, var, titles) {
  ggobj <- ggplot(data = dat, aes_string(x = var)) + 
    geom_histogram(fill = "#33638dff") +
    theme_light() +
    labs(title = paste0(titles),
         subtitle = "By Census Block Group, 2017",
         x = "value") 
  ggsave(sprintf("%s.png", var))
}


## MAKE MAPS ##


# testing
for(i in 1:length(testvars)){
  makeHist(sfdat, testvars[i], testtitles[i])
}

