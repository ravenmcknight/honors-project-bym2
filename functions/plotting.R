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

## MAKE HISTOGRAMS ##

makeHist <- function(dat, var, titles) {
  ggobj <- ggplot(data = dat, aes_string(x = var)) + 
    geom_histogram(fill = "#33638dff") +
    theme_light() +
    labs(title = titles,
         subtitle = "By Census Block Group, 2017",
         x = "value") 
  ggsave(sprintf("img/hist/%s.png", var))
}


## MAKE MAPS ##
# theoretically should be basically the same

makeMap <- function(dat, var, titles) {
  ggobj <- ggplot(data = dat, aes_string(x = var)) + 
    geom_sf() +
    theme_light() +
    labs(title = paste0(titles),
         subtitle = "By Census Block Group, 2017",
         x = "value") 
  ggsave(sprintf("%s.png", var))
}


