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

makeHist <- function(dat, var, titles, subtitles) {
  ggobj <- ggplot(data = dat, aes_string(x = var)) + 
    geom_histogram(fill = "#33638dff") +
    theme_light() +
    labs(title = titles,
         subtitle = subtitles,
         x = "value") 
  ggsave(sprintf("img/hist/%s.png", var))
}


## MAKE MAPS ##
# theoretically should be basically the same

makeMap <- function(dat, var, titles, legend_titles, subtitles) {
  ggobj <- ggplot(data = dat) + 
    geom_sf(aes_string(fill = var), lwd = 0) +
    theme_light() +
    labs(title = paste0(titles),
         subtitle = subtitles, 
         fill = legend_titles) +
    scale_fill_viridis_c(na.value = "transparent")
  ggsave(sprintf("img/map/%s.png", var))
}

