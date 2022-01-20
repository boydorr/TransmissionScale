# Plots the simulation grids 
# Written to check the distribution of dog densities across time at different spatial scales
# to ensure that they are consistent
# 2019-09, Rebecca Mancy
rm(list=ls())
options(stringsAsFactors=F) # otherwise bad things happen to dates

library(data.table)
library(ggplot2)
library(rgdal) # to read in map
library(ggsn) # for scalebar

source("Style_Sheet.R") # ggplot style sheet for colour scheme, etc.
ffigs <- "figs/"

#
#' @title plotSimGrid
#' @description Returns a ggplot plot
#' @param grd Grid information
#' @param SD.2002 Serengeti district map shapefile
#' @param cases Case list csv (or NA if no cases to be plotted)
#' @export
#' 
plotSimGrid <- function(grd, resolution, SD.2002, cases = NA, plot_legend = TRUE) {
  # Other options for this joining here: https://stackoverflow.com/questions/22096787/how-keep-information-from-shapefile-after-fortify
  grd.df = data.table(fortify(grd, data = grd@data$`0`))
  grd.df = data.table(fortify(grd))
  grd.data <- data.table(cbind(grd@data), id = rownames(grd@data))
  print(resolution)
  print(((resolution/1000)^2))
  grd.data$pop <- grd.data$`2557` / ((resolution/1000)^2) # compute population density from carrying capacity
  print(summary(grd.data$pop))
  setkey(grd.df,id)
  setkey(grd.data, id)
  grd.df[grd.data, pop:=pop] # Note that for this to work inside the package function, need to add data.table to Imports: and Depends: in DESCRIPTION file
  # See https://stackoverflow.com/questions/27980835/r-data-table-works-in-direct-call-but-same-function-in-a-package-fails
  
  SD.df <- data.table(fortify(SD.2002))
  
  # Shading of population density
  p.grd <- ggplot(grd.df, aes(x = long, y = lat)) +
    geom_map(map = grd.df, aes(map_id=id, fill = pop)) +
    scale_fill_gradient(low = "white", high = sc.colours$dark.navy, "Density")
  # Case locations
  if (!is.na(cases)) { # Case locations
    p.grd <- p.grd + geom_point(data=cases, aes(x = x_coord, y = y_coord), colour = sc.colours$bright.red, shape = 20, size = 0.5)
  }
  # Lines between villages
  p.grd <- p.grd +
      geom_polygon(data = SD.df, aes(x = long, y = lat, group=id), fill=NA, colour=sc.colours$grey, size=0.2) + # villages!!
      coord_equal() + xlab("") + ylab("") +
      theme_classic() + theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(), legend.position = plot_legend); # "none" or "c(0.95,0.83)"
  #scalebar(d = 1000, xy = c(650000, 9780000), type = "line", divs = 2, below = "metres", location="topright", st.size=2) ;
  p.grd
  
  return(p.grd)
}

# Read in map
SD.2002 <- readOGR(dsn = path.expand("data/"), layer="SD_Villages_2002_From_HHS_250m_Smoothed_UTM")

#########################################################################
load(file = "data/grd_500.rda")
grd <- grd.500
bb <- data.frame(bbox(grd))
grd.df = data.table(fortify(grd, data = grd@data$`0`))
grd.df = data.table(fortify(grd))
p <- plotSimGrid(grd = grd.500, SD.2002 = SD.2002, cases = NA, resolution = 500, plot_legend = c(0.95,0.83))
p <- p + annotation_scale(width_hint = 0.3)
p; p500 <- p
ggsave(plot = p, path = ffigs, filename = "Sim_Grid_500.pdf", width = sc.1col.w, height = sc.1col.w, units = "cm")
rm(grd.500)

#########################################################################
load(file = "data/grd_1000.rda")
grd <- grd.1000
bb <- data.frame(bbox(grd))
grd.df = data.table(fortify(grd, data = grd@data$`0`))
grd.df = data.table(fortify(grd))
p <- plotSimGrid(grd = grd.1000, SD.2002 = SD.2002, cases = NA, resolution = 1000, plot_legend = c(0.95,0.83))
p; p1000 <- p
ggsave(plot = p, path = ffigs, filename = "Sim_Grid_1000.pdf", width = sc.1col.w, height = sc.1col.w, units = "cm")
rm(grd.1000)

#########################################################################
load(file = "data/grd_2000.rda")
grd <- grd.2000
bb <- data.frame(bbox(grd))
grd.df = data.table(fortify(grd, data = grd@data$`0`))
grd.df = data.table(fortify(grd))
p <- plotSimGrid(grd = grd.2000, SD.2002 = SD.2002, cases = NA, resolution = 2000, plot_legend = c(0.95,0.83))
p; p2000 <- p
ggsave(plot = p, path = ffigs, filename = "Sim_Grid_2000.pdf", width = sc.1col.w, height = sc.1col.w, units = "cm")
rm(grd.2000)

#########################################################################
load(file = "data/grd_4000.rda")
ggplot(grd.4000@data) + geom_histogram(aes(x=`0`/16), alpha = 0.1, fill = "orange", col = "orange", binwidth = 10) +
  geom_histogram(aes(x=`2557`/16), alpha = 0.1, fill = "blue", col = "blue", binwidth = 10) + 
  geom_histogram(aes(x=`5082`/16), alpha = 0.1, fill = "green", col = "green", binwidth = 10) +
  theme_light()
grd <- grd.4000
bb <- data.frame(bbox(grd))
grd.df = data.table(fortify(grd, data = grd@data$`0`))
grd.df = data.table(fortify(grd))
p <- plotSimGrid(grd = grd.4000, SD.2002 = SD.2002, cases = NA, resolution = 4000, plot_legend = c(0.95,0.83))
p; p4000 <- p
ggsave(plot = p, path = ffigs, filename = "Sim_Grid_4000.pdf", width = sc.1col.w, height = sc.1col.w, units = "cm")
rm(grd.4000)

#########################################################################
p_all <- cowplot::plot_grid(p500, p1000, p2000, p4000, 
                            nrow = 2, rel_heights = c(0.5, 0.5), align = "v", 
                            labels = c('A', 'B', 'C','D'), label_size = 20)
ggsave(plot = p_all, path = ffigs, filename = "fig_S1_maps.pdf", width = 21, height = 21, units="cm")

