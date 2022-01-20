# Spatial distribution of R
rm(list=ls()) 
options(stringsAsFactors = F)

library(ggplot2)
library(dplyr)
library(tidyr) # library(sp)
library(sf) # For maps
library(viridis)
library(scales)
library(gridExtra)
library(ggpubr)
library(rgeos)

source("Style_Sheet.R")

# Import data for histograms of R0 (and Re)
sum_Re <- readRDS(file="java_output/summary_Re.rds")
sum_endemic <- readRDS(file = "java_output/summary_R0_endemic.rds")
sum_density <- readRDS(file = "java_output/summary_R0_density.rds")
sum_location <- readRDS(file = "java_output/summary_R0_location.rds")

summary_stats_long <- rbind.data.frame(sum_Re, sum_endemic, sum_density, sum_location)

# Plot the distributions of mean values per run for number of transmissions
g_Re <- ggplot(filter(summary_stats_long, "Simulation" %in% c("Simulation","nTransmissions")), aes(nTransmissions)) + 
  geom_histogram(aes(y=after_stat(density), fill=Simulation), binwidth = 0.01, alpha = 0.35, position = "identity") +
  scale_x_continuous('R', breaks = seq(0.6, 2, by=0.1), lim = c(0.7,1.7)) +
  scale_fill_manual("", labels = c("Dog density","Transmission network","Locations","Re"), 
                    values = c(sc.colours$orange, sc.colours$bright.red, sc.colours$blue, sc.colours$grey)) +
  ss + theme(legend.position = "right")
g_Re

# Alternative formats for presentation
ggplot(summary_stats_long, aes(nTransmissions)) + 
  geom_freqpoly(aes(y=after_stat(density), colour=Simulation), 
                binwidth = 0.02, alpha = 0.5) + ss

ggplot(summary_stats_long, aes(x=nTransmissions, colour = Simulation, fill = Simulation)) + 
  geom_density(alpha = 0.2) + ss

# Load up map
load(file = "data/grd_1000.rda") # grd.1000@data <- grd.1000@data[,1:2]
head(grd.1000@data); dim(grd.1000) # 2770
popID_levels <- grd.1000$popID

# Convert grid for ggplot
grd_pops <- grd.1000@data[, 3:ncol(grd.1000)]; dim(grd_pops)
sf_grd_1000 <- readRDS(file="java_output/grd_1000_summary.rds")

# Index cases simulated under dog density
Cases_dog_density <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = AvgCases_dog_density), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0, end = 1,
                       limits = c(0, 25), oob = scales::squish) + 
  ggtitle("\n Dog density") +
  theme_classic() + 
  theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(), 
        plot.title = element_text(lineheight=.8),
        legend.position=c(-0.1,0.5), 
        legend.key.height= unit(1, 'cm'), 
        legend.key.width= unit(0.5, 'cm'),
        legend.title=element_blank())
Cases_dog_density

# Cases according to various simulations
# Under low coverage - endemic circulation
Cases_lv <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = AvgCases_lv), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0, end = 1,
                       limits = c(0, 25), oob = scales::squish) + 
  ggtitle("Cases") + # guides(fill=FALSE) + ggtitle("Cases") +
  theme_classic() + 
  theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(), 
        plot.title = element_text(lineheight=.8),
        legend.position=c(-0.1,0.5), 
        legend.key.height= unit(1, 'cm'), 
        legend.key.width= unit(0.5, 'cm'),
        legend.title=element_blank()) + labs(tag = "A")
Cases_lv

# Index cases simulated from the epidemic network
Cases_endemic <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = AvgCases_naive), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0, end = 1,
                       limits = c(0, 25), oob = scales::squish) + 
  guides(fill=FALSE) +   ggtitle("Transmission network") +
  theme_classic() + 
  theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(),
        plot.title = element_text(lineheight=.8))
Cases_endemic

#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
mylegend <- g_legend(Cases_dog_density)

# DOG DENSITY - midpoint in timeseries
sf_grd_1000$mid_density <- sf_grd_1000$'2557' # 5082/2

dog_density <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = mid_density), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0, end = 1, 
                       limits = c(0, 100), oob = scales::squish) + 
  ggtitle("Dog density") +
  theme_classic() + 
  theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(), 
        plot.title = element_text(lineheight=.8),
        legend.position=c(-0.1,0.5), 
        legend.key.height= unit(1, 'cm'), 
        legend.key.width= unit(0.5, 'cm'),
        legend.title=element_blank()) + labs(tag = "B")
dog_density
ggsave(plot = dog_density, width = 20, height = 12, units = "cm", filename = "figs/map_dog_density.pdf")

# R0
# from index cases by dog density
hist(sf_grd_1000$R_dog_density, breaks = seq(0,2,0.001))
R0_dog_density <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = R_dog_density), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0, end = 1, 
                       limits = c(0.5, 2), oob = scales::squish) + 
  ggtitle(expression(R[0]~(density))) + #  guides(fill=FALSE) + ggtitle("R \nDog density") +
  theme_classic() +   
  theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(), 
        plot.title = element_text(lineheight=.8),
        legend.position="none", # legend.position=c(0,0.5), 
        legend.key.height= unit(1, 'cm'), 
        legend.key.width= unit(0.5, 'cm'),
        legend.title=element_blank()) + labs(tag = "D")
R0_dog_density
ggsave(plot = R0_dog_density, width = 20, height = 12, units = "cm", filename = "figs/R_dog_density.pdf")

# Re - i.e. under low vaccination coverage
hist(sf_grd_1000$R_lv, breaks = seq(0,2,0.001))
Re_lv <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = R_lv), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0, end = 1, 
                       limits = c(0, 2.1), oob = scales::squish) + 
  ggtitle(expression('R'[e])) +
  theme_classic() + 
  theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(),
        plot.title = element_text(lineheight=.8), 
        legend.position=c(-0.1,0.5), 
        legend.key.height= unit(1, 'cm'), 
        legend.key.width= unit(0.5, 'cm'),
        legend.title=element_blank()) + labs(tag = "C")
Re_lv

# R0 from locations (random selection)
hist(sf_grd_1000$R_location, breaks = seq(0,2,0.001))
R0_location <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = R_location), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0, end = 1, 
                       limits = c(0, 2.1), oob = scales::squish) +
  ggtitle(expression(R[0]~(locations))) + #  ggtitle("R") + 
  theme_classic() + 
  theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(),
        plot.title = element_text(lineheight=.8), legend.position="none") + labs(tag = "F")
R0_location

# R0 from endemic network (by case distribution)
hist(sf_grd_1000$R_naive, breaks = seq(0,2,0.001))
R0_endemic <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = R_naive), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0, end = 1, 
                       limits = c(0, 2.1), oob = scales::squish) +
  ggtitle(expression(R[0]~(Transmission~network))) + #  ggtitle("R") + 
  theme_classic() + 
  theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(),
        plot.title = element_text(lineheight=.8), legend.position="none") + labs(tag = "E")
R0_endemic

# density maps (dog density and R0)
map_density_combo <- grid.arrange(arrangeGrob(dog_density, R0_dog_density), nrow=1)
ggsave(plot = map_density_combo, width = 12, height = 21, units = "cm", filename = "figs/map_density_combo.pdf")

# COMBO OF CASES, DENSITY AND R
map_combo <- grid.arrange(
  arrangeGrob(Cases_lv, dog_density, 
              Re_lv, R0_dog_density,
              R0_endemic, R0_location), nrow=1)
map_combo 
ggsave(plot = map_combo, width = 21, height = 21, units = "cm", filename = "figs/Fig_S11_map_combo.pdf")


# Quick check of the distribution on the map....
bks = seq(0,2,0.01)
R_dens <- hist(na.omit(sf_grd_1000$Re_dog_density), breaks = bks, plot = FALSE)$counts
R_loc <-hist(na.omit(sf_grd_1000$Re_location), breaks = bks, plot = FALSE)$counts
R_network <-hist(na.omit(sf_grd_1000$Re_naive), breaks = bks, plot = FALSE)$counts

map_stats_long <- data.frame(
  R0s = c(sf_grd_1000$Re_dog_density, sf_grd_1000$Re_location, sf_grd_1000$Re_naive),
  indexes = c(rep("dens", nrow(sf_grd_1000)), 
              rep("loc",  nrow(sf_grd_1000)), 
              rep("network", nrow(sf_grd_1000))))

ggplot(map_stats_long, aes(x=R0s, colour = indexes, fill = indexes)) + 
  geom_density(alpha = 0.2) + ss


#########################################################################

# Now compare difference in transmission for index cases vs under endemic scenario
hist(sf_grd_1000$R_lv, breaks = seq(0,2,0.001))
hist(sf_grd_1000$R_naive, breaks = seq(0,2,0.001))

sf_grd_1000$R_reduction <-  (sf_grd_1000$R_naive) - (sf_grd_1000$R_lv)
sf_grd_1000$R_reduction_pc <-  sf_grd_1000$R_reduction/(sf_grd_1000$R_naive)

# Reduction in terms of per case transmission
par(mfrow=c(2,1))
hist(sf_grd_1000$R_reduction, breaks = seq(0,1.1,0.025))
hist(sf_grd_1000$R_reduction_pc, breaks = seq(0,1.1,0.025))

R_reduction <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = R_reduction), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0.1, end = 0.98, 
                       limits = c(0, 1), oob = scales::squish) + 
  ggtitle("\n ") +
  theme_classic() +   
  theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(), 
        plot.title = element_text(lineheight=.8),
        legend.position=c(-0.15,0.5), 
        legend.key.height= unit(1, 'cm'), 
        legend.key.width= unit(0.5, 'cm'),
        legend.title=element_blank())
R_reduction

# Percentage reduction in transmission
R_pc_freq <- hist(sf_grd_1000$R_reduction_pc, breaks = seq(0,1.1,0.025))
R_pc = data.frame(freq = R_pc_freq$counts, dens = R_pc_freq$density, mids = R_pc_freq$mids)
R_pc_freq$counts[40]*100/sum(R_pc_freq$counts)

R_pc_barplot <- ggplot(R_pc, aes(x=mids*100, y=dens)) +
  geom_bar(stat="identity", fill = "black", alpha = 0.5) + 
  scale_x_continuous("% reduction") + 
  scale_y_continuous("Density") +
  theme_classic() + 
  theme(axis.text.y = element_text(size=7), axis.title.y = element_text(size=8)) +
  theme(axis.text.x = element_text(size=7), axis.title.x = element_text(size=8)) +
  theme(legend.background = element_rect(fill = "transparent"), 
        legend.box.background = element_rect(fill = "transparent"))
R_pc_barplot

R_reduction_pc <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = R_reduction_pc*100), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0.1, end = 0.98, 
                       limits = c(0, 100), oob = scales::squish) + 
  ggtitle("\n ") +
  theme_classic() +   
  theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(), 
        plot.title = element_text(lineheight=.8),
        legend.position=c(-0.05,0.5), 
        legend.key.height= unit(1, 'cm'), 
        legend.key.width= unit(0.5, 'cm'),
        legend.title=element_blank())
R_reduction_pc
ggsave(plot = R_reduction_pc, width = 12, height = 12, units = "cm", filename = "figs/map_pc_R_reduction.pdf")

# Now prep the combo of maps etc.....
R_reduction_pc
R_pc_barplot

R_plot <- R_reduction_pc + 
  annotation_custom(ggplotGrob(R_pc_barplot), 
                    xmin = 687000, xmax = 715000, 
                    ymin = 9820000, ymax = 9845000)
R_plot
ggsave(plot = R_plot, width = 12, height = 12, units = "cm", filename = "figs/map_pc_R_reduction.pdf")



