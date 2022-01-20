# Figure 4 - Holling curves & maps
rm(list=ls())
options(stringsAsFactors = F)

library(tidyverse)
library(scales)
library(cowplot)
library(rgdal) # for reading in shapefiles
library(scales) # for date_format
library(lubridate)
library(sf) # For ggplot maps
library(sp)
library(ggpubr)
library(mgcv) # for gam fit to scatter plot
library(ggspatial) # For scalebar on map
library(gridExtra)
library(patchwork) #; update.packages("patchwork")
library(viridis)

source("Style_Sheet.R")

# read in combined output from all spatial scales and detection levels for runs using ACCEPTED parameters
pr.acc <- readRDS(file="java_output/Round2_Compiled/All_Round2_Reliability.rda")

load("data/grd_1000.rda")  # for maps
grd.1000.occ <- subset(grd.1000@data, `0`>0)[,3:ncol(grd.1000@data)] # Remove empty cells
midpoint <- ncol(grd.1000.occ)/2
max_dens <- 500

dens <- hist(grd.1000.occ[,midpoint], 0:max_dens, plot = FALSE)
dens_min <- hist(grd.1000.occ[,1], 0:max_dens, plot = FALSE)
dens_max <- hist(grd.1000.occ[,ncol(grd.1000.occ)], 0:max_dens, plot = FALSE)
grd.dens <- data.frame(dens = dens$density, 
                       dens_min = dens_min$density, 
                       dens_max = dens_max$density, 
                       freq = dens$counts/(length(dens$counts)),
                       x = (1:max_dens)-0.5, 
                       prop = dens$counts/nrow(grd.1000.occ),
                       counts = dens$counts,
                       cumdens = cumsum(dens$density))

# Examine densities 
grd.dens$x[which(grd.dens$cumdens > 0.5)[1]]; 

# Over occupied cells - look at population growth
median(grd.1000.occ[,1])
median(grd.1000.occ[,midpoint])
median(grd.1000.occ[,ncol(grd.1000.occ)])

# Over all cells
median(grd.1000@data[,1+2])
median(grd.1000@data[,midpoint+2])
median(grd.1000@data[,ncol(grd.1000)])

# Look at quantiles of density at midpoint in timeseries
quantile(grd.1000.occ[,midpoint], c(0.05, 0.1, 0.5, 0.9, 0.95, 0.99)) # 2.5, 4.5, 16.4, 64.5, 125
length(which(grd.1000.occ[,midpoint]>100))/nrow(grd.1000.occ) # 2%
length(which(grd.1000.occ[,midpoint]>50))/nrow(grd.1000.occ) # <9%
length(which(grd.1000.occ[,midpoint]>11))/nrow(grd.1000.occ) # <70%

# general density stats
mean_density <- mean(grd.1000.occ[,midpoint]); mean_density # 23
median_density <- median(grd.1000.occ[,midpoint]); median_density # 16
mode_density <- grd.dens$x[which.max(grd.dens$freq)]; mode_density # 7.5

# Panel A Holling Curve with inset ----
# Select most reliable parameter sets at each spatial scale
best_Holling <- pr.acc %>%
  group_by(Res) %>%
  filter(propAcc == max(propAcc))
best_Holling

# There are two parameter sets at the 500x500 scale, so choose the one with the lower maximum number of dogs bitten (biological constraint)
best_Holling <- filter(best_Holling, maxMaxDD < 258)
best_Holling

#' Computes the contact rate
#' Using the Density, the (Th) Handling time and the Td) Discovery time (1/attack rate)
computeContactRate <- function(dens, Th, Td) {
  dens.adj <- dens + 1
  rc <- (dens.adj / Td) / (1 + (Th*dens.adj/Td))
  return (rc)
}

holling.density <- seq(0, 250, by=0.5)
holling.data <- data.frame(density = holling.density,
                           rc_500 = computeContactRate(dens = holling.density, Th = filter(best_Holling, Res == "500")$ThMean, Td = filter(best_Holling, Res == "500")$TdMean),
                           rc_1000 = computeContactRate(dens = holling.density, Th = filter(best_Holling, Res == "1000")$ThMean, Td = filter(best_Holling, Res == "1000")$TdMean),
                           rc_2000 = computeContactRate(dens = holling.density, Th = filter(best_Holling, Res == "2000")$ThMean, Td = filter(best_Holling, Res == "2000")$TdMean),
                           rc_4000 = computeContactRate(dens = holling.density, Th = filter(best_Holling, Res == "4000")$ThMean, Td = filter(best_Holling, Res == "4000")$TdMean),
                           rc_all = computeContactRate(dens = holling.density, Th = filter(best_Holling, Res == "all")$ThMean, Td = filter(best_Holling, Res == "all")$TdMean)
)
head(holling.data)
holling_data_long <- gather(holling.data, "Scale", "r_c", -density)
head(holling_data_long)

# All on the same figure
cols <- c("rc_500" = sc.colours$grey, "rc_1000" = sc.colours$bright.red, "rc_2000" = sc.colours$grey, "rc_4000" = sc.colours$grey, "rc_all" = sc.colours$grey)
linetypes <- c("rc_500" = "dotted", "rc_1000" = "solid", "rc_2000" = "dotdash", "rc_4000" = "dashed", "rc_all" = "longdash")
lbls <- c(expression(0.25~km^2), expression(1~km^2), expression(4~km^2), expression(16~km^2), "District")

p_holling <- ggplot(holling_data_long, aes(x = density, y = r_c * 2, colour = Scale, linetype = Scale)) +
  annotate("rect", xmin = 0, xmax = median_density, ymin = 0, ymax = 5, fill = sc.colours$bg.grey, alpha = 0.5) + 
  scale_x_continuous("", lim = c(0,300)) +
  scale_y_continuous(name = "", lim = c(0,5)) + # name = expression(r[c])
  scale_colour_manual("Spatial scale", breaks = c("rc_500", "rc_1000","rc_2000", "rc_4000", "rc_all"),
                      labels = lbls,
                      values = cols) +
  scale_linetype_manual("Scale", breaks = c("rc_500", "rc_1000","rc_2000", "rc_4000", "rc_all"),
                        labels = lbls,
                        values = linetypes) + 
  geom_line() + 
  ss +
  theme(legend.position = "none", axis.title = element_blank(), 
        plot.margin = margin(c(-0.005, -0.005, -0.005, -0.005)), 
        axis.text = element_text(size = 8))
p_holling

# Reformat on log scale
p_holling_log <- ggplot(holling_data_long, aes(x = density, y = r_c * 2, colour = Scale, linetype = Scale)) +
  annotate("rect", xmin = 0, xmax = median_density, ymin = 0, ymax = 4, fill = sc.colours$bg.grey, alpha = 0.5) + 
  scale_x_continuous(name = "Dog density", trans='log10', lim = c(1,300)) + 
  scale_y_continuous(name = "Expected contacts", limits = c(0,4), expand = c(0, 0)) +
  scale_colour_manual("Scale", breaks = c("rc_500", "rc_1000","rc_2000", "rc_4000", "rc_all"),
                      labels = lbls, values = cols) +
  scale_linetype_manual("Scale", breaks = c("rc_500", "rc_1000","rc_2000", "rc_4000", "rc_all"),
                        labels = lbls, values = linetypes) + 
  geom_line() + # https://ggplot2.tidyverse.org/reference/annotate.html
  geom_hline(yintercept = 2, sc.colours$grey, alpha = 0.25, size = 0.5) +
  ss +  theme(legend.position = "bottom", plot.margin = unit(c(0, 5, 0, 5), "points"),
              legend.key.size = unit(.5, 'cm'),legend.text=element_text(size=8), legend.title=element_text(size=8),
              axis.text = element_text(size = 8), 
              axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 9)) # + labs(tag = "A")
p_holling_log


# Panel B: Distribution of dog densities ----
# Make density insets
dens_inset_log <- ggplot(grd.dens, aes(x = x, y = dens)) + 
  annotate("rect", xmin = 0, xmax = median_density, ymin = 0, ymax = 0.05, fill = sc.colours$bg.grey, alpha = 0.5) + 
  geom_point(colour = "black", alpha = 0.5, size = 1.5) +
  scale_x_continuous("", trans='log10', lim=c(1,300)) + 
  scale_y_continuous("Proportion", limits = c(0,0.05), expand = c(0, 0)) + 
  ss + theme(plot.margin = unit(c(0, 5, 0, 0), "points"), 
             plot.tag = element_text(size = 10, face="bold"),
             axis.text = element_text(size = 8), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8)) + 
  labs(tag = "A", element_text(face = 'bold', size = 8))
dens_inset_log

dens_inset <- ggplot(grd.dens, aes(x = x, y = dens)) + 
  annotate("rect", xmin = 0, xmax = median_density, ymin = 0, ymax = 0.05, fill = sc.colours$bg.grey, alpha = 0.5) + 
  geom_point(colour = "black", alpha = 0.5, size = 3) +
  scale_x_continuous("Dog density", lim=c(1,300)) + 
  scale_y_continuous("Proportion", lim = c(0,0.05)) + 
  ss +
  labs(tag = "B")
dens_inset

# Combine together
p_holling_with_inset <-
  ggdraw() +
  draw_plot(p_holling_log, x=0.05, y=0, width = 0.95, height = 1) +
  draw_plot(p_holling, x = 0.55, y = .22, width = .35, height = .35) + 
  draw_plot_label(label="B", x=0.05, y=1, size = 10)
p_holling_with_inset 

ggarrange(dens_inset_log, p_holling_with_inset, heights = c(1, 3), widths = c(2,1.2), ncol = 1, nrow = 2)

# Panel C: R0 histogram ----
# Import data for histograms of R0 (and Re)
sum_Re <- readRDS(file="java_output/summary_Re.rds")
sum_endemic <- readRDS(file = "java_output/summary_R0_endemic.rds")
sum_density <- readRDS(file = "java_output/summary_R0_density.rds")
sum_location <- readRDS(file = "java_output/summary_R0_location.rds")

summary_stats_long <- rbind.data.frame(sum_Re, sum_endemic, sum_density, sum_location)

# Plot the distributions of mean values per run for number of transmissions
g_R0 <- ggplot(filter(summary_stats_long, "Simulation" %in% c("Simulation","nTransmissions")), aes(nTransmissions)) + 
  geom_histogram(aes(y=after_stat(density), fill=Simulation), binwidth = 0.01, alpha = 0.35, position = "identity") +
  scale_x_continuous(expression('R'[0]), breaks = seq(1.2, 1.8, by=0.1), lim = c(1.2,1.7)) +
  scale_y_continuous(name = "Density") +
  scale_fill_manual("", labels = c("Dog density","Transmission network","Locations"), 
                    values = c(sc.colours$orange, sc.colours$bright.red, sc.colours$blue)) +
  ss + theme(legend.text = element_text(size = 8), legend.key.size = unit(.25, 'cm'),
             plot.tag = element_text(size=10, face="bold"),
             axis.text = element_text(size = 8),axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8)) + 
  labs(tag = "C")
g_R0 

# Panels D & E: Maps of R0 & dog density ----
# Load up map
head(grd.1000@data); dim(grd.1000) # 2770
popID_levels <- grd.1000$popID

# Convert grid for ggplot
grd_pops <- grd.1000@data[, 3:ncol(grd.1000)]; dim(grd_pops)
sf_grd_1000 <- readRDS(file="java_output/grd_1000_summary.rds")

# DOG DENSITY
sf_grd_1000$mid_density <- sf_grd_1000$'2557' # 5082/2

dog_density <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = mid_density), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0, end = 1, 
                       limits = c(0, 100), oob = scales::squish, 
                       name = "Density") + 
  theme_classic() + 
  theme(axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.line = element_blank(), 
        legend.key.size = unit(.4, 'cm'), legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        plot.margin = margin(c(0, 0, 0, 0)), plot.tag = element_text(size=10, face="bold")) + labs(tag = "E")
dog_density

# R0
hist(sf_grd_1000$R_dog_density, breaks = seq(0,2,0.001))

# from index cases by dog density
R_dog_density <- ggplot(data=sf_grd_1000) + 
  geom_sf(aes(fill = R_dog_density), colour = NA) + 
  scale_fill_viridis_c(option = "rocket", na.value = "white",
                       direction = -1, begin = 0, end = 1, 
                       limits = c(0.5, 2), oob = scales::squish, 
                       name = expression('R'[0])) + 
  # ggtitle("\n ") + #  guides(fill=FALSE) + ggtitle("R \nDog density") +
  theme_classic() +   
  theme(axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.line = element_blank(), 
        legend.key.size = unit(.4, 'cm'),legend.title = element_text(size = 8), legend.text = element_text(size = 6),
        plot.margin = margin(c(0, 0, 0, 0)), plot.tag = element_text(size=10, face="bold")) + 
  labs(tag = "D") 
R_dog_density

p_density_maps <-
  ggdraw() +
  draw_plot(dog_density, x = 0, y = 0, width = .9, height = .5) +
  draw_plot(R_dog_density, x = 0, y = .5, width = .9, height = .5)
p_density_maps

# Combine all into a single figure
holling_combo <- grid.arrange(dens_inset_log, g_R0, p_holling_with_inset, p_density_maps, 
                     ncol=2, nrow=2, widths=c(3,3), heights=c(1.25, 4))
ggsave("figs/fig4_combined.jpeg", holling_combo, width = 19, height = 19, units = "cm")
ggsave("figs/fig4_combined.pdf", holling_combo, width = 19, height = 16, units = "cm")




