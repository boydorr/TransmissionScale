rm(list=ls())
options(stringsAsFactors=F) # otherwise bad things happen to dates

library(ggplot2)
library(cowplot)
library(rgdal) # for readOGR
library(lubridate)
library(dplyr)
library(ggpubr)
library(sf) # For maps
library(gridExtra)


source("Style_Sheet.R") 

# Set parameters
start.date <- as.Date("2002-01-01")
end.date <- as.Date("2015-12-31")
ffigs <- "figs/"

###################################################################################
#                Figure S2(A), Bar chart of dogs vaccinated per annum
###################################################################################

vc <- read.csv(file = "data/vc_1000.csv") 

vc$year <- year(floor_date(start.date + vc$dayVaccinations, unit = "year"))
vc.by.year <- vc %>% group_by(year) %>%
  summarise(DogsVaccinated = sum(DogsVaccinated))
head(vc.by.year)

p.vc <- ggplot(data=vc.by.year, aes(x=year, y=DogsVaccinated)) + geom_bar(stat="identity", fill = sc.colours$bright.red) +
  scale_x_continuous("", breaks=seq(2002, 2015, by=2)) +
  scale_y_continuous("Dogs vaccinated") +
  ss + 
  theme(axis.line.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.y=element_text(size=14)); p.vc

###################################################################################
#        Figure S2(B), Line graph of vaccination coverage (with waning)
###################################################################################
# Time series of coverage information
cov <- read.csv(file = "java_output/Round3_COVERAGE_1000/output_files/Round3_COVERAGE_1000_mixedpop_coverage_001.csv") 
cov$Date = as.Date("2002-01-01") + as.numeric(cov$time)
cov <- subset(cov, Date <= end.date)
p.cov <- ggplot(cov, aes(x=Date, y=coverage)) + geom_line(colour = sc.colours$black) + 
  scale_y_continuous("Rolling coverage", limits = c(0, 0.5), labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(breaks = seq(as.Date("2002-01-01"), as.Date("2016-01-01"), by="2 years"), labels = seq(2002, 2016, by=2) ) +
  ss + theme(axis.text=element_text(size=14)); p.cov

###################################################################################
#       Figure S2(C-D), Maps of vaccination campaigns, 2005 and 2015
###################################################################################

# Load up maps
SD.2002 <- readOGR(dsn = path.expand("data/"), layer = "SD_Villages_2002_From_HHS_250m_Smoothed_UTM")
load("data/final_vc_250_1000_4000_all_with2016.rda")
load("data/grd_250.rda")

# Make dates for years of interest (2005 and 2015)
yrs <- c(as.Date("2005-01-01"), as.Date("2015-01-01"))
final.vc.250$year <- floor_date(as.Date("2002-01-01") + final.vc.250$dayVaccinations, unit = "year")

# Convert everything to sf format for easy plotting of maps in ggplot
SD.2002.sf <- sf::st_read("data/SD_Villages_2002_From_HHS_250m_Smoothed_UTM.shp")
grd.250.sf <- sf::st_as_sf(grd.250)
grd.250.sf <- grd.250.sf[,-seq(4,170)]
head(grd.250.sf)
grd.250.sf.vc <- left_join(x=grd.250.sf, y=final.vc.250, by=c("popID"="popID.250"))
summary(grd.250.sf.vc)
grd.250.sf.vc$Vaccinated <- grd.250.sf.vc$Dogs.Vaccinated > 0

# Map for 2005
map.cov.2005 <- ggplot(data = filter(grd.250.sf.vc, Year == as.Date("2005-01-01"))) + 
  geom_sf(data = SD.2002.sf, colour = sc.colours$grey, size = 0.3, fill = "white") + 
  geom_sf(aes(fill = Vaccinated, colour = Vaccinated)) +
  scale_fill_manual(values = c(sc.colours$black), guide = NULL) +
  scale_colour_manual(values = c(sc.colours$black), guide = NULL) +
  #scale_fill_gradient("Dogs vaccinated", low = "gold", high = "red3") +
  annotate(geom = "text", x = Inf , y = Inf, label = "2005", hjust = 1.0, vjust = 1.0, size = 10) +
  theme_void(); map.cov.2005

# Map for 2015
map.cov.2015 <- ggplot(data = filter(grd.250.sf.vc, Year == as.Date("2015-01-01"))) + 
  geom_sf(data = SD.2002.sf, colour = sc.colours$grey, size = 0.3, fill = "white") + 
  geom_sf(aes(fill = Vaccinated, colour = Vaccinated)) +
  scale_fill_manual(values = c(sc.colours$black), guide = NULL) +
  scale_colour_manual(values = c(sc.colours$black), guide = NULL) +
  annotate(geom = "text", x = Inf , y = Inf, label = "2015", hjust = 1.0, vjust = 1.0, size = 10) +
  theme_void(); map.cov.2015

###################################################################################
#                Figure S2 - Combine into a single figure
###################################################################################

p_vacc_maps <-
  ggdraw() +
  draw_plot(map.cov.2005, x = 0, y = .5, width = .9, height = .49) +
  draw_plot(map.cov.2015, x = 0, y = 0, width = .9, height = .49) +
  draw_plot_label(label="C", x=0.005, y=1, size = 20) +
  draw_plot_label(label="D", x=0.005, y=.5, size = 20)
p_vacc_maps

p_vacc_ts <-
  ggdraw() +
  draw_plot(p.vc, x = 0, y = 0.5, width = .99, height = .5) +
  draw_plot(p.cov, x = 0.025, y = 0, width = .94, height = .5) +
  draw_plot_label(label="A", x=0.005, y=1, size = 20) +
  draw_plot_label(label="B", x=0.005, y=.5, size = 20)
p_vacc_ts

# Combine all into a single figure
fig_S2 <- grid.arrange(p_vacc_ts, p_vacc_maps, ncol=2, nrow=1, widths=c(3,3))
fig_S2
# fig_S3 <- plot_grid(p.vc, map.cov.2005, p.cov, map.cov.2015,
#                     ncol = 2, rel_heights = c(1, 1), rel_widths = c(1,1),
#                     labels = c('A', 'C', 'B', 'D'), label_size = 12) + 
#           theme(plot.background = element_rect(fill = "white")); fig_S3

ggsave(filename = paste0(ffigs, "Fig_S2.pdf"), plot = fig_S2, width = 25, height = 25, units = "cm")
ggsave(filename = paste0(ffigs, "Fig_S2.png"), plot = fig_S2, width = 32, height = 32, units = "cm")
