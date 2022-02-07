# Figures_Main - updated on 2021-11-03
rm(list=ls())
options(stringsAsFactors=F) # otherwise bad things happen to dates

library(scales)
library(cowplot)
library(rgdal) # for reading in shapefiles
library(tidyverse)
library(lubridate)
library(sf) # For ggplot maps
library(ggpubr)
library(mgcv) # for gam fit to scatter plot
library(ggspatial) # For scalebar on map
library(bbmle) # fit negative binomial
library(fitdistrplus)
library(MASS)

source("Style_Sheet.R") 
ff_figs <- "figs/"

###################################################################################
#                      Figure 1, Data and key observations
###################################################################################

# Set static parameters
start.date <- as.Date("2002-01-01")
end.date <- as.Date("2015-12-31")
mid.date <- start.date + (as.numeric(end.date - start.date) / 2) + 1
mid.days <- as.numeric(mid.date - start.date) # 2557 (NB: hard-coded into code below because of how column naming works!!)

# Load up village shapefile and case information
SD.2002 <- readOGR(dsn = "data/", layer="SD_Villages_2002_From_HHS_250m_Smoothed_UTM")
SD.2002.sf <- sf::st_read("data/SD_Villages_2002_From_HHS_250m_Smoothed_UTM.shp") # Map for ggplot
load("data/grd_1000.rda")

grd.1000.sf <- sf::st_as_sf(grd.1000)
grd.1000.sf$Mid <- grd.1000.sf$`2557`; summary(grd.1000.sf$Mid)
grd.1000.sf$End <- grd.1000.sf$`5082`; summary(grd.1000.sf$End)
cases_corrected <- readRDS(file = "output/matching_data_caselist.rda") # CREATED IN 06_make_caselist
# See 06_Make_Case_List.R for generation of this file; includes Species
# Identify type of case, sort out dates by month

cases_corrected$Animal <- factor(ifelse(cases_corrected$Species == "Domestic dog", "Dogs", "Others"), levels = c("Others", "Dogs"), ordered = T)
cases_corrected$Month.symptoms.started <- floor_date(cases_corrected$Symptoms.started, unit="months")
cases_corrected$Quarter.symptoms.started <- floor_date(cases_corrected$Symptoms.started, unit="quarter")

# CAN REMOVE WHEN 06_Male_Case_List.R FIXED!!!!
# Tidy version of cases_corrected
# cases_corrected <- cases_corrected %>% 
#   select(ID, Biter.ID, Chain.ID, 
#          UTM.Easting, UTM.Northing, 
#          Animal, Dog.density, Dog.density.incursions, Species, 
#          Symptoms.started, Month.symptoms.started, Quarter.symptoms.started, 
#          Dogs.bitten, Animals.bitten, Carnivores.bitten, date_col)
# summary(cases_corrected)

# -------- Fig 1A Map showing densities and case locations ---------
p_case_map <- ggplot() + 
  geom_sf(data = grd.1000.sf, aes(fill = Mid), colour = NA, lwd = 0) + 
  geom_sf(data = SD.2002.sf, fill = NA, colour = sc.colours$grey, size = 0.2) +
  geom_point(data = cases_corrected, aes(x=UTM.Easting, y=UTM.Northing), colour = sc.colours$bright.red, size = 0.2, alpha = 0.75) + 
  scale_fill_gradient("Dog density", low = "white", high = sc.colours$grey) + 
  annotation_scale(width_hint = 0.4, text_cex = 1) +
  annotate("text", label = "Tarime\n district", x = 678000, y = 9833000, size = 5, type = 2) + 
  annotate("text", label = "Butiama\n district", x = 638000, y = 9835000, size = 5, type = 2) + 
  annotate("text", label = "Bunda\n district", x = 637000, y = 9790000, size = 5, type = 2) + 
  annotate("text", label = "Protected\n area", x = 705000, y = 9810000, size = 5, type = 3) + 
  annotate("text", label = "Protected area", x = 652000, y = 9760000, size = 5, type = 2) + 
  theme_classic() + 
  theme(axis.text=element_blank(), axis.ticks=element_blank(), 
        axis.line = element_blank(), axis.title = element_blank(),
        plot.title = element_text(lineheight=.8), legend.position=c(-0.05,0.7),
        legend.text=element_text(size=15), legend.title=element_text(size=15)); 
p_case_map

# tz_inset
nearby_countries <- readOGR("data", "nearby_countries")
Districts <- readOGR("data", "Tanzania_District_Basic_Population_2012_removedbuffed")

# Transform shapefile for plotting
shp_to_df <- function(shapefile){
  shapefile@data$id <- rownames(shapefile@data)
  shp_df <- fortify(shapefile, region = "id")
  shp_df <- merge(shp_df, shapefile@data, by = "id")
  return(shp_df)
}

eafrica <- shp_to_df(nearby_countries)
districts <- shp_to_df(Districts)

tz_inset <- ggplot() +
  geom_polygon(data=eafrica[which(eafrica$COUNTRY=="Tanzania"),], aes(x=long, y=lat, group=group), fill="white", color="dimgrey") +
  geom_polygon(data=districts[which(districts$DISTRICT=="Serengeti"),], aes(x=long, y=lat, group=group), fill="grey", color="grey") +
  annotate("text", label = "Tanzania", x = 35, y = -6, size = 5, type = 2) + 
  theme_void() +
  theme(panel.background = element_rect(fill="white", color="white"),
        plot.background = element_rect(fill="white", color="white")) +
  coord_equal(xlim=c(29.5,41), ylim=c(-12.5,0.7))
tz_inset

p_serengeti.with.inset <-
  ggdraw() +
  draw_plot(p_case_map) +
  draw_plot(tz_inset, x = 0.7, y = .7, width = .3, height = .3) +
  draw_plot_label(label="A", x=0.05, y=0.9, size = 24)
p_serengeti.with.inset 

# -------------- Fig 1B DATA: Time series from the data of dogs and other carnivores---------------
# Time series by quarter
cases_by_quarter <- cases_corrected %>%
  group_by(Quarter.symptoms.started, Animal) %>%
  summarise(cases = length(Chain.ID))
p_ts_quarterly <- ggplot(cases_by_quarter, aes(x=Quarter.symptoms.started, y=cases)) +
  geom_bar(stat = "identity", aes(fill = Animal), colour = "white", lwd=0.04) +
  scale_fill_manual("", values = c(sc.colours$grey, sc.colours$bright.red)) +
  scale_x_date("", date_breaks = "2 years", labels = date_format("%Y")) +
  scale_y_continuous("Quarterly cases") + ss + theme(legend.position=c(0.85,0.85)); p_ts_quarterly

# Time series by month
cases_by_month <- cases_corrected %>% 
  group_by(Month.symptoms.started, Animal) %>%
  summarise(cases = length(Chain.ID))
p_ts_monthly <- ggplot(cases_by_month, aes(x=Month.symptoms.started, y=cases)) + 
  geom_bar(stat = "identity", aes(fill = Animal), colour = "white", lwd=0.04) +
  scale_fill_manual("", values = c(sc.colours$grey, sc.colours$bright.red)) +
  scale_x_date("", date_breaks = "2 years", labels = date_format("%Y")) + 
  scale_y_continuous("Monthly cases") + ss + theme(legend.position=c(0.85,0.85)); 
p_ts_monthly

p_ts.with.label <-
#  ggdraw() + draw_plot(p_ts_monthly) +
  ggdraw() + draw_plot(p_ts_quarterly) +
  draw_plot_label(label="B", x=0.02, y=0.98, size = 24)
p_ts.with.label

# ----- Fig 1C Scatterplot of bites by density in the data, showing no apparent correlation -----
# NB: here, we use Dog.density.incursions, meaning that all dogs have a non-zero density
# Inset figure with gam fitted to data
gam.all.contact <- gam(formula = Dogs.bitten ~ s(Dog.density.incursions, sp = 0.01), 
                       data = cases_corrected )
summary(gam.all.contact)
se.all.contact <- predict(gam.all.contact, se.fit = T)
all.df <- data.frame(dogCount = cases_corrected$Dog.density.incursions,
                     c.est = gam.all.contact$fitted.values, 
                     c.ub = gam.all.contact$fitted.values + se.all.contact$se.fit, 
                     c.lb = gam.all.contact$fitted.values - se.all.contact$se.fit)

p_scatter_gam_log <- ggplot(cases_corrected, aes(x=Dog.density.incursions, y=Dogs.bitten)) + 
  geom_point(size=1, shape = 20, colour=sc.colours$grey, alpha = 0.5) +
  geom_line(data= all.df, aes(x=dogCount, y=c.est), colour = sc.colours$bright.red) + 
  geom_line(data= all.df, aes(x=dogCount, y=c.lb), colour = sc.colours$grey, size = 0.3) + 
  geom_line(data= all.df, aes(x=dogCount, y=c.ub), colour = sc.colours$grey, size = 0.3) +
  scale_x_continuous(bquote(''), trans='log10') +
  scale_y_continuous("Dogs bitten", limits = c(0,10)) +
  ss; 
p_scatter_gam_log

p_scatter_gam <- ggplot(cases_corrected, aes(x=Dog.density.incursions, y=Dogs.bitten)) + 
  geom_point(size=1, shape = 20, colour=sc.colours$grey, alpha = 0.8) +
  geom_line(data= all.df, aes(x=dogCount, y=c.est), colour = sc.colours$bright.red) + 
  geom_line(data= all.df, aes(x=dogCount, y=c.lb), colour = sc.colours$grey, size = 0.2) + 
  geom_line(data= all.df, aes(x=dogCount, y=c.ub), colour = sc.colours$grey, size = 0.2) +
  scale_x_continuous(bquote('Dog density ('*km^-2*')'), limits = c(0,400)) + 
  scale_y_continuous("Dogs bitten") +
  ss +  theme(legend.position = "none", axis.text = element_text(size = 15));
p_scatter_gam

p_scatter <- ggplot(cases_corrected) +
  geom_point(aes(x=Dog.density.incursions, y=Dogs.bitten), size=1, shape=20, colour=sc.colours$grey, alpha=0.8) +
  geom_line(data= all.df, aes(x=dogCount, y=c.est), colour = sc.colours$bright.red) + 
  geom_line(data= all.df, aes(x=dogCount, y=c.lb), colour = sc.colours$grey, size = 0.2) + 
  geom_line(data= all.df, aes(x=dogCount, y=c.ub), colour = sc.colours$grey, size = 0.2) +
  scale_x_continuous(bquote('Dog density ('*km^-2*')'), limits = c(0,400)) + 
  scale_y_continuous("Dogs bitten") +
  ss; 
p_scatter

p_scatter.with.label <-
  ggdraw() +
  draw_plot(p_scatter, x = 0.04, y = 0, width = .89, height = 1) +
#  draw_plot(p_scatter_gam, x = 0.45, y = .45, width = .5, height = .5) +
  draw_plot_label(label="D", x=0.05, y=1, size = 24)
p_scatter.with.label 

# ----------------- Fig 1D DATA: Relative density across landscape and at case locations ------------------
# Mean population density, computed at 1x1km scale
densities.midpoint <- grd.1000$'2557'
non.zero.densities.midpoint <- densities.midpoint[densities.midpoint > 0]
mean.non.zero.density.midpoint <- mean(non.zero.densities.midpoint)
median.non.zero.density.midpoint <- median(non.zero.densities.midpoint)
all.gridcell.densities <- as.matrix(grd.1000@data[,3:ncol(grd.1000@data)])
mean.density.period <- mean(all.gridcell.densities)
median.density.period <- median(all.gridcell.densities)
mean.non.zero.density.period <- mean(all.gridcell.densities[all.gridcell.densities>0])
uq.density.midpoint <- quantile(non.zero.densities.midpoint, 0.95) # 95% of cells had < 65 dogs
# And maximum poopulation density
max(densities.midpoint)

# Group data for plotting
group.by = 5
my.breaks <- seq(0, max(grd.1000.sf$End)+group.by, by=group.by)

# Allocate populations to interval by dog density
populations <- as.data.frame(grd.1000.sf) %>% select(popID, Mid, End)
populations$Mid.interval <- findInterval(x = populations$Mid, vec = my.breaks)
populations$Mid.dog.density.grouped <- my.breaks[populations$Mid.interval]
populations <- filter(populations, Mid > 0)
summary(populations)
shift_population <- populations %>%
  group_by(Mid.dog.density.grouped) %>%
  summarise(Cells = (length(popID)/nrow(populations))/group.by)
summary(shift_population)

# Allocate cases to interval by dog density
cases_corrected$Dog.density.incursions.interval <- findInterval(x = cases_corrected$Dog.density.incursions,
                                                               vec = my.breaks)
cases_corrected$Dog.density.incursions.grouped <- my.breaks[cases_corrected$Dog.density.incursions.interval]
head(data.frame(raw = cases_corrected$Dog.density.incursions, 
           grouped = cases_corrected$Dog.density.incursions.grouped), 20) # grouped to lower bound
summary(cases_corrected$Dog.density.incursions.grouped)
shift_cases <- cases_corrected %>% 
  group_by(Dog.density.incursions.grouped) %>%
  summarise(Cases = (length(Chain.ID)/nrow(cases_corrected)/group.by))

# Join two cases and population density and prepare for ggplot
density_info <- data.frame(lb = my.breaks)
shift_densities <- left_join(x=density_info, y=shift_population, by=c("lb" = "Mid.dog.density.grouped"))
shift_densities <- left_join(shift_densities, y=shift_cases, by = c("lb" = "Dog.density.incursions.grouped"))
head(shift_densities, 20)
shift_densities_long <- gather(shift_densities, "Type", "Proportion", -lb)
shift_densities_long$Type <- factor(shift_densities_long$Type, levels = c("Cells","Cases"), labels = c("Dogs", "Cases"), ordered = T)
shift_densities_long$Proportion <- ifelse(is.na(shift_densities_long$Proportion), 0, shift_densities_long$Proportion)
head(shift_densities_long, 20)

# Main panel
p_shift <- ggplot() + 
  geom_line(data = shift_densities_long, aes(x=lb, y=Proportion, colour = Type)) +
  #scale_x_continuous(bquote('Dog density ('*km^-2*')'), limits = c(0,400))  +
  scale_x_continuous("", limits = c(0,400))  +
  scale_colour_manual("", values = c(sc.colours$grey, sc.colours$bright.red)) +
  geom_point(aes(x = mean.non.zero.density.midpoint, y=0), colour=sc.colours$grey, size = 1.5, shape = 15) +
  geom_point(aes(x = mean(cases_corrected$Dog.density.incursions), y=0), colour=sc.colours$bright.red, size = 1.5, shape = 15) +
  ss  + theme(legend.position = c(0.7,0.98)); 
p_shift

# Inset figure on log scale
p_shift_logged <- ggplot() + 
  geom_line(data = shift_densities_long, aes(x=lb, y=Proportion, colour = Type)) +
  scale_colour_manual("", values = c(sc.colours$grey, sc.colours$bright.red)) +
  geom_point(aes(x = mean.non.zero.density.midpoint, y=0), colour=sc.colours$grey, size = 1.5, shape = 15) +
  geom_point(aes(x = mean(cases_corrected$Dog.density.incursions), y=0), colour=sc.colours$bright.red, size = 1.5, shape = 15) +
  #ss + theme(legend.position = "none", axis.text = element_text(size = 15))  +
  ss + theme(legend.position = c(0.75,0.95), axis.text = element_text(size = 15), legend.text = element_text(size = 15))  +  
  scale_y_continuous("Proportion")  +
  scale_x_log10(""); 
p_shift_logged

p_shift.with.inset <-
  ggdraw() +
  draw_plot(p_shift, x = 0, y = 0.05, width = .93, height = .75) +
  draw_plot(p_shift_logged, x = 0.35, y = .25, width = .55, height = .4) +
  draw_plot_label(label="C", x=0.05, y=0.75, size = 24)
p_shift.with.inset

# p_shift_logged + p_scatter.with.label 
p_shift_scatter <-
  ggdraw() +
  draw_plot(p_scatter, x = 0, y = 0, width = 1, height = 0.95) +
  draw_plot(p_shift_logged, x = 0.35, y = .35, width = .65, height = .6) +
  draw_plot_label(label="C", x=0.05, y=1, size = 24)
p_shift_scatter

###################################################################################
#                       Incubation and infectious periods
###################################################################################
# Read in data
inc.inf <- read.csv("output/d_inc_inf_with_zeros.csv") # from Make_Incubation_Infectious_Period

# Mean incubation period
mean(inc.inf$Generation.interval - inc.inf$Infectious.period) # 26.2527
quantile(inc.inf$Generation.interval - inc.inf$Infectious.period, probs = 0.96) # ~ 4% exceed 88 days
mean(inc.inf$Infectious.period) # 1.552996
mean(inc.inf$Generation.interval - inc.inf$Infectious.period) / mean(inc.inf$Infectious.period)

# Incubation and infectious periods
inc <- ggplot(inc.inf, aes(Generation.interval-Infectious.period)) + 
  geom_histogram(fill = sc.colours$grey, alpha = 0.5, breaks = seq(0,300,by=5)) +
  scale_x_continuous("Incubation") +scale_y_continuous("Frequency", limits = c(0,170)) +
  ss; inc
ggsave(plot = inc, path = ff_figs, filename = "incubation_distribution.pdf", width = sc.1col.w, height = 0.7*sc.1col.w, units = "cm")

inf <- ggplot(inc.inf, aes(Infectious.period)) + 
  geom_histogram(fill = sc.colours$grey, alpha = 0.5, binwidth = 1) +
  scale_x_continuous("Infectious (days)", limits = c(0,12), breaks = c(0,5,10), labels = c(0,5,10)) + 
  scale_y_continuous("Frequency", limits = c(0,170)) +
  ss + theme(axis.text = element_text(size = 15)); 
inf 


ggsave(plot = inf, path = ff_figs, filename = "infectious_period_distribution.pdf", width = sc.1col.w, height = 0.7*sc.1col.w, units = "cm")

# Serial interval
rabid_carnivores <- readRDS("output/rabid_carnivores_with_imputed_infectious_periods.rda")
SI_pars <- read.csv("output/SI_params.csv",header=T)
sim.SIs <- rlnorm(n = 1000000, meanlog = SI_pars$SI_ml, sdlog = SI_pars$SI_sdlog)
SIs.max <- max(rabid_carnivores$Serial.Interval.Clean, sim.SIs,na.rm=T)
SI.breaks.by <- 5
SI.breaks <- seq(0,SIs.max+SI.breaks.by,by=SI.breaks.by)
hist.SIs <- hist(rabid_carnivores$Serial.Interval.Clean, breaks=SI.breaks, plot = F)
hist.sim.SIs <- hist(sim.SIs, breaks=SI.breaks, plot = F)

SIs.df <- data.frame(mids = hist.SIs$mids, 
                     density.data = hist.SIs$density, 
                     density.sim = hist.sim.SIs$density)

SI <- ggplot(subset(SIs.df, mids < (max(rabid_carnivores$Serial.Interval.Clean,na.rm=T)+SI.breaks.by)), aes(x=mids, y=density.data)) +
  geom_bar(stat="identity", fill = sc.colours$grey, alpha = 0.5) +
  geom_line(aes(x=mids, y=density.sim), colour = sc.colours$bright.red) +
  scale_x_continuous("Serial interval (days)", breaks = seq(0,(max(rabid_carnivores$Serial.Interval.Clean,na.rm=T)+SI.breaks.by),by=100)) +
  scale_y_continuous("Probability density", limits = c(0,0.04), expand = c(0,0.001)) +
  ss; SI

ggsave(plot = SI, path = ff_figs, filename = "serial_interval.pdf", width = sc.1col.w, height = 0.7*sc.1col.w, units = "cm")

# Combine into panel
p_SI <-
  ggdraw() +
  draw_plot(SI, x = 0, y = 0.02, width = 1, height = 0.9) +
  draw_plot(inf, x = 0.35, y = .35, width = .5, height = .6) +
  draw_plot_label(label="E", x=0.05, y=1, size = 24)
p_SI

###################################################################################
#                       Step lengths & Distance kernel
###################################################################################
# Read in data - step lengths distribution and fitted Weibull distribution
steps.m <- read.csv("output/steps_m.csv") 
steps.m.distribution <- read.csv("output/steps.distribution.csv")  
steps.weibull.shape <- steps.m.distribution$shape 
steps.weibull.scale <- steps.m.distribution$scale 

# Simulate steps using the same parameters
sim.steps <- rweibull(n = 1000000, shape = steps.weibull.shape, scale = steps.weibull.scale)
steps.max <- max(steps.m$step.length, sim.steps)
step.breaks.by <- 250
step.breaks <- seq(0,steps.max+step.breaks.by,by=step.breaks.by)

# Make histograms
hist.steps.m <- hist(steps.m$step.length, breaks=step.breaks, plot = F)
hist.sim.steps <- hist(sim.steps, breaks=step.breaks, plot = F)
steps.df <- data.frame(mids = hist.steps.m$mids, 
                       density.data = hist.steps.m$density*step.breaks.by, 
                       density.sim = hist.sim.steps$density*step.breaks.by)

# Plot the distributions
steps <- ggplot(subset(steps.df, mids < 16000), aes(x=mids, y=density.data)) +
  geom_bar(stat="identity", fill = sc.colours$grey, alpha = 0.5) +
  geom_line(aes(x=mids, y=density.sim), colour = sc.colours$bright.red) +
  scale_x_continuous("Distance (km)", breaks = seq(0,16000,by=2000), labels = seq(0,16000,by=2000)/1000 ) +
  scale_y_sqrt("Probability density", limits = c(0,.9)) +
  ss; steps
ggsave(plot = steps, path = ff_figs, filename = "Steplengths.pdf", width = sc.1col.w, height = 0.7*sc.1col.w, units = "cm")

###################################################################################
distances <- read.csv("output/dist_contact_all.csv", header=T)
DK_pars <- read.csv("output/DK_params.csv", header=T)
distances.weibull.shape <- DK_pars$par1est[which(DK_pars$kernel_type=="DK" & DK_pars$dist=="weibull")] 
distances.weibull.scale <- DK_pars$par2est[which(DK_pars$kernel_type=="DK" & DK_pars$dist=="weibull")]

# Simulate kernel using the same parameters
sim.distances <- rweibull(n = 1000000, shape = distances.weibull.shape, scale = distances.weibull.scale)
distances.max <- max(distances$distance, sim.distances)
distance.breaks.by <- 250
distance.breaks <- seq(0,distances.max+distance.breaks.by,by=distance.breaks.by)

# Make histograms
hist.distances <- hist(distances$distance, breaks=distance.breaks, plot = F)
hist.sim.distances <- hist(sim.distances, breaks=distance.breaks, plot = F)
distances.df <- data.frame(mids = hist.distances$mids, 
                           density.data = hist.distances$density, 
                           density.sim = hist.sim.distances$density)

# Plot the distributions
DK <- ggplot(subset(distances.df, mids < 17000), aes(x=mids, y=density.data)) +
  geom_bar(stat="identity", fill = sc.colours$grey, alpha = 0.5) +
  geom_line(aes(x=mids, y=density.sim), colour = sc.colours$bright.red) +
  scale_x_continuous("", breaks = seq(0,17000,by=4000), labels = seq(0,17000,by=4000)/1000 ) +
  scale_y_sqrt("") + 
  ss + theme(axis.text = element_text(size = 15)); DK
ggsave(plot = DK, path = ff_figs, filename = "distance_kernel.pdf", width = sc.1col.w, height = 0.7*sc.1col.w, units = "cm")

# Combine into panel
p_move <-
  ggdraw() +
  draw_plot(steps, x = 0.04, y = 0, width = 0.95, height = 0.95) +
  draw_plot(DK, x = 0.45, y = .3, width = .5, height = .6) +
  draw_plot_label(label="D", x=0.05, y=1, size = 24)
p_move

###################################################################################
#                       Distribution of carnivores bitten
###################################################################################
# Read in data
case_list <- readRDS(file = "output/clean_bite_data_no_densities_deid.rda")

sim_bites <- readRDS("java_output/nBitten_distribution.rds")
sim_bite_density <- readRDS("java_output/densityBitten_distribution.rds")
sim_bites_range <- apply(sim_bites, 2, quantile, c(0.05, 0.5, 0.95))
sim_bites_dens_range <- apply(sim_bite_density, 2, quantile, c(0.05, 0.5, 0.95))
simBites <- data.frame(bitten = 0:80, 
                       lci_bitten = sim_bites_range[1, 1:81], 
                       uci_bitten = sim_bites_range[3, 1:81], 
                       med_bitten = sim_bites_range[2, 1:81],
                       lci_bite_dens = sim_bites_dens_range[1, 1:81], 
                       uci_bite_dens = sim_bites_dens_range[3, 1:81],
                       med_bite_dens = sim_bites_dens_range[2, 1:81])
head(simBites)

# Distribution of carnivores bitten
# data
biters <- case_list %>% 
  filter(Species == "Domestic dog" & Rabid == "TRUE") %>% # Essential that only rabid domestic dogs chosen (or density incorrect!)
  filter(Symptoms.started >= "2002-01-01" & Symptoms.started <= "2015-12-31")
NB_params_data <- fitdistr(biters$Dogs.bitten,"negative binomial", method = "SANN")$estimate
# sims
sim_bites_expand <- rep(0:max(simBites$bitten), simBites$med_bitten)
NB_params_sims <- fitdistr(sim_bites_expand,"negative binomial", method = "SANN")$estimate

dogsBitten <- ggplot(aes(Dogs.bitten, ..density..)) + 
  geom_histogram(fill = sc.colours$grey, alpha = 0.5, binwidth = 1) +
  scale_x_continuous("Dogs bitten") + 
  scale_y_continuous("Density") +
  geom_point(data = simBites, aes(x = bitten, y = med_bite_dens), size=0.1, colour = "red", alpha = 0.5) +
  annotate("segment", x = simBites$bitten, xend = simBites$bitten, 
           y = simBites$lci_bite_dens, yend = simBites$uci_bite_dens,
           colour = "red", alpha = 0.5) + ss; 
dogsBitten
ggsave(plot = dogsBitten, path = ff_figs, filename = "carnivores_bitten_distribution.pdf", width = sc.1col.w, height = 0.7*sc.1col.w, units = "cm")

NB_params_data
NB_params_sims
check_data_NB <- rnbinom(n=1000,  size = NB_params_data["size"], mu = NB_params_data["mu"])
hist(check_data_NB, breaks = -1:30)
mean(check_data_NB)
check_sims_NB <- rnbinom(n=100,  size = NB_params_sims["size"], mu = NB_params_sims["mu"])
hist(check_sims_NB, breaks = -1:30)
mean(check_sims_NB)

###################################################################################


# And with sqrt scale
dogsBittenSqrt <- case_list %>% 
  filter(Species == "Domestic dog" & Rabid == "TRUE") %>% 
  filter(Symptoms.started >= "2002-01-01" & Symptoms.started <= "2015-12-31") %>% 
  ggplot(aes(Dogs.bitten, ..density..)) + 
  geom_histogram(fill = sc.colours$grey, alpha = 0.5, binwidth = 1) +
  scale_y_sqrt("Probability density",  limits = c(0,.9)) +   
  scale_x_continuous("Dogs bitten") + 
  geom_point(data = simBites, aes(x = bitten, y = med_bite_dens), size=0.1, colour = "red", alpha = 0.5) +
  annotate("segment", x = simBites$bitten, xend = simBites$bitten, 
           y = simBites$lci_bite_dens, yend = simBites$uci_bite_dens,
           colour = "red", alpha = 0.5) + ss
  # ss + theme(axis.text = element_text(size = 15), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)); 
dogsBittenSqrt
ggsave(plot = dogsBittenSqrt, path = ff_figs, filename = "carnivores_bitten_distribution_sqrt_y.pdf", width = sc.1col.w, height = 0.7*sc.1col.w, units = "cm")



#Combine as panel
p_bitten <-
  ggdraw() +
  draw_plot(dogsBittenSqrt, x = 0.04, y = 0, width = 0.95, height = 0.95) +
  # draw_plot(dogsBitten, x = 0.4, y = .3, width = .5, height = .6) +
  draw_plot_label(label="F", x=0.05, y=1, size = 24)
p_bitten

###################################################################################
#                Combine to make Figure 1
###################################################################################
ggarrange(p_serengeti.with.inset, p_shift_scatter, p_SI,
          p_ts.with.label, p_move, p_bitten, 
          heights = c(2, 1.3), widths = c(2,1,1),
          ncol = 3, nrow = 2)
ggsave("figs/fig_1_combined.pdf", height=13, width=19)


# ----------------------- Computations for the captions ---------------------
# Work out some numbers for the caption of these figures - number of cases in the data, and number of carnivore cases
(total.dog.cases <- nrow(filter(cases_corrected, Animal == "Dogs")))
(total.carnivore.cases <- nrow(cases_corrected))
(cases.by.species <- table(cases_corrected$Species))
(prolific.biters <- sum(cases_corrected$Dogs.bitten>=60))
(proportion.prolific.biters <- 100*sum(cases_corrected$Dogs.bitten>=60)/nrow(cases_corrected))

# # Total number of households
# test <- read.csv("data/SDcompiled.csv"); head(test)
# dim(test)
# # Total number of dogs
# sum(grd.1000$`0`)
# sum(densities.midpoint)
# sum(grd.1000$`5082`)
# sum(test$dogs + test$pups)

# % of dog population ever concurrently infected ... can do this by normalising the d.cases.month by using the densities in grd.1000
tot.pops <- data.frame(totPop = colSums(grd.1000@data[,3:ncol(grd.1000@data)])) # populations by month
tot.pops$Day.numeric <- rownames(tot.pops)
tot.pops$Month <- start.date + as.numeric(tot.pops$Day.numeric)
all_cases_by_month <- cases_corrected %>% 
  group_by(Month.symptoms.started) %>%
  summarise(cases = length(Chain.ID))
head(all_cases_by_month)
all_cases_by_month <- left_join(x = tot.pops, y = all_cases_by_month, by = c("Month"="Month.symptoms.started"))
head(all_cases_by_month)
all_cases_by_month$Incidence <- all_cases_by_month$cases/all_cases_by_month$totPop
max(all_cases_by_month$Incidence, na.rm = T) * 100 # Max monthly incidence, in %
max(all_cases_by_month$Incidence, na.rm = T) * 100 * 12 # Approximate yearly equivalent, in % (i.e. % incidence per annum)
hist(all_cases_by_month$Incidence, breaks = seq(0,0.0015, 0.0001))

# Mean and maximum population density at midpoint of the period, at 1km2
max(densities.midpoint) # 329.6768
mean.non.zero.density.midpoint
mean(cases_corrected$Dog.density.incursions)
mean(cases_corrected$Dog.density, na.rm = T)

# t-test on difference between dog population density and cases
t.test(x = log(cases_corrected$Dog.density.incursions), y = log(populations$Mid))


