###################################################################################
#                             Figure ABC Criteria
###################################################################################
rm(list=ls())
options(stringsAsFactors=F) # otherwise bad things happen to dates

library(ggplot2)
library(scales)
library(rgdal)
library(RColorBrewer)
library(mapproj)
library(data.table)
library(plyr)
library(lubridate)
library(ggpubr)

source("Style_Sheet.R") 

ff_abc_criteria = "output/abc_criteria/"
ff_figs <- "figs/" 

# Figure - Cases per month bounds
c.cases.per.month <- readRDS(paste0(ff_abc_criteria, "c_cases_per_month.rda"))
c.cases.per.month.bounds <- readRDS(paste0(ff_abc_criteria, "c_cases_per_month_bounds.rda")) # for time series (data), also includes cases.per.month
c.cases.per.month.hist.breaks <- readRDS(paste0(ff_abc_criteria, "c_cases_per_month_hist_breaks.rda"))
hist.data <- hist(x = c.cases.per.month$cases, breaks = c.cases.per.month.hist.breaks, plot = F)
c.cases.per.month.bounds$data.counts <- hist.data$counts

p1 <- ggplot(c.cases.per.month.bounds, aes(x=cases, y=data.counts)) + 
  geom_ribbon(aes(x=cases, ymin=lb, ymax=ub), fill = sc.colours$bg.grey) +
  geom_line(colour = sc.colours$bright.red, lwd = 0.5) + 
  geom_point(aes(x=cases, y=data.counts), colour = sc.colours$bright.red, shape = 16) +
  geom_line(aes(x=cases, y=lb), lty = 2, lwd = 0.3, colour = sc.colours$grey) +
  geom_line(aes(x=cases, y=ub), lty = 2, lwd = 0.3, colour = sc.colours$grey) + 
  scale_x_continuous("Cases per month", limits=c(0, 250)) + 
  scale_y_continuous("Number of months") + ss; 
p1
ggsave(plot = p1, path = ff_figs, filename = "abc_cases_per_month.pdf", width = sc.1col.w, height = sc.1col.w, units = "cm")
ggsave(plot = p1, path = ff_figs, filename = "abc_cases_per_month.png", width = sc.1col.w, height = sc.1col.w, units = "cm")

# Figure - shift bounds
shift.bounds <- readRDS(paste0(ff_abc_criteria, "shift_bounds.rda"))
dog.densities.hist <- readRDS(paste0(ff_abc_criteria, "dog_densities_hist.rda"))
shift.bounds$density <- dog.densities.hist$density
p2 <- ggplot(shift.bounds, aes(x=cases, y=density)) + 
  geom_ribbon(aes(x=cases, ymin=lb, ymax=ub), fill = sc.colours$bg.grey) +
  geom_line(colour = sc.colours$bright.red, lwd = 0.5) + 
  geom_line(aes(x=cases, y=lb), lty = 2, lwd = 0.3, colour = sc.colours$grey) +
  geom_line(aes(x=cases, y=ub), lty = 2, lwd = 0.3, colour = sc.colours$grey) +
  scale_x_continuous("Case location dog density") + 
  scale_y_continuous("Proportion of cases") + ss; p2
ggsave(plot = p2, path = ff_figs, filename = "abc_shift.pdf", width = sc.1col.w, height = sc.1col.w, units = "cm")
ggsave(plot = p2, path = ff_figs, filename = "abc_shift.png", width = sc.1col.w, height = sc.1col.w, units = "cm")

# COMBINE TO A 2PANEL FIGURE
pABC <- ggarrange(p1, p2, ncol = 2, nrow = 1, labels = c("A", "B"), font.label = list(size = 26))
pABC
ggsave(plot = pABC, path = ff_figs, filename = "fig_S7_ABC_criteria.pdf", width = sc.2col.w, height = 0.6*sc.2col.w, units = "cm")
ggsave(plot = pABC, path = ff_figs, filename = "fig_S7_ABC_criteria.png", width = sc.2col.w, height = 0.6*sc.2col.w, units = "cm")
