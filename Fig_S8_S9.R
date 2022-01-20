# Figures S9 (posterior distributions) and S10 (reliability boxplots)
rm(list=ls()); options(stringsAsFactors = F)
library(tidyverse)
library(ggplot2)
library(cowplot)
source("Style_Sheet.R")
ff_figs <- "figs/"

all.accepted <- readRDS(file="java_output/Round2_Compiled/All_Round2_Accepted.rda")
pr.acc <- readRDS(file="java_output/Round2_Compiled/All_Round2_Reliability.rda")

##############################################################################
#                                    Figure S9                               #
##############################################################################

# (a) ThMean and ThShape 
# (b) ThMean and TdMean

# Parameter prior ranges, to adjust ranges on plots
# ThShape <- runif(1, 0.5, 10)
# ThMean <- runif(1, 0.001, 3) 
# Td <- runif(1, 0, 25) # i.e. TdMean

# Proportions re-simulated accepted runs (alpha), 
propAcc_threshold <- 0.5

# Make a data.frame of posterior values with repetitions for geom_density_2d
posterior <- pr.acc %>% filter(propAcc > propAcc_threshold) %>%
  slice(rep(1:n(), times = propAcc*100))

# (a) By parameter value (ThShape, ThMean)
p.acc.ThMean.ThShape <- ggplot(pr.acc, aes(x=ThMean, y=ThShape)) +
  geom_point(data=subset(pr.acc, propAcc > 0), 
             aes(alpha=propAcc, colour = (propAcc > propAcc_threshold)), shape=16, size=1) +
  geom_density_2d(data = posterior, bins = 6,
                  aes(x = ThMean, y = ThShape), colour = sc.colours$grey, size = 0.2, alpha = 0.8) + 
  geom_point(data = posterior, aes(x = ThMean, y = ThShape), colour = sc.colours$bright.red, size = 0.7, alpha = 1) + 
  #guides(colour = guide_colourbar(title="Density dependence", title.position = "top", title.hjust = 0),
  #       alpha = guide_legend(title="Proportion accepted", title.position = "top", label.position = "bottom", order=1)) +
  scale_alpha(guide = "none") + 
  scale_colour_manual(guide = "none", values = c(sc.colours$grey, sc.colours$bright.red)) + 
  scale_x_continuous(expression(italic(T['h,mean'])), limits=c(0,3)) + 
  scale_y_continuous(expression(italic(T['h,shape'])), limits=c(0.5,10)) +
  facet_grid(~Res.label) + 
  ss + 
  theme(
    legend.position="bottom",
  )
p.acc.ThMean.ThShape

# (b) By parameter value (Th-Td plot), showing density dependence in colour
p.acc.ThMean.TdMean <- ggplot(pr.acc, aes(x=ThMean, y=TdMean)) +
  geom_point(data=subset(pr.acc, propAcc > 0), 
             aes(alpha=propAcc, colour = (propAcc > propAcc_threshold)), shape=16, size=1) +
  geom_density_2d(data = posterior, bins = 6,
                  aes(x = ThMean, y = TdMean), colour = sc.colours$grey, size = 0.2, alpha = 0.8) + 
  geom_point(data = posterior, aes(x = ThMean, y = TdMean), colour = sc.colours$bright.red, size = 0.7, alpha = 1) + 
  guides(alpha = guide_legend(title="Proportion accepted", label.position = "bottom", order=1)) +
  scale_colour_manual(guide = "none", values = c(sc.colours$grey, sc.colours$bright.red)) + 
  scale_x_continuous(expression(italic(T['h,mean'])), limits=c(0,3)) + 
  scale_y_continuous(expression(italic(1/r['s,d'])), limits=c(0,25), labels = scales::number_format(accuracy = 0.1)) +
  facet_grid(~Res.label) + 
  ss + 
  theme(legend.position="bottom",     
        strip.background = element_blank(),
        strip.text.x = element_blank()
  ); 
p.acc.ThMean.TdMean

fig_S8 <- plot_grid(p.acc.ThMean.ThShape, p.acc.ThMean.TdMean, 
          ncol = 1, rel_heights = c(1, 1.2), 
          labels = c('A', 'B'), label_size = 18)
fig_S8
ggsave(plot = fig_S8, width = 30, height = 27, units = "cm", path = ff_figs, filename = "fig_S8.pdf")
ggsave(plot = fig_S8, width = 40, height = 25, units = "cm", path = ff_figs, filename = "fig_S8.png")

# For our reference, shape and scale of handling time
# (a) By parameter value (ThScale, ThShape)
p.acc.ThScale.ThShape <- ggplot(pr.acc, aes(x=ThScale, y=ThShape)) +
  geom_point(data=subset(pr.acc, propAcc > 0), 
             aes(alpha=propAcc, colour = (propAcc > propAcc_threshold)), shape=16, size=1) +
  geom_density_2d(data = posterior, bins = 6,
                  aes(x = ThScale, y = ThShape), colour = sc.colours$black, size = 0.5, alpha = 0.6) + 
  #guides(colour = guide_colourbar(title="Density dependence", title.position = "top", title.hjust = 0),
  #       alpha = guide_legend(title="Proportion accepted", title.position = "top", label.position = "bottom", order=1)) +
  scale_alpha(guide = "none") + 
  scale_colour_manual(guide = "none", values = c(sc.colours$grey, sc.colours$bright.red)) + 
  scale_x_continuous(expression(italic(T['h,scale'])), limits=c(0,3)) + 
  scale_y_continuous(expression(italic(T['h,shape'])), limits=c(0.5,10)) +
  facet_grid(~Res.label) + 
  ss + 
  theme(
    legend.position="bottom",
  )
p.acc.ThScale.ThShape

##############################################################################
#                                 Figure S9                                  #
##############################################################################
# Boxplot: Proportion of re-runs accepted across parameter values
p.acc.boxplot <- ggplot(pr.acc, aes(y=propAcc, x=Res.label, group=Res.label)) + 
  geom_boxplot(fill=sc.colours$bg.grey) + #geom_point(alpha=0.2) +
  scale_x_discrete("Spatial scale of mixing") + 
  scale_y_continuous("Proportion accepted") +
  geom_hline(data = NULL, aes(yintercept = 0.5), colour = sc.colours$bright.red) +
  ss; p.acc.boxplot
ggsave(plot = p.acc.boxplot, width = 21, height = 17, units = "cm", path = ff_figs, filename = "Fig_S9_propAcc.pdf")
ggsave(plot = p.acc.boxplot, width = 21, height = 17, units = "cm", path = ff_figs, filename = "Fig_S9_propAcc.png")

##############################################################################
#                    Other figures not shown in manuscript                   #
##############################################################################

# # Not shown in manuscript, but shows a negative relationship between density dependence and reliability
# # Plot the proportion accepted against density dependence at each spatial scale
# p <- ggplot(pr.acc, aes(y=propAcc, x=densityDependence)) + 
#   geom_smooth(method = "lm", colour="grey80") +
#   geom_point(aes(colour=medR0<1.10)) + 
#   theme_light() +
#   facet_wrap(~Res.label); p
# ggsave(plot = p, width = 21, height = 17, units = "cm", path = ff_figs, filename = "Round2_Incursions_Scatter_densityDependence_R0_propAcc.pdf")

