# Estimated detection ----

# pkgs & scripts ----
library(treerabid)
library(igraph)
library(data.table)
library(dplyr)
library(sf)
library(tidyr)
library(lubridate)
library(magrittr)
library(scales)
library(ggplot2)
library(patchwork)
library(cowplot)
library(ggbeeswarm)
library(ggridges)
library(here)
select <- dplyr::select

# data ----
recover_detect <- fread("output/trees/recovered_detect_ests.csv")
est_detect <- fread("output/trees/detection_ests.csv")

# expectations ----
exps <- lapply(seq(0.01, 0.99, by = 0.05), 
               function(x) {
                 data.table(prop = dgeom(seq_len(11) - 1, x), 
                            pi = x, 
                            kappa = seq_len(11))
                })
exps <- rbindlist(exps)
               
exp_kappas <-
  ggplot(exps) +
  geom_line(aes(x = kappa - 1, y = prop, color = pi, group = pi)) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) + 
  scale_color_distiller(palette = "PuBu", direction = 1, 
                        name = "Detection \n prob") +
  labs(x = "Number of unobserved cases \n between linked case pairs", 
       y = "Expected propotion given \n detection prob") +
  theme_minimal_hgrid(font_size = 12)

recov_long <- melt(recover_detect, id.vars = c("true", "sim", "nobs", "tree_type"))
recov_summ <- 
  recov_long[, .(mean = mean(value), max = max(value), min = min(value)), 
             by = c("true", "sim", "nobs", "tree_type", "variable")]

recov <-
  ggplot(recov_summ) +
  geom_pointrange(aes(x = true, y = mean, ymin = min, ymax = max, 
                      shape = tree_type, fill = variable, 
                      color = ifelse(tree_type == "mod", "black", "NA"), 
                      alpha = ifelse(tree_type == "mod", 1, 0.3))) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_abline(slope = 1, intercept = -0.1, linetype = 2, color = "grey50") +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_shape_manual(values = c(22, 21), name = "Type of simulation", 
                     labels  = c("Expectation from A", "Model simulation")) +
  scale_fill_brewer(palette = "Accent", name = "",
                    labels = c("Ranked", "Naive")) +
  labs(x = "True detection prob", y = "Estimated detection prob") +
  theme_minimal_hgrid(font_size = 12) +
  guides(fill = guide_legend(override.aes = list(shape = 21)), 
         shape = guide_legend(override.aes= list(fill = "black", shape = c(22, 21), 
                                                 alpha = c(0.3, 1))))

est_summ <-
  est_detect[, .(mean_detect = mean(detection)), by = c("cutoff", "tree_type", "sim")]

ests <-
  ggplot(data = est_summ) +
  geom_density(aes(x = mean_detect, 
                   fill = factor(cutoff), group = factor(cutoff)), 
               alpha = 0.75) +
  scale_fill_brewer(palette = "Dark2", name = "Pruning \nthreshold", 
                    labels = c("95%", "97.5%", "Unpruned")) +
  theme_minimal_hgrid(font_size = 12) +
  labs(x = "Estimated detection probability", y = "Density")

# probability of detecting at least one case given a chain size of x
chain_dets <- expand_grid(est_summ, 
                          size = seq_len(10))
chain_dets <- mutate(chain_dets, prob = 1 - (1 - mean_detect)^size)

chains <-
  ggplot(data = chain_dets) +
  geom_quasirandom(aes(x = factor(size), y = prob, color = factor(cutoff))) +
  scale_color_brewer(palette = "Dark2", name = "Pruning \nthreshold", 
                    labels = c("95%", "97.5%", "Unpruned"), guide = "none") +
  theme_minimal_hgrid(font_size = 12) +
  labs(y = "Probability of detecting \nat least one case in clade", x = "Clade size")


sfig_detection <- 
  (exp_kappas | recov)  / 
  ((ests | chains) + plot_layout(guides = "collect")) + 
  plot_annotation(tag_levels = "A") 
ggsave("figs/fig_S12_detection.jpeg", sfig_detection, height = 8, width = 8)

