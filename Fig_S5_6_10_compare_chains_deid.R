# Compare chains by tree type & against simulated ----
rm(list = ls())

# pkgs & scripts ----
library(R.utils)
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
library(parallel)
select <- dplyr::select
source("R/sim_comps.R")

# data ----
trees_best <- fread("output/trees/trees_sampled_best.gz") # Needs to be created by running the tree code
trees_consensus <- fread("output/trees/consensus_links_best.csv")
# case_dt <- data.table(readRDS("output/clean_bite_data_canonical.rda")) 
# Deidentified data
case_dt <- data.table(readRDS("output/clean_bite_data_canonical_deid.rda")) 
case_dt$UTM.Easting <- biting_animals$UTM.Easting.jitter
case_dt$UTM.Northing <- biting_animals$UTM.Northing.jitter

# cutoff colors & labs 
cut_cols <- brewer_pal(palette = "Dark2")(8)[5:8]
cut_cols[4] <- alpha(cut_cols[4], 0.05)
names(cut_cols) <- c("Consensus", "Majority", "MCC", "Random sample")
cut_labs <-  c("Consensus", "Majority", "MCC", "Random sample \n (N = 100 trees)")
names(cut_labs) <- names(cut_cols)

# chain size, length, persistence ----
trees_consensus <- 
  case_dt[, 
          .(id_case = ID, date_symptoms = Symptoms.started, 
            x_coord = UTM.Easting, y_coord = UTM.Northing)][trees_consensus, on = "id_case"]

chain_stats <-
  rbind(
    trees_best[, .(size = .N, persistence = as.numeric(max(t) - min(t))), 
             by = c("membership", "sim", "cutoff", "mcc", "majority")], 
    trees_consensus[, .(size = .N, persistence = as.numeric(max(date_symptoms) - min(date_symptoms))), 
                    by = c("membership", 
                           "cutoff")][, c("sim", "mcc", "majority") := .(0, 0, 0)]
  )

chain_stats[, tree_type := fcase(sim == 0, "Consensus", 
                                 majority == 1, "Majority", 
                                 mcc == 1, "MCC", 
                                 majority == 0 & mcc == 0 & sim != 0, 
                                 "Random sample")]
sort(subset(chain_stats, tree_type == "MCC" & cutoff == 0.95)$persistence, decreasing = TRUE)/365
sort(subset(chain_stats, tree_type == "Consensus" & cutoff == 0.95)$persistence, decreasing = TRUE)/365

sizes <- 
  chain_stats[, .N, 
            by = c("sim", "size", 
                   "tree_type", 
                   "cutoff")]
sizes %>% 
  group_by(sim, cutoff, tree_type) %>% 
  mutate(prop = N/sum(N)) %>%
  complete(size = 1:max(size), fill = list(N = 0, prop = 0)) -> sizes

size_dist <-
  ggplot(sizes) +
  geom_line(aes(x = size, y = prop, color = tree_type,   
                group = interaction(sim, cutoff), 
                linetype = factor(cutoff))) +
  scale_color_manual(values = cut_cols, labels = cut_labs) +
  scale_x_continuous(trans = "sqrt", breaks = c(1, 25, 100, 400)) +
  labs(x = "Clade size", 
       y = "Frequency (proportion)", 
       color = "Tree type") + 
  scale_linetype(name = "Pruning \nthreshold", 
                 labels = c("95%", "97.5%")) +
  theme_minimal_hgrid(font_size = 12) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

persist_dist <-
  ggplot(chain_stats) +
  geom_density(aes(x = (persistence + 1) / 7, color = tree_type, 
                   group = interaction(sim, cutoff), 
                   linetype = factor(cutoff))) +
  scale_color_manual(values = cut_cols) +
  scale_x_continuous(trans = "sqrt", breaks = c(1, 100, 400)) +
  labs(x = "Persistence time of clade (weeks)", 
       y = "Frequency (density)", 
       color = "Tree type") +
  scale_linetype(name = "Pruning \nthreshold", 
                 labels = c("95%", "97.5%")) +
  theme_minimal_hgrid(font_size = 12) +
  guides(color = "none", linetype = "none")


# re ests ----
re_stats <-
  rbind(
    trees_best[, .(re = .N), 
               by = c("id_progen", "sim", "cutoff", "mcc", "majority")][!is.na(id_progen)], 
    trees_consensus[, .(re = .N), 
                    by = c("id_progen", 
                           "cutoff")][, c("sim", "mcc", "majority") := .(0, 0, 0)][!is.na(id_progen)]
  )

re_stats[, tree_type := fcase(sim == 0, "Consensus", 
                              majority == 1, "Majority", 
                              mcc == 1, "MCC", 
                              majority == 0 & mcc == 0 & sim != 0, 
                              "Random sample")]

all <- expand_grid(id_case = unique(case_dt$ID), 
            re_stats[, .N, by = c("sim", "cutoff", "tree_type")])

re_stats %>%
  mutate(id_case = id_progen) %>%
  select(-id_progen) %>%
  right_join(select(all, -N)) %>%
  replace_na(list(re = 0)) -> re_stats

re_stats %>%
  group_by(sim, cutoff, tree_type, re) %>%
  summarize(n = n()) %>%
  group_by(sim, cutoff, tree_type) %>%
  mutate(prop = n/sum(n)) -> re_props

re_dist <-
  ggplot(re_props) +
  geom_line(aes(x = re, y = prop, color = tree_type, group = interaction(sim, cutoff), 
                linetype = factor(cutoff))) +
  scale_color_manual(values = cut_cols) +
  labs(x = "Secondary cases", 
       y = "Frequency (proportion)", 
       color = "Tree type") +
  scale_linetype(name = "Pruning \nthreshold", 
                 labels = c("95%", "97.5%")) +
  theme_minimal_hgrid(font_size = 12) +
  guides(color = "none", linetype = "none")

# all cases -----
cases_all <-
  rbind(
    trees_best[, c("majority", "sim", "mcc", "cutoff", "t_diff", "dist_diff")], 
    trees_consensus[, .(t_diff = t_diff_median_days, 
                        dist_diff = dist_diff_meters, 
                        cutoff)][, c("sim", "mcc", "majority") := .(0, 0, 0)]
  )
cases_all[, tree_type := fcase(sim == 0, "Consensus", 
                              majority == 1, "Majority", 
                              mcc == 1, "MCC", 
                              majority == 0 & mcc == 0 & sim != 0, 
                              "Random sample")]

time_dist <-
  ggplot(cases_all) +
  stat_density(aes(x = t_diff, color = tree_type, # To get the right type of leg
                   group = interaction(sim, cutoff), 
                   linetype = factor(cutoff)),
               geom = "line", position = "identity") +
  scale_color_manual(values = cut_cols) +
  labs(x = "Time between linked cases (days)", 
       y = "Density", 
       color = "Tree type") +
  scale_linetype(name = "Pruning \nthreshold", 
                 labels = c("95%", "97.5%")) +
  theme_minimal_hgrid(font_size = 12) 

dist_dist <-
  ggplot(cases_all) +
  geom_density(aes(x = dist_diff, color = tree_type, 
                   group = interaction(sim, cutoff), 
                   linetype = factor(cutoff))) +
  scale_color_manual(values = cut_cols) +
  labs(x = "Distance between linked cases (meters)", 
       y = "Density", 
       color = "Tree type") +
  scale_linetype(name = "Pruning \nthreshold", 
                 labels = c("95%", "97.5%")) +
  theme_minimal_hgrid(font_size = 12) +
  guides(color = "none", linetype = "none")

# Figure S5
sfig_chain_comps <- 
  (size_dist / persist_dist / re_dist) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")
ggsave("figs/fig_S5_chain_comps.jpeg", sfig_chain_comps, height = 8, width = 6)

# Figure S6
sfig_diff_comps <- (time_dist / dist_dist) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")
ggsave("figs/fig_S6_SI_DK_comp.jpeg", sfig_diff_comps, height = 8, width = 6)


# filter these to 0.95 for comparison to IBM sims ----
re_props_0.95 <- filter(re_props, cutoff == 0.95)
chain_stats_0.95 <- chain_stats[cutoff == 0.95]
sizes_0.95 <- filter(sizes, cutoff == 0.95)

# do the same for vax & no vax scenarios ----
sim_dirs <- c(vax = "java_output/Round3_1000/output_files/", 
              low_vax = "java_output/Round3_LOWER_VAX_1000/output_files/")

# Summarize all the sims (takes ~ 4 mins with three cores) ----
system.time(chain_summary <- rbindlist(summarize_sims(dirs = sim_dirs, 
                                            apply_fun = summarize_chains)))
system.time(re_summary <- rbindlist(summarize_sims(dirs = sim_dirs, 
                                         apply_fun = summarize_re)))
chain_summary %>% 
  group_by(sim, sim_type, size) %>% 
  summarize(N = n()) %>%
  group_by(sim, sim_type) %>%
  mutate(prop = N/sum(N)) %>%
  complete(size = 1:max(size), fill = list(N = 0, prop = 0)) %>%
  bind_rows(sizes_0.95) %>%
  replace_na(list(tree_type = "Simulated", sim_type = "Reconstructed")) -> size_comp

chain_summary %>%
  select(persistence = days, sim, sim_type) %>%
  bind_rows(select(chain_stats_0.95, persistence, tree_type, sim)) %>%
  replace_na(list(tree_type = "Simulated", sim_type = "Reconstructed")) -> pers_comp

re_summary %>% 
  group_by(sim, sim_type, ntrans) %>% 
  summarize(N = n()) %>%
  group_by(sim, sim_type) %>%
  mutate(prop = N/sum(N)) %>%
  complete(ntrans = 1:max(ntrans), fill = list(N = 0, prop = 0)) %>%
  bind_rows(select(re_props_0.95, ntrans = re, prop)) %>%
  replace_na(list(tree_type = "Simulated", sim_type = "Reconstructed")) -> re_comp

sim_comp_cols <- alpha(brewer_pal(palette = "Accent")(3), c(0.9, 0.5, 0.1))
sim_comp_labs <- c("Reconstructed from transmission trees", 
                   "Simulated with low vax",
                   "Simulated with observed vax")
names(sim_comp_labs) <- names(sim_comp_cols) <- c("Reconstructed", 
                                                  "low_vax", "vax")
# Persistence comparison (Fig S11 B)
pers_sim <-
  ggplot(pers_comp) +
  geom_density(aes(x = scales::oob_squish((persistence + 1) / 7, 
                                          range = c(0, 100)), color = sim_type, 
                   group = interaction(sim, sim_type, tree_type))) +
  scale_color_manual(values = sim_comp_cols, labels = sim_comp_labs) +
  labs(x = "Persistence time of chain (weeks)", 
       y = "Frequency (density)", 
       color = "Simulation type") +
  scale_x_continuous(breaks = c(1, 25, 50, 75, 100), 
                     labels = c(1, 25, 50, 75, "100+")) +
  theme_minimal_hgrid(font_size = 12) +
  guides(color = "none")
pers_sim

# Clade comparison (Fig S11 A)
size_comp %>% 
  mutate(size = scales::oob_squish(size, range = c(0, 100))) %>% 
  group_by(tree_type, size, sim_type, sim) %>% 
  summarize(prop = sum(prop)) -> size_comp_summary

size_sim <-
  ggplot(size_comp_summary) +
  geom_line(aes(x = size, 
                y = prop, color = sim_type, group = interaction(sim, sim_type, tree_type))) +
  scale_color_manual(values = sim_comp_cols, labels = sim_comp_labs) +
  labs(x = "Clade size", 
       y = "Frequency (proportion)", 
       color = "Simulation type") +
  scale_x_continuous(breaks = c(1, 25, 50, 75, 100), 
                     labels = c(1, 25, 50, 75, "100+")) +
  theme_minimal_hgrid(font_size = 12) +
  guides(color = "none")
size_sim 

# Secondary case comparison (Fig S11 A)
re_comp %>% 
  mutate(ntrans = scales::oob_squish(ntrans, range = c(0, 25))) %>% 
  group_by(tree_type, ntrans, sim_type, sim) %>% 
  summarize(prop = sum(prop)) -> re_comp_summary

re_sim <-
  ggplot(re_comp_summary) +
  geom_line(aes(x = ntrans, y = prop, 
                color = sim_type, 
                group = interaction(sim, sim_type, tree_type))) +
  scale_color_manual(values = sim_comp_cols, labels = sim_comp_labs) +
  labs(x = "Secondary cases", 
       y = "Frequency (proportion)", 
       color = "Simulation type") +
  scale_x_continuous(breaks = c(0, 1, 5, 15, 25), 
                     labels = c(0, 1, 5, 15, "25+")) +
  theme_minimal_hgrid(font_size = 12) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))
re_sim

# max | min | mean for size / persistence / re for the different comps ----
re_stats_long  <- melt(re_stats[cutoff == 0.95], 
                       id.vars = c("sim", "tree_type"), 
                      measure.vars = "re")
chain_stats_long <- melt(chain_stats[cutoff == 0.95], 
                         id.vars = c("sim", "tree_type"), 
                    measure.vars = c("size", "persistence"))
chain_summary_long <- melt(chain_summary, 
                           id.vars = c("sim", "sim_type"), 
                           measure.vars = c("size", "days"))
re_summary_long <- melt(re_summary, 
                        id.vars = c("sim", "sim_type"), 
                        measure.vars = "ntrans")
chain_comps_long <- rbind(re_stats_long, chain_stats_long, chain_summary_long, 
                          re_summary_long, fill = TRUE)
chain_comps_long$tree_type[is.na(chain_comps_long$tree_type)] <- "Simulated"  
chain_comps_long$sim_type[is.na(chain_comps_long$sim_type)] <- "Reconstructed"  

chain_comp_summary <- chain_comps_long[, .(mean = mean(value),
                                          median = median(value),
                                          max = max(value)), 
                                      by = c("sim", "sim_type", 
                                             "tree_type", "variable")]
chain_comp_summary <-  melt(chain_comp_summary, 
                            id.vars = c("sim", "tree_type", "sim_type", "variable"), 
                            variable.name = "metric")
chain_comp_summary$variable[chain_comp_summary$variable == "ntrans"] <- "re"
chain_comp_summary$variable[chain_comp_summary$variable == "days"] <- "persistence"
chain_comp_summary[, val := ifelse(variable == "persistence", 
                                        (value + 1) / 7, 
                                        value)]
chain_comp_summary$sim_type <- factor(chain_comp_summary$sim_type, 
                                      levels = c("Reconstructed", "vax", "low_vax"))
chain_comp_summary$metric <- stringr::str_to_sentence(chain_comp_summary$metric)

met_comp_cols <- brewer_pal(palette = "Accent")(3)

# Comparison of secondary cases - set ylimits (all go to zero and up to defined max)
chain_comp_summary[,y_min := 0, by = metric]
chain_comp_summary[,y_max := value*1.25, by = metric]

met_re <-
  ggplot(chain_comp_summary[variable == "re"]) +
  geom_violin(aes(x = metric, y = val, fill = sim_type, color = sim_type)) +
  geom_hline(data = filter(chain_comp_summary, variable == "re", 
                           tree_type == "Consensus"), 
             aes(yintercept = val, color = sim_type)) +
  scale_fill_manual(values = met_comp_cols, aesthetics = c("color", "fill"),
                    name = "Simulation type", 
                    labels = sim_comp_labs) +
  labs(x = "", y = "Secondary cases") +
  facet_wrap(~ metric, scales = "free", 
             nrow = 1) +
  geom_blank(aes(y = y_min)) +
  geom_blank(aes(y = y_max)) +
  theme_half_open(font_size = 12) +
  theme(strip.text = element_blank())
met_re

# max chain size w/ no vacc
range(chain_comp_summary[variable == "size"]$val)

# Clade sizes = - set ylimits (all go to zero! and up to defined max)
met_size <-
  ggplot(chain_comp_summary[variable == "size"]) +
  geom_violin(aes(x = metric, y = oob_squish(val, c(0, 5000)), fill = sim_type, color = sim_type)) +
  geom_hline(data = filter(chain_comp_summary, variable == "size", 
                           tree_type == "Consensus"), 
             aes(yintercept = val, color = sim_type)) +
  scale_fill_manual(values = met_comp_cols, aesthetics = c("color", "fill"),
                    name = "Simulation type", 
                    labels = sim_comp_labs) +
  labs(x = "", y = "Clade size") +
  facet_wrap(~ metric, scales = "free", 
             nrow = 1) + 
  geom_blank(aes(y = y_min)) +
  theme_half_open(font_size = 12) +
  theme(strip.text = element_blank())
met_size

# Clade persistence = - set ylimits (all go to zero! and up to defined max)
met_pers <-
  ggplot(chain_comp_summary[variable == "persistence"]) +
  geom_violin(aes(x = metric, y = val, fill = sim_type, color = sim_type)) +
  geom_hline(data = filter(chain_comp_summary, variable == "persistence", 
                           tree_type == "Consensus"), 
             aes(yintercept = val, color = sim_type)) +
  scale_fill_manual(values = met_comp_cols, aesthetics = c("color", "fill"), 
                    name = "Simulation type", 
                    labels = sim_comp_labs) +
  labs(x = "", y = "Clade persistence (weeks)") +
  facet_wrap(~ metric, scales = "free", 
             nrow = 1) + 
  geom_blank(aes(y = y_min)) +
  theme_half_open(font_size = 12) +
  theme(strip.text = element_blank())
met_pers

# Combine everything to make S12
sfigs_sims <- 
  (((size_sim / pers_sim / re_sim) & guides(color = "none")) |
  (met_size / met_pers / met_re)) +
  plot_layout(guides = "collect", widths = c(1, 2)) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = 'bottom')
sfigs_sims 

ggsave("figs/fig_S10_metrics.jpeg", sfigs_sims, height = 9, width = 10)
ggsave("figs/fig_S10_metrics.pdf", sfigs_sims, height = 9, width = 10)

# index = which(chain_comp_summary$sim_type == "low_vax" & 
#                 chain_comp_summary$metric == "Max" & 
#                 chain_comp_summary$variable == "size")
# chain_comp_summary$value[index]
# hist(chain_comp_summary$value[index], breaks = seq(0, 33000, 1000))
# which(chain_comp_summary$value[index] > 20000)

# Chain size - reconstructed vs simulated under vaccination
median(chain_comp_summary[variable == "size" & sim_type == "vax" & metric == "Mean"]$val)
median(chain_comp_summary[variable == "size" & sim_type == "Reconstructed" & metric == "Mean"]$val)
median(chain_comp_summary[variable == "size" & sim_type == "vax" & metric == "Median"]$val)
median(chain_comp_summary[variable == "size" & sim_type == "Reconstructed" & metric == "Median"]$val)

mean(chain_comp_summary[variable == "size" & sim_type == "vax" & metric == "Mean"]$val)
mean(chain_comp_summary[variable == "size" & sim_type == "Reconstructed" & metric == "Mean"]$val)
mean(chain_comp_summary[variable == "size" & sim_type == "vax" & metric == "Median"]$val)
mean(chain_comp_summary[variable == "size" & sim_type == "Reconstructed" & metric == "Median"]$val)


chain_comp_summary[variable == "size" & sim_type == "Reconstructed" & metric == "Median"]$val
chain_comp_summary[variable == "size" & sim_type == "vax" & metric == "Median"]$val

