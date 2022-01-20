# Make figures for supplement ----

# Pkgs ----
library(treerabid)
library(igraph)
library(data.table)
library(dplyr)
library(stringr)
library(sf)
library(tidyr)
library(lubridate)
library(magrittr)
library(scales)
library(forcats)
library(glue)
library(ggplot2)
library(patchwork)
library(cowplot)
library(ggbeeswarm)
library(ggridges)
library(readr)
library(here)
select <- dplyr::select

# Data ---
links_consensus_se <- fread("output/trees/links_consensus_se.csv")
sd_shape <- st_read("data/SD_Villages_2002_From_HHS_250m_Smoothed_UTM.shp")
case_dt <- data.table(readRDS("output/clean_bite_data_canonical_deid.rda"))

setnames(case_dt, "ID", "id_case")
known <- case_dt[Biter.ID %in% id_case]$id_case
owned <- case_dt[Owner %in% "Known"]$id_case

# Compare distributions + cutoffs ----
se_diffs <- links_consensus_se[, .N, 
                               by = c("si_pdist", "dist_pdist", "cutoff")]
se_diffs[, c("dist_fun", "si_fun") := .(paste0("dist_", dist_pdist, "_mixed"), 
                                        paste0("si_", si_pdist, "1"))]

cutoffs <- 
  rbindlist(
    lapply(split(se_diffs, 1:nrow(se_diffs)), 
           function(x) {
             ttree <- data.table(owned = c(TRUE, FALSE), x)
             ttree$time_cut <- get(x$si_fun)(ttree = ttree, 
                                       params = params_treerabid, cutoff = x$cutoff)
             ttree$dist_cut <- get(x$dist_fun)(ttree = ttree, 
                                        params = params_treerabid, cutoff = x$cutoff)
             return(ttree)
             
           })
  )

cutoffs %>%
  group_by(si_pdist, cutoff, owned) %>%
  slice(1) %>%
  filter(owned == TRUE) %>%
  select(dist = si_pdist, cutoff, owned, cut_val = time_cut) %>%
  mutate(type = "time") -> times


cutoffs %>%
  group_by(dist_pdist, cutoff, owned) %>%
  slice(1) %>%
  select(dist = dist_pdist, cutoff, owned, cut_val = dist_cut) %>%
  mutate(type = "distance") %>%
  bind_rows(times) -> cutoff_table

cutoff_table %>%
  pivot_wider(values_from = cut_val, names_from = owned) %>%
  mutate(cutoff_val = ifelse(type == "distance", 
                         paste0("Owned: ", round(`TRUE`, 0), ";\n", 
                                "Unowned/wildlife: ", round(`FALSE`, 0)), 
                         paste0(round(`TRUE`, 0)))) %>%
  select(dist, cutoff, type, cutoff_val) %>%
  pivot_wider(values_from = cutoff_val, names_from = type) %>%
  arrange(dist) %>%
  ungroup() %>%
  mutate(across(where(is.character), list(stringr::str_to_title), 
                .names = "{.col}"), 
         cutoff = paste0(cutoff * 100)) -> cutoff_table

names(cutoff_table) <- c("Distribution", "Pruning threshold (%)",
                         "Distance cutoff (meters)", "Time cutoff (days)")

write_csv(cutoff_table, "output/trees/cutoff_table.csv")

# Compare to tracing data ----
trace_comp <- links_consensus_se[use_known == FALSE & id_case %in% known]
trace_comp <- case_dt[, c("id_case", "Biter.ID")][trace_comp, on = "id_case"]
trace_comp[, c("correct", 
               "mis_id") := .(Biter.ID == id_progen & !is.na(id_progen), 
                              is.na(id_progen) & Biter.ID != 0)]
trace_comp <- trace_comp[, .(prop_right = sum(correct)/.N, 
                             prop_misid = sum(mis_id)/.N), 
                         by = c("si_pdist", "dist_pdist", "prune", "cutoff", "use_known")]

trace_comp_wide <- pivot_wider(trace_comp, names_from = cutoff, values_from = c(prop_right, prop_misid))

dist_levs <- levels(interaction(trace_comp$si_pdist, trace_comp$dist_pdist))
dist_labs <- gsub("lnorm", "Lognormal", dist_levs, fixed = TRUE)
dist_labs <- stringr::str_to_title(gsub(".", " | ", dist_labs, fixed = TRUE))
names(dist_labs) <- dist_levs

tree_comp_a <- ggplot(trace_comp) +
  geom_point(aes(x = interaction(si_pdist, dist_pdist), 
                 y = prop_misid, color = factor(cutoff))) +
  geom_linerange(data = trace_comp_wide, 
                 aes(x = interaction(si_pdist, dist_pdist), ymin = prop_misid_0.95, 
                     ymax = prop_misid_0.975)) + 
  scale_y_continuous(name = "Traced cases \n misidentified as incursions", 
                     labels = scales::percent) +
  scale_x_discrete(labels = dist_labs, 
                   name = "Distribution for Serial interval | Distance Kernel") +
  scale_color_brewer(palette = "Dark2", name = "Pruning \nthreshold", 
                     labels = c("95%", "97.5%")) +
  coord_flip() +
  theme_minimal_hgrid(font_size = 12)

write_csv(trace_comp, "output/trees/comp_stats.csv")

# Compare against each other (incs & agreement) ----
se_comp <- links_consensus_se[use_known == TRUE & !(id_case %in% known)]

cons_comp <- se_comp[se_comp, on = "id_case", allow.cartesian = TRUE]
setnafill(cons_comp, cols = c("id_progen", "i.id_progen"), fill = 0)
cons_comp[, match := id_progen == i.id_progen]
cons_comp <-
  cons_comp[, .(prop_matching = sum(match)/.N), 
            by = c("si_pdist", "dist_pdist", "convolve", "prune", "cutoff", "use_known",
                   "i.si_pdist", "i.dist_pdist", "i.convolve", "i.prune", "i.cutoff", "i.use_known")]
cons_comp[, c("same_dist", "same_si", "same_cutoff", "same_convol") := 
            .(si_pdist == i.si_pdist, dist_pdist == i.dist_pdist, cutoff == i.cutoff, 
              convolve == i.convolve)]
cons_comp[, group_val := fcase(same_dist & same_si, "Both same", 
                               !same_dist & !same_si, "Both different", 
                               !same_dist & same_si, "Same SI", 
                               same_dist & !same_si, "Same DK")]
write_csv(cons_comp, "output/trees/agreement_stats.csv")

incs_summary <- 
  links_consensus_se[use_known == TRUE][, .(n_incs = sum(is.na(id_progen)), 
                                          prop_incs = sum(is.na(id_progen))/.N, 
                                          incs_per_yr = sum(is.na(id_progen))/13), 
                                      by = c("si_pdist", "dist_pdist", "convolve", "prune", "cutoff", "use_known")]
incs_wide <- pivot_wider(incs_summary, names_from = cutoff, values_from = c(prop_incs, n_incs, incs_per_yr))

tree_comp_b <-
  ggplot(incs_summary) +
  geom_point(aes(x = interaction(si_pdist, dist_pdist), 
                 y = n_incs, color = factor(cutoff))) +
  geom_linerange(data = incs_wide, 
                 aes(x = interaction(si_pdist, dist_pdist), ymin = n_incs_0.975, 
                     ymax = n_incs_0.95)) + 
  scale_x_discrete(labels = dist_labs, 
                   name = "") +
  scale_color_brewer(palette = "Dark2", name = "Pruning \nthreshold", 
                     labels = c("95%", "97.5%")) +
  scale_y_continuous(name = "Number of incursions") +
  coord_flip() +
  theme_minimal_hgrid(font_size = 12) +
  theme(axis.text.y = element_blank())

sfig_tree_perf <-
  (tree_comp_a | tree_comp_b) + 
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = "A")
ggsave("figs/fig_S3_tree_comps.jpeg", sfig_tree_perf, height = 5, width = 8)


# Compare probabilities of selection (convolved vs. mixed) ----
cutoff_labs <- c("0.95" = "Cutoff = 95%", "0.975" = "Cutoff = 97.5%")

# filter out traced links and select the best distributions
comp_probs <- links_consensus_se[si_pdist == "lnorm" & dist_pdist == "weibull" & 
                                   !(id_case %in% known) & use_known]

# get the reference distributions
t_max <- max(comp_probs$t_diff_median_days, na.rm = TRUE) + 30
dist_max <- max(comp_probs$dist_diff_meters, na.rm = TRUE) + 1000

# times
ref_t <- data.frame(t_diff = rlnorm(10000, 
                                    meanlog = params_treerabid$SI_meanlog, 
                                    sdlog = params_treerabid$SI_sdlog))

# distances
ref_d <- rbind(data.frame(dist_diff = rweibull(10000, 
                                               shape = params_treerabid$DK_shape_weibull, 
                                               scale = params_treerabid$DK_scale_weibull), 
                          name = "baseline"), 
               data.frame(dist_diff = rweibull(10000, 
                                               shape = params_treerabid$DK2_shape_weibull, 
                                               scale = params_treerabid$DK2_scale_weibull), 
                          name = "convolved"))

best_probs <-
  ggplot(comp_probs) +
  geom_density(aes(x = prob, fill = factor(cutoff), color = factor(cutoff)), 
               alpha = 0.6) +
  scale_fill_brewer(palette = "Dark2",  name = "Pruning \nthreshold", 
                    guide = "none", aesthetics = c("color", "fill")) +
  labs(x = "Highest probability progenitor", y = "Density") + 
  theme_minimal_hgrid(font_size = 12)

best_times <-
  ggplot(comp_probs) +
  geom_density(aes(x = t_diff_median_days, fill = factor(cutoff), color = factor(cutoff)), 
               alpha = 0.6) +
  scale_x_continuous(limits = c(0, t_max)) +
  scale_fill_brewer(palette = "Dark2",  name = "Pruning \nthreshold", 
                    labels = cutoff_labs, aesthetics = c("color", "fill"), 
                    guide = "none") +
  geom_density(data = ref_t, aes(x = t_diff), linetype = 1) +
  labs(x = "Time between linked cases (days)", y = "Density") + 
  theme_minimal_hgrid(font_size = 12) +
  guides(fill = guide_legend(override.aes = list(color = "NA")))

# add in info about ownership
comp_probs[, name := ifelse(id_case %in% owned, "baseline", "convolved")]
comp_probs[, dist_diff_meters_c := ifelse(dist_diff_meters < 100, 100, dist_diff_meters)]
best_dists <- 
  ggplot(comp_probs) +
  geom_density(aes(x = dist_diff_meters_c, fill = factor(cutoff), 
                   color = factor(cutoff)), 
                   alpha = 0.6) +
  scale_fill_brewer(palette = "Dark2",  name = "Pruning \nthreshold", 
                    labels = cutoff_labs, aesthetics = c("color", "fill"), 
                    guide = "none") +
  scale_x_continuous(breaks = c(100, 3000, 6000, 9000), limits = c(0, dist_max)) +
  geom_density(data = ref_d, 
               aes(x = scales::oob_censor(dist_diff, c(100, Inf)), linetype = name)) +
  scale_linetype_manual(values = c(1, 3), name = "Reference distribution", 
                        labels = c("Baseline", "Convolution")) +
  labs(x = "Distance between linked cases (meters)", y = "Density") + 
  theme_minimal_hgrid(font_size = 12) +
  facet_wrap(~name, labeller = as_labeller(c("baseline" = "Owned", "convolved" = "Unknown/wildlife")))

sfig_best_comp <- 
  (best_probs / best_times / best_dists) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")

ggsave("figs/fig_S4_best_comp.jpeg", sfig_best_comp, height = 8, width = 6)

