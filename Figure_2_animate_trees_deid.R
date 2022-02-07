# Making animations of transmission tree

# pkgs ----
library(treerabid) # devtools::install_github("mrajeev08/treerabid")
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
library(ggforce)
library(animation)
library(forcats)
library(glue)
library(here)
library(ggspatial) # for the scale

select <- dplyr::select
source("R/gif_funs.R")

# Pull in and process best ----
case_dt <- data.table(readRDS("output/clean_bite_data_canonical_deid.rda"))
sd_shape <- st_read("data/SD_Villages_2002_From_HHS_250m_Smoothed_UTM.shp")
links_best <- fread("output/trees/consensus_links_best.csv")
cons_split <- split(links_best, 
                    links_best$cutoff)
cons_graphs <-
  lapply(cons_split, function(x) {
    get_graph(from = x$id_progen, to = x$id_case, 
              attrs = data.table(id_case = unique(x$id_case)))
  }
)

# helper fun to get cutoff val as column
rbind_withcol <- function(x) {
  lapply(1:length(x), function(n) {
    x[[n]][, cutoff := as.numeric(names(x)[n])] # modifies in place
  })
  rbindlist(x)
}

# Get the chains stats
chains_consensus <- rbind_withcol(lapply(cons_graphs, get_chain_membership))
chain_stats <- rbind_withcol(lapply(cons_graphs, get_chain_stats))
chains_consensus <- chains_consensus[chain_stats, on = c("membership", "cutoff")]
chains_consensus[, id_case := as.numeric(id_case)]
chains_consensus <- chains_consensus[links_best, on = c("cutoff", "id_case")]
chains_consensus <- 
  case_dt[, 
          .(id_case = ID, date_symptoms = Symptoms.started, 
            x_coord = UTM.Easting, y_coord = UTM.Northing)][chains_consensus, on = "id_case"]
chains_consensus[, persistence := as.numeric(max(date_symptoms) - min(date_symptoms) + 1), 
                 by = c("membership", "cutoff")]

# Figure showing distribution of chains across time & space (best) ----
cutoff_labs <- c("0.95" = "Cutoff = 95%", "0.975" = "Cutoff = 97.5%")
cols_chains <- c("#ebac23", "#b80058", "#008cf9", "#006e00", "#00bbad", 
                 "#d163e6", "#b24502", "#ff9287", "#5954d6", "#00c6f8", 
                 "#878500", "#CACACA")
names(cols_chains) <- c(1:11, "All others")

chain_stats %>% 
  group_by(cutoff) %>% 
  mutate(rank = row_number(desc(size))) %>%
  left_join(chains_consensus) %>%
  group_by(membership, cutoff) %>%
  mutate(month = floor_date(date_symptoms, unit = "months"), 
         chain_id = ifelse(rank <= 11, as.character(rank), 
                           "All others"), 
         start_date = min(month)) -> top_chains
top_chains %>%
  group_by(membership, month, cutoff, chain_id) %>%
  summarise(cases = n()) %>%
  filter(cutoff == 0.95) -> top_chain_hist # filter to 95% cutoff

top_chain_hist$chain_id <- fct_relevel(factor(top_chain_hist$chain_id), 
                                       "All others", after = Inf)

top_hist_95 <- 
  ggplot(top_chain_hist) +
  geom_col(aes(x = month, y = cases, fill = chain_id), 
           position = position_stack(reverse = TRUE)) +
  scale_x_date(date_breaks = "24 months", date_labels = "%b %Y") + 
  scale_fill_manual(values = cols_chains, guide = "none") +
  labs(x = "", y = "Monthly cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  cowplot::theme_minimal_hgrid() 

top_chains %>% 
  group_by(membership, cutoff) %>%
  slice(1) %>%
  ungroup() %>%
  filter(cutoff == 0.95 & rank < 12) %>%
  arrange(start_date) -> chain_labs

chain_labs_95 <-  c(glue("N = {chain_labs$size}"), 
                    glue("All others \n (N < {min(chain_labs$size)})")) 
names(chain_labs_95) <- c(chain_labs$rank,"All others")
top_chains$chain_id <- factor(top_chains$chain_id, levels = names(chain_labs_95))

scale_params <- tibble::tibble(
  chain_id = "All others",
  width_hint = 0.5,
  location = "br",
  unit_category = "metric",
)

top_n_chains_95 <-
  ggplot(filter(top_chains, cutoff == 0.95)) +
  geom_sf(data = sd_shape, fill = "black", color = "black") +
  geom_point(aes(x = x_coord, y = y_coord, color = factor(chain_id)), 
             shape = 21) + 
  scale_color_manual(values = cols_chains, guide = "none", aesthetics = c("color", "fill")) +
  annotation_scale(data = scale_params, 
                   aes(location = location, unit_category = unit_category, 
                       width_hint = width_hint), 
                   pad_y = unit(-0.25, "cm")) +
  facet_wrap(~chain_id, labeller = as_labeller(chain_labs_95)) +
  guides(color = "none", ncol = 3) +
  coord_sf(clip = "off") +
  theme_map() 

top_chains_95 <- 
  top_hist_95 + top_n_chains_95 +
  plot_layout(heights = c(1.3, 4)) +
  plot_annotation(tag_levels = "A", 
                  theme = theme(plot.margin = margin(2, 2, 2, 2), 
                                text = element_text(size = 8)))

ggsave("figs/fig2_top_chains_95.jpeg", top_chains_95, height = 19, width = 19, units = "cm")
ggsave("figs/fig2_top_chains_95.pdf", top_chains_95, height = 19, width = 19, units = "cm")


# set working directory to store animation
fpath <- getwd()
setwd(paste0(fpath,"/figs"))

# set up colors
cols_chains <- c("#ebac23", "#b80058", "#008cf9", "#006e00", "#00bbad", 
                 "#d163e6", "#b24502", "#ff9287", "#5954d6", "#00c6f8", 
                 "#878500", "#CACACA")
names(cols_chains) <- c(1:11, "All others")

# GIF of spread ----
top_chains %>%
  filter(cutoff == 0.95) %>%
  select(id_case, x_coord_to = x_coord, y_coord_to = y_coord, id_progen, 
         month, chain_id, date_infectious = date_symptoms) %>%
  left_join(select(case_dt, id_progen = ID, x_coord_from = UTM.Easting,  
                   y_coord_from = UTM.Northing, date_exposed = Symptoms.started)) %>%
  ungroup() %>%
  mutate(group = 1:nrow(.), 
         date_exposed = case_when(is.na(date_exposed) ~ ymd(date_infectious), 
                                  TRUE ~ ymd(date_exposed)), 
         x_coord_from = ifelse(is.na(x_coord_from), x_coord_to, x_coord_from), 
         y_coord_from = ifelse(is.na(y_coord_from), y_coord_to, y_coord_from)) -> chain_segs

bez_pts <- get_bezier_pts(from = as.matrix(select(chain_segs, lat = y_coord_from, long = x_coord_from)),
                          to = as.matrix(select(chain_segs, lat = y_coord_to, long = x_coord_to)),
                          frac = 0.25,
                          transform = function(x) log(1 / x) * 500)

bez_pts <- left_join(chain_segs, bez_pts, by = "group")

# trying it here
bez_pts %>% 
  mutate(date_infectious = floor_date(date_infectious, unit = "month"), 
         date_exposed = floor_date(date_exposed, unit = "month")) -> bez_pts

# save as an html (takes abt 5 mins)
system.time(
  saveHTML({
    make_gif(bez_pts, top_chain_hist, cols_chains,
             sd_shape, 
             date_seqs = seq.Date(from = ymd("2002-01-01"), 
                                  to = ymd("2015-12-01"), 
                                  by = "month"), 
             fade_out = 1)  
  }, 
  img.name = 'trans_plot', title = 'Reconstructed transmission trees', 
  description = 
    c('Reconstructed transmission trees using lognormal SI and weibull dk'),
  interval = 0.5, nmax = 50, ani.dev = "png", ani.type = "png",
  ani.res = 800, ani.width = 1200, ani.height = 800)
)

# and as a movie (more manageable file size and res than gif/pdf)
system.time(
  saveVideo({
    make_gif(bez_pts, top_chain_hist, cols_chains,
             sd_shape,
             date_seqs = seq.Date(from = ymd("2002-01-01"),
                                  to = ymd("2015-12-01"),
                                  by = "month"),
             fade_out = 1)
  },
  img.name = 'trans_plot', title = 'Reconstructed transmission trees',
  description =
    c('Reconstructed transmission trees using lognormal SI and weibull dk'),
  interval = 0.5, nmax = 50, ani.dev = "png", ani.type = "png",
  ani.res = 800, ani.width = 1200, ani.height = 800))

# try as a gif
system.time(
  saveGIF({
    make_gif(bez_pts, top_chain_hist, cols_chains,
             sd_shape,
             date_seqs = seq.Date(from = ymd("2002-01-01"),
                                  to = ymd("2015-12-01"),
                                  by = "month"),
             fade_out = 1)
  },
  img.name = 'trans_plot', title = 'Reconstructed transmission trees',
  description =
    c('Reconstructed transmission trees using lognormal SI and weibull dk'),
  interval = 0.5, ani.width = 1200, ani.height = 800))
# 
# findGlobals(plot.pemba.trees, merge=FALSE)
# ani.options(convert = "/opt/local/bin/convert")
# setwd("/Users/katiehampson")
# saveGIF({plot.pemba.trees(rabid=rabid, monthlycases=monthlycases)},
#         interval = 0.7, movie.name = "serengeti_trees.gif", ani.width = 800, ani.height = 1000, loop=1)

# change the working directory back so as not to mess things up!
setwd(fpath)
