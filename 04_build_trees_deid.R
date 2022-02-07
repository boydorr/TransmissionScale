# Build best trees & incursion data frame for java ----
rm(list=ls())

# Packages
library(treerabid) # devtools::install_github("mrajeev08/treerabid")
library(data.table)
library(lubridate)
library(dplyr)
library(lubridate)
library(magrittr)
library(foreach)
library(iterators)
library(doRNG)
library(igraph)
library(glue)

# clean up (no cases with NA location or time & filter to start/end dates)
# case_dt <- readRDS(file = "output/clean_bite_data.rda")
# TREE BUILDING WILL NOT BE CORRECT WHEN RUN OFF DEIDENTIFIED DATA WITH JITTERED GPS
# CORRECT OUTPUTS ARE SAVED IN THE REPO
case_dt <- readRDS(file = "output/clean_bite_data_deid.rda")
case_dt$UTM.Easting <- case_dt$UTM.Easting.jitter
case_dt$UTM.Northing <- case_dt$UTM.Northing.jitter

case_dt %<>%
  dplyr::filter(!is.na(Symptoms.started), 
                !is.na(UTM.Easting), 
                !is.na(UTM.Northing), 
                Symptoms.started >= ymd("2002-01-01"),
                Symptoms.started >= "2002-01-01", 
                Symptoms.started <= ymd("2015-12-31")) %>%
  # get uncertainty in days
  mutate(days_uncertain = case_when(Symptoms.started.accuracy == "+/- 14 days" ~ 14L, 
                                    Symptoms.started.accuracy == "+/- 7 days" ~ 7L,
                                    Symptoms.started.accuracy == "+/- 28 days" ~ 28L, 
                                    Symptoms.started.accuracy == "0" ~ 0L, 
                                    TRUE ~ 0L), 
         owned = ifelse(Owner %in% "Known", TRUE, FALSE)) 

# Table 1: agreement to CT data (use known tree for each type of cutoff situation)
ct_data <- data.table(id_case = case_dt$ID,
                      id_biter = case_dt$Biter.ID, 
                      x_coord = case_dt$UTM.Easting, 
                      y_coord = case_dt$UTM.Northing, 
                      owned = case_dt$owned, 
                      date_symptoms = case_dt$Symptoms.started,
                      days_uncertain = case_dt$days_uncertain)
ct_data <- ct_data[ct_data[, .I[which.max(date_symptoms)],  by = "id_case"]$V1] # pick one case only per id
# fwrite(ct_data, "Output/trees/tree_ct_data.csv")
fwrite(ct_data, "Output/trees/tree_ct_data_deid.csv")


# Use the `best` dists/cutoffs & known source to generate trees + incs ----
pars_selected <-
  tidyr::expand_grid(si_pdist = "lnorm", 
                     dist_pdist = "weibull",
                     convolve = "mixed",
                     prune = TRUE, 
                     cutoff = c(0.95, 0.975), 
                     use_known = TRUE, 
                     nsim = 1000)
pars_selected$seed <- 45 * 1:nrow(pars_selected)

comb <- function(...) {
  mapply('rbind', ..., SIMPLIFY = FALSE, fill = TRUE)
}

cl <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(cl)

# This takes about 10 mins on my computer with 3 cores
system.time({
  consensus_known <-
    foreach(i = iter(pars_selected, by = "row"), .combine = comb) %do% {
      
      # ct data with dates w/out uncertainty & only one record per ID
      case_dates <- data.table(id_case = ct_data$id_case, 
                               symptoms_started = ct_data$date_symptoms)
      
      # Distribution functions from treerabid
      if(i$convolve == "mixed") {
        si_pdist <- get(paste0("si_", i$si_pdist, "1"))
        dist_pdist <- get(paste0("dist_", i$dist_pdist, "_mixed"))
      }
      
      if(i$convolve == "convolved") {
        si_pdist <- get(paste0("si_", i$si_pdist, "2"))
        dist_pdist <- get(paste0("dist_", i$dist_pdist, "2"))
      }
      
      if(i$convolve == "baseline") {
        si_pdist <- get(paste0("si_", i$si_pdist, "1"))
        dist_pdist <- get(paste0("dist_", i$dist_pdist, "1"))
      }
      
      ttrees <- 
        boot_trees(id_case = case_dt$ID,
                   id_biter = case_dt$Biter.ID, 
                   x_coord = case_dt$UTM.Easting,
                   y_coord = case_dt$UTM.Northing,
                   owned = case_dt$owned, 
                   date_symptoms = case_dt$Symptoms.started,
                   days_uncertain = case_dt$days_uncertain,
                   use_known_source = i$use_known,
                   prune = i$prune,
                   si_fun = si_pdist,
                   dist_fun = dist_pdist, 
                   params = treerabid::params_treerabid, 
                   cutoff = i$cutoff,
                   N = i$nsim, 
                   seed = i$seed) 
      
      # Summarize the trees
      # do this outside of function to get min t_diff as well
      links_all <- ttrees[, .(links = .N,
                              t_diff_min_days = min(t_diff),
                              t_diff_median_days = median(t_diff),
                              dist_diff_meters = median(dist_diff)),
                          by = c("id_case", "id_progen")][, prob := links/i$nsim]
      links_consensus <- build_consensus_links(links_all, case_dates)
      tree_ids <- c(mcc = 
                      build_consensus_tree(links_consensus, ttrees, links_all,
                                           type = "mcc", output = "sim"), 
                    majority = 
                      build_consensus_tree(links_consensus, ttrees, links_all,
                                           type = "majority", output = "sim"))
      ttrees$mcc <- ifelse(ttrees$sim %in% tree_ids["mcc"], 1, 0)
      ttrees$majority <- ifelse(ttrees$sim %in% tree_ids["majority"], 1, 0)
      set.seed(i$seed)
      out_trees <- ttrees[sim %in% c(sample((1:i$nsim)[-tree_ids], 100), tree_ids)]
      
      list(links = cbind(links_consensus, i), 
           ttrees_all = data.table(out_trees, cutoff = i$cutoff))
    }
})

parallel::stopCluster(cl)

# Write out files
consensus_links <- consensus_known$links
lapply(split(consensus_links, interaction(consensus_links$cutoff, consensus_links$convolve)), check_loops)
fwrite(consensus_known$ttrees_all, "output/trees/trees_sampled_best.gz")
fwrite(consensus_links, "output/trees/consensus_links_best.csv")

# Write out incursions for Rebecca: Biter.ID, ID, Suspect
incs_best <- consensus_links[is.na(id_progen)]
template <- data.table(readRDS("output/clean_bite_data_canonical_deid.rda"))
# template <- data.table(readRDS("output/clean_bite_data_canonical.rda"))
template[, c("incursions_best_0.95", 
             "incursions_best_0.975") := .(ID %in% incs_best[cutoff == 0.950]$id_case, 
                                           ID %in% incs_best[cutoff == 0.975]$id_case)]
# stats for text
sum(template$incursions_best_0.95) # total number of introdcutions
# annual average and range
incs <- template[, year := year(Symptoms.started)][incursions_best_0.95 == TRUE]
incs <- incs[, .N, by = "year"][data.table(year = seq(2002, 2015), by = 1), 
                                on = "year"]
mean(incs$N)
range(incs$N)

# write out for Rebecca's java code
template <- template[, c("Biter.ID", "ID", "Suspect", "incursions_best_0.95", 
                         "incursions_best_0.975")]
fwrite(template, "output/trees/incursions_updated.csv")

