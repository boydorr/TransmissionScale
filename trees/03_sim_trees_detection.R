# Simulate unpruned trees for comp to detection ----

# sub_cmd:=-t 2 -n 3 -jn test -wt 5m -sn -mem 6000

if(Sys.getenv("SLURM_JOB_ID") != "") {
  ncores <- as.numeric(Sys.getenv("SLURM_NTASKS")) 
} else {
  ncores <- parallel::detectCores() - 1
} 

print(ncores)
cl <- parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl)

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

# clean up (no cases with NA location or time & filter to start/end dates) ----
case_dt <- readRDS(file = "output/clean_bite_data.rda")

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

# filter to one record per case ----
case_dt %>%
  group_by(ID) %>%
  slice(1) %>%
  as.data.table() -> case_dt

case_dates <- data.table(id_case = case_dt$ID, 
                         symptoms_started = case_dt$Symptoms.started)

# Use the `best` dists/cutoffs & known source to generate trees + incs ----
# This takes about 15 mins on my computer with 3 cores
i <-
  tidyr::expand_grid(si_pdist = "lnorm", 
                     dist_pdist = "weibull",
                     convolve = "mixed",
                     prune = FALSE, 
                     cutoff = 1, 
                     use_known = TRUE, 
                     nsim = 1000)
i$seed <- 49 

ttrees <- boot_trees(id_case = case_dt$ID,
                     id_biter = case_dt$Biter.ID, 
                     x_coord = case_dt$UTM.Easting,
                     y_coord = case_dt$UTM.Northing,
                     owned = case_dt$owned, 
                     date_symptoms = case_dt$Symptoms.started,
                     days_uncertain = case_dt$days_uncertain,
                     use_known_source = TRUE,
                     prune = i$prune,
                     si_fun = si_lnorm1,
                     dist_fun = dist_weibull_mixed, 
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

set.seed(5679)
out_trees <- ttrees[sim %in% c(sample((1:i$nsim)[-tree_ids], 100), tree_ids)]

links_consensus <- cbind(links_consensus, i)
ttrees_all <- data.table(out_trees, cutoff = i$cutoff)

parallel::stopCluster(cl)

# Write out files
fwrite(ttrees_all, "output/trees/trees_sampled_unpruned.gz")
fwrite(links_consensus, "output/trees/consensus_links_unpruned.csv")

# Parse these from subutil for where to put things
syncto <- "~/Documents/Projects/Serengeti_Rabies/output/"
syncfrom <- "mrajeev@della.princeton.edu:Serengeti_Rabies/output/trees"



