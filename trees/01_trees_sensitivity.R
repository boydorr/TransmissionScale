# Running full sensitivity analysis for supplement ----

# This will take a couple hours to run all the combinations

# sub_cmd:=-t 4 -n 3 -jn test -wt 5m -sn -mem 6000

if(Sys.getenv("SLURM_JOB_ID") != "") {
  ncores <- as.numeric(Sys.getenv("SLURM_NTASKS")) 
  } else {
    ncores <- parallel::detectCores() - 1
  } 
print(ncores)
cl <- parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl)

# Packages 
library(treerabid)  
library(data.table)
library(lubridate)
library(dplyr)
library(lubridate)
library(magrittr)
library(foreach)
library(iterators)
library(doRNG)
library(igraph)

# Data ----
case_dt <- readRDS(file = "output/clean_bite_data.rda")

# clean up (no cases with NA location or time & filter to start/end dates)
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

# Get the "canonical" data (i.e. with only one record per case)
# For running trees without using known tracing data
# & for joining up consensus links to epi data
case_dt %>%
  group_by(ID) %>%
  slice(1) %>%
  as.data.table() -> case_dt

# Part I: Do ttree reconstruction comparing different dists/cutoffs ----

# Running the full set of trees with various settings ----
tree_pars <- tidyr::expand_grid(si_pdist = c("gamma", "lnorm", "weibull"), 
                                dist_pdist = c("gamma", "lnorm", "weibull"),
                                convolve = "mixed",
                                prune = TRUE, 
                                cutoff = c(0.95, 0.975), 
                                use_known = c(TRUE, FALSE), 
                                nsim = 1000)
tree_pars$seed <- 42 * 1:nrow(tree_pars)

comb <- function(...) {
  mapply('rbind', ..., SIMPLIFY = FALSE, fill = TRUE)
}

# This will take about 40 minutes with 3 cores
system.time({
  trees_se <-
    foreach(i = iter(tree_pars, by = "row"), .combine = comb) %do% {
      
      # ct data with dates w/out uncertainty
      case_dates <- data.table(id_case = case_dt$ID, 
                               symptoms_started = case_dt$Symptoms.started)
      
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
                   date_symptoms = case_dt$Symptoms.started, # needs to be in a date class
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
      links_all <- build_all_links(ttrees, N = i$nsim)
      links_consensus <- build_consensus_links(links_all, case_dates)
      list(links_consensus = cbind(links_consensus, i))
    }
})

parallel::stopCluster(cl)

# Write out files of the trees and the links (consensus & all)
fwrite(trees_se$links_consensus, "output/trees/links_consensus_se.csv")

# Parse these from subutil for where to put things
syncto <- "~/Documents/Projects/Serengeti_Rabies/output/"
syncfrom <- "mrajeev@della.princeton.edu:Serengeti_Rabies/output/trees"


