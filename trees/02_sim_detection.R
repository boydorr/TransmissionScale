# Validate detection probabilities ----

# sub_cmd:=-t 12 -n 5 -jn test -wt 5m -sn -mem 6000

# cluster set up if using ----
if(Sys.getenv("SLURM_JOB_ID") != "") {
  ncores <- as.numeric(Sys.getenv("SLURM_NTASKS")) 
} else {
  ncores <- parallel::detectCores() - 1
} 
print(ncores)
cl <- parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl)

# pkgs ---- 
library(treerabid)  
library(data.table)
library(lubridate)
library(foreach)
library(iterators)
library(doRNG)
library(igraph)

# simulate given expectation of kappa ----
set.seed(156)
sim_kappa <- 
  rbindlist(
    lapply(
      seq_len(1000),  
      function(x) {
        z <- runif(1, min = 0.01, max = 0.99)
        t_diff <- sim_times_pi(si_fun_lnorm, nobs = 3000, 
                               params = treerabid::params_treerabid, alpha = 0.01,
                               pi = z)
        ests_sorted <- fit_sims_pi(t_diff, nsims = 10, 
                                   candidate_pis = seq(0.01, 0.99, by = 0.01),
                                   si_fun_lnorm, params = treerabid::params_treerabid, 
                                   alpha = 0.01, seed = round(z) * 1000)
        ests_unsorted <- fit_sims_pi(t_diff, nsims = 10, 
                                   candidate_pis = seq(0.01, 0.99, by = 0.01),
                                   si_fun_lnorm, params = treerabid::params_treerabid, 
                                   alpha = 0.01, seed = round(z) * 1000, sort = FALSE)
        data.table(true = z, est_sorted = ests_sorted, 
                   est_unsorted = ests_unsorted,
                   sim = x, nobs = 3000, tree_type = "kappa")
      })
    )


# Testing against simulated data ----
cands <- list.files("java_output/Round3_1000/output_files", full.names = TRUE)
cands <- cands[grep("sim_cases", cands)]
samps <- sample(length(cands), 100)

# helper function to reconstruct closest detected link
get_detected_tdiffs <- function(case_dt) {

  ids_detected <- case_dt[detected == 1]$caseID
  case_dt[, progen_detected := parentID %in% ids_detected]
  
  detected <- case_dt[detected == TRUE]
  to_find <- detected[progen_detected == FALSE]

  gr <- get_graph(from = case_dt$parentID[case_dt$parentID != 0], 
                  to = case_dt$caseID[case_dt$parentID != 0],
                  attrs = data.table(unique(case_dt$caseID)))
  dist_mat <- distances(gr, v = as.character(to_find$caseID), 
                        to = as.character(detected$caseID), mode = "in")
  found <- apply(dist_mat, 
                 1, function(x) {
                   x <- x[x!= 0 & !is.infinite(x)]
                   if(length(x) < 1) {
                     0
                   } else {
                     as.numeric(names(which.min(x)))
                   }
                  })
  found <- data.table(data.frame(found), keep.rownames = TRUE)
  
  case_pairs <- rbind(found[, .(caseID = as.numeric(rn), progen_detected = as.numeric(found))], 
                      detected[progen_detected == TRUE][, .(caseID, progen_detected = parentID)])
  case_pairs <- case_dt[, .(caseID, case_date = dayInfectious)][case_pairs, on = "caseID"]
  case_pairs <- case_dt[, .(progen_detected = caseID, progen_date = dayInfectious)][case_pairs, on = "progen_detected"]
  case_pairs[, t_diff := case_date - progen_date]
  return(case_pairs$t_diff[!is.na(case_pairs$t_diff)])
}


system.time({
   sim_tree_recon <-
      foreach(i = samps, .combine = rbind) %do% {
        
        pi <- runif(1, 0.1, 0.95)
        case_dt <- fread(cands[i])
        case_dt[, detected := rbinom(.N, size = 1, prob = pi)]

        t_diff <- get_detected_tdiffs(case_dt)
        
        if(length(t_diff) > 0) {
          ests_sorted <- fit_sims_pi(t_diff = t_diff, 
                                     nsims = 10, 
                                     candidate_pis = seq(0.01, 0.99, by = 0.01),
                                     si_fun = treerabid::si_fun_lnorm, 
                                     params = treerabid::params_treerabid, 
                                     alpha = 0.01, 
                                     seed = round(pi * 1000))
          ests_unsorted <- fit_sims_pi(t_diff = t_diff, 
                                       nsims = 10, 
                                       candidate_pis = seq(0.01, 0.99, by = 0.01),
                                       si_fun = treerabid::si_fun_lnorm, 
                                       params = treerabid::params_treerabid, 
                                       alpha = 0.01, 
                                       seed = round(pi * 1000), 
                                       sort = FALSE)
          est_detect <- data.table(true = pi, 
                                   est_sorted = ests_sorted,
                                   est_unsorted = ests_unsorted,
                                   sim = i, 
                                   nobs = length(t_diff), 
                                   tree_type = "mod")
              
        } else {
          est_detect <- data.table(true = pi, 
                                   est_sorted = NA,
                                   est_unsorted = NA,
                                   sim = i, 
                                   nobs = 0, 
                                   tree_type = "mod")
        }
        
        est_detect
        
      }
  })

out_sim_detects <- rbind(sim_kappa, sim_tree_recon)


# Write out ----
parallel::stopCluster(cl)

# Write out files of the trees and the links (consensus & all)
fwrite(out_sim_detects, "output/trees/recovered_detect_ests.csv")

# Parse these from subutil for where to put things
syncto <- "~/Documents/Projects/Serengeti_Rabies/output/"
syncfrom <- "mrajeev@della.princeton.edu:Serengeti_Rabies/output/trees"

