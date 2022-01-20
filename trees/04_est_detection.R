# Estimating detection probabilities from trees ----

# sub_cmd:=-t 2 -n 5 -jn test -wt 1m -sn

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
library(foreach)
library(iterators)
library(doRNG)

# Estimate detection ----
comp_times_dist <- fread("output/trees/trees_sampled_best.gz")
comp_times_unpruned <- fread("output/trees/trees_sampled_unpruned.gz")
known <- unique(comp_times_dist$id_case[comp_times_dist$type == "traced"])
comp_times_dist <- rbind(comp_times_dist[, c("t_diff", "sim", "mcc", "id_case", "majority", "cutoff")],
                         comp_times_unpruned[, c("t_diff", "sim", "mcc", "id_case", "majority", "cutoff")])

comp_times_dist[, tree_type := fcase(majority == 1, "Majority", 
                                     mcc == 1, "MCC", 
                                     majority == 0 & mcc == 0, 
                                     "Random sample")]
comp_times_dist[, known_kappa := ifelse(id_case %in% known, 1, 0)]
comp_times_dist[, levs := interaction(sim, tree_type, 
                                      cutoff,
                                      drop = TRUE)]
comp_times_dist <- comp_times_dist[!is.na(t_diff)]
t_diffs <- split(comp_times_dist,
                 comp_times_dist$levs)

system.time({
  est_detect <-
    foreach(k = iter(t_diffs), .combine = rbind) %do% {
      
      ests <- fit_sims_pi(t_diff = k$t_diff, 
                          nsims = 100, 
                          candidate_pis = seq(0.01, 0.99, by = 0.01),
                          si_fun = treerabid::si_fun_lnorm, 
                          params = treerabid::params_treerabid, 
                          alpha = 0.01, 
                          known_kappas = k$known_kappa,
                          seed = as.numeric(k$levs[1])
                          )
      data.table(detection = ests, 
                 k[1, -c("known_kappa", "id_case", "t_diff", "levs")], 
                 nobs = nrow(k))
      
    }
})

# Each tree lookup ----
parallel::stopCluster(cl)

# Write out files of the trees and the links (consensus & all)
fwrite(est_detect, "output/trees/detection_ests.csv")

# Parse these from subutil for where to put things
syncto <- "~/Documents/Projects/Serengeti_Rabies/output/"
syncfrom <- "mrajeev@della.princeton.edu:Serengeti_Rabies/output/trees"
