summarize_chains <- function(x, sim_files) {
  sim <- fread(sim_files[x])
  sim$t <- sim$dayInfectious + ymd("2002-01-01")
  chain_stats <- sim[, .(size = .N, days = as.numeric(max(t) - min(t))), by = "strainID"]
  gr <- 
    igraph::graph_from_data_frame(d = sim[, c("parentID", "caseID")][parentID != 0], 
                                  vertices = sim[, c("caseID", "t", "strainID")], 
                                  directed = TRUE)
  chains <- igraph::decompose(gr)
  lengths <- unlist(lapply(chains, igraph::diameter))
  ID <- unlist(lapply(chains, function(x) igraph::vertex_attr(x)$strainID[1]))
  chain_stats[, length := lengths[strainID == ID]]
  chain_stats$sim <- x
  return(chain_stats) 
}

summarize_re <- function(x, sim_files) {
  sim <- fread(sim_files[x])
  sim$ntrans <- tabulate(sim$parentID, nbins = max(sim$caseID))[sim$caseID]
  sim$ntrans[is.na(sim$ntrans)] <- 0
  sim$sim <- x
  return(sim[, c("ntrans", "sim", "caseID", "strainID", "typeOfCase")])
}

summarize_sims <- function(dirs = c(vax = "java_output/Round3_1000/output_files/", 
                                    low_vax = "java_output/Round3_LOWER_VAX_1000/output_files/", 
                                    no_het = "java_output/Round3_WO_DOG_VARIATION_1000/output_files/"), 
                           apply_fun = summarize_chains, 
                           nsample = 100) {
  out <- list()
  
  for(i in seq_len(length(dirs))) {
    sim_files <- list.files(dirs[i], full.names = TRUE)
    sim_files <- sim_files[grep("sim_cases", sim_files)]
    nsim <- sample(length(sim_files), nsample)
    sim_outs <-
      rbindlist(
        mclapply(nsim, function(x) apply_fun(x, sim_files = sim_files)) 
      ) 
    sim_outs$sim_type = names(dirs)[i]
    out[[i]] <- sim_outs
  }
  return(out)
}