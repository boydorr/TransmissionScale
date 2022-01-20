#' @title Compiles step lengths into a single list (for parameter estimation)
#' @description Compiles step lengths into a single list for parameter estimation
#' @param animals.with.footprints ACT data for animals with footprints (i.e. that went on to bite)
#' @param tot.steps Total number of steps made
#' @export
compileSteps <- function(animals.with.footprints, tot.steps) {
  all.step.lengths <- data.frame(Symptoms.started = rep(as.Date("2000-01-01"), tot.steps), step.length = rep(0, tot.steps)) # this is probably too long if tot.steps computed on more than just dogs; we reduce later
  rows.to.do <- nrow(animals.with.footprints)
  asl.rr <- 1
  for (rr in 1:rows.to.do) { # loop over animals data.frame
    ### print(paste0( rr, " out of ", rows.to.do, " animals and currently got to new step.length number ",asl.rr, " out of ", nrow(all.step.lengths)))
    csl <- unlist(animals.with.footprints[rr, "Footprint.step.lengths"]) # Get all step lengths
    csl <- csl[!is.na(csl)] # Remove any NAs (including the first of these)
    ### print(csl); print(length(csl)); print(class(csl))
    if (length(csl)>0) {
      all.step.lengths$step.length[asl.rr : (asl.rr + length(csl) - 1)] <- csl
      all.step.lengths$Symptoms.started[asl.rr : (asl.rr + length(csl) - 1)] <- animals.with.footprints[rr, "Symptoms.started"]
      asl.rr <- asl.rr + length(csl)
      ### print(all.step.lengths)
      ### print(asl.rr)
    }
  }
  # Remove unused rows of df
  all.step.lengths <- data.frame(all.step.lengths[1:asl.rr,])
  print(paste0("Total number of step lengths is ", nrow(all.step.lengths)))
  return(all.step.lengths)
}
