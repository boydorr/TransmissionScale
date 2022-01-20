#' @title Computes travelling salesperson one-way distances
#' @description Computes travelling salesperson tour distance (full tour) based on a set of coordinates
#'    (don't need to check these are unique), uses library(TSP)
#' @param mother.xy A one-row dataframe with x, y variables giving location of "mother" case
#' @param daughters.xy A dataframe with x, y variables giving locations of "daughter" cases
#' @details Daughter locations do not need to be unique. In the returned value, the first entry of step.lengths
#'     is NA, so step.lengths gives the step length *to* the point on that row. Note that if there is more
#'     than one shortest tour, then only one is returned by ETSP, so we may not get the true shortest
#'     path once we remove the return part of the tour.
#' @return Returns a df with columns x, y, step.lengths, ordered so that the path is a shortest TSP tour, minus the return portion.
#' @importFrom stats dist
#' @import TSP
#' @export
addTSPDists <- function(mother.id, mother.xy, daughters.xy) {
  
  print(paste0("--------------",mother.id,"---------------"))
  print(rbind(mother.xy, daughters.xy))

  # Separate out NA xy daughters as they crash the TSP algorithms
  which.na <- is.na(daughters.xy$x) | is.na(daughters.xy$y)
  daughters.na <- daughters.xy[which.na,]
  daughters.xy <- daughters.xy[!which.na,]
  # Combine mothers and daughters for convenience
  all.xy <- rbind(mother.xy, daughters.xy)
  
  # Deal with mother.xy is NA, and other trivial cases
  if ( is.na(mother.xy$x) | is.na(mother.xy$y) ) return(NA) # Mother location NA, so return
  if ( nrow(daughters.xy) == 0 ) {
    mother.xy$step.lengths <- NA
    return (mother.xy) # No daughter cases so return mother location and NA step lengths
  }

  # Create a distance matrix between all points; 1 is always the mother, by ordering.
  between.dists <- dist(rbind(mother.xy, daughters.xy), method = "euclidean", diag = TRUE)

  # Create an asymmetric TSP from these distances
  atsp <- as.ATSP(between.dists)
  mother <- which(labels(atsp) == "1") # location of the mother among the points
  atsp[, mother] <- 0 # Distances to mother location are set to zero
  # Repeat a number of times and pick the shortest
  for (my.rep in 1:max(nrow(all.xy),20)) {
    initial_tour <- solve_TSP(atsp, method="nn")
    current.tour <- solve_TSP(atsp, method = "two_opt", control = list(tour = initial_tour))
    if (my.rep == 1) {
      tour <- current.tour
    } else {
      if ((tour_length(current.tour))<tour_length(tour)) {
        tour <- current.tour
      }
    }
  }
  
  # Create a path, cutting at the mother
  path <- cut_tour(tour, mother, exclude_cut = FALSE)
  
  # plot_path_SD_No_Map <- function(path){
  #   plot(all.xy, pch = 3, cex = 0.4, col = "red")
  #   path_line <- SpatialLines(list(Lines(list(Line(all.xy[path,])), ID="1")))
  #   plot(path_line, add=TRUE, col = "black")
  #   points(all.xy[1,], pch = 16, cex = 1.5, col="orange") # start location in orange!
  #   title(mother.id)
  # }
  #if(nrow(daughters.xy)>=4) {
  #  plot_path_SD_No_Map(path)   
  #}

  # Create a visit list of points visited
  visit.list <- all.xy[path,]
  step.lengths <- numeric()
  for(rr in 1:(nrow(visit.list)-1)) {
    step.lengths <- c(step.lengths, atsp[path[rr], path[(rr+1)]])
  }
  visit.list$step.lengths <- c(NA, step.lengths)
  # Add daughters with NA locations back in
  visit.list <- rbind(visit.list, cbind(daughters.na, step.lengths=rep(NA, nrow(daughters.na))))
  print(visit.list)
  return(visit.list)
}
