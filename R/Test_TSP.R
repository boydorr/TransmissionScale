# Finding the shortest Hamiltonian path through all cities disregarding the endpoints can be achieved by inserting a ‘dummy city’ 
# which has a distance of zero to all other cities. The position of this city in the final tour represents the cutting point for the path. 

library("TSP")
data("USCA312")

# Plotting functions
# Thanks Rebecca! Makes sense – sadly not the method we were struggling to get working though, so doesn’t help us much :(   
# Also, just a heads up – by default the TSP is optimising for the shortest round trip.  
# So if you take that and cut it, it isn’t necessarily going to be the same result that you’d get if you were optimising 
# for the shortest route starting at a specified point and ending at any other unspecified point.  
# The folk who wrote the package recommend that for that, you should force all the distances back to the starting city to be 0, 
# so the final link in the round trip is irrelevant (see pp. 12-13 here: https://cran.r-project.org/web/packages/TSP/vignettes/TSP.pdf).  
# And I think cut_tour always cuts the link to the cut point, rather than the link from the cut point, without checking which is longer..

library("maps")
library("sp")
library("maptools")
data("USCA312_map")
plot_path <- function(path){
  plot(as(USCA312_coords, "Spatial"), axes = TRUE)
  plot(USCA312_basemap, add = TRUE, col = "gray")
  points(USCA312_coords, pch = 3, cex = 0.4, col = "red")
  path_line <- SpatialLines(list(Lines(list(Line(USCA312_coords[path,])), ID="1")))
  plot(path_line, add=TRUE, col = "black")
  points(USCA312_coords[c(head(path,1), tail(path,1)),], pch = 19, col = "black") 
}



#tsp <- insert_dummy(USCA312, label = "cut")
#tsp


# The first problem is to find the shortest Hamiltonian path starting with a given city. In this case, all distances to the selected city are set to zero, forcing the evaluation of all possible paths starting with this city and disregarding the way back from the final city in the tour. By modifying the distances the symmetric TSP is changed into an asymmetric TSP (ATSP) since the distances between the starting city and all other cities are no longer symmetric.
# As an example, we choose New York as the starting city. We transform the data set into an ATSP and set the column corresponding to New York to zero before solving it. Thus, the distance to return from the last city in the path to New York does not contribute to the path length. We use the nearest neighbor heuristic to calculate an initial tour which is then improved using 2-Opt moves and cut at New York to create a path.

atsp <- as.ATSP(USCA312)
ny <- which(labels(USCA312) == "New York, NY") # location of New York among the cities
atsp[, ny] <- 0
initial_tour <- solve_TSP(atsp, method="nn")
initial_tour

tour <- solve_TSP(atsp, method = "two_opt", control = list(tour = initial_tour))
tour

path <- cut_tour(tour, ny, exclude_cut = FALSE)
head(labels(path))
tail(labels(path))


# Plotting
plot_path(path)




# Now try one of our own, based on x-y locations

plot_path_SD <- function(path){
  plot(grd.1000, axes = TRUE, col = "grey50", border=NA) 
  points(all.xy, pch = 3, cex = 0.4, col = "red")
  path_line <- SpatialLines(list(Lines(list(Line(all.xy[path,])), ID="1")))
  plot(path_line, add=TRUE, col = "black")
  points(all.xy[1,], pch = 16, cex = 0.6, col="orange") # start location in orange!
}

plot_path_SD_No_Map <- function(path){
  plot(all.xy, pch = 3, cex = 0.4, col = "red")
  path_line <- SpatialLines(list(Lines(list(Line(all.xy[path,])), ID="1")))
  plot(path_line, add=TRUE, col = "black")
  points(all.xy[1,], pch = 16, cex = 1.5, col="orange") # start location in orange!
}

mother.xy <- data.frame(x = 670939.1, y = 9798185)
daughters.xy <- data.frame(x = c(670939.1, 670939.1, 670027.2), y = c(9798185, 9798185, 9797981))
daughters.xy <- data.frame(x = c(670939.1+rnorm(n = 1, sd = 500), 670939.1+rnorm(n = 1, sd = 500), 670027.2+rnorm(n = 1, sd = 500)), 
                           y = c(9798185+rnorm(n = 1, sd = 500), 9798185+rnorm(n = 1, sd = 500), 9797981+rnorm(n = 1, sd = 500)))
daughters.xy <- data.frame(x = c(670939.1+rnorm(n = 1, sd = 500), 670939.1+rnorm(n = 1, sd = 500), NA), 
                           y = c(9798185+rnorm(n = 1, sd = 500), 9798185+rnorm(n = 1, sd = 500), NA))
daughters.xy <- data.frame(x = NA, y= NA)
daughters.xy <- data.frame(x = c(670939.1), y = c(9798185))
daughters.xy <- data.frame(x = c(670939.1+rnorm(n = 1, sd = 500)), y = c(9798185+rnorm(n = 1, sd = 500)))

all.xy <- rbind(mother.xy, daughters.xy)
if(nrow(all.xy)==1) {
  all.xy$step.lengths <- NA
  return(all.xy)
}
# Separate out NA xy daughters
which.na <- is.na(daughters.xy$x) | is.na(daughters.xy$y)
daughters.na <- daughters.xy[which.na,]
daughters.xy <- daughters.xy[!which.na,]

# Remove locations that are the same as the mother location for animals considered unknown ... here,
# we will assume that the animal is known.

# Then, we need to create a distance matrix, with return distances to the mother equal to zero
between.dists <- dist(rbind(mother.xy, daughters.xy), method = "euclidean", diag = TRUE)
# 1 is always the mother, by ordering.

atsp <- as.ATSP(between.dists)
mother <- which(labels(atsp) == "1") # location of New York among the cities
atsp[, mother] <- 0 # Distances to mother location are set to zero
initial_tour <- solve_TSP(atsp, method="nn")
initial_tour

tour <- solve_TSP(atsp, method = "two_opt", control = list(tour = initial_tour))
tour

path <- cut_tour(tour, mother, exclude_cut = FALSE)
head(labels(path))
tail(labels(path))

# Plot the path identified
# plot_path_SD(path)
plot_path_SD_No_Map(path)

#step.lengths <- atsp[tour[1:(length(tour)-1)], tour[2:length(tour)]]
#atsp[tour, tour]

# Create a visit list of points visited
visit.list <- all.xy[path,]
step.lengths <- numeric()
for(rr in 1:(nrow(visit.list)-1)) {
  step.lengths <- c(step.lengths, atsp[path[rr], path[(rr+1)]])
}
visit.list$step.lengths <- c(NA, step.lengths)
visit.list <- rbind(visit.list, cbind(daughters.na, step.lengths=rep(NA, nrow(daughters.na))))
print(visit.list)
