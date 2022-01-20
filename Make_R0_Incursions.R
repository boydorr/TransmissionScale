# Make incursions to simulate multiple runs to compute R0, Rebecca Mancy, started 2021-08-10

rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr) # library(sp)
library(sp)

# setwd("~/Developer/Serengeti_Rabies/")
ff <- "java_output/"  #ff <- "/Volumes/SimOutput01/Rabies_Sim_Output/"
end.in.days <- 5112

f_LOWER_VAX <- "Round3_LOWER_VAX_1000"
f_R0_MULTI <- "Round3_R0_MULTI_1000"
f_R0_by_location <- "Round3_R0_by_location_1000"

n_runs <- 1000 

load("data/grd_1000.rda")
grd_pops <- grd.1000@data[, 3:ncol(grd.1000)]
sum_pops <- colSums(grd_pops); plot(1:168, sum_pops) # totals by time
month_bounds <- as.numeric(names(grd_pops))
sum_all <- sum(grd_pops)
tw_weight = sum_pops/sum_all # time-window weight
gc_weight = grd_pops[,1]/sum(grd_pops[,1]) # gridcell weight
xy_weight = rep(1,nrow(grd_pops))/nrow(grd_pops)
popID_levels <- grd.1000$popID

system.time({
  
for (i in 1:n_runs) {
  # Read in output from the LOWER_VAX scenario
  sim_output_LOWER_VAX <- read.csv(file = paste0(ff, f_LOWER_VAX, "/output_files/", f_LOWER_VAX, "_sim_cases_",sprintf("%03d",i),".csv"))
  n_incursions <-  nrow(sim_output_LOWER_VAX) 
  print(paste0("Run ", i, " with ", n_incursions, " cases"))  
  
  # Draw a gridcell and time window from grd.1000 by drawing proportional to grd_pops
  time_window_start <- as.numeric(sample(names(sum_pops), size = n_incursions, replace = T, prob = tw_weight))
  populations <- sample(grd.1000@data$popID, size = n_incursions, replace = T, prob = gc_weight)  
  # head(time_window_start); head(populations)
  
  incursion_cases_x <- numeric(n_incursions)
  incursion_cases_y <- numeric(n_incursions)
  
  # Draw random date within each population from above
  sample_days <- time_window_start + runif(n = n_incursions, min = 0, max = 28)
  
  # Draw random location within each population from above
  for (j in 1:n_incursions) {
    sample_coords <- spsample(x = subset(grd.1000, grd.1000@data$popID == populations[j]), n = 1, type = "random", iter = 5)
    incursion_cases_x[j] <- sample_coords@coords[1] 
    incursion_cases_y[j] <- sample_coords@coords[2] 
  }
  
  # Replace new dates and coordinates into sim_output_LOWER_VAX
  incursion_list <- sim_output_LOWER_VAX
  incursion_list[, 3:ncol(incursion_list)] <- -1
  incursion_list$popID <- populations # incursion_cases$popID
  incursion_list$x_coord <- incursion_cases_x # incursion_cases$x_coord
  incursion_list$y_coord <- incursion_cases_y # incursion_cases$y_coord
  incursion_list$dayInfectious <- sample_days # incursion_cases$dayInfectious
  # Transform all cases to being recorded as INCURSION type
  incursion_list$typeOfCase <- "INCURSION"
  
  # Write out to file
  print(paste0(ff, f_R0_MULTI, "/incursions/incursions_",sprintf("%03d",i),".csv"))
  write.table(incursion_list, file = paste0(ff, f_R0_MULTI, "/incursions/incursions_",sprintf("%03d",i),".csv"), 
              append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = "")
  
}
})

###############################
# PLOT OLD INCURSIONS (with N/S gradient)
# PLOT NEW INCURSIONS (by pop density)
end.in.days <- 5112

pdf(paste0(ff, f_R0_MULTI, "/incursions/incursionsTEST.pdf"))
par(mfrow = c(2,1), mar = c(2,2,1,1)) # set up plotting area

for (i in 1:n_runs) {
  print(i)
  #  sim_incursions <- read.csv(file = paste0(ff, f_R0_MULTI, "/incursions/incursions_",sprintf("%03d",i),".csv"))
  sim_incursions <- read.csv(file = paste0(ff, f_R0_by_location, "/incursions/incursions_",sprintf("%03d",i),".csv"))
  sim_incursions <- filter(sim_incursions, dayInfectious < end.in.days)
  sim_incursions$popID_Factor <- factor(sim_incursions$popID, levels = popID_levels)
  
  inc_summary <- sim_incursions %>%
    group_by(popID_Factor, .drop = F) %>%
    summarise(nCases = length(caseID))
  cases = sum(inc_summary$nCases)
  
  # plots for the first 100 runs to sanity check some outputs!
  if(i < 100) {
    hist(sim_incursions$dayInfectious, breaks = seq(0, end.in.days + 30.5, 30.5),
         main = paste0("Cases = ", cases))
    plot(sim_incursions$x_coord, sim_incursions$y_coord, cex = 0.1, col = alpha("black", 0.4))
  }
}
dev.off()

###############################


# now try to generate index cases by grid cell
system.time({
  
  for (i in 1:n_runs) {
  #for (i in 297:302) {
      # Read in output from the LOWER_VAX scenario
    sim_output_LOWER_VAX <- read.csv(file = paste0(ff, f_LOWER_VAX, "/output_files/", f_LOWER_VAX, "_sim_cases_",sprintf("%03d",i),".csv"))
    n_incursions <-  nrow(sim_output_LOWER_VAX) 
    print(paste0("Run ", i, " with ", n_incursions, " cases"))  
    
    # Draw a gridcell and time window from grd.1000 by drawing proportional to grd_pops
    time_window_start <- as.numeric(sample(names(sum_pops), size = n_incursions, replace = T, prob = tw_weight))
    populations <- sample(grd.1000@data$popID, size = n_incursions, replace = T, prob = xy_weight)  
    # head(time_window_start); head(populations)
    
    incursion_cases_x <- numeric(n_incursions)
    incursion_cases_y <- numeric(n_incursions)
    
    # Draw random date within each population from above
    sample_days <- time_window_start + runif(n = n_incursions, min = 0, max = 28)
    
    # Draw random location within each population from above
    for (j in 1:n_incursions) {
      sample_coords <- spsample(x = subset(grd.1000, grd.1000@data$popID == populations[j]), n = 1, type = "random", iter = 8)
      incursion_cases_x[j] <- sample_coords@coords[1] 
      incursion_cases_y[j] <- sample_coords@coords[2] 
    }
    
    # Replace new dates and coordinates into sim_output_LOWER_VAX
    incursion_list <- sim_output_LOWER_VAX
    incursion_list[, 3:ncol(incursion_list)] <- -1
    incursion_list$popID <- populations # incursion_cases$popID
    incursion_list$x_coord <- incursion_cases_x # incursion_cases$x_coord
    incursion_list$y_coord <- incursion_cases_y # incursion_cases$y_coord
    incursion_list$dayInfectious <- sample_days # incursion_cases$dayInfectious
    # Transform all cases to being recorded as INCURSION type
    incursion_list$typeOfCase <- "INCURSION"
    
    # Write out to file
    print(paste0(ff, f_R0_by_location, "/incursions/incursions_",sprintf("%03d",i),".csv"))
    write.table(incursion_list, file = paste0(ff, f_R0_by_location, "/incursions/incursions_",sprintf("%03d",i),".csv"), 
                append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = "")
    
  }
})

###############################
# PLOT OLD INCURSIONS (with N/S gradient)
# PLOT NEW INCURSIONS (by pop density)
end.in.days <- 5112

pdf(paste0(ff, f_R0_by_location, "/incursions/incursionsTEST.pdf"))
par(mfrow = c(2,1), mar = c(2,2,1,1)) # set up plotting area

for (i in 1:28) {
# for (i in 1:n_runs) {
  print(i)
#  sim_incursions <- read.csv(file = paste0(ff, f_R0_MULTI, "/incursions/incursions_",sprintf("%03d",i),".csv"))
  sim_incursions <- read.csv(file = paste0(ff, f_R0_by_location, "/incursions/incursions_",sprintf("%03d",i),".csv"))
  sim_incursions <- filter(sim_incursions, dayInfectious < end.in.days)
  sim_incursions$popID_Factor <- factor(sim_incursions$popID, levels = popID_levels)

  inc_summary <- sim_incursions %>%
    group_by(popID_Factor, .drop = F) %>%
    summarise(nCases = length(caseID))
  cases = sum(inc_summary$nCases)

  # plots for the first 100 runs to sanity check some outputs!
  if(i < 100) {
    hist(sim_incursions$dayInfectious, breaks = seq(0, end.in.days + 30.5, 30.5),
         main = paste0("Cases = ", cases))
    plot(sim_incursions$x_coord, sim_incursions$y_coord, cex = 0.1, col = alpha("black", 0.4))
  }
}
dev.off()









for (i in 1:n_runs) {
  # Read in output from the LOWER_VAX scenario
  sim_output_LOWER_VAX <- read.csv(file = paste0(ff, f_LOWER_VAX, "/output_files/", f_LOWER_VAX, "_sim_cases_",sprintf("%03d",i),".csv"))
  #### CHECK WITH A SPEEDIER VERSION #### sim_output_LOWER_VAX <- sim_output_LOWER_VAX[1:floor(nrow(sim_output_LOWER_VAX)/10),]
  n_incursions <-  nrow(sim_output_LOWER_VAX)
  print(paste0("Run ", i, " with ", n_incursions, " cases"))

  # Draw a gridcell and time window from grd.1000 by drawing proportional to grd_pops
  time_window_start <- as.numeric(sample(names(sum_pops), size = n_incursions, replace = T, prob = tw_weight))
  populations <- sample(grd.1000@data$popID, size = n_incursions, replace = T, prob = xy_weight)
  # THIS GENERATED SAMPLING BY ID NOT DENSITY ### populations <- sample(grd.1000@data$popID, size = n_incursions, replace = T, prob = grd.1000@data$popID/sum(grd.1000@data$popID, na.rm=T))
  # head(time_window_start); head(populations)

  incursion_cases_x <- numeric(n_incursions)
  incursion_cases_y <- numeric(n_incursions)

  sample_days <- time_window_start + runif(n = n_incursions, min = 0, max = 28)

  # Draw random date and location within each population from above
  for (j in 1:n_incursions) {
    #     max_days <- month_bounds[match(time_window_start[j], month_bounds)+1] #max_days <- month_bounds[which(month_bounds == as.numeric(time_window_start[j]))+1]
    #    if (is.na(max_days)) { max_days <- end.in.days }
    #    print(paste0("min = ", as.numeric(time_window_start[j]), " and max = ", max_days))
    # sample_days <- runif(n = 1, min = as.numeric(time_window_start[j]), max = max_days)
    sample_coords <- spsample(x = subset(grd.1000, grd.1000@data$popID == populations[j]), n = 1, type = "random", iter = 5)
    incursion_cases_x[j] <- sample_coords@coords[1] # sample_x =
    incursion_cases_y[j] <- sample_coords@coords[2] # sample_y =
    # if(nrow(incursion_cases) == 0) { incursion_cases <- data.frame(popID = populations[j], x_coord = sample_coords@coords[1], y_coord = sample_coords@coords[2], dayInfectious = sample_days[j]) } else {
    #   incursion_cases <- rbind(incursion_cases, data.frame(popID = populations[j], x_coord = sample_coords@coords[1], y_coord = sample_coords@coords[2], dayInfectious = sample_days[j]))}
  }

  # Replace new dates and coordinates into sim_output_LOWER_VAX
  incursion_list <- sim_output_LOWER_VAX
  incursion_list[, 3:ncol(incursion_list)] <- -1
  incursion_list$popID <- populations # incursion_cases$popID
  incursion_list$x_coord <- # incursion_cases$x_coord
    incursion_list$y_coord <- # incursion_cases$y_coord
    incursion_list$dayInfectious <- sample_days # incursion_cases$dayInfectious
  # Transform all cases to being recorded as INCURSION type
  incursion_list$typeOfCase <- "INCURSION"

  # Write out to file
  print(paste0(ff, f_R0_by_location, "/incursions/incursions_",sprintf("%03d",i),".csv"))
  write.table(incursion_list, file = paste0(ff, f_R0_by_location, "/incursions/incursions_",sprintf("%03d",i),".csv"),
              append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = "")

}

# TEST 238 lines - is this working?
  