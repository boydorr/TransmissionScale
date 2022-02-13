# Step 5: Generate incursions for java ----
# NOTE - THIS MAY TAKE POINTS OFF THE ALLOCATED GRIDS DUE TO DE-IDENTIFYING DATA

library(lubridate)
library(dplyr)
library(sp)
library(rgdal)
library(rgeos)

start.date <- as.Date("2002-01-01")
end.date <- as.Date("2015-12-31")

# Read in incursions as identified by treerabid (June 2020)
# NOTE file is as long as the rabies data without duplicates, but
# with cases without known coordinates which will be reallocated to 
# locations in step 6 and which we are essentially assuming are not incursions 
incursions <- read.csv(file = "output/trees/incursions_updated.csv", stringsAsFactors = FALSE)
nrow(incursions) # 3281 rows 

# Subset on incursions by identification threshold
# uses lognormal for the serial interval & weibull for the distance kernel
# incursions_best_0.95 == TRUE for the 95% cutoff
# incursions_best_0.975 == TRUE for the 97.5% cutoff
sum(incursions$incursions_best_0.95) #  N = 238
sum(incursions$incursions_best_0.975) # N = 107
incursions <- subset(incursions, incursions_best_0.95 == TRUE); nrow(incursions) 
summary(factor(incursions$Suspect)) # very small number marked "To Do"
names(incursions)

# Read in the canonical data with densities (and with ids filtered to one unique
# record per ID)
# biting_animals <- readRDS("output/clean_bite_data_canonical.rda")
biting_animals <- readRDS("output/clean_bite_data_canonical_deid.rda")
biting_animals$UTM.Easting <- biting_animals$UTM.Easting.jitter
biting_animals$UTM.Northing <- biting_animals$UTM.Northing.jitter

dim(biting_animals) # 3281 

# join and filter to incursions at zero or NA densities
incursions <- left_join(incursions, biting_animals)
incs_to_correct <- 
  filter(incursions, is.na(Dog.density.incursions) | Dog.density.incursions == 0)
nrow(incs_to_correct) # 10 cases

load("data/grd_500.rda")
load("data/grd_1000.rda")
grd_coords <- coordinates(grd.500)
grd_data <- grd.500@data
inds_ord <-
  apply(incs_to_correct, 1, 
       function(x) {
         # Euclidean distance (utm so ok)
         dists <- sqrt((grd_coords[, 1] - x$UTM.Easting)^2 + (grd_coords[, 2] - x$UTM.Northing)^2)
         
         # ordered by closest (default is min -> max)
         dist_inds <- order(dists) 
         
         # Find the closest which also has non-zero density
         dens_ind <- dist_inds[which(grd_data[dist_inds, x$date_col] > 0)[1]]
         
         coords <- cbind(UTM.Easting = grd_coords[dens_ind, 1], 
                         UTM.Northing = grd_coords[dens_ind, 2])
         
         loc_1000 <- sp::over(SpatialPoints(coords, proj4string = grd.1000@proj4string), 
                              grd.1000)
         
         Dog.density.incursions <-
           as.numeric(loc_1000[, x$date_col])
         
         # Return the coordinates & density at that location
         return(data.frame(coords, Dog.density.incursions))
})

# Plot check (can delete this part if Rebecca ok's)
plot(grd.500)
points(incs_to_correct$UTM.Easting, incs_to_correct$UTM.Northing, 
       col = "darkred") # before
incs_to_correct[, c("UTM.Easting", "UTM.Northing", "Dog.density.incursions")] <- do.call("rbind", inds_ord)
points(incs_to_correct$UTM.Easting, incs_to_correct$UTM.Northing, 
       col = "red", pch = 16, cex = 0.5) # after
# Check that all incursions have been corrected and none are at zero-densities
all(incs_to_correct$Dog.density.incursions > 0) # TRUE

# Check that no reallocated introductions overlap with the lines of the other 
# gridded polygons
scales_test <- c(500, 1000, 2000, 4000, "all")
coords <- cbind(incs_to_correct$UTM.Easting, 
                              incs_to_correct$UTM.Northing)
test <- 
  lapply(scales_test, 
       function(x) {
         name <- load(paste0("data/grd_", x, ".rda"))
         grd <- get(name)
         lines <- as(grd, "SpatialLines")
         test <- rgeos::gIntersects(SpatialPoints(coords, 
                                                  proj4string = lines@proj4string),
                                    lines)
       })

if (!all(unlist(test) == FALSE)) {
  stop("Some points were reallocated to the edge of two grid cells, double check
       the allocation code")
}

# Join back up 
incs_corrected <- bind_rows(filter(incursions, !(ID %in% incs_to_correct$ID)), 
                                   incs_to_correct)
nrow(incs_corrected) == nrow(incursions) # TRUE

# Output per Rebecca's file structure
incursions.reallocated <- 
  data.frame(caseID = incs_corrected$ID,  
             Th = -1, Td = -1, dayGenerated = -1, 
             typeOfCase = "INCURSION", 
             x_coord = incs_corrected$UTM.Easting, 
             y_coord = incs_corrected$UTM.Northing, 
             Symptoms.started = incs_corrected$Symptoms.started,
             dogDensityK = 0,
             ParentID = 0, popID = 0, popName = 0, dogCount = 0, 
             dogsE = 0, dogsV = 0, 
             serialInterval = 0, 
             dayInfectious = as.numeric(incs_corrected$Symptoms.started - start.date), 
             infectiousPeriod = 0, 
             strainID = seq(1, (nrow(incs_corrected))), 
             nExcursions = 0, nDistinctDogs = 0, 
             diedE = 0, 
             nAbandonments = 0, 
             nBitesE = 0, 
             nBitesV = 0	, 
             nReinfections = 0, 
             nFailedTransmissions = 0, 
             nTransmissions = 0, 
             Month.symptoms.started = 0, 
             Quarter.symptoms.started = 0)

# MR: adding this line to make sure I can match up incursions in the simulated
# outputs for comparing to empirical trees
readr::write_csv(incursions.reallocated[, c("caseID", "strainID")], "output/inc_lookups_deid.csv")

# Subset to the variables that we want to include for the file for the Java code
col_names <- c("caseID","ParentID","popID","popName","dogCount","dogsE","dogsV",	
               "Th","Td","dogDensityK","dayGenerated","serialInterval","dayInfectious","infectiousPeriod",	
               "x_coord","y_coord","strainID","typeOfCase",	
               "nExcursions","nDistinctDogs","diedE","nAbandonments","nBitesE","nBitesV","nReinfections",
               "nFailedTransmissions","nTransmissions","Symptoms.started","Month.symptoms.started",	
               "Quarter.symptoms.started")

# Check for any missing variable names
setdiff(col_names, names(incursions.reallocated))
inc <- incursions.reallocated[col_names]
nrow(inc) == nrow(incursions)

# Write csv of incursions to file
# write.table(inc, file = "output/incursions_java.csv", append = FALSE, 
#             quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", 
#             row.names = FALSE, col.names = TRUE, fileEncoding = "")
write.table(inc, file = "output/incursions_java_deid.csv", append = FALSE,
            quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".",
            row.names = FALSE, col.names = TRUE, fileEncoding = "")

