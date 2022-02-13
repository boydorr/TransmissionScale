# Make case list for java ----
# NOTE - THIS MAY TAKE POINTS OFF THE ALLOCATED GRIDS DUE TO DE-IDENTIFYING DATA 

library(lubridate)
library(dplyr)
library(readr)
library(sp)
library(rgdal)
library(rgeos)

# for drawing locations reproducibly
set.seed(1354)

# Set end date for truncating data and read in shapefile
start.date <- as.Date("2002-01-01")
end.date <- as.Date("2015-12-31")
SD.2002 <- readOGR(dsn = "data", layer = "SD_Villages_2002_From_HHS_250m_Smoothed_UTM")
p4s <- CRS(proj4string(SD.2002))

# To do this, we need to use the canonical data
# This is the data with one record per rabid carnivore
# and later subset to remove any extraneous information
# rabid_carnivores <- readRDS(file = "output/clean_bite_data_canonical.rda")
# Deidentified data - NOTE THIS WILL HAVE JITTERED LOCATIONS
rabid_carnivores <- readRDS(file = "output/clean_bite_data_canonical_deid.rda")
rabid_carnivores$UTM.Easting <- rabid_carnivores$UTM.Easting.jitter
rabid_carnivores$UTM.Northing <- rabid_carnivores$UTM.Northing.jitter
nrow(rabid_carnivores) # 3281

plot(SD.2002)
points(rabid_carnivores$UTM.Easting, rabid_carnivores$UTM.Northing, col = "red")
points(subset(rabid_carnivores, ID == 3907)$UTM.Easting, subset(rabid_carnivores, ID == 3907)$UTM.Northing, col = "blue")

# --- Impute locations (dogs) ---
summary(rabid_carnivores$UTM.Easting) # 44 with no location information
summary(factor(subset(rabid_carnivores, is.na(UTM.Easting))$Species)) # species of those without UTMs
summary(factor(rabid_carnivores$Dog.density.incursions)) # NA 119 and zero density = 32
# There is one location just outside Serengeti District , so adjust slightly
nrow(subset(rabid_carnivores, ID == 3907))
rabid_carnivores$UTM.Easting <- ifelse(rabid_carnivores$ID == 3907, 703169.1, rabid_carnivores$UTM.Easting)

# Algorithm: Identify dogs with either missing UTM data or outside the area. 
# We then draw a location from among locations of dogs within the same village.
rabid_animals.with.locations <- subset(rabid_carnivores, !is.na(UTM.Easting))
nrow(rabid_animals.with.locations) # 3237

# Add villages according to 2002 spatial structure using shapefile
rabid_animals.with.locations$UTM.Easting.temp <- ifelse(is.na(rabid_animals.with.locations$UTM.Easting), -1, rabid_animals.with.locations$UTM.Easting)
rabid_animals.with.locations$UTM.Northing.temp <- ifelse(is.na(rabid_animals.with.locations$UTM.Northing), -1, rabid_animals.with.locations$UTM.Northing)
sp <- SpatialPoints(coords = cbind(rabid_animals.with.locations$UTM.Easting.temp, rabid_animals.with.locations$UTM.Northing.temp), proj4string = p4s)
cv <- over(sp, SD.2002) # in resulting dataframe, Village is the village in the grd ... (but IGNORE POINTS in this dataframe)

rabid_animals.with.locations$Vill_2002 <- cv$Vill_2002
rabid_animals.with.locations$UTM.Easting.temp <- NULL
rabid_animals.with.locations$UTM.Northing.temp <- NULL
summary(factor(rabid_animals.with.locations$Vill_2002))

# Work out the villages that have cases, for imputing
caseVillages <- data.frame(
  UTM.Easting = rabid_animals.with.locations$UTM.Easting,
  UTM.Northing = rabid_animals.with.locations$UTM.Northing,
  Vill_2002 = rabid_animals.with.locations$Vill_2002
)

matching.data <- rabid_carnivores
matching.data$UTM.Imputed <- FALSE

# Randomly select a case in the same village and allocate the UTM of this 
# (allocation relative to existing density of cases in this village)
cid.missing.locations <- subset(matching.data, is.na(UTM.Easting))
dim(cid.missing.locations)
for (rr in 1:nrow(cid.missing.locations)) {
  cid <- cid.missing.locations[rr, "Chain.ID"]
  cvill2002 <- cid.missing.locations[rr, "Village.2002"]
  print(paste0(rr, " ", cid, " ", cvill2002))
  if (cvill2002 != "") {
    vill.cases <- subset(caseVillages, Vill_2002 == cvill2002)
    if(nrow(vill.cases) == 0) browser()
    r.in.vill.cases <- runif(1, min = 1, max = nrow(vill.cases))
    r.in.caselist <- which(matching.data$Chain.ID == cid)
    matching.data$UTM.Easting[r.in.caselist] <- vill.cases[r.in.vill.cases, ]$UTM.Easting
    matching.data$UTM.Northing[r.in.caselist] <- vill.cases[r.in.vill.cases, ]$UTM.Northing
    matching.data$UTM.Imputed[r.in.caselist] <- T
  }
}
# Check all now complete
summary(matching.data$UTM.Easting) # All cases now have locations
summary(matching.data$UTM.Imputed) # 44 locations have been imputed

# Compute how many of these locations are outwith the area of the 1km^2 grid, to reallocate in Java
# join and filter to incursions at zero or NA densities
cases_to_correct <- 
  filter(matching.data, is.na(Dog.density.incursions) | Dog.density.incursions == 0)
nrow(cases_to_correct) # 151 cases

load("data/grd_500.rda")
load("data/grd_1000.rda")
grd_coords <- coordinates(grd.500)
grd_data <- grd.500@data
inds_ord <-
  apply(cases_to_correct, 1, 
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
          
          Dog.density <- ifelse(!is.na(x$Rabid) & x$Rabid &
                                  !(x$Owner %in% c("Not applicable", "Unknown") |
                                      substr(x$Species, start = 1, stop = 8) == "Wildlife"), 
                                Dog.density.incursions, NA)
          
          # Return the coordinates & density at that location
          return(data.frame(coords, Dog.density.incursions, Dog.density))
          
        })

# Plot check (can delete this part if Rebecca ok's)
plot(grd.500)
points(cases_to_correct$UTM.Easting, cases_to_correct$UTM.Northing, 
       col = "darkred") # before
cases_to_correct[, c("UTM.Easting", "UTM.Northing", "Dog.density.incursions", 
                     "Dog.density")] <- do.call("rbind", inds_ord)
points(cases_to_correct$UTM.Easting, cases_to_correct$UTM.Northing, 
       col = "red", pch = 16, cex = 0.5) # after
# Check that all incursions have been corrected and non are at zero-densities
all(cases_to_correct$Dog.density.incursions > 0) # TRUE

# Check that no reallocated introductions overlap with the lines of the other 
# gridded polygons
scales_test <- c(500, 1000, 2000, 4000, "all")
coords <- cbind(cases_to_correct$UTM.Easting, 
                cases_to_correct$UTM.Northing)
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
cases_corrected <- bind_rows(filter(matching.data, !(ID %in% cases_to_correct$ID)), 
                             cases_to_correct)
nrow(cases_corrected) == nrow(matching.data) # TRUE

# Work out how many carnivore and dog cases we have
nrow(cases_corrected) # 3281
table(cases_corrected$Species) # 3067 Domestic dogs

# Get incursions to pass to typeOfCase
# incs <- read_csv("output/incursions_java.csv")
incs <- read_csv("output/incursions_java_deid.csv")

# Make sure we have the right column names for Java...
md <- data.frame(caseID = cases_corrected$ID, 
                 ParentID = cases_corrected$Biter.ID, 
                 popID = 0, popName = 0, dogCount = 0, 
                 dogsE = 0, dogsV = 0, Th = -1, Td = -1, 
                 dogDensityK = 0, 
                 dayGenerated = ifelse(!is.na(cases_corrected$Date.bitten), 
                                       as.numeric(cases_corrected$Date.bitten - start.date), -1), 
                 serialInterval = -1, 
                 dayInfectious = as.numeric(cases_corrected$Symptoms.started - start.date),
                 infectiousPeriod = -1, 
                 x_coord = cases_corrected$UTM.Easting, 
                 y_coord = cases_corrected$UTM.Northing, 
                 strainID = 0, 
                 typeOfCase = ifelse(cases_corrected$ID %in% incs$caseID, 
                                     "INCURSION", "ENDOGENOUS"), 
                 nExcursions = 0, 
                 nDistinctDogs = cases_corrected$Dogs.bitten,
                 diedE = FALSE, 
                 nAbandonments = 0, 
                 nBitesE = 0, 
                 nBitesV = 0, 
                 nReinfections = 0, 
                 nFailedTransmissions = 0, 
                 nTransmissions = 0, 
                 mpDogCount = -1,
                 mpDogsE = -1,
                 mpDogsV = -1,
                 nDistinctS = -1,
                 nDistinctE = -1,
                 nDistinctV = -1,
                 Symptoms.started = 0, 
                 Month.symptoms.started = 0, 
                 Quarter.symptoms.started = 0
)


# Subset to the variables that we want to include for the file for the Java code
col_names <- c("caseID","ParentID","popID","popName","dogCount","dogsE","dogsV",	
               "Th","Td","dogDensityK","dayGenerated","serialInterval","dayInfectious","infectiousPeriod",	
               "x_coord","y_coord","strainID","typeOfCase",	
               "nExcursions","nDistinctDogs","diedE","nAbandonments","nBitesE","nBitesV","nReinfections",
               "nFailedTransmissions","nTransmissions","Symptoms.started","Month.symptoms.started",	
               "Quarter.symptoms.started")

# Check for any missing variable names
setdiff(col_names, names(md))
md <- md[col_names]
nrow(md) == nrow(cases_corrected)

# Write out the caselist that we use for matching
write.table(md, file = "output/d_cases_java_deid.csv", append = FALSE, quote = FALSE, 
            sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, 
            col.names = TRUE, fileEncoding = "")
dim(md) # 3281

# Tidy version of cases_corrected
cases_corrected <- cases_corrected %>% 
  select(ID, Biter.ID, Chain.ID, 
         UTM.Easting, UTM.Northing, 
         Animal, Dog.density, Dog.density.incursions, Species, 
         Symptoms.started, Dogs.bitten, Animals.bitten, Carnivores.bitten, date_col)
summary(cases_corrected)

# Write out to file (with original row names)
saveRDS(object = cases_corrected, file = paste0("output/matching_data_caselist_deid.rda"))
# cases_corrected <- readRDS(file = paste0("output/matching_data_caselist.rda"))



