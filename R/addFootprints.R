# # Add footprints
# # We have to add footprints to all animals, and then subset (otherwise we miss bitees of different species, outside dates, etc.)
# animals.plus <- add.footprints(animals, Dog.biters.only = F, Dog.bitees.only = T) # returns only dogs if Dog.biters.only = T (i.e. subsets the df)
# print("Summary of footprint distances:")
# print(summary(animals.plus$animals$Footprint.total.dist))
# return(animals.plus)

#' @title Adds information on bite "footprints" (NB: other Domestic dogs bitten)
#' @description Adds information on bite footprints into dataframe
#' @param animals A dataframe with columns ID, Biter.ID, Species, Northing, Easting
#' @return The dataframe, with additional information, total number of steps and mean step length
#'     This has additional fields: Footprint.x, Footprint.y, Footprint.step.lengths, each of which is a list of numeric
#'     NB: Footprint.x, Footprint.y, Footprint.step.lengths might be shorter than Locations (or even Animals.bitten). This arises 
#'         when some animals are bitten at the mother location, if mother unknown or wildlife. These are removed as step lengths are
#'         then unreliable because these animals were recorded not at their home location, but where they were first observed biting.
#' @export
addFootprints <- function(animals) {

  tot.steps <- 0
  tot.dist <- 0
  rows.to.do <- nrow(animals)
  animals$Footprint.x <- NA
  animals$Footprint.y <- NA
  animals$Footprint.step.lengths <- NA
  animals$Footprint.total.dist <- NA
  animals$Footprint.median.dist <- NA
  animals$Footprint.max.dist <- NA
  animals$Footprint.mean.dist <- NA # set up storage

  # Loop over rows in df
  for (rr in 1:rows.to.do) {
    #print(rr)
    mother <- animals[rr,] # "mother" = biting dog
    cid <- mother$ID # id of current dog under consideration (i.e. of the "mother")

    # Continue if we have a location for the mother dog    
    if (!is.na(mother$UTM.Easting) & !is.na(mother$UTM.Northing)) {
      # Get locations for mother and daughter cases
      daughter.rows <- (!is.na(animals$Biter.ID) & animals$Biter.ID == cid)
      daughters <- animals[daughter.rows,]
      mother.xy <- data.frame(x=mother$UTM.Easting, y=mother$UTM.Northing)
      daughters.xy <- data.frame(x=daughters$UTM.Easting, y=daughters$UTM.Northing)
      
      # Remove all points at mother location if animal is wildlife or owner unknown (i.e. remove all zero distances from progenitor location)
      if (mother$Owner %in% c("Not applicable","Unknown") | substr(mother$Species, start = 1, stop = 8) == "Wildlife") {
        print(paste0("Got one unknown/wildlife ", mother$Species, " ", mother$Owner, " ID=", mother$ID))
        daughters.xy <- subset(daughters.xy, x!=mother.xy$x & y!=mother.xy$y)
      }

      # Get all information about bitten animals for each "mother"
      footprint <- addTSPDists(mother.id = cid, mother.xy = mother.xy, daughters.xy = daughters.xy)
      
      # Increment total step count and total distance travelled by all animals
      tot.steps <- tot.steps + nrow(footprint) - 1
      tot.dist <- tot.dist + sum(footprint$step.lengths, na.rm = T)

      if (nrow(footprint) > 1) { # at least one step to bite recorded
        animals$Footprint.x[rr] <- I(list(footprint$x))
        animals$Footprint.y[rr] <- I(list(footprint$y))
        animals$Footprint.step.lengths[rr] <- I(list(footprint$step.lengths))
        animals$Footprint.total.dist[rr] <- sum(footprint$step.lengths, na.rm=T)
        animals$Footprint.mean.dist[rr] <- mean(footprint$step.lengths, na.rm=T)
        animals$Footprint.median.dist[rr] <- median(footprint$step.lengths, na.rm=T)
        animals$Footprint.max.dist[rr] <- max(footprint$step.lengths, na.rm=T)
      }
    }
  }
  mean.step <- tot.dist / tot.steps
  return(list(animals=animals, tot.steps=tot.steps, mean.step = mean.step))
}
