#' @title Add number of animals and dogs bitten
#' @description Adds animals and dogs bitten
#' @param animals
#' @importFrom plyr ddply
#' @export
addBiteCounts <- function(animals) {

  # Note that we compute the number of bites made per animal before subsetting on suspect dogs
  # because otherwise we would exclude non-suspect dogs as receivers of bites.
  animals$Animals.bitten <- 0; animals$Dogs.bitten <- 0; animals$Locations <- 0

  # To compute all the number of bites an animal made, we group all those with the same Biter.ID (removing Biter.ID == 0 as these are unknown[?] dogs)
  Animals.bitten.info <- subset( ddply(animals, .(Biter.ID), summarise, Animals.bitten = length(unique(ID))), Biter.ID != 0)

  # We then need to find out which animals they correspond to by ID
  Animals.bitten.info$animals.rows <- match(Animals.bitten.info$Biter.ID, animals$ID); dim(Animals.bitten.info)
  animals$Animals.bitten[Animals.bitten.info$animals.rows] <- Animals.bitten.info$Animals.bitten
  rm(Animals.bitten.info)

  # Do the same, but subsetting on bites made of Domestic dogs
  Dogs.bitten.info <- subset( ddply(subset(animals, Species=="Domestic dog"), .(Biter.ID), summarise, Dogs.bitten = length(unique(ID))), Biter.ID != 0)
  Dogs.bitten.info$animals.rows <- match(Dogs.bitten.info$Biter.ID, animals$ID); dim(Dogs.bitten.info)
  animals$Dogs.bitten[Dogs.bitten.info$animals.rows] <- Dogs.bitten.info$Dogs.bitten
  rm(Dogs.bitten.info)
  
  # # Do the same, but subsetting on bites made of Carnivore species
  # species_to_exclude <- c("Wildlife: Wildebeest", "Wildlife: Sykes monkey", "Wildlife: Monkey", "Wildlife: Baboon", "Other", "Human", "Unknown")
  # Carnivores.bitten.info <- subset( ddply(subset(animals, !(Species %in% species_to_exclude)), .(Biter.ID), summarise, Carnivores.bitten = length(unique(ID))), Biter.ID != 0)
  # Carnivores.bitten.info$animals.rows <- match(Carnivores.bitten.info$Biter.ID, animals$ID); dim(Carnivores.bitten.info)
  # animals$Carnivores.bitten[Carnivores.bitten.info$animals.rows] <- Carnivores.bitten.info$Carnivores.bitten
  # rm(Carnivores.bitten.info)

  # Compute the number of separate bite locations by animal
  Locations.info <- subset( ddply(animals, .(Biter.ID), summarise, Locations = length(unique(UTM.Easting))), Biter.ID != 0)
  Locations.info$animals.rows <- match(Locations.info$Biter.ID, animals$ID); dim(Locations.info)
  animals$Locations[Locations.info$animals.rows] <- Locations.info$Locations
  rm(Locations.info)

  return(animals)
}
