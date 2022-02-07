#' Step 1: Data cleaning ----
#' Code provided but data contains identifying information so not included
#' Instead deidentified data provided for running subsequent steps

# Pkgs ----
library(lubridate)
library(plyr)
library(TSP)
library(sp)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(readr) 
library(glue) 
library(here) # for managing paths relative to the project repository

options(stringsAsFactors = F)
# So this function doesn't get masked by spatial functions
select <- dplyr::select

start.date <- as.Date("2002-01-01")
end.date <- as.Date("2015-12-31")

# import data for parameterization ----
# generating step length distribution
# biting_animals <- readRDS("output/clean_bite_data_no_densities_deid.rda") 

# Build the footprint data ----
# Add locations (in UTMs) at which bites took place 
# i.e. ("footprint" of biting activity); used later for computing step lengths;
source(here("R/addFootprints.R"))
source(here("R/addTSPDists.R"))

# Set seed here to make sure footprints are reproducible
set.seed(142)
step_info <- addFootprints(animals = biting_animals) # returns a list - animals, tot.steps, mean.step
logtest_total_steps <- step_info$tot.steps
logtest_mean_steplength_meters <- step_info$mean.step

# Do checks on biting animals 
logtest_nrow_nodensities_equals_footprints <- nrow(biting_animals) == nrow(step_info$animals)
biting_animals <- step_info$animals

# Save data with footprints (because the above step is fairly slow) ----
### saveRDS(object = biting_animals_deid_footprints, here(paste0("output/clean_bite_data_with_footprints_deid.rda")))

# Subset to rabid carnivores ----
# add information on whether the species is carnivorous
biting_animals$Carnivore <- ifelse(substr(biting_animals$Species, 1, 9) != "Livestock" & 
                                     !biting_animals$Species %in% species_to_exclude, 
                                   TRUE, FALSE)
logtest_animals_nrabid <- nrow(subset(biting_animals, Rabid == TRUE & 
                                        Symptoms.started <= end.date))
write.csv(table(subset(biting_animals, Rabid == TRUE & Symptoms.started <= end.date)$Species), "output/all_rabid_no_filter.csv") 

logtest_dogs_nrabid <- nrow(subset(biting_animals, Rabid == TRUE & 
                                     Species == "Domestic dog" &
                                     Symptoms.started <= end.date))
rabid_carnivores <- subset(biting_animals, Carnivore & Rabid)

# Subset on end date, having added step information (to "offspring" cases)
rabid_carnivores <- subset(rabid_carnivores, Symptoms.started <= end.date)

# Check for duplicated IDs (i.e. ones that appear more than once because
# multiple known biters)
rabid_carnivores$Duplicated <- duplicated(rabid_carnivores$ID)
logtest_multiple_biters <- sum(rabid_carnivores$Duplicated) 

# One manual fix 
# 514 infecting 526 seems unlikely as their symptoms started on same day, so
# remove that link. Otherwise no way of telling which bite was the transmission
# event, so just leave both records and determine which is most likely through
# transmission trees
rabid_carnivores <- rabid_carnivores[which(rabid_carnivores$Chain.ID != "514_526"), ]
logtest_rabid_carnivores <- nrow(rabid_carnivores) 

# report cases in rabid carnivores that are wildlife 
# and rabid domestic animals where the owner was unknown 
logtest_unknown_carnivores <- sum(rabid_carnivores$Owner != "Known")
logtest_unknown_domestic_animals <- sum(rabid_carnivores$Owner == "Unknown")
logtest_rabid_wild_carnivores <- length(grep("Wildlife", rabid_carnivores$Species))
table(rabid_carnivores$Species[which(rabid_carnivores$Owner == "Unknown")])
write.csv(table(rabid_carnivores$Species), "output/rabid_carnivores_spp.csv") 

# And where the biter was known/ unknown (and the percentage of links)
logtest_known_biter <- sum(rabid_carnivores$Biter.ID > 0)
logtest_known_biter_pc <- sum(rabid_carnivores$Biter.ID > 0)/ nrow(rabid_carnivores)

# Save output of rabid carnivores with footprints ----
#### saveRDS(object = biting_animals_deid_footprints, 
###         here(paste0("output/rabid_carnivores_with_footprints_deid.rda")))

# Add dog densities ----
# information on the dog density at the case location using the 1km^2 scale
load(file = "data/grd_1000.rda")
p4s <- proj4string(grd.1000)
days_cols <- as.numeric(colnames(grd.1000@data)[3:ncol(grd.1000@data)])
mean_inc <- as.numeric(mean(rabid_carnivores$Symptoms.started - rabid_carnivores$Date.bitten, 
                            na.rm = TRUE)) # mean incubation period
rabid_carnivores$row_id <- 1:nrow(rabid_carnivores)

rabid_carnivores <-
  rabid_carnivores %>%
  mutate(days_since_start = ifelse(!is.na(Symptoms.started),
                                   as.numeric(Symptoms.started - start.date),
                                   as.numeric(Date.bitten + mean_inc - start.date)), 
         date_col = findInterval(days_since_start, days_cols) + 2) 
rabid_densities <- filter(rabid_carnivores, 
                          !is.na(UTM.Easting) & !is.na(UTM.Northing) & 
                          !is.na(days_since_start))

overlay_points <- sp::over(SpatialPoints(cbind(rabid_densities$UTM.Easting, 
                                               rabid_densities$UTM.Northing), 
                                         proj4string = CRS(p4s)), grd.1000)

rabid_densities$Dog.density.incursions <-
  as.numeric(overlay_points[cbind(1:nrow(rabid_densities), rabid_densities$date_col)])
# NB: Dog.density is only filled in if a known animal (for case list, and thus distribution of dog densities)
#     Dog.density.incursions is added for *all* cases considered rabid at this point.
rabid_carnivores <-
  rabid_densities %>%
  mutate(Dog.density = ifelse(!is.na(Rabid) & Rabid &
                              !(Owner %in% c("Not applicable", "Unknown") |
                                substr(Species, start = 1, stop = 8) == "Wildlife"), 
                              Dog.density.incursions, 
                              NA)) %>%
  select(row_id, Dog.density, Dog.density.incursions) %>%
  right_join(rabid_carnivores) %>%
  select(-row_id, -days_since_start)

# # Reality check that densities look consistent with the densities in the shapefile
# SD <- readOGR(dsn = "data", layer = "SD_Villages_2002_From_HHS_250m_Smoothed_UTM")
# grd = grd.1000; resolution = 1000
# bb <- data.frame(bbox(grd))
# grd.df = data.table(fortify(grd, data = grd@data$`0`))
# grd.df = data.table(fortify(grd))
# grd.data <- data.table(cbind(grd@data), id = rownames(grd@data))
# grd.data$pop <- grd.data$`2557` / (resolution/1000) # compute population density from carrying capacity
# setkey(grd.df,id)
# setkey(grd.data, id)
# grd.df[grd.data, pop:=pop] # Note that for this to work inside the package function, need to add data.table to Imports: and Depends: in DESCRIPTION file
# # See https://stackoverflow.com/questions/27980835/r-data-table-works-in-direct-call-but-same-function-in-a-package-fails
# SD.df <- data.table(fortify(SD))
# # Plot the map with cases overlaid
# p.grd <- ggplot(grd.df, aes(x = long, y = lat)) +
#   geom_map(map = grd.df, aes(map_id=id, fill = pop)) +
#   scale_fill_gradient(low = "white", high = sc.colours$grey, "Population\ndensity") + # Population density at 1x1km scale
#   geom_polygon(data = SD.df, aes(x = long, y = lat, group=id), fill=NA, colour=sc.colours$grey, size=0.2) + # villages!!
#   geom_point(data = rabid_carnivores, aes(x = UTM.Easting, y = UTM.Northing,colour = is.na(Dog.density.incursions)), shape = 20, size = 0.4, alpha = 0.7) + # Cases
#   coord_equal() + xlab("") + ylab("") +
#   theme_classic() + theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(), legend.position=c(0.95,0.83)) +
#   scalebar(grd.df, dist = 10, dist_unit = "km", location = "topleft", anchor = c(x=bb$min[1], y=bb$min[2])-1500, height = 0.01, st.size = 3, transform = F)
# p.grd
# summary(rabid_carnivores$Dog.density.incursions) # 170 in locations with no dogs
# head(subset(rabid_carnivores, is.na(Dog.density.incursions))[c("Dog.density","Dog.density.incursions","UTM.Easting","UTM.Northing", "Rabid","Symptoms.started","Date.bitten")], 20)

# Check whether step lengths differ by dog density at mother location
complete_cases <- subset(rabid_carnivores, !is.na(Dog.density))
logtest_ncases_with_densities <- nrow(complete_cases) 
logtest_cor_dogdensity_steplength <- round(cor.test(complete_cases$Dog.density, 
                                              complete_cases$Footprint.mean.dist)$estimate,
                                           5)
mod <- lm(Footprint.mean.dist ~ Dog.density + I(Dog.density^2), data = complete_cases)
summary(mod)

# This is the mean-of-means of step lengths
# Mean step length is shorter than size of even the 500x500m cells and correlation is very weak, at least on this raw data.
# Therefore inclusion of the effect of leaving the cell will be marginal given scale of simulations.
logtest_mean_footprint_meters <- round(mean(complete_cases$Footprint.mean.dist, 
                                      na.rm = TRUE), 5)

# ggplot(data = complete_cases) +
#   geom_point(aes(x = Dog.density, y = Footprint.mean.dist)) +
#   geom_smooth(aes(x = Dog.density, y = Footprint.mean.dist)) +
#   scale_x_sqrt() +
#   scale_y_sqrt()

# Adding one additional step for Biter.IDs that are not actually in the case list
# I.e. ones where there are no dates or locations corresponding to a traced biter
logtest_biterIDs_not_in_caselist <- nrow(filter(rabid_carnivores, !(Biter.ID %in% ID) & Biter.ID != 0))

# Save the data
# saveRDS(object = rabid_carnivores_deid, here(paste0("output/clean_bite_data_deid.rda")))

# Also write out the canonical bite data (i.e. with one record per rabid carnivore)
# Selecting first record
rabid_carnivores %>%
  group_by(ID) %>%
  dplyr::slice(1) %>%
  as.data.frame() -> ct_canonical
# saveRDS(object = ct_canonical, here(paste0("output/clean_bite_data_canonical_deid.rda")))

logtest_rabid_carnivores_bitten_animals <- sum(rabid_carnivores$Animals.bitten)

# Clean up human exposure data ----
# In Serengeti
# humans <- read.csv(here("data/Tanzania_Human_CT.csv"), stringsAsFactors = FALSE)
humans <- subset(humans, District == "Serengeti")
logtest_humans_in_serengeti <- nrow(humans)

# Subset to dates
humans$Date.bitten <- as.Date(as.character(humans$Date.bitten), format = "%d-%b-%Y")
humans$Date.reported <- as.Date(as.character(humans$Date.reported), format = "%d-%b-%Y")
no_date_humans <- which(is.na(humans$Date.bitten) & is.na(humans$Date.reported))
humans <- humans[-no_date_humans, ]
humans <- subset(humans, (Date.reported > start.date) | (Date.bitten > start.date) | (Date.reported < end.date) | (Date.bitten < end.date))

# Check row numbers and save output for humans ----
logtest_humans_bitten <- nrow(humans) 
logtest_humans_exposed <- nrow(subset(humans, Rabid %in% "Yes"))
logtest_humans_exposed_bydogs <- nrow(subset(humans, Attacking.species == "Domestic dog" & Rabid %in% "Yes"))
logtest_humans_rabies_deaths <- nrow(subset(humans, Cause.of.death == "Rabies" & Rabid %in% "Yes"))


# check the household census data
# dogpop <- read.csv("output/dog_distribution.csv", stringsAsFactors = FALSE)
# humanpop <- read.csv("output/human_distribution.csv", stringsAsFactors = FALSE)
# pop <- read.csv("output/SDcompiled.csv", stringsAsFactors = FALSE)
range(pop$dogs, na.rm=TRUE)
range(pop$pups, na.rm=TRUE)
range(pop$cats, na.rm=TRUE)
range(pop$Adults, na.rm=TRUE)
range(pop$Children, na.rm=TRUE)

sum(pop$dogs) + sum(pop$pups)
sum(pop$dogs)
sum(pop$pups)
sum(pop$cats, na.rm=TRUE)
sum(pop$kittens, na.rm=TRUE)
sum(pop$cats, na.rm=TRUE) + sum(pop$kittens, na.rm=TRUE)

sum(pop$Adults) + sum(pop$Children)
sum(pop$Adults)
sum(pop$Children)

nrow(dogpop)
nrow(pop)
(sum(pop$Adults) + sum(pop$Children)) / (sum(pop$dogs) + sum(pop$pups))
