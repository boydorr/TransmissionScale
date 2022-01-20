library(dplyr)

load("/Volumes/LECKIE1/Rabies_Sim_Output/Round2_1000/postprocessing/abc_stats_001_7100.rda")
VC <- abc
sim_VC <- read.csv(file = ("/Volumes/LECKIE1/Rabies_Sim_Output/Round2_1000/output_files/epi_outcomes_001_10000.csv"))
load("/Volumes/LECKIE1/Rabies_Sim_Output/Round2_NoVC_1000/postprocessing/abc_stats_001_7100.rda")
NoVC <- abc
sim_NoVC <- read.csv(file = ("/Volumes/LECKIE1/Rabies_Sim_Output/Round2_NoVC_1000/output_files/epi_outcomes_001_10000.csv"))

rm(abc)

names(VC) # variables of interest: meanDD, R0 = nTransmissions/numDetectableDogs
names(NoVC)
summary(VC$meanDD) # distinct dogs = 2.365
summary(VC$nTransmissions/VC$numDetectableDogs) # Transmissions per dog = 0.9433
summary(NoVC$meanDD) # distinct dogs = 2.330 
summary(NoVC$nTransmissions/NoVC$numDetectableDogs) # Transmissions per dog = 1.053

# I think that we want to be able to say:
# "For comparison, we simulated to obtain R0 estimates in a fully susceptible population, 
#      obtaining an R0 of XXX. We observed a reduction in R0 in normal runs (without vaccination) to approximately 
#      1.053, without a corresponding reduction in the number of distinct dogs bitten (2.330)."

# Save a copy locally
saveRDS(object = VC, file = "~/Downloads/Round2_VC.rda")
saveRDS(object = NoVC, file = "~/Downloads/Round2_NoVC.rda")
VC <- readRDS("~/Downloads/Round2_VC.rda")
NoVC <- readRDS("~/Downloads/Round2_NoVC.rda")
dim(VC); dim(NoVC)

# Process VC - Accept or reject runs
thresh.cpm <- 2
thresh.shift <- 4
VC$Accepted <- (VC$cases.per.month.outside <= thresh.cpm & VC$shift.histogram.points.outside <= thresh.shift & VC$stoppingReason == "Completed")
VC <- subset(VC, Param!="data") # exclude the data
dim(VC); table(VC$Accepted)

# Compute proportion accepted of re-simulated runs
pr.acc.VC <- ddply(VC, .(TdMean, ThMean, ThShape, ThScale, densityDependence), summarise,
                propAcc = sum(Accepted)/length(Accepted),
                medMaxDD = median(maxDD),
                maxMaxDD = max(maxDD),
                medR0 = median(R0),
                maxR0 = max(R0),
                medMaxPerMonth = median(maxPerMonth),
                maxMaxPerMonth = max(maxPerMonth))
#pr.acc.filtered <- subset(pr.acc, propAcc>=0.35)

pr.acc.VC <- VC %>% group_by(TdMean, ThMean, ThShape, ThScale, densityDependence) %>%
  summarise(propAcc = sum(Accepted)/length(Accepted),
            medMaxDD = median(maxDD),
            maxMaxDD = max(maxDD),
            medR0 = median(R0),
            maxR0 = max(R0),
            medMaxPerMonth = median(maxPerMonth),
            maxMaxPerMonth = max(maxPerMonth))
dim(pr.acc.VC)


# Process NoVC
# Accept or reject runs
thresh.nCases <- 10000
NoVC$Accepted <- (NoVC$numDetectableDogs <= thresh.nCases & NoVC$stoppingReason == "Completed")
NoVC <- subset(NoVC, Param!="data") # exclude the data
NoVC$Res <- 1000
NoVC$Res.label <- 1000
dim(NoVC); table(NoVC$Accepted)

# # Compute proportion accepted of re-simulated runs
pr.acc.NoVC <- NoVC %>% group_by(TdMean, ThMean, ThShape, ThScale, densityDependence, Res, Res.label) %>%
  summarise(propAcc = sum(Accepted)/length(Accepted),
            medMaxDD = median(maxDD),
            maxMaxDD = max(maxDD),
            medR0 = median(R0),
            maxR0 = max(R0),
            medMaxPerMonth = median(maxPerMonth),
            maxMaxPerMonth = max(maxPerMonth))
pr.acc.NoVC <- ddply(NoVC, .(TdMean, ThMean, ThShape, ThScale, densityDependence), summarise,
                   propAcc = sum(Accepted)/length(Accepted),
                   medMaxDD = median(maxDD),
                   maxMaxDD = max(maxDD),
                   medR0 = median(R0),
                   maxR0 = max(R0),
                   medMaxPerMonth = median(maxPerMonth),
                   maxMaxPerMonth = max(maxPerMonth))
dim(pr.acc.NoVC)

# Combine the two datasets
names(pr.acc.VC); names(pr.acc.NoVC)

test <- left_join(x = pr.acc.VC, y = pr.acc.NoVC, 
                  by = c("ThShape" = "ThShape", "ThMean" = "ThMean", "ThShape" = "ThShape", "ThScale" = "ThScale", "densityDependence" = "densityDependence"))
test$propAcc.VC <- test$propAcc.x
test$NoVC_NoBlowup <- test$propAcc.y
dim(test)
head(test)
summary(test)
dim(VC_Accepted)
dim(NoVC_Accepted)
summary(test$propAcc.x); summary(test$propAcc.y)
p <- ggplot(test, aes(x=propAcc.VC, y=NoVC_NoBlowup)) +  geom_point(alpha = 0.5)

filter(pr.acc.VC, propAcc > 0.3)
filter(pr.acc.NoVC, round(TdMean, digits = 5) == 0.26297)
filter(pr.acc.NoVC, round(TdMean, digits = 5) == 0.31228)


filter(test, propAcc.x > .2 & propAcc.y > 0.4)


