
# I think that we want to be able to say:
# "For comparison, we simulated to obtain R0 estimates in a fully susceptible population, 
#      obtaining an R0 of XXX. We observed a reduction in R0 in normal runs (without vaccination) to approximately 
#      1.053, without a corresponding reduction in the number of distinct dogs bitten (2.330)."

rm(list = ls())
detach(package:plyr)
library(dplyr)
library(ggplot2)

# We want to read in the runs with and without vaccination (or with reduced vaccination) and link them so that we can plot
#    the proportion that did not terminate early wiith reduced vaccination against the proportion accepted with vaccination.
incursion_type <- "Gamma95" # "LogN95"  "Gamma95"
max_count <-  24100 # 15600 #24100
load(paste0("/Volumes/LECKIE1/Rabies_Sim_Output/Round2_", incursion_type, "_1000/postprocessing/abc_stats_001_",max_count,".rda"))
VC <- abc
ThirdVC <- read.csv(file = (paste0("/Volumes/LECKIE1/Rabies_Sim_Output/Round2_", incursion_type, "_ThirdVC_1000/output_files/epi_outcomes_001_",max_count,".csv")))
NoVC <- read.csv(file = (paste0("/Volumes/LECKIE1/Rabies_Sim_Output/Round2_", incursion_type, "_NoVC_1000/output_files/epi_outcomes_001_",max_count,".csv")))

names(VC) # variables of interest: meanDD, R0 = nTransmissions/numDetectableDogs
names(ThirdVC)
names(NoVC)

# Save a copy locally
saveRDS(object = VC, file = "~/Downloads/Round2_VC.rda")
saveRDS(object = NoVC, file = "~/Downloads/Round2_NoVC.rda")
saveRDS(object = ThirdVC, file = "~/Downloads/Round2_ThirdVC.rda")
VC <- readRDS("~/Downloads/Round2_VC.rda")
NoVC <- readRDS("~/Downloads/Round2_NoVC.rda")
ThirdVC <- readRDS("~/Downloads/Round2_ThirdVC.rda")
dim(VC); dim(NoVC); dim(ThirdVC)

# Process VC - Accept or reject runs
thresh.cpm <- 2
thresh.shift <- 4
VC$Accepted <- (VC$cases.per.month.outside <= thresh.cpm & VC$shift.histogram.points.outside <= thresh.shift & VC$stoppingReason == "Completed")
VC <- subset(VC, Param!="data") # exclude the data
dim(VC); table(VC$Accepted)

# Join VC and NoVC stoppingReason by order of runs
names(ThirdVC) <- paste0(names(ThirdVC),"_", "ThirdVC")
names(NoVC) <- paste0(names(NoVC),"_", "NoVC")
All <- cbind(VC, NoVC, ThirdVC)
#All <- cbind(VC, ThirdVC)
All$Param <- sprintf("%03d", seq(1, max_count))
dim(All)

pr.acc.All <- All %>% group_by(TdMean, ThMean, ThShape, ThScale, densityDependence) %>%
  summarise(propAcc = sum(Accepted)/length(Accepted),
            propThirdVCCompleted = sum(stoppingReason_ThirdVC=="Completed")/length(stoppingReason_ThirdVC),
            #propNoVCCompleted = sum(stoppingReason_NoVC=="Completed")/length(stoppingReason_NoVC),
            meanNICases_ThirdVC = mean(nICases_ThirdVC),
            medianNICases_ThirdVC = median(nICases_ThirdVC),
            minNICases_ThirdVC = min(nICases_ThirdVC),
            maxNICases_ThirdVC = max(nICases_ThirdVC),
            medMaxDD = median(maxDD),
            maxMaxDD = max(maxDD),
            medR0 = median(R0),
            maxR0 = max(R0),
            medMaxPerMonth = median(maxPerMonth),
            maxMaxPerMonth = max(maxPerMonth))


dim(pr.acc.All)

# Create parameter set ID
pr.acc.All$paramsetID <- seq(1,nrow(pr.acc.All))
# Join parameterset ID back into All
All$paramsetID <- left_join(All, pr.acc.All[c("TdMean","ThMean","ThShape","ThScale","densityDependence","paramsetID")])$paramsetID
dim(All)

summary(pr.acc.All)
# Plots for third of vaccination
g <- ggplot(pr.acc.All, aes(x=propAcc, y=propThirdVCCompleted)) + geom_point(aes(colour = densityDependence)); g
ggsave(filename = paste0("propAcc_propThirdVCCompleted_",incursion_type,".pdf"), plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")
g <- ggplot(pr.acc.All, aes(x=ThMean, y=TdMean)) + geom_point(aes(colour = propAcc, alpha = propThirdVCCompleted )) + 
  scale_colour_gradient(low = "white", high = "darkblue"); g
ggsave(filename = paste0("ThTd_propThirdVCCompleted_",incursion_type,".pdf"), plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")
g <- ggplot(pr.acc.All, aes(x=ThMean, y=TdMean)) + geom_point(aes(colour = propAcc > 0.5, alpha = propThirdVCCompleted > 0.5 )); g
ggsave(filename = paste0("ThTd_propThirdVCCompleted_",incursion_type,".pdf"), plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")
# NICases is total outbreak size
g <- ggplot(pr.acc.All, aes(x=propAcc, y=meanNICases_ThirdVC)) + geom_point(aes(colour = propAcc > 0.5, size = densityDependence )); g
ggsave(filename = paste0("propAcc_meanNICases_ThirdVC_",incursion_type,".pdf"), plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")
g <- ggplot(pr.acc.All, aes(x=propAcc, y=medianNICases_ThirdVC)) + geom_point(aes(colour = propAcc > 0.5, size = densityDependence )); g
ggsave(filename = paste0("propAcc_medianNICases_ThirdVC_",incursion_type,".pdf"), plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")
g <- ggplot(pr.acc.All, aes(x=propAcc, y=minNICases_ThirdVC)) + geom_point(aes(colour = propAcc > 0.5, size = densityDependence )); g
ggsave(filename = paste0("propAcc_minNICases_ThirdVC_",incursion_type,".pdf"), plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")
g <- ggplot(pr.acc.All, aes(x=propAcc, y=maxNICases_ThirdVC)) + geom_point(aes(colour = propAcc > 0.5, size = densityDependence )); g
ggsave(filename = paste0("propAcc_maxNICases_ThirdVC_",incursion_type,".pdf"), plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")
g <- ggplot(pr.acc.All, aes(x=ThMean, y=TdMean)) + geom_point(aes(colour = medianNICases_ThirdVC, alpha = propAcc)) + 
  scale_colour_gradient(low = "red", high = "darkblue"); g
ggsave(filename = paste0("ThTd_medianNICases_ThirdVC_",incursion_type,".pdf"), plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")

g <- ggplot(pr.acc.All, aes(x=ThMean, y=TdMean)) + geom_point(aes(alpha = propThirdVCCompleted > 0.8, colour = propAcc > 0.5)) ; g
ggsave(filename = paste0("ThTd_propThirdVCCompleted_ThirdVC_",incursion_type,".pdf"), plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")

g <- ggplot(pr.acc.All, aes(x=ThMean, y=TdMean)) + geom_point(aes(alpha = propThirdVCCompleted > 0.8, colour = propAcc > 0.5, size = medianNICases_ThirdVC)) ; g
ggsave(filename = paste0("ThTd_propThirdVCCompletedSize_ThirdVC_",incursion_type,".pdf"), plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")


# Plots for no vaccination
g <- ggplot(pr.acc.All, aes(x=propAcc, y=propNoVCCompleted)) + geom_point(aes(colour = densityDependence)); g
ggsave(filename = "propAcc_propNoVCCompleted.pdf", plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")
g <- ggplot(pr.acc.All, aes(x=ThMean, y=TdMean)) + geom_point(aes(colour = propAcc, alpha = propNoVCCompleted )) + 
  scale_colour_gradient(low = "white", high = "darkblue"); g
ggsave(filename = "ThTd_propNoVCCompleted.pdf", plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")
g <- ggplot(pr.acc.All, aes(x=ThMean, y=TdMean)) + geom_point(aes(colour = propAcc > 0.5, alpha = propNoVCCompleted > 0.5 )); g
ggsave(filename = "ThTd_propNoVCCompleted.pdf", plot = g, path = "~/Nextcloud/Random-Backups/", height = 20, width = 20, units = "cm")

# Choose runs where propAcc > 0.7 and propThirdVCCompleted > 0.9
propAccThreshold <- 0.75
propThirdVCCompletedThreshold <- 0.9
posterior_params <- filter(pr.acc.All, propAcc > propAccThreshold & propThirdVCCompleted > propThirdVCCompletedThreshold)
dim(posterior_params)
# Examine where these are in parameter space
g <- ggplot(pr.acc.All, aes(x=ThMean, y=TdMean)) + geom_point(aes(colour = propAcc > propAccThreshold & propThirdVCCompleted > propThirdVCCompletedThreshold )); g
g <- ggplot(pr.acc.All, aes(x=propAcc, y=propThirdVCCompleted)) + geom_point(aes(colour = propAcc > propAccThreshold & propThirdVCCompleted > propThirdVCCompletedThreshold )); g
# Check the median number of cases per run against proportion completed
g <- ggplot(pr.acc.All, aes(x=medianNICases_ThirdVC, y=propThirdVCCompleted)) + geom_point(aes(colour = propAcc > propAccThreshold & propThirdVCCompleted > propThirdVCCompletedThreshold )); g


# Create set of "priors" from these posterior parameters
spatial.scale <- 1000
posterior_params$weighting <- posterior_params$propAcc * posterior_params$propThirdVCCompleted
posterior_params$prob <- posterior_params$weighting / sum(posterior_params$weighting)
n <- 10000
sample_rows <- sample(x = 1:nrow(posterior_params), size = n, prob = posterior_params$prob, replace = T)
posteriors <- posterior_params[sample_rows,]
posteriors$Td <- posteriors$TdMean # Rename for Java
write.table(posteriors[c("ThShape","ThScale","Td")], 
            file = paste0("/Volumes/LECKIE1/", "/Round3_Priors_Gamma95_Posterior_", spatial.scale, ".csv"), append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = "")



# # Would like to plot timeseries for ThirdVC where both propAcc and ThirdVCCompleted > e.g. 0.75
# goodness.threshold <- 0.75
# pr.acc.All$good <- pr.acc.All$propAcc > goodness.threshold & pr.acc.All$propThirdVCCompleted > goodness.threshold
# summary(pr.acc.All)
# 
# ts_NoVC <- read.csv(file = ("/Volumes/LECKIE1/Rabies_Sim_Output/Round2_Gamma95_NoVC_1000/postprocessing/monthlyTally_001_24100.csv"))
# dim(ts_NoVC)
# ts_ThirdVC <- read.csv(file = ("/Volumes/LECKIE1/Rabies_Sim_Output/Round2_Gamma95_ThirdVC_1000/postprocessing/monthlyTally_001_24100.csv"))
# # Join parameterset ID back into All
# ts_ThirdVC$paramsetID <- c(-1, All$paramsetID)
# dim(abc)
# #load("/Volumes/LECKIE1/Rabies_Sim_Output/Round2_Gamma95_1000/postprocessing/abc_stats_001_24100.rda")
# #rm(abc)
# #ts_ThirdVC <- left_join(ts_ThirdVC, All, by = c("Param" = "Param"))
# # ts_ThirdVC$good <- ts_ThirdVC$Param %in% pr.acc.All$good
# # summary(ts_ThirdVC)
# # dim(ts_ThirdVC)
# # ts_ThirdVC_matrix <- matrix(ts_ThirdVC[,1:((ncol(ts_ThirdVC)-2))])
# # head(rowSums(ts_ThirdVC[,1:((ncol(ts_ThirdVC)-2))]))
# 
# library(readr)
# my_names <- parse_number(names(ts_ThirdVC))
# names(ts_ThirdVC)[1:168] <- my_names[1:168]
# 
# library(tidyr)
# ts_Third_gather <- gather(data = ts_ThirdVC[,c(1:168, 170)], "date", "count", -paramsetID)
# ts_Third_gather$date <- parse_number(ts_Third_gather$date)
# head(ts_Third_gather)
# summary(ts_Third_gather)
# 
# ts_Third_gather <- filter(ts_Third_gather, paramsetID < 3)
# str(ts_Third_gather)
# 
# 
# ggplot(ts_Third_gather, aes(x=date, y=count, group = paramsetID)) + 
#          geom_line()

       