rm(list=ls()); options(stringsAsFactors = F)

library(dplyr)
library(tidyr) # for gather, etc.
library(ggplot2)

source("Style_Sheet.R")

# Combine output from all spatial scales and detection levels for runs using ACCEPTED parameters
files.to.read <- data.frame(spatial.scale = c(500, 1000, 2000, 4000, "all"),
                            detection.level = c(100, 100, 100, 100, 100),
                            param.max = c(2100, 16800, 8200, 6500, 3400),
                            model.name = c("","","","",""))

ff <- "/Volumes/SimOutput01/Rabies_Sim_Output/" # External hard disk as large files
ff_output <- "java_output/Round2_Compiled/"
model <- "Round2"

# Read in and combine the outcomes at the different spatial scales
all.accepted <- data.frame()
for (rr in 1:nrow(files.to.read)) {
  my.row <- files.to.read[rr,]
  paramMinMax.str <- paste(sprintf("%.3d", c(1, my.row$param.max)), collapse = "_")
  filename <- paste0(ff, model,"",my.row$model.name, "_", my.row$spatial.scale, "/postprocessing/", "abc_stats_",paramMinMax.str, ".rda")
  load(filename)
  print(names(abc))
  print("--")
  abc$Res <- my.row$spatial.scale
  abc$Model.name <- my.row$model.name
  
  if (nrow(all.accepted) == 0) {
    all.accepted <- abc
  } else {
    all.accepted <- rbind(all.accepted, abc)
  }
}
summary(all.accepted)
# Label for different spatial scales
all.accepted$Res.label <- factor(all.accepted$Res, levels = c(500,1000,2000,4000,"all"), labels = c("500x500m","1x1km","2x2km","4x4km","Fully mixed"))
# Resolution (grid cell size) information appropriate for plotting (NB: Recoding "all" as 8000 for location on boxplots only)
all.accepted$Res.factor <- factor(all.accepted$Res, levels = c("500", "1000", "2000", "4000", "all"), labels = c(500,1000,2000,4000,8000))
all.accepted$Res.numeric <- as.numeric(as.character(all.accepted$Res.factor))

saveRDS(all.accepted, file=paste0(ff_output, "All_Round2_Accepted.rda"))

summary(all.accepted)
dim(all.accepted)

# Accept or reject runs
thresh.cpm <- 2
thresh.shift <- 4
max.dd <- 100
table(abc$cases.per.month.outside <= thresh.cpm)

# Acceptance step
all.accepted$Accepted <- (all.accepted$cases.per.month.outside <= thresh.cpm & all.accepted$shift.histogram.points.outside <= thresh.shift & all.accepted$stoppingReason == "Completed" & all.accepted$maxDD <= max.dd)
summary(all.accepted$Accepted)
table(all.accepted$Accepted)
all.accepted <- subset(all.accepted, Param!="data") # exclude the data

# Compute proportion accepted of re-simulated runs
pr.acc <- all.accepted %>% 
  group_by(TdMean, ThMean, ThShape, ThScale, densityDependence, Res, Res.label, Res.factor, Res.numeric, Model.name) %>%
  summarise(propAcc = sum(Accepted)/length(Accepted),
            propAcc = sum(Accepted)/length(Accepted),
            medMaxDD = median(maxDD),
            maxMaxDD = max(maxDD),
            medR0 = median(R0),
            maxR0 = max(R0),
            medMaxPerMonth = median(maxPerMonth),
            maxMaxPerMonth = max(maxPerMonth)) %>%
  arrange(Res.factor, propAcc, densityDependence)

dim(subset(pr.acc, Res==500))
dim(subset(pr.acc, Res==1000))
dim(subset(pr.acc, Res==2000))
dim(subset(pr.acc, Res==4000))
dim(subset(pr.acc, Res=="all"))

saveRDS(pr.acc, file=paste0(ff_output, "All_Round2_Reliability.rda"))


############################### Make posteriors from "top-scoring runs" ##################################
# Make posteriors from "top-scoring runs" - filter all accepted runs, irrespective of spatial scale and model
top.scoring.proportion <- 0.5 
posterior.info <- filter(pr.acc, Res == 1000 & propAcc > top.scoring.proportion)
dim(posterior.info) #
hist(pr.acc$propAcc)
summary(posterior.info$medR0)

# Summary of ranges of parameter values for final posteriors (ThMean, ThShape, TdMean)
summary(subset(pr.acc, Res==1000 & propAcc > top.scoring.proportion))

# For 1000 spatial scale
# Draw parameter sets proportional to proportion of runs accepted
n.draws <- 1000
my.rows <- sample(1:nrow(posterior.info), size = 1000, replace = T, prob = posterior.info$propAcc)
posterior.draws <- posterior.info[my.rows, c("ThShape","ThScale","TdMean")]
names(posterior.draws) <- c("ThShape","ThScale","Td")
hist(posterior.draws$ThShape)
hist(posterior.draws$ThScale)
write.table(posterior.draws, file = paste0(ff,"Round3_1000/Round3_priors_1000.csv"), append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = "")



