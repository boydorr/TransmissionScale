# Version updated March 2022
# Files required to run this code are too large to be provided in the repo.

library(lubridate)
library(plyr)
library(ggplot2)
library(reshape2)
library(fitdistrplus)

rm(list=ls()); options(stringsAsFactors = F)

source("R/computeCasesPerMonthOutside.R")
source("R/computeShiftHistogramPointsOutside.R")

# Set up file structures
ff <- "java_output/"
ff_criteria <- "output/abc_criteria/"
start.date <- as.Date('2002-01-01')
end.date <- as.Date('2015-12-31')
mths <- seq(start.date, end.date, by="month")
qrts <- seq(start.date, end.date, by="quarter")
month.vec.dots <- format(seq(start.date, end.date, by="month"), format = "%Y.%m.%d") #; year.vec <- as.character(seq(start.date, end.date, by="year"))
month.vec <- format(seq(start.date, end.date, by="month"), format = "%Y-%m-%d") #; year.vec <- as.character(seq(start.date, end.date, by="year"))

source("Style_Sheet.R")

# ---- Load up bounds for acceptance/rejection ----
cases.per.month.bounds <- readRDS(paste0(ff_criteria, "c_cases_per_month_bounds.rda"))
cases.per.month.hist.breaks <- readRDS(paste0(ff_criteria, "c_cases_per_month_hist_breaks.rda"))
cases.per.month <- readRDS(paste0(ff_criteria, "c_cases_per_month.rda"))

shift.bounds <- readRDS(paste0(ff_criteria, "shift_bounds.rda"))
dog.densities.hist <- readRDS(paste0(ff_criteria, "dog_densities_hist.rda"))
dog.density.hist.breaks <- readRDS(paste0(ff_criteria, "dog_density_hist_breaks.rda"))
dog.density.hist.mids <- readRDS(paste0(ff_criteria, "dog_density_hist_mids.rda"))
j.dog.density.hist.breaks <- readRDS(paste0(ff_criteria, "j_dog_density_hist_breaks.rda"))
density.breaks.by <- readRDS(paste0(ff_criteria, "density_breaks_by.rda"))

# ---- Post-processing ----
mean.density.period <- 22.43698

# Set up the spatial scale, model (Round1), parameter sets, etc. [edit here to select right set of runs]
model <- "Round1"; 
spatial.scale <- "all";
paramMinMax <- c(1,50000)
load.only <- F
param.ids.str <- sprintf("%.3d", seq(paramMinMax[1], paramMinMax[2]))
paramMinMax.str <- paste(sprintf("%.3d", paramMinMax),collapse="_")

date.format <- "%Y-%m-%d"

postprocessing.folder <- paste0("postprocessing")
experiment.id <- paste0(model, "_", spatial.scale, "/", postprocessing.folder)

# Read in summary files generated by Java code
abc <- read.csv(paste0(ff, experiment.id, "/abc_", paramMinMax.str, ".csv")) # This is the file produced by the post-processing step in Java
mt <- read.csv(paste0(ff, experiment.id, "/monthlyTally_", paramMinMax.str, ".csv"))
mh <- read.csv(paste0(ff, experiment.id, "/monthlyHistogram_", paramMinMax.str, ".csv"))
dh1000 <- read.csv(paste0(ff, experiment.id, "/densityHistogram1000_", paramMinMax.str, ".csv"))
outputs <- read.csv(paste0(ff, model, "_", spatial.scale, "/", "/output_files/epi_outcomes_", paramMinMax.str,".csv"))
output.cols <- c("nDemographicEvents","nBiteEvents","nTransmissions","nAbandonments","nBitesE","nBitesV","nFailedTransmissions",
                 "nReinfections","nDistinctS","nDistinctE","nDistinctV","nDiedE")
abc <- cbind(abc, rbind(rep(-1, length(output.cols)), outputs[output.cols]))

ddh <- read.csv(paste0(ff, experiment.id, "/distinctDogs_", paramMinMax.str, ".csv"))


# ------------------------- COMPUTE ABC STATS ----------------------------

if (load.only) {
  load(paste0(paste0(ff, experiment.id, "/abc_stats_", paramMinMax.str, ".rda")))
} else {
  
  abc$Proportion.noncontributors <- abc$numNonContributors / abc$numDetectableDogs
  abc$Proportion.superspreaders <- abc$numSuperspreaders / abc$numDetectableDogs

  # Cases per month falls inside ... (how many of the points are outside!)
  abc$cases.per.month.outside <- 0
  abc$shift.histogram.points.outside <- 0

  # Convert monthly tally to a version without Param to use in matching
  mt.num <- mt; mt.num$Param <- NULL

  for (rr in 1:nrow(abc)) {
    # Compute test statistics and add to abc data.frame
    if (rr > 1){
      print(rr)
      # Cases per month distribution
      abc$cases.per.month.outside[rr] <- computeCasesPerMonthOutside(cases.per.month.bounds, as.numeric(mt.num[rr,]), cases.per.month.hist.breaks, do.plot=F)

      # Density - shift - distribution
      if (ncol(dh1000)!=(length(dog.density.hist.breaks))) stop("The number of columns in dh1000 is not equal to the number of breaks to be used.")
      density.counts <- as.numeric(dh1000[rr,1:(length(dog.density.hist.breaks)-1)])
      density.relative <- (1/density.breaks.by) * density.counts / sum(density.counts)
      abc$shift.histogram.points.outside[rr] <- computeShiftHistogramPointsOutside(shift.bounds, densities = density.relative, hist.breaks = dog.density.hist.breaks, hist.mids = dog.density.hist.mids, do.plot=F)
    }
  }
  # Some additional statistics to add in ...
  # Total number of cases and maximum number of cases per month
  for (rr in 1:nrow(mt)) {
    max <- max(mt[rr,1:(ncol(mt)-1)])
    abc$maxPerMonth[rr] <- ifelse((abc$stoppedEarly[rr]=="true" & abc$Param[rr] != "data"), -1, max)
  }
  summary(abc$maxPerMonth); summary(abc$maxPerMonthPer1000); summary(abc$totalCases)
  save(abc, file=paste0(paste0(ff, experiment.id, "/abc_stats_", paramMinMax.str, ".rda")))
}

# Cross-checking: How big can an outbreak get before we stop it early?
summary(subset(abc, stoppedEarly == "false")$numDetectableDogs)

# ---- Accept / Reject ----

table(abc$stoppedEarly)/nrow(abc)
table(cpm=abc$cases.per.month.outside, shift=abc$shift.histogram.points.outside)

# Threshold points ... (baseline 2, 4, 100)
thresh.cpm <- 2
thresh.shift <- 4
max.dd <- 100

abc$Accepted <- (abc$cases.per.month.outside <= thresh.cpm & abc$shift.histogram.points.outside <= thresh.shift & abc$stoppingReason == "Completed" & abc$maxDD <= max.dd)
table(abc$Accepted)

# --------------------------------- Make priors for Round 2 where appropriate ------------------------------------
if(substr(model, 1,6)=="Round1") {
  # Make best-fit parameters for second stage
  acc <- subset(abc, Accepted & Param!="data")
  dim(acc)
  acc$Td <- acc$TdMean
  my.priors <- acc[c("ThShape","ThScale","Td")]
  my.priors
  n <- 100
  which_rows <- rep(1:nrow(my.priors), each = n)
  repeated.priors <- my.priors[which_rows,]
  dim(repeated.priors)
  write.table(repeated.priors[c("ThShape","ThScale","Td")], 
              file = paste0(ff, experiment.id, "/Round2_Priors_", spatial.scale, ".csv"), append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = "")
}

