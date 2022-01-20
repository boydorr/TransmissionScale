#' Step 2: Generate incubation & infectious period estimates ----

#' This file is to be run after Data_Cleaning.R
#' It takes the rabid carnivores dataset to get incubation & infectious periods
#' & serial intervals
rm(list=ls())

# pkgs ----
library(ggplot2)
library(fitdistrplus)
library(MASS)
source("R/computePeriod.R")
source("R/format_param_est.R")

# set seed here so all fits are reproducible
set.seed(0)

# To do this, we need to use the augmented rabid_carnivores dataset
rabid_carnivores <- readRDS(file = "output/clean_bite_data_deid.rda")
nrow(rabid_carnivores) 

# Identify records of animals bitten by multiple other rabid animals (and so we know true date bitten)
MultiBite <- which(rabid_carnivores$ID %in% rabid_carnivores$ID[which(duplicated(rabid_carnivores$ID))])

# Get cleaned incubation & infectious periods & serial intervals ----

# Convert time intervals into units of days and incorporate uncertainty
# See explanation of algorithm in computePeriod.
# Incubation period
rabid_carnivores$Incubation.Period.Clean <- computePeriod(
  rabid_carnivores$Incubation.period,
  rabid_carnivores$Incubation.period.units,
  rabid_carnivores$Date.bitten,
  rabid_carnivores$Symptoms.started,
  rabid_carnivores$Date.bitten.uncertainty,
  rabid_carnivores$Symptoms.started.accuracy
)
# discard incubation periods for dogs that have multiple possible values
rabid_carnivores$Incubation.Period.Clean[MultiBite] <- NA 
summary(rabid_carnivores$Incubation.Period.Clean)
hist(rabid_carnivores$Incubation.Period.Clean, col = "yellow", breaks = seq(-0.5, 450.5, 5))

# Infectious period
rabid_carnivores$Infectious.Period.Clean <- computePeriod(
  rabid_carnivores$Infectious.period,
  rabid_carnivores$Infectious.period.units,
  rabid_carnivores$Symptoms.started,
  rep(NA, nrow(rabid_carnivores)), # no end dates possible for infectious periods
  rabid_carnivores$Symptoms.started.accuracy,
  rep(NA, nrow(rabid_carnivores))
)
summary(rabid_carnivores$Infectious.Period.Clean)
hist(rabid_carnivores$Infectious.Period.Clean, col = "red", breaks = seq(-0.5, 12.5, 1)) # no zeros

sum(table(rabid_carnivores$Outcome)[4:9])/nrow(rabid_carnivores)

# Serial interval
rabid_carnivores$Serial.Interval.Clean <- computePeriod(
  rep(NA, nrow(rabid_carnivores)), # no SIs directly recorded in data
  rep(NA, nrow(rabid_carnivores)),
  rabid_carnivores$Symptoms.started[match(rabid_carnivores$Biter.ID, rabid_carnivores$ID)],
  rabid_carnivores$Symptoms.started,
  rabid_carnivores$Symptoms.started.uncertainty[match(rabid_carnivores$Biter.ID, rabid_carnivores$ID)],
  rabid_carnivores$Symptoms.started.accuracy
)
rabid_carnivores$Serial.Interval.Clean[MultiBite] <- NA # discard intervals for dogs that have multiple possible values
summary(rabid_carnivores$Serial.Interval.Clean)
hist(rabid_carnivores$Serial.Interval.Clean, col = "dodgerblue", breaks = seq(-0.5, 500.5, 5))
length(which(!is.na(rabid_carnivores$Serial.Interval.Clean)))

table(Exists.Inc = !is.na(rabid_carnivores$Incubation.Period.Clean), 
      Exists.Inf = !is.na(rabid_carnivores$Infectious.Period.Clean))

# Check whether incubation periods and infectious periods are independent
ggplot(data = rabid_carnivores, aes(x = Incubation.Period.Clean, y = Infectious.Period.Clean)) +
  geom_point(alpha = 0.5) +
  scale_x_log10()
cor.test(log(rabid_carnivores$Incubation.Period.Clean), rabid_carnivores$Infectious.Period.Clean) # r = 0.22

# Export paired incubation and infectious periods for IBM -----
# (inferring infectious period for animals killed before biting)
# From the perspective of the manuscript: The number of bites is related to the infectious period
#    via the Holling functional response. Therefore, it was important for infectious periods to span the
#    observed range. Because the shortest infectious periods in the dataset are for one day,
#    despite anecdotal evidence that dogs are often killed as soon as symptoms are noticed, we imputed
#    infectious periods for dogs that were killed without biting, and for which incubation periods but
#    not infectious periods were recorded. A number of days for the infectious period was drawn from an
#    exponential distribution, chosen to model the waiting time until being killed on each day.

# New algorithm - subtract one to account for lower bound & add noise, then simulate for those that
#     were killed an infectious period from an exponential distribution (mean = 1/p)
#     On any day, dogs that are not biting have a probability of being killed (based on symptoms recognised)

# Dogs that were killed without biting often had no recorded infectious period, even when the incubation period was recorded.
# To retain realistic heterogeneity in incubation periods, we therefore imputed infectious periods for dogs that were killed
# without biting, and for which incubation periods but not infectious periods were recorded.

# Identify those for which we need to impute an infectious period - dogs that don't have an infectious period but do have an incubation period
# and that are non-contributors in the sense that they made no bites, and were restrained/killed (so couldn't make bites)
outcomes.non.contributors <- c("Killed", "Killed after bite", "Killed by hyenas",
                               "Tied", "Tied then killed")
rabid_carnivores$To.impute <- (
  is.na(rabid_carnivores$Infectious.Period.Clean) &
  !is.na(rabid_carnivores$Incubation.Period.Clean) &
  rabid_carnivores$Animals.bitten == 0 &
  (rabid_carnivores$Outcome %in% outcomes.non.contributors) &
  rabid_carnivores$Incubation.Period.Clean > 0
)
sum(rabid_carnivores$To.impute) 

# Add noise to existing infectious periods so that they are real numbers rather 
# than integers, and so that they start at zero rather than 1
rabid_carnivores$Infectious.Period.Jitter <- 
  rabid_carnivores$Infectious.Period.Clean - 0.5 + runif(nrow(rabid_carnivores), -0.5, 0.5)
rabid_carnivores$Infectious.Period.Jitter <- 
  ifelse(rabid_carnivores$Infectious.Period.Jitter < 0, 0, rabid_carnivores$Infectious.Period.Jitter)
summary(rabid_carnivores$Infectious.Period.Jitter)

ggplot(data = rabid_carnivores, aes(x = Incubation.Period.Clean, y = Infectious.Period.Jitter)) +
  geom_point(alpha = 0.5) +
  scale_x_log10()
cor.test(log(rabid_carnivores$Incubation.Period.Clean), rabid_carnivores$Infectious.Period.Jitter) # r depends on randomisation step, but correlation still present

# Information for Supplementary Materials - summary of available data
paste0("Of the ",  length(unique(rabid_carnivores$ID)),
       " rabid carnivores that we traced, we identified progenitors for ", 
       length(which(rabid_carnivores$Biter.ID > 0 )), " of them, i.e. ",
       round(100 * length(which(rabid_carnivores$Biter.ID > 0 )) / 
               length(unique(rabid_carnivores$ID))), " %") 

dups = which(duplicated(rabid_carnivores$ID) == TRUE)
paste0("The number of carnivores with incubation period available: ", 
       sum(!is.na(rabid_carnivores$Incubation.Period.Clean[-dups]))) 
paste0("The percentage with incubation period information: ", 
       round(100 * sum(!is.na(rabid_carnivores$Incubation.Period.Clean[-dups])) / 
         nrow(rabid_carnivores)), " %") 

paste0("The number of dogs with infectious period provided: ", 
       sum(!is.na(rabid_carnivores$Infectious.Period.Clean[-dups]))) 
paste0("The percentage with infectious period information: ", 
       round(100 * sum(!is.na(rabid_carnivores$Infectious.Period.Clean[-dups])) / 
         nrow(rabid_carnivores)), " %") 

paste0("The total number of Rabid carnivores used for incubation and infectious periods is (i.e. whole dataset): ", 
       length(unique(rabid_carnivores$ID[-dups])))
paste0("Of which, ", sum(!is.na(rabid_carnivores$Incubation.Period.Clean[-dups])),
       " had the incubation period recorded") 
paste0("And of these, ", sum(!is.na(rabid_carnivores$Incubation.Period.Clean[-dups]) & 
                               !is.na(rabid_carnivores$Infectious.Period.Clean[-dups])), 
       " also had information on the infectious period") 
paste0(" which is ", 100 * sum(!is.na(rabid_carnivores$Incubation.Period.Clean[-dups]) &
                                       !is.na(rabid_carnivores$Infectious.Period.Clean[-dups])) / nrow(rabid_carnivores),
       " % of the total") 

# For non-contributors without infectious periods, impute an infectious period assuming an exponential waiting time until a dog is killed/restrained with mean 0.5 days
#      If any infectious period exceeds 10 days, start again
too.long <- T
while (too.long) {
  rabid_carnivores$Imputed <- ifelse(rabid_carnivores$To.impute, rexp(n = nrow(rabid_carnivores), rate = 1 / 0.5), NA)
  hist(rabid_carnivores$Imputed) # Assume exponential waiting time until dog is killed or restrained with mean 0.5 days
  too.long <- max(rabid_carnivores$Imputed, na.rm = T) > 10
}
hist(rabid_carnivores$Infectious.Period.Jitter, breaks = seq(-1, 25, 0.25), col = alpha("grey", alpha = 0.5), ylim = c(0, 150))
hist(rabid_carnivores$Imputed, add = T, col = alpha("red", alpha = 0.5), breaks = seq(-1, 25, 0.25))

rabid_carnivores$Infectious.Period.With.Imputed <- ifelse(rabid_carnivores$To.impute, rabid_carnivores$Imputed, rabid_carnivores$Infectious.Period.Jitter)
hist(rabid_carnivores$Infectious.Period.With.Imputed, breaks = seq(-1, 25, 0.25), col = alpha("yellow", alpha = 0.5))

# Now have additional imputed dates, so save again
# SAVE THIS ANONYMISED!
saveRDS(object = rabid_carnivores, 
        file = paste0("output/rabid_carnivores_with_imputed_infectious_periods.rda"))

# Check the correlation again
ggplot(data = rabid_carnivores, aes(x = Incubation.Period.Clean, y = Infectious.Period.With.Imputed)) +
  geom_point(alpha = 0.5) +
  scale_x_log10()
cor.test(log(rabid_carnivores$Incubation.Period.Clean), rabid_carnivores$Infectious.Period.With.Imputed) # correlation partially masked by imputed values (i.e. because not present at very short infectious periods)

# For the data on incubation and infectious periods, retain only those with both incubation and infectious period
Inc_Inf_Data <- subset(rabid_carnivores, !is.na(Incubation.Period.Clean) & !is.na(Infectious.Period.With.Imputed))
nrow(Inc_Inf_Data) 

# Rename variables and write out as csv
Inc_Inf_Data$Generation.interval <- Inc_Inf_Data$Incubation.Period.Clean + Inc_Inf_Data$Infectious.Period.With.Imputed # This GI estimate uses full infectious period, not just time from symptoms to bite...
Inc_Inf_Data$Infectious.period <- Inc_Inf_Data$Infectious.Period.With.Imputed
summary(Inc_Inf_Data$Infectious.period)
Inc_Inf_Data <- subset(Inc_Inf_Data, Infectious.period > 0) # Otherwise this crashes Java

ggplot(Inc_Inf_Data, aes(x = Generation.interval)) +
  geom_histogram()
summary(Inc_Inf_Data$Incubation.Period.Clean)
summary(Inc_Inf_Data$Infectious.period)
summary(Inc_Inf_Data$Generation.interval)

write.table(Inc_Inf_Data[c("Generation.interval", "Infectious.period")],
  file = "output/d_inc_inf_with_zeros.csv", append = FALSE, quote = FALSE,
  sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = ""
)

# Fit distributions and output for transmission trees ----

# Fit incubation period distributions
which(rabid_carnivores$Incubation.Period.Clean == 0) # 2 cases
rabid_carnivores$Incubation.Period.Clean[which(rabid_carnivores$Incubation.Period.Clean == 0)] <- 1 # change 0 periods to 1
inc_idx <- which(!is.na(rabid_carnivores$Incubation.Period.Clean))
length(inc_idx) # 1147
length(which(rabid_carnivores$Incubation.Period.Clean < 1)) # 2 outliers removed
inc_gamma <- fitdist(rabid_carnivores$Incubation.Period.Clean[inc_idx], "gamma")
inc_lnorm <- fitdist(rabid_carnivores$Incubation.Period.Clean[inc_idx], "lnorm")
inc_weibull <- fitdist(rabid_carnivores$Incubation.Period.Clean[inc_idx], "weibull")
inc_gamma$aic
inc_lnorm$aic
inc_weibull$aic # lnorm has best fit

# Fit infectious period distributions
inf_idx <- which(!is.na(rabid_carnivores$Infectious.Period.Clean))
inf_gamma <- fitdist(rabid_carnivores$Infectious.Period.Clean[inf_idx], "gamma")
inf_lnorm <- fitdist(rabid_carnivores$Infectious.Period.Clean[inf_idx], "lnorm")
inf_weibull <- fitdist(rabid_carnivores$Infectious.Period.Clean[inf_idx], "weibull")
inf_gamma$aic
inf_lnorm$aic
inf_weibull$aic # lnorm has best fit

# Fit infectious period distributions with censoring
censdata <- data.frame(
  "left" = ifelse(rabid_carnivores$Infectious.Period.Clean[inf_idx] > 1, 
                  rabid_carnivores$Infectious.Period.Clean[inf_idx] - 0.5, 0),
  "right" = ifelse(rabid_carnivores$Infectious.Period.Clean[inf_idx] > 1, 
                   rabid_carnivores$Infectious.Period.Clean[inf_idx] + 0.5, 1.5)
)
inf_cens_gamma <- fitdistcens(censdata, "gamma")
inf_cens_lnorm <- fitdistcens(censdata, "lnorm")
inf_cens_weibull <- fitdistcens(censdata, "weibull")
inf_cens_gamma$aic
inf_cens_lnorm$aic
inf_cens_weibull$aic # gamma now has best fit

# Fit distributions to serial intervals estimated directly from data
which(rabid_carnivores$Serial.Interval.Clean == 0) # 2 cases
rabid_carnivores$Serial.Interval.Clean[which(rabid_carnivores$Serial.Interval.Clean == 0)] <- 1 # change 0 period to 1
ser_idx <- which(!is.na(rabid_carnivores$Serial.Interval.Clean))
ser_gamma <- fitdist(rabid_carnivores$Serial.Interval.Clean[ser_idx], "gamma")
ser_lnorm <- fitdist(rabid_carnivores$Serial.Interval.Clean[ser_idx], "lnorm")
ser_weibull <- fitdist(rabid_carnivores$Serial.Interval.Clean[ser_idx], "weibull")
ser_gamma$aic
ser_lnorm$aic
ser_weibull$aic # lnorm has best fit

## Simulate serial interval convolutions from lnorm
nsim <- 1000000
SIdouble_lnorm_sim <- 
  rlnorm(nsim, meanlog = ser_lnorm$estimate["meanlog"], sdlog = ser_lnorm$estimate["sdlog"]) + 
  rlnorm(nsim, meanlog = ser_lnorm$estimate["meanlog"], sdlog = ser_lnorm$estimate["sdlog"])
SIdouble_lnorm_lnorm <- fitdist(SIdouble_lnorm_sim, "lnorm")
SIdouble_lnorm_gamma <- fitdist(SIdouble_lnorm_sim, "gamma")
SIdouble_lnorm_weibull <- fitdist(SIdouble_lnorm_sim, "weibull")
SIdouble_lnorm_lnorm$aic
SIdouble_lnorm_gamma$aic
SIdouble_lnorm_weibull$aic # lnorm best

## Simulate serial interval convolutions from weibull
nsim <- 1000000
SIdouble_weibull_sim <- 
  rweibull(nsim, ser_weibull$estimate["shape"], ser_weibull$estimate["scale"]) + 
  rweibull(nsim, ser_weibull$estimate["shape"], ser_weibull$estimate["scale"])
SIdouble_weibull_lnorm <- fitdist(SIdouble_weibull_sim, "lnorm")
SIdouble_weibull_gamma <- fitdist(SIdouble_weibull_sim, "gamma")
SIdouble_weibull_weibull <- fitdist(SIdouble_weibull_sim, "weibull")
SIdouble_weibull_lnorm$aic
SIdouble_weibull_gamma$aic
SIdouble_weibull_weibull$aic # gamma actually better here.. but weibull not too far off

## OLD APPROACH ##
# # Simulated serial intervals (incubation period + Infectious period wait time (i.e. time to a bite) & convolution
# # fitted with a lognormal distribution
# x = 1000000
# incx = rlnorm(x, meanlog = inc_lnorm$estimate["meanlog"], sdlog = inc_lnorm$estimate["sdlog"])
# infx = rlnorm(x, meanlog = inf_lnorm$estimate["meanlog"], sdlog = inf_lnorm$estimate["sdlog"])
# SI_lnorm = fitdist(incx + runif(x, min=0, max=infx), "lnorm"); SI_lnorm
# SIdouble_lnorm = fitdist(rlnorm(x, SI_lnorm$estimate["meanlog"], SI_lnorm$estimate["sdlog"]) +
#                            rlnorm(x, SI_lnorm$estimate["meanlog"], SI_lnorm$estimate["sdlog"]), "lnorm")
# # fitted with a Gamma distribution
# incx = rgamma(x, shape = inc_gamma$estimate["shape"], rate = inc_gamma$estimate["rate"])
# infx = rgamma(x, shape = inf_gamma$estimate["shape"], rate = inf_gamma$estimate["rate"]);
# SI_gamma = fitdist(incx + runif(x, min=0, max=infx), "gamma")
# SIdouble_gamma = fitdist(rgamma(x, shape = SI_gamma$estimate["shape"], rate = SI_gamma$estimate["rate"]) +
#                            rgamma(x, shape = SI_gamma$estimate["shape"], rate = SI_gamma$estimate["rate"]), "gamma")

# Write parameters for transmission tree inference
SI_params <- data.frame(
  SI_ml = coef(ser_lnorm)["meanlog"], SI_sdlog = coef(ser_lnorm)["sdlog"],
  SI2_ml = coef(SIdouble_lnorm_lnorm)["meanlog"], SI2_sdlog = coef(SIdouble_lnorm_lnorm)["sdlog"],
  SI_shape = coef(ser_gamma)["shape"], SI_scale = 1 / coef(ser_gamma)["rate"],
  SI2_shape = coef(ser_gamma)["shape"]*2, SI2_scale = 1 / coef(ser_gamma)["rate"],
  SI_shape_weibull = coef(ser_weibull)["shape"], SI_scale_weibull = coef(ser_weibull)["scale"],
  SI2_shape_weibull = coef(SIdouble_weibull_weibull)["shape"], SI2_scale_weibull = coef(SIdouble_weibull_weibull)["scale"]  
)
write.csv(SI_params, "output/SI_params.csv", row.names = FALSE)

# Check plot of SI distribution
hist(rabid_carnivores$Serial.Interval.Clean,
  breaks = seq(-0.5, 415.5, 5), cex.main = 0.9,
  col = "lightgrey", main = "", xlab = "Serial Interval", ylab = "Density", freq = F, axes = T,
  ylim = c(0, 0.04)
  )
box(bty = "l")
lines(seq(0, 450, 1), dgamma(seq(0, 450, 1), shape = coef(ser_gamma)["shape"], rate = coef(ser_gamma)["rate"]),
  col = "navy", lwd = 1.5
  )
lines(rep(qgamma(.975, shape = coef(ser_gamma)["shape"], rate = coef(ser_gamma)["rate"]), 100),
  seq(0, 1, length = 100),
  col = "navy", lty = 1
  )
lines(seq(0, 450, 1), dlnorm(seq(0, 450, 1), meanlog = coef(ser_lnorm)["meanlog"], sdlog = coef(ser_lnorm)["sdlog"]),
  col = "orange", lwd = 1.5, lty = 2
  )
lines(rep(qlnorm(.975, meanlog = coef(ser_lnorm)["meanlog"], sdlog = coef(ser_lnorm)["sdlog"]), 100),
  seq(0, 1, length = 100),
  col = "orange", lty = 2
  )
lines(seq(0, 450, 1), dweibull(seq(0, 450, 1), shape = coef(ser_weibull)["shape"], scale = coef(ser_weibull)["scale"]),
  col = "green3", lwd = 1.5, lty = 3
  )
lines(rep(qweibull(.975, shape = coef(ser_weibull)["shape"], scale = coef(ser_weibull)["scale"]), 100),
  seq(0, 1, length = 100),
  col = "green3", lty = 3
  )
lines(rep(quantile(rabid_carnivores$Serial.Interval.Clean, 0.975, na.rm = T), 100),
  seq(0, 1, length = 100),
  col = "lightgrey", lty = 4
  )
legend("topright",
  paste(c("gamma", "lognormal", "weibull"), ", AIC=", round(c(ser_gamma$aic, ser_lnorm$aic, ser_weibull$aic)), sep = ""),
  lty = c(1:3), col = c("navy", "orange", "green3"), bty = "n", lwd = 1.5, cex = 0.7
  )
text(370, 0.035, paste("mean=", round(mean(rabid_carnivores$Serial.Interval.Clean, na.rm = T), 2), sep = ""), cex = 0.7)

# Check plot of double SI distribution
hist(SIdouble_lnorm_sim,
  breaks = seq(-0.5, 3015.5, 5), cex.main = 0.9, xlim = c(0, 420), ylim = c(0, 0.02),
  col = "lightgrey", main = "", xlab = "Double Serial Interval", ylab = "Density", freq = F, axes = T
)
box(bty = "l")
lines(seq(0, 450, 1), dlnorm(seq(0, 450, 1), meanlog = coef(SIdouble_lnorm_lnorm)["meanlog"], sdlog = coef(SIdouble_lnorm_lnorm)["sdlog"]),
  col = "orange", lwd = 1.5, lty = 2
)
text(350, 0.015, paste("mean=", round(mean(SIdouble_lnorm_sim), 2), sep = ""), cex = 0.7)

# Store data table - Incubation period, Infectious period, Serial interval
params_table <- rbind(
  # Incubation period
  dist_fit_info(inc_gamma, "incubation"), # gamma
  dist_fit_info(inc_lnorm, "incubation"), # lnorm
  dist_fit_info(inc_weibull, "incubation"), # weibull
  # Infectious period
  dist_fit_info(inf_cens_gamma, "infectious"), # gamma
  dist_fit_info(inf_cens_lnorm, "infectious"), # lnorm
  dist_fit_info(inf_cens_weibull, "infectious"), # weibull
  # Serial interval
  dist_fit_info(ser_gamma, "SI"), # lnorm
  dist_fit_info(ser_lnorm, "SI"), # gamma
  dist_fit_info(ser_weibull, "SI") # weibull
)

write.csv(params_table, "output/params_table1.csv", row.names = FALSE)
