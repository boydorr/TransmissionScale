# Code to produce distribution of Rt with and without clustering effects
rm(list=ls())
options(stringsAsFactors = F)

library(ggplot2)
library(dplyr)
library(tidyr)

source("Style_Sheet.R")

# Compute bite distribution from final posterior distribution simulation
nruns <- 1000
nBitten <- densityBitten <- data.frame()
plot(NA, NA, xlim = c(0,50), ylim = c(0,400))

for(i in 1:nruns){
  sim_output <- read.csv(file = paste0("java_output/Round3_1000/output_files/Round3_1000_sim_cases_", sprintf("%03d", i),".csv"))
  bite_dist <- hist(sim_output$nDistinctDogs, -1:200, plot =FALSE)
  nBites <- bite_dist$counts
  densBites <- bite_dist$density
  nBitten <- rbind(nBitten, nBites) 
  densityBitten <- rbind(densityBitten, densBites) 
  lines(0:200, nBitten[i,])
  print(i)
}
saveRDS(nBitten, file = "java_output/nBitten_distribution.rds")
saveRDS(densityBitten, file = "java_output/densityBitten_distribution.rds")

##########################################################
ff <- "java_output/"
ff_figs <- "figs/"
end.in.days <- 5112

# Construct dataset for LOWER_VAX and NAIVE_POP
f_LOWER_VAX <- "Round3_LOWER_VAX_1000" # full simulation - combination of incursions but mostly endogenous
f_NAIVE_POP <- "Round3_NAIVE_POP_1000" # index cases selected from endemic network
f_R0_MULTI <- "Round3_R0_MULTI_1000" # index cases selected by density
f_R0_by_location <- "Round3_R0_MULTI_LOCATION_1000" # index cases selected by grid cell

compile_sim_output <- function(scenario, n_runs){ 
  # scenario <- f_R0_by_location; n_runs <- 1
  summary_stats <- data.frame()
  
  for (i in 1:n_runs) {
    print(i)
    sim_output <- read.csv(file = paste0(ff, scenario, "/output_files/", scenario, "_sim_cases_", sprintf("%03d", i),".csv"))
    sim_output <- filter(sim_output, dayInfectious < end.in.days)
    # dim(sim_output); summary(factor(sim_output$typeOfCase)); summary(sim_output)

    run_summary <- sim_output %>% 
      select(nDistinctDogs, nDistinctE, nDistinctS, nDistinctV, nTransmissions) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
    
    # if (is.nan(run_summary$nTransmissions) | is.na(run_summary$nTransmissions)) {
    #   print(paste0("Problem at run ", i))
    #   print(run_summary)
    # }
    
    if (nrow(summary_stats) == 0) { summary_stats <- cbind(run_id = i, run_summary)
    } else { summary_stats <- rbind(summary_stats, cbind(run_id = i, run_summary)) }# inefficient, but for convenience
  }
  return(summary_stats)
}

sum_Re <- compile_sim_output(f_LOWER_VAX, 1000); # low vax endemic simulation
sum_endemic <- compile_sim_output(f_NAIVE_POP, 1000); # low vax - transmission network index
sum_density <- compile_sim_output(f_R0_MULTI, 1000); # by density index
sum_location <- compile_sim_output(f_R0_by_location, 1000); # by location index
sum_Re$Simulation <- "Re"
sum_endemic$Simulation <- "endemic"
sum_density$Simulation <- "density"
sum_location$Simulation <- "location"

saveRDS(sum_Re, file = "java_output/summary_Re.rds")
saveRDS(sum_endemic, file = "java_output/summary_R0_endemic.rds")
saveRDS(sum_density, file = "java_output/summary_R0_density.rds")
saveRDS(sum_location, file = "java_output/summary_R0_location.rds")

# Mean R0s and Re, and their quantiles
nCases <- mean(sum_Re$nTransmissions); nCases
nCases_index <- median(sum_endemic$nTransmissions); nCases_index
mean(sum_density$nTransmissions, na.rm=T)
mean(sum_location$nTransmissions, na.rm=T)

summary(sum_Re$nTransmissions)
summary(sum_endemic$nTransmissions)
summary(sum_density$nTransmissions)
summary(sum_location$nTransmissions)

quantile(sum_Re$nTransmissions, c(0.025, 0.5, 0.975), na.rm=TRUE)
quantile(sum_endemic$nTransmissions, c(0.025, 0.5, 0.975), na.rm=TRUE)
quantile(sum_density$nTransmissions, c(0.025, 0.5, 0.975), na.rm=TRUE)
quantile(sum_location$nTransmissions, c(0.025, 0.5, 0.975), na.rm=TRUE)


# Mean susceptibles bitten and their quantiles
nBites <- median(sum_Re$nDistinctDogs); nBites # 2.22
quantile(sum_Re$nDistinctDogs, c(0.05, 0.5, 0.95))

median(sum_endemic$nDistinctDogs) # 2.922
median(sum_density$nDistinctDogs, na.rm=T) # 2.90
median(sum_location$nDistinctDogs, na.rm=T) # 2.61 by location, just get 2.6
pInfect <- nCases_index/nBites_index

# Calculate the percentage reductions:
perc_reduction <- mean(sum_Re$nTransmission, na.rm = T) / mean(sum_endemic$nTransmissions, na.rm = T) 
paste0("The effect of clusters of cases reduced overall transmission by ", 
       sprintf("%0.2f", perc_reduction*100), "%, relative to equivalent index cases. (but endemic scenario has some vax")

num_reduction <- mean(sum_endemic$nTransmissions, na.rm = T) - mean(sum_Re$nTransmissions, na.rm = T)
paste0("The effect of clusters of cases reduced the mean number of transmissions per case by ", 
       sprintf("%0.2f", num_reduction), " secondary cases.")

# Work out reduction in transmission without any vaccination effects
# Reduced dogs bitten in endemic vs index (endemic network) scenario
dogs_bitten_reduction <- mean(sum_endemic$nDistinctDogs, na.rm=TRUE) - mean(sum_Re$nDistinctDogs, na.rm=TRUE); dogs_reduction 
nDistinctDogs_diff <- mean(sum_endemic$nDistinctDogs - sum_Re$nDistinctDogs); nDistinctDogs_diff
paste0("The effect of clusters of cases reduced the mean number of dogs bitten per case by ", 
       sprintf("%0.2f", dogs_bitten_reduction))

# Of dogs bitten - some of them are already exposed in endemic vs index (endemic network) scenario
# Overall reduction in Susceptible dogs bitten - approx 1 - therefore 0.5 reduction in R
nDistinctS_diff <- mean(sum_endemic$nDistinctS) - mean(sum_Re$nDistinctS); nDistinctS_diff 
nDistinctS_diff <- mean(sum_endemic$nDistinctS - sum_Re$nDistinctS); nDistinctS_diff 

# Exposed dogs that were bitten: 0.15 
exposed <- mean(sum_Re$nDistinctE) - mean(sum_endemic$nDistinctE); exposed
nDistinctE_diff <- mean(sum_Re$nDistinctE) - mean(sum_endemic$nDistinctE); nDistinctE_diff 
nExp <-  mean(sum_Re$nDistinctE)
quantile(sum_Re$nDistinctE, c(0.05, 0.5, 0.95))

# Vaccinated bitten dogs
vax_bitten <- mean(sum_Re$nDistinctV) - mean(sum_endemic$nDistinctV); vax_bitten
nVax <- mean(sum_Re$nDistinctV)
quantile(sum_Re$nDistinctV, c(0.05, 0.5, 0.95))

diff_bitten = (vax_bitten + exposed + nDistinctS_diff)
mean(sum_endemic$nDistinctS) 
nBites_index <- median(sum_endemic$nDistinctDogs); nBites_index
quantile(sum_endemic$nDistinctDogs, c(0.05, 0.5, 0.95))
mean(sum_Re$nDistinctS); quantile(sum_Re$nDistinctS, c(0.05, 0.5, 0.95))

mean(sum_Re$nDistinctDogs) 
mean(sum_endemic$nDistinctDogs) - diff_bitten

# % reduction in dogs bitten due to prior exposure
exp_red_pc = mean(sum_Re$nDistinctE)/mean(sum_Re$nDistinctDogs) # 7% reduction
# % reduction in dogs bitten 
bite_red_pc = (mean(sum_endemic$nDistinctDogs) - mean(sum_Re$nDistinctDogs))/mean(sum_endemic$nDistinctDogs) # 24%
total_red = exp_red_pc + bite_red_pc; total_red # 31%
exp_red_pc/total_red; bite_red_pc/total_red

total_bitten/2 # 0.664 would have gone on to get rabies
v_reduction/2 # bute some were protected by vax
(total_bitten/2) - (v_reduction/2)
#    We observe a reduction of about 1 less susceptible dog bitten (but some is due to vaccination, 0.164), 
# due to about 0.15 exposed dogs bitten (0.7+0.15) = 0.85 removed dogs (?). 

# Under Re scenario (endemic transmission) the number of *transmissions* is reduced because of:
#       (a) reduced number of dogs available to bite; and (b) biting of exposed dogs.

# Proportion distinct bites to S and E by sim type
propS_Re <- mean(sum_Re$nDistinctS) / mean(sum_Re$nDistinctS + sum_Re$nDistinctE) # Re i.e. endemic - 0.926
propS_R0_endemic <- mean(sum_endemic$nDistinctS) / mean(sum_endemic$nDistinctS + sum_endemic$nDistinctE) # R0 on endemic network # = 1, trivially
# Comparing the normal version with R0 doesn't really make sense because we know that we have vaccination 
# and the LOWER_VAX version is tricky to compare with R0 (or is it?)

####################################################
# Estimates of how contacts and transmission are affected
####################################################

# Index scenario
nBites_index
# Baseline scenario
nBites; pInfect; nVax; nExp

##########################
# Proportion of susceptibles reduced:
pS_Deaths = (nBites_index - nBites)/nBites_index; pS_Deaths # due to deaths 
pS_Vax = nVax/nBites; pS_Vax # due to Vax 
pS_Exp = nExp/nBites; pS_Exp # due to exposures

# loss of effective contacts
nS = nBites - nVax - nExp
lost_contacts = nBites_index - nS
lost_contacts
lost_contacts/nBites_index
R = nS * pInfect; R # in endemic situation R --> 1

# reduced transmission
R0 = nBites_index * pInfect; R0
ReDeaths = nBites * pInfect; ReDeaths
pR_deaths = (R0 - ReDeaths)/R0; pR_deaths # 23% reduction in transmission due to deaths 

R_adj = nBites_index * (1-pS_Vax) * pInfect; R_adj # discount vaccination
ReSus = nBites_index * (1-pS_Vax-pS_Exp) * pInfect; ReSus # discount prior exposures
pR_exp = (R_adj - ReSus)/R_adj; pR_exp # 7.3% reduction in transmission from prior exposure

Re = nBites * (1-pS_Vax-pS_Exp) * pInfect; Re
pR = (R_adj - Re)/ R_adj
pR # 30% overall reduction in transmission (discount vax)

##########################
# 23% due to deaths, 8% due to prior exposure --> 
pR_deaths + pR_exp
pR_deaths/(pR_deaths + pR_exp) # 76% due to deaths
pR_exp/(pR_deaths + pR_exp) # 24% from exposures



################# Process for mapping R ############################
# Load up map
load(file = "data/grd_1000.rda") # grd.1000@data <- grd.1000@data[,1:2]
head(grd.1000@data)
dim(grd.1000) # 2770

# Convert grid for ggplot
sf_grd_1000 <- st_as_sf(grd.1000)

# Compile data on number of cases per gridcell
n_runs <- 1000
popID_levels <- grd.1000$popID
grd_pops <- grd.1000@data[, 3:ncol(grd.1000)]
dim(grd_pops)

# Take scenario and number of runs # scenario =  f_LOWER_VAX; f_NAIVE_POP; f_R0_multi
compile_sim_output <- function(scenario, n_runs){ 
  
  # Prepare dataframes for storage
  mapped_R <- data.frame()
  mapped_nCases <- data.frame()
  
  # Create a pdf to look at outputs (just 100 for now) 
  # scenario <- f_R0_by_location 
  # n_runs = 1
  fname <- paste("java_output/",scenario,"sim_output_runs.pdf")
  pdf(fname)
  par(mfrow = c(2,1), mar = c(2,2,1,1)) # set up plotting area
  
  for (i in 1:n_runs) {
    print(i)
    sim_output <- read.csv(file = paste0(ff, scenario, "/output_files/", scenario, "_sim_cases_", sprintf("%03d", i),".csv"))
    
    #if(nrow(sim_output>0)){
    sim_output <- filter(sim_output, dayInfectious < end.in.days)
    # dim(sim_output); summary(factor(sim_output$typeOfCase))
    sim_output$popID_Factor <- factor(sim_output$popID, levels = popID_levels)
    
    run_summary <- sim_output %>%
      group_by(popID_Factor, .drop = F) %>%
      dplyr::summarise(nCases = length(caseID), 
                       nTransmissions = sum(nTransmissions, na.rm=T))
    run_summary$R <- run_summary$nTransmissions / run_summary$nCases # all transmissions in this cell divided by all cases
    
    # head(run_summary); dim(run_summary)
    # } else { run_summary <- data.frame(popID_Factor = factor(popID_levels), nTransmissions = NA, nCases = NA, R = NaN)}
    
    if (nrow(mapped_R) == 0) {
      mapped_R <- run_summary[c("popID_Factor","R")]
      mapped_nCases <- run_summary[c("popID_Factor","nCases")]
    } else {
      mapped_R <- cbind(mapped_R, run_summary[c("R")])
      mapped_nCases <- cbind(mapped_nCases, run_summary[c("nCases")])
    } 
    
    # prepare stats to annotate figures
    # R = mean(run_summary$R[which(run_summary$R!="NaN")]) # doing the mean across cells underestimates overall R (because lots of zero cells)
    cases = sum(run_summary$nCases)
    transmissions = sum(run_summary$nTransmissions)
    R = round(transmissions/cases,3)
    
    # plots for the first 100 runs to sanity check some outputs!
    if(i < 20 & !is.na(cases)) { 
      hist(sim_output$dayInfectious, breaks = seq(0, end.in.days + 30.5, 30.5), 
           main = paste0("R = ", R, " cases = ", cases, " transmission = ", transmissions))
      plot(sim_output$x_coord, sim_output$y_coord, cex = 0.1, col = alpha("black", 0.4))
    }
  }
  dev.off()
  
  # Compute R and cases
  mapped_R$popID_Factor <- NULL
  mapped_nCases$popID_Factor <- NULL
  matrix_mapped_R <- as.matrix(mapped_R)
  matrix_mapped_nCases <- as.matrix(mapped_nCases)
  #ifelse(nrow(matrix_mapped_R))
  
  # output the simulation metrics
  list(mapped_R = mapped_R,        
       median_R = matrixStats::rowMedians(matrix_mapped_R, na.rm = T, nan.rm = T), #, # median R in each cell
       mean_R = matrixStats::rowMeans2(matrix_mapped_R, na.rm = T, nan.rm = T), # mean R in each cell
       mapped_Cases = mapped_nCases,
       median_nCases = matrixStats::rowMedians(matrix_mapped_nCases, na.rm = T, nan.rm = T),
       mean_nCases = matrixStats::rowMeans2(matrix_mapped_nCases, na.rm = T, nan.rm = T) # mean_nCases_adj <- mean_nCases; mean_nCases_adj[which(mean_nCases > 25)] <- 25
  )
}

# Generate outputs - takes 5 mins!
low_vax = compile_sim_output(f_LOWER_VAX, 1000) # Re
naive = compile_sim_output(f_NAIVE_POP, 1000) # CASE DENSITY 
dog_density = compile_sim_output(f_R0_multi, 1000) # DOG DENSITY 
dog_location = compile_sim_output(f_R0_by_location, 1000) # DOG LOCATION
saveRDS(low_vax, file = "java_output/low_vax_R.rds")
saveRDS(naive, file = "java_output/naive.rds")
saveRDS(dog_density, file = "java_output/density.rds")
saveRDS(dog_location, file = "java_output/location.rds")

# low_vax <- readRDS(file = "java_output/low_vax_R.rds")
# naive <- readRDS(file = "java_output/naive.rds")
# dog_density <- readRDS(file = "java_output/density.rds")
# dog_location <- readRDS(file = "java_output/location.rds")

sf_grd_1000$nCases_lv <- low_vax$median_nCases; range(sf_grd_1000$nCases_lv)
sf_grd_1000$AvgCases_lv <- low_vax$mean_nCases; range(sf_grd_1000$AvgCases_lv)
sf_grd_1000$nCases_naive <- naive$median_nCases; range(sf_grd_1000$nCases_naive)
sf_grd_1000$AvgCases_naive <- naive$mean_nCases; range(sf_grd_1000$AvgCases_naive)
sf_grd_1000$nCases_dog_density <- dog_density$median_nCases; range(sf_grd_1000$nCases_dog_density)
sf_grd_1000$AvgCases_dog_density <- dog_density$mean_nCases; range(sf_grd_1000$AvgCases_dog_density)
sf_grd_1000$nCases_location <- dog_location$median_nCases; range(sf_grd_1000$nCases_location)
sf_grd_1000$AvgCases_location <- dog_location$mean_nCases; range(sf_grd_1000$AvgCases_location)

sf_grd_1000$R_lv <- low_vax$median_R; range(sf_grd_1000$R_lv[-which(sf_grd_1000$R_lv == "NaN")])
sf_grd_1000$Re_lv <- low_vax$mean_R; range(sf_grd_1000$Re_lv[-which(sf_grd_1000$Re_lv == "NaN")])
sf_grd_1000$R_naive <- naive$median_R; range(sf_grd_1000$R_naive[-which(sf_grd_1000$R_naive == "NaN")])
sf_grd_1000$Re_naive <- naive$mean_R; range(sf_grd_1000$Re_naive[-which(sf_grd_1000$Re_naive == "NaN")])
sf_grd_1000$R_dog_density <- dog_density$median_R; range(sf_grd_1000$R_dog_density[-which(sf_grd_1000$R_dog_density == "NaN")])
sf_grd_1000$Re_dog_density <- dog_density$mean_R; range(sf_grd_1000$Re_dog_density[-which(sf_grd_1000$Re_dog_density == "NaN")])
sf_grd_1000$R_location <- dog_location$median_R; range(sf_grd_1000$R_location)
sf_grd_1000$Re_location <- dog_location$mean_R; range(sf_grd_1000$Re_location)

saveRDS(sf_grd_1000, file = "java_output/grd_1000_summary.rds")
# sf_grd_1000 <- readRDS(file="java_output/grd_1000_summary.rds")




