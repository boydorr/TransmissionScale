# Code to compute the effect of NAIVE_POP from specially-run simulations, Rebecca Mancy, 2021-06
rm(list=ls())

library(ggplot2)
library(dplyr)

# setwd("~/Developer/Serengeti_Rabies/")
# ff <- "/Volumes/SimOutput01/Rabies_Sim_Output/"
ff <- "java_output/"

f_LOWER_VAX <- "Round3_LOWER_VAX_1000" # NO VAX, so get R
f_NAIVE_POP <- "Round3_NAIVE_POP_1000" # cases are treated as index cases, so get R0_max
f_R0_multi <- "Round3_R0_MULTI_1000" # index cases selected by density

end.in.days <- 5112

# Loop over runs, and construct dataset
n_runs <- 1000
summary_stats <- data.frame()
for (i in 1:n_runs) {
  print(i)
  sim_output_LOWER_VAX <- read.csv(file = paste0(ff, f_LOWER_VAX, "/output_files/", f_LOWER_VAX, "_sim_cases_", sprintf("%03d", i),".csv"))
  sim_output_NAIVE_POP <- read.csv(file = paste0(ff, f_NAIVE_POP, "/output_files/", f_NAIVE_POP, "_sim_cases_", sprintf("%03d", i),".csv"))
  sim_output_LOWER_VAX <- filter(sim_output_LOWER_VAX, dayInfectious < end.in.days)
  sim_output_NAIVE_POP <- filter(sim_output_NAIVE_POP, dayInfectious < end.in.days)
  
  dim(sim_output_LOWER_VAX)
  dim(sim_output_NAIVE_POP)
  summary(sim_output_LOWER_VAX$typeOfCase)
  summary(sim_output_NAIVE_POP$typeOfCase)
  
  my_data <- left_join(sim_output_LOWER_VAX[c("caseID","dayInfectious","nExcursions","nTransmissions","dogCount","dogsE","nDistinctDogs","nDistinctS","nDistinctE","nDistinctV")], 
                       sim_output_NAIVE_POP[c("caseID","nExcursions","nTransmissions","dogCount","dogsE","nDistinctDogs","nDistinctS","nDistinctE","nDistinctV")], by = c("caseID" = "caseID"),
                       suffix = c("_LOWER_VAX","_NAIVE_POP"))
  dim(my_data)
  summary(my_data)
  
  # Compute proportional number of transmissions in x (lower vax) relative to y (NAIVE_POP)
  perc_reduction <- mean(my_data$nTransmissions_LOWER_VAX, na.rm = T) / mean(my_data$nTransmissions_NAIVE_POP, na.rm = T) 
  paste0("The effect of clusters of cases reduced overall transmission to ", sprintf("%0.2f", perc_reduction*100), "%, relative to naive introductions.")
  num_reduction <- mean(my_data$nTransmissions_NAIVE_POP, na.rm = T) - mean(my_data$nTransmissions_LOWER_VAX, na.rm = T) 
  paste0("The effect of clusters of cases reduced the mean number of transmissions per case by ", sprintf("%0.2f", num_reduction), " secondary cases, relative to naive introductions.")
  
  run_summary <- my_data %>% 
    select(nDistinctDogs_LOWER_VAX, nDistinctE_LOWER_VAX, nDistinctS_LOWER_VAX, nDistinctV_LOWER_VAX, nTransmissions_LOWER_VAX, nDistinctDogs_NAIVE_POP, nTransmissions_NAIVE_POP) %>%
    summarise(across(everything(), list(mean)))
  if (nrow(summary_stats) == 0) {
    summary_stats <- cbind(run_id = i, run_summary)
  } else {
    summary_stats <- rbind(summary_stats, cbind(run_id = i, run_summary)) # inefficient, but for convenience
  } 
  
  # Interpret red shifted to the right as meaning more transmissions in the NAIVE_POP test case than in the normal run (i.e. as expected)
  ### ggplot(my_data, aes(x=nTransmissions_LOWER_VAX)) + geom_histogram(alpha = 0.5, fill = "blue", binwidth = 2) + geom_histogram(aes(x=nTransmissions_NAIVE_POP), alpha = 0.5, fill="red", binwidth = 2)
  
}
summary(summary_stats)

# Work out statistics
dogs_reduction <- mean(my_data$dogCount_NAIVE_POP) - mean(my_data$dogCount_LOWER_VAX) # Fewer dogs in the LOWER_VAX case
e_reduction <- mean(my_data$dogsE_NAIVE_POP) - mean(my_data$dogsE_LOWER_VAX) # More E dogs in the LOWER_VAX
# What do we want to know to understand NAIVE_POP? In the LOWER_VAX we have fewer dogs overall, but more exposed dogs in each cell where a case occurs
#   So there are proportionally more bites made of exposed dogs (?)
#   So we would expect lower numbers of nDistinctS (because fewer dogs overall), and higher nDistinctE (because more dogs are exposed in the area)
nDistinctDogs_diff <- mean(my_data$nDistinctDogs_NAIVE_POP) - mean(my_data$nDistinctDogs_LOWER_VAX); nDistinctDogs_diff # LOWER_VAX numbers of distinctDogs is lower by about 0.6
nDistinctDogs_diff <- mean(my_data$nDistinctDogs_NAIVE_POP - my_data$nDistinctDogs_LOWER_VAX); nDistinctDogs_diff# LOWER_VAX numbers of distinctDogs is lower by about 0.6
nDistinctS_diff <- mean(my_data$nDistinctS_NAIVE_POP) - mean(my_data$nDistinctS_LOWER_VAX); nDistinctS_diff # LOWER_VAX numbers of distinctS is lower by about 0.95
nDistinctS_diff <- mean(my_data$nDistinctS_NAIVE_POP - my_data$nDistinctS_LOWER_VAX); nDistinctS_diff # LOWER_VAX numbers of distinctS is lower by about 0.95

nDistinctE_diff <- mean(my_data$nDistinctE_NAIVE_POP) - mean(my_data$nDistinctE_LOWER_VAX) # LOWER_VAX numbers of distinctE higher by about 0.17 (i.e. NAIVE_POP means each case bites more E dogs)
# In the LOWER_VAX situation, each case bites fewer dogs overall (because there are fewer dogs in the local area), but this is made up of a reduction in the number of susceptible dogs bitten, 
#    and an increase in exposed animals.
# So the reduction in the number of transmissions is driven by a local reduction in the number of dogs combined with the exposed status of dogs in the cell.
#    We observe a reduction of about 1 less susceptible dog bitten, due to about 0.17 exposed dogs bitten, and 0.6-0.17=0.44 removed dogs (?). The latter is 
#        the reduction in numbers of distinctDogs bitten overall in the LOWER_VAX scenario, minus those that were exposed, i.e. the 
#        So about one third of the effect 
#    is due to biting of exposed dogs, and two thirds due to reductions in numbers of dogs (?) - does this logic make sense?

# So the number of *transmissions* is reduced because of:
#       (a) reduced number of dogs available to bite; and (b) biting of exposed dogs, in the LOWER_VAX situation.
# There are fewer dogs available to bite by some number X, and 

# Proportion distinct bites to S and E by sim type
propS_LOWER_VAX <- mean(my_data$nDistinctS_LOWER_VAX) / mean(my_data$nDistinctS_LOWER_VAX + my_data$nDistinctE_LOWER_VAX)
propS_NAIVE_POP <- mean(my_data$nDistinctS_NAIVE_POP) / mean(my_data$nDistinctS_NAIVE_POP + my_data$nDistinctE_NAIVE_POP) # = 1, trivially

names(sim_output_NAIVE_POP)
# The NAIVE_POP run is a kind of R0, based on the location of cases (expect this to be a bit higher than R0), rather than on dogs on the landscape, so what we compute above is not strictly speaking a reduction in R0.
# Comparing the normal version with R0 doesn't really make sense because we know that we have vaccination ... and the LOWER_VAX version is tricky to compare
#    with R0 (or is it?)
# If we compared the effect of NAIVE_POP using the runs with vaccination (NORMAL), could we then compute 

