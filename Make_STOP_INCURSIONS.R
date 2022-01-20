# Make shorter incursion list, for STOP_INCURSIONS simulations, Rebecca Mancy, started 2021-06-29
rm(list=ls())
library(dplyr)

setwd("~/Developer/Serengeti_Rabies/")

f_INCURSIONS <- "output/incursions_java.csv"
f_STOP_INCURSIONS <- "output/stop_incursions_java.csv"

# Read in incursions file
incursions_java <- read.csv(file = paste0(f_INCURSIONS))
dim(incursions_java)
  
# Filter on incursions during the first year only
stop_incursions_java <- filter(incursions_java, dayInfectious <= 365)
dim(stop_incursions_java)
  
# Write out to file
write.table(stop_incursions_java, file = paste0(f_STOP_INCURSIONS), 
              append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = "")
  
