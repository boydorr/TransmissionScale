# Make incursions under clustered scenario, Rebecca Mancy, started 2021-03-30

# Note that the files required to run this script are large and are not provided.

rm(list=ls())

ff <- "java_output/"
f_LOWER_VAX <- "Round3_LOWER_VAX_1000"
f_NAIVE_POP <- "Round3_NAIVE_POP_1000"
n_runs <- 1000

for (i in 1:n_runs) {
  print(i)
  
  # Read in output from the LOWER_VAX scenario
  sim_output_LOWER_VAX <- read.csv(file = paste0(ff, f_LOWER_VAX, "/output_files/", f_LOWER_VAX, "_sim_cases_",sprintf("%03d",i),".csv"))
  
  # Transform all cases to being recorded as INCURSION type
  sim_output_LOWER_VAX$typeOfCase <- "INCURSION"
  
  # Write out to file
  write.table(sim_output_LOWER_VAX, file = paste0(ff, f_NAIVE_POP, "/incursions/incursions_",sprintf("%03d",i),".csv"), 
              append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = "")
  
}

