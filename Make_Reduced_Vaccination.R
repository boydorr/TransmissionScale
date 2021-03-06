# Rebecca Mancy, started 2021-01

rm(list=ls())
start.date <- as.Date("2002-01-01")
end.date <- as.Date("2015-12-31")

# Read in baseline vaccination (i.e. measured) and reduce to 1/3 of original coverage
vc <- read.csv(file = "java_code/Rabies_Sim_Output_Test/static_files/vc_1000.csv", stringsAsFactors = FALSE); nrow(vc)
vc_third <- vc
vc_third$DogsVaccinated <- round(vc$DogsVaccinated/3)
# Write csv of vc to file
write.table(vc_third, file = "java_code/Rabies_Sim_Output_Test/static_files/vc_third_1000.csv", append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = "")
