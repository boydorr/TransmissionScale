n.priors <- 50000

# Use a Gamma distribution for handling times, thus Shape and Scale
my.priors <- data.frame(ThShape = rep(0, n.priors), ThScale = rep(0, n.priors), Td = rep(0, n.priors)) 
for (rr in 1:n.priors) {
  print(rr)
  ThShape <- runif(1, 0.5, 10)
  ThMean <- runif(1, 0.001, 3) # Mean handling time shouldn't be less than 0.0347 days = 50 minutes
  ThScale <- ThMean/ThShape
  Td <- runif(1, 0, 25)
  my.priors[rr,1:3] <- c(ThShape, ThScale, Td)
}
hist(my.priors$ThShape)
hist(my.priors$ThScale)
write.table(my.priors, file = "output/Round1_priors.csv", append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = "")
#write.table(my.priors, file = paste0("../data_transferred/sampling_params_",model,".csv"), append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, fileEncoding = "")
summary(my.priors$ThShape*my.priors$ThScale) # Mean handling time

# # Check that it all worked OK
# test <- read.csv(file = "~/Documents/large_data/v04/infData_fixedDD_1000/output_files/infData_fixedDD_1000_sim_cases_001_001.csv")
# hist(test$Th, freq = F); mean(test$Th)
# ThScale=2.42325876135146
# ThShape=5.5735694821924
# hist(rgamma(10000, shape = ThShape, scale = ThScale), border="red", add=T, freq = F)
# 
# test <- read.csv(file = "~/Documents/large_data/v04/infData_fixedDD_1000/output_files/infData_fixedDD_1000_sim_cases_002_001.csv")
# hist(test$Th, freq = F); mean(test$Th)
# ThScale=9.5999389491044
# ThShape=0.945977188763209
# hist(rgamma(10000, shape = ThShape, scale = ThScale), border="red", add=T, freq = F)
# min(test$Th)
# 
# test <- read.csv(file = "~/Documents/large_data/v04/infData_fixedDD_1000/output_files/infData_fixedDD_1000_sim_cases_006_001.csv")
# hist(test$Th, freq = F); mean(test$Th)
# ThScale=7.21694943943294
# ThShape=0.52040371124167
# hist(rgamma(10000, shape = ThShape, scale = ThScale), border="red", add=T, freq = F)
# min(test$Th)
