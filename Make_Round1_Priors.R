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
summary(my.priors$ThShape*my.priors$ThScale) # Mean handling time


