#' @title Computes confidence intervals around distribution of standardised time-consistent log densities
#' @description A horribly unwieldy piece of code, but is only run once! Uses bootstrapping approach
#' @export
computeShiftBounds <- function(stdTCLogDensities, ci = 0.95, hist.breaks, n.draws = 10000, n.to.draw, sd.jitter=0.01) {

  h.data <- hist(as.numeric(stdTCLogDensities), breaks=hist.breaks, plot=F)
  hist.mids <- h.data$mids

  # Draw, with replacement, the n.to.draw case densities
  my.draws <- matrix(nrow = n.draws, ncol = n.to.draw)
  for (rr in 1:n.draws) {
    sampled.data <- sample(stdTCLogDensities, size = n.to.draw, replace = T)
    my.draws[rr,] <- sampled.data + rnorm(n = n.to.draw, mean = 0, sd = sd.jitter)
  }
  my.hists <- matrix(nrow = n.draws, ncol = length(hist.mids))
  for (rr in 1:nrow(my.draws)) {
    h <- hist(my.draws[rr,], plot=F, breaks = hist.breaks)
    my.hists[rr,] <- h$density
  }
  ci.edge <- (1 - ci)/2
  my.bounds <- data.frame(cases=hist.mids, lb = NA, ub = NA)
  for (dr in 1:ncol(my.hists)) {
    my.bounds[dr,c("lb","ub")] <- as.numeric(quantile(my.hists[,dr], probs = c(ci.edge, 1-ci.edge)))
  }
  plot(my.bounds$cases, my.bounds$ub, type="l", lty=3, xlab="Standardised log density", ylab="Relative frequency")
  lines(my.bounds$cases, my.bounds$lb, type="l", lty=3)
  lines(h.data$mids, h.data$density, type="l")

  return (my.bounds)
}
#my.bounds <- compute.cases.per.month.bounds(cases.per.month, ci = 0.95, hist.breaks = seq(0,250,by=1), n.draws = 10000)
#dim(my.bounds)
