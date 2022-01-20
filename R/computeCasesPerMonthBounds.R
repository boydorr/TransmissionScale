#' @title Computes confidence intervals around distribution of cases per month
#' @description Only run once, so not optimised
#' @export
computeCasesPerMonthBounds <- function(cases.per.month, ci = 0.95, hist.breaks, n.draws = 10000, n.months = 168) {
  # Fit a negative binomial distribution to the data
  fit.nb <- fitdist(cases.per.month, distr="nbinom", method="mle")
  print(fit.nb)
  h.data <- hist(as.numeric(cases.per.month), breaks=hist.breaks, plot=F)
  hist.mids <- h.data$mids

  # Now draw from the ml distribution and compute bounds which contain ci % of the draws
  my.draws <- matrix(nrow = n.draws, ncol = n.months)
  for (rr in 1:nrow(my.draws)) {
    my.draws[rr,] <- rnbinom(n = n.months, size = fit.nb$estimate["size"], mu = fit.nb$estimate["mu"])
  }
  #plot(h.data$mids, h.data$counts, type="l", xlab="Cases", ylab="Months (density)")
  my.hists <- matrix(nrow = n.draws, ncol = length(hist.mids))
  for (rr in 1:nrow(my.draws)) {
    h <- hist(my.draws[rr,], plot=F, breaks = hist.breaks)
    my.hists[rr,] <- h$counts
  }
  ci.edge <- (1 - ci)/2
  my.bounds <- data.frame(cases=hist.mids, lb = NA, ub = NA)
  for (dr in 1:ncol(my.hists)) {
    my.bounds[dr,c("lb","ub")] <- as.numeric(quantile(my.hists[,dr], probs = c(ci.edge, 1-ci.edge)))
  }
  # Plot bounds and the data
  plot.bounds <- subset(my.bounds, cases < max(hist.mids)) # exclude the last one!
  plot(plot.bounds$cases, plot.bounds$ub, type="l", lty=3, xlab="Cases", ylab="Months (count)")
  lines(plot.bounds$cases, plot.bounds$lb, type="l", lty=3)
  lines(h.data$mids, h.data$counts, type="l")

  # Max number of monthly cases that could be accepted
  print(paste0("The max number of cases in a month that could be accepted is ", my.bounds$cases[max(which(my.bounds$ub > 0))]))

  return (my.bounds)
}
#my.bounds <- compute.cases.per.month.bounds(cases.per.month, ci = 0.95, hist.breaks = seq(0,250,by=1), n.draws = 10000)
#dim(my.bounds)
