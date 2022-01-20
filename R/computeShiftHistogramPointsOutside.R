#' @title Computes the number of points in the shift histogram that are outside the bounds
#' @description Does this for cases.per.month
#' @export
computeShiftHistogramPointsOutside <- function(my.bounds, densities, hist.breaks, hist.mids, do.plot = F) {

  densities.hist <- list(density=densities, mids = hist.mids)

  # check number of points outside the bounds
  densities.hist$lower.outside <- ((densities.hist$density - my.bounds$lb) < 0 & my.bounds$lb >= 0.015)
  densities.hist$upper.outside <- ((densities.hist$density - my.bounds$ub) > 0)
  densities.hist$outside <- densities.hist$lower.outside | densities.hist$upper.outside

  if (do.plot) {
    plot(my.bounds$cases, my.bounds$ub, type="l", lty=3)
    lines(densities.hist$mids, densities.hist$density, type="l", xlab="Cases", ylab="Months (count)")
    lines(my.bounds$cases, my.bounds$lb, lty=3)
    lines(densities.hist$mids, densities.hist$density, col="orange")
    points(densities.hist$mids, densities.hist$density, col=factor(densities.hist$outside))
    title(sum(densities.hist$outside))
  }

  # Compute the total number that are outside
  return(sum(densities.hist$outside))
}
# hist.breaks <- c(seq(0,250,by=2),10000)
# cases.per.month.bounds <- compute.cases.per.month.bounds(cases.per.month, ci = 0.95, hist.breaks = hist.breaks, n.draws = 1000)
# test <- compute.cases.per.month.outside(cases.per.month.bounds, cases.per.month, hist.breaks)
# for (rr in 1:100) {
#   test <- compute.cases.per.month.outside(cases.per.month.bounds, sim.cases.per.month=as.numeric(mt[rr,]), hist.breaks, do.plot=T); print(test)
# }
#cases.per.month.bounds <- compute.cases.per.month.bounds(cases.per.month, ci = 0.95, hist.breaks = hist.breaks, n.draws = 1000)
