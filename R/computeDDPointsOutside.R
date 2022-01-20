#' @title Computes the number of cases per month points that are outside the bounds
#' @description Does this for cases.per.month
#' @export
computeDDPointsOutside <- function(dd.bounds, sim.dd, hist.breaks, hist.breaks.mids, do.plot = F) {

  # h <- hist(sim.dd, breaks=hist.breaks, plot=F)
  # we want densities, rather than counts ... (the postprocessor provides counts)
  h <- data.frame(count = sim.dd, density=sim.dd/sum(sim.dd))
  h$mids <- hist.breaks.mids

  # check number of points outside the bounds
  h$lower.outside <- ((h$density - dd.bounds$lb) < 0)
  h$upper.outside <- ((h$density - dd.bounds$ub) > 0)
  h$outside <- h$lower.outside | h$upper.outside

  if (do.plot) {
    plot(dd.bounds$cases, dd.bounds$ub, type="l", lty=3, xlim=c(0,250))
    lines(h$mids, h$density, type="l", xlab="Cases", ylab="Months (count)")
    lines(dd.bounds$cases, dd.bounds$lb, lty=3)
    lines(h$mids, h$density, col="orange")
    points(h$mids, h$density, col=factor(h$outside))
  }

  # Compute the total number that are outside
  return(sum(h$outside))
}
