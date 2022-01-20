#' @title Computes the number of cases per month points that are outside the bounds
#' @description Does this for cases.per.month
#' @export
computeCasesPerMonthOutside <- function(cases.per.month.bounds, sim.cases.per.month, hist.breaks, do.plot = F) {
  h <- hist(sim.cases.per.month, breaks=hist.breaks, plot=F)
  # check number of points outside the bounds
  h$lower.outside <- ((h$counts - cases.per.month.bounds$lb) < 0 & cases.per.month.bounds$lb >= 0.002976190)
  h$upper.outside <- ((h$counts - cases.per.month.bounds$ub) > 0)
  h$outside <- h$lower.outside | h$upper.outside

  if (do.plot) {
    plot(cases.per.month.bounds$cases, cases.per.month.bounds$ub, type="l", lty=3, xlim=c(0,250))
    lines(h$mids, h$counts, type="l", xlab="Cases", ylab="Months (count)")
    lines(cases.per.month.bounds$cases, cases.per.month.bounds$lb, lty=3)
    lines(h$mids, h$counts, col="orange")
    points(h$mids, h$counts, col=factor(h$outside))
  }

  # Compute the total number that are outside
  return(sum(h$outside))
}
# hist.breaks <- c(seq(0,250,by=2),10000)
# cases.per.month.bounds <- compute.cases.per.month.bounds(cases.per.month, ci = 0.95, hist.breaks = hist.breaks, n.draws = 1000)
# test <- compute.cases.per.month.outside(cases.per.month.bounds, cases.per.month, hist.breaks)
# for (rr in 1:100) {
#   test <- compute.cases.per.month.outside(cases.per.month.bounds, sim.cases.per.month=as.numeric(mt[rr,]), hist.breaks, do.plot=T); print(test)
# }
#cases.per.month.bounds <- compute.cases.per.month.bounds(cases.per.month, ci = 0.95, hist.breaks = hist.breaks, n.draws = 1000)
