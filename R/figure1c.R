#' @title Figure1c
#' @description Returns a ggplot plot
#' @param grd.1000
#' @param group.by Breaks for histogram calculations
#' @export
figure1c <- function(group.by, grd.1000.occ) {

  my.breaks <- seq(0, max(grd.1000.occ)+group.by, by=group.by)
  hist.density <- function(vec, my.breaks) {
    return (hist(vec, breaks = my.breaks, plot=F)$density)
  }
  # Compile histogram data into a data.frame, output, in which columns represent timepoints, and rows are the breaks for the histogram
  output <- data.frame()
  for (colnum in 1:ncol(grd.1000.occ)) {
    x <- hist.density(grd.1000.occ[,colnum], my.breaks)
    if (colnum == 1) {
      output <- data.frame(x)
    } else {
      output <- cbind(output, x)
    }
  }
  rmax <- apply(output, MARGIN = 1, FUN = max, na.rm=TRUE) # max for each row
  rmin <- apply(output, MARGIN = 1, FUN = min, na.rm=TRUE) # min for each row
  rmid <- hist.density(grd.1000.occ$`2557`, my.breaks)
  rcases <- hist.density(tcd.cases$dogDensityK, my.breaks)
  grd.densities <- data.frame(x = my.breaks[2:length(my.breaks)], dmin = rmin, dmax = rmax, dmid = rmid, cases = rcases)
  p <- ggplot(grd.densities, aes(x=x, y=dmid)) +
    geom_ribbon(aes(ymin = dmin, ymax = dmax), fill = sc.colours$bg.grey) + geom_line(colour = sc.colours$grey) +
    geom_line(aes(x=x, y=cases), colour = sc.colours$bright.red) +
    scale_x_continuous(~"Dogs per km" ^2) + scale_y_continuous("Proportion") +
    ss

return(p)
}
