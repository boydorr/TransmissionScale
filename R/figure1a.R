#
#' @title Figure1a
#' @description Returns a ggplot plot
#' @param grd
#' @param SD.2002
#' @param cases Case list csv
#' @export
figure1a <- function(grd, resolution, SD.2002, cases = NA) {
  # Other options for this joining here: https://stackoverflow.com/questions/22096787/how-keep-information-from-shapefile-after-fortify
  grd.df = data.table(fortify(grd, data = grd@data$`0`))
  grd.df = data.table(fortify(grd))
  grd.data <- data.table(cbind(grd@data), id = rownames(grd@data))
  grd.data$pop <- grd.data$`2557` / (resolution/1000) # compute population density from carrying capacity
  setkey(grd.df,id)
  setkey(grd.data, id)
  grd.df[grd.data, pop:=pop] # Note that for this to work inside the package function, need to add data.table to Imports: and Depends: in DESCRIPTION file
    # See https://stackoverflow.com/questions/27980835/r-data-table-works-in-direct-call-but-same-function-in-a-package-fails

  SD.df <- data.table(fortify(SD.2002))

  p.grd <- ggplot(grd.df, aes(x = long, y = lat)) +
    geom_map(map = grd.df, aes(map_id=id, fill = pop)) +
    scale_fill_gradient(low = "white", high = sc.colours$dark.navy, "Population\ndensity")
  if (!is.na(cases)) { # Case locations
    p.grd <- p.grd + geom_point(data=cases, aes(x = x_coord, y = y_coord), colour = sc.colours$bright.red, shape = 20, size = 0.5)
  }
  p.grd <- p.grd +
    geom_polygon(data = SD.df, aes(x = long, y = lat, group=id), fill=NA, colour=sc.colours$grey, size=0.2) + # villages!!
    coord_equal() + xlab("") + ylab("") +
    theme_classic() + theme(axis.text=element_blank(), axis.ticks=element_blank(), axis.line = element_blank(), legend.position=c(0.95,0.83)); p.grd

  return(p.grd)
}
