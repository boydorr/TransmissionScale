# Saves a ggplot object A4 landscape, or for dimensions in cm, adding title. 
# Gets around some ggplot issue that means that ggsave doesn't save properly when used in a loop. 
# Also allows dimensions in cm, for those of us who struggle with inches!!
# Arguments: 
#   p - the ggplot plot object
#   title - title to be added at top of plot
#   file - full file path to save to (including .pdf)
#   width, height (optional) - in cm - will do A4 landscape by default
#   theme.override - optional - F by default; if true, will format using theme_classic()
save.ggplot <- function(p, title="", file, width.cm=29.7, height.cm=21.0, theme.override=F) {
  # Convert dimensions to inches
  width.in <- 0.39370079 * width.cm
  height.in <- 0.39370079 * height.cm 
  if (nchar(title>0)) { p <- p + ggtitle(title) } # Add title to plot object if longer than 0
  if (!theme.override) { p <- p + theme_classic() }
  ggsave(file, width=width.in, height=height.in, units=c("in"))
}
