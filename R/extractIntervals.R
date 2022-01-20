## Functions for dealing with uncertain dates/periods
extract_uncertainty = function(uncertainty){
  temp1 <- gregexpr("[0-9]+", uncertainty)  # Numbers with any number of digits
  as.numeric(unlist(regmatches(uncertainty, temp1))) # extract digits
}

convertDay <- Vectorize(function(x){
  if(x==""){return(NA)}
  if(x=="Day"){return(1)}
  if(x=="Week"){return(7)}
  if(x=="Month"){return(365.25/12)}
  stop("Can't convert unit", x)
})

intervalBound <- function(x){
  return(convertDay(x)/2)
}

