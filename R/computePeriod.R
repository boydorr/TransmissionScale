#' @title Constructs periods based on data with uncertainties, but does not add randomness
#' @description Written by Rebecca Mancy
#' @param period variable Incubation.period or Infectious.period from bite data
#' @param units variable Incubation.period.units or Infectious.period.units from bite data
#' @param start.date variable Date.bitten in string format
#' @param end.date variable Symptoms.started in string format
#' @param start.uncertainty variable Date.bitten.uncertainty
#' @param end.uncertainty variable Symptoms.started.accuracy
#' @export
computePeriod <- function(period, units, date.start, date.end, start.uncertainty, end.uncertainty, print.breakdown=F){
  date.start = as.Date(date.start)
  date.end = as.Date(date.end)

  # We note that, where dataset=="Manual", data were often collected noting e.g. "in May", "early May", "mid May", "late May". In this case
  #    data were processed prior to uploading, replacing with 15 +/- 28 days; 5 +/- 7 days, 15 +/- 7 days, 25 +/-7 days
  #    In other words, these were not measured directly, but converted when added to the database.
  #    This can lead to cases in which Symptoms.started <= Date.bitten.
  # Also, incubation periods of <3 days are biologically implausible.
  # The algorithm used here to compute incubation periods applies the following:
  #    1. If uncertainty for both Date.bitten and Symptoms.started == 0 (i.e. measured dates are exact), then
  #       compute incubation period from dates
  #    2. If there is some uncertainty about dates and using dates gives us an incubation period of < 3 days, then
  #       use Incubation.period if this is completed, and 3 otherwise
  #    3. If there is uncertainty and computing based on dates gives us >= 3 days, use dates, except when
  #       at least one of the uncertainties is in months, and incubation period is provided in weeks or days.

  # We therefore selected the measure with the best reported accuracy between periods and intervals,
  # on the basis of the least accurate of the uncertainties for the interval, and then the period in preference
  # if both approaches had the same uncertainty, because there are two uncertainties for the interval measure.
  # All measures are converted to days for analysis.

  # We convert all values to days, including incertainty
  period_units = rep(NA, length(units))
  day = grep("Day", units) # which ones are in days
  week = grep("Week", units)
  month = grep("Month", units)
  period_units[day] <- 1 # i.e. one day units
  period_units[week] <- 7 # i.e. 7 days = 1 week
  period_units[month] <- 30 # i.e. 30 days = 1 month
  P1 <- period * period_units # Period, directly from periods

  # Use the start and end dates, i.e. interval, to compute the period
  # Simulate start and end dates, incorporating uncertainties
  start_uncertainty = rep(NA, length(date.start))
  Suc0 = grep("0", start.uncertainty) # Suc1 = startUncertainty => which ones are +/- 1 week?
  Suc1 = grep("+/- 7 days", start.uncertainty) # Suc1 = startUncertainty => which ones are +/- 1 week?
  Suc2 = grep("+/- 14 days", start.uncertainty)
  Suc3 = grep("+/- 28 days", start.uncertainty)
  start_uncertainty[Suc0] <- 0
  start_uncertainty[Suc1] <- 7
  start_uncertainty[Suc2] <- 14
  start_uncertainty[Suc3] <- 28
  # Add random +/- to dates
  date.start[Suc1] = date.start[Suc1]+(runif(length(Suc1), -4, +4))   # +/- 7 days = early, late or mid in raw data
  date.start[Suc2] = date.start[Suc2]+(runif(length(Suc2), -10, +10)) # +/- 14 days (only in new data)
  date.start[Suc3] = date.start[Suc3]+(runif(length(Suc3), -20, +20)) # +/- 28 days = "in" the month

  end_uncertainty = rep(NA, length(date.end))
  Euc0 = grep("0", end.uncertainty)
  Euc1 = grep("+/- 7 days", end.uncertainty)
  Euc2 = grep("+/- 14 days", end.uncertainty)
  Euc3 = grep("+/- 28 days", end.uncertainty)
  end_uncertainty[Euc0] <- 0
  end_uncertainty[Euc1] <- 7
  end_uncertainty[Euc2] <- 14
  end_uncertainty[Euc3] <- 28
  # Add random +/- to dates
  date.end[Euc1] = date.end[Euc1]+(runif(length(Euc1), -4, +4))   # +/- 7 days = early, late or mid in raw data
  date.end[Euc2] = date.end[Euc2]+(runif(length(Euc2), -10, +10)) # +/- 14 days (only in new data)
  date.end[Euc3] = date.end[Euc3]+(runif(length(Euc3), -20, +20)) # +/- 28 days = "in" the month

  # Find the maximum level of uncertainty in the dates, and compute period from the dates
  date_uncertainty <- pmax(start_uncertainty, end_uncertainty, na.rm = T)
  P2 = as.numeric(date.end - date.start) # Period from the interval (dates)

  # The algorithm used here to compute incubation periods applies the following:
  #    1. If uncertainty for both Date.bitten and Symptoms.started == 0 (i.e. measured dates are exact), then
  #       compute incubation period from dates
  #    2. If there is some uncertainty about dates and using dates gives us an incubation period of < 3 days, then
  #       use Incubation.period if this is completed, and 3 otherwise
  #    3. If there is uncertainty and computing based on dates gives us >= 3 days, use dates, except when
  #       at least one of the uncertainties is in fortnights or months, and incubation period is provided in weeks or days.
  #    4. If there is a period, and no information from dates

  P <- rep(NA, length(date.start))
  P <- P2 # Initialise as based on dates

  # 1. In case 1., no need to do anything here
  # 2.
  P <- ifelse (date_uncertainty!=0 & P2 < 3 & !is.na(P1), P1, P)
  P <- ifelse (date_uncertainty!=0 & P2 < 3 &  is.na(P1),  3, P)
  # 3.
  P <- ifelse (date_uncertainty>=14 & !is.na(P1) & period_units<=7, P1, P)
  # 4.
  P <- ifelse (is.na(P) & !is.na(P1), P1, P)

  if (print.breakdown) {
    periods.df <- data.frame(period=P1, period.units=period_units, interval=P2, interval.uncertainty=date_uncertainty, final=P)
    print(periods.df)
  }
  return(P)
}
