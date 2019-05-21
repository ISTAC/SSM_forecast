
#' @title Extract the date of a ts object
#' @description Function to extract the date of a ts object
#' @param X Times series 
#' @return date of the time series  
#' @example 
#   as.Date.ts(my_time_series)

as_Date_ts <- function(x) {
  time.x <- unclass(time(x))
  if (frequency(x) == 1)
    {as.Date(paste(time.x, 1, 1, sep = "-"))}
  else if (frequency(x) == 4)
    {as.Date(paste((time.x + .001) %/% 1, 3*(cycle(x) - 1) + 1, 1, sep = "-"))}
  else if (frequency(x) == 12)
    {as.Date(paste((time.x + .001) %/% 1, cycle(x), 1, sep = "-"))}
  else
    {stop("unable to convert ts time to Date class")}
}
