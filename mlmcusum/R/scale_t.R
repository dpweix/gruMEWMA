#' Scale a TS
#'
#' Scale a univariate time series between lower bound a and upper bound b.
#' Used for generating t for a simulation study.
#' 
#'
#' @param ts A univariate time series.
#' @param a Lower bound of time series, default is 0.01.
#' @param b Upper bound of time series, default is 2.
#' @return Returns the scaled time series.
#' @export

scale_t <- function(ts, a = .01, b = 2) {
  as.numeric((b-a)*(ts - min(ts))/(max(ts) - min(ts)) + a)
}