#' Create Design Matrix
#'
#' A helper function to create a design matrix to be the 
#' input for the machine learning MCUSUM methods. Requires
#' choosing the number of lags in addition to each variable.
#' Potentially later the number of lags could be a vector
#' of length p. That way the number of lags could be different
#' for each variable.
#' 
#'
#' @param data A dataframe or matrix of observations. 
#' @param lags The number of lags of each variable to be included in the design matrix.
#' @return A design matrix for training/predictions with the machine learning MCUSUM methods.
#' @export


create_X <- function(data, lags) {
  N <- nrow(data)
  p <- ncol(data)
  l <- lags
  
  
  1:(N-l) |> 
    purrr::map(
      \(x) {
        data[x:(x+l-1), 1:p] |> 
          unlist() |> 
          as.numeric()
      }) |> 
    unlist() |> 
    matrix(ncol = l*p, byrow = TRUE)
}