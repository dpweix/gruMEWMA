#' Create Prediction Matrix
#'
#' A helper function to create a prediction matrix for 
#' use when training the machine learning MCUSUM methods.
#' Requires choosing the number of lags so that the prediction
#' and design matrices line up.
#' 
#'
#' @param data A dataframe or matrix of observations. 
#' @param lags The number of lags of each variable, the first row of this matrix will be row lag + 1 of the data.
#' @return A prediction matrix which can be used for training/testing the machine learning MCUSUM methods.
#' @export


create_Y <- function(data, lags) {
  N <- nrow(data)
  p <- ncol(data)
  l <- lags
  
  data[(l+1):N, ] |> 
    as.matrix()
}