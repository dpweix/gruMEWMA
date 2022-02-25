#' Create tau vector
#'
#' A helper function to create the tau vector given a
#' vector of residuals from one of our methods.
#' 
#'
#' @param residuals The errors from one of our prediction methods on a Y matrix.
#' @return The tau vector to be used for calculating the S vector.
#' @export


calc_tau <- function(residuals) {
  p <- ncol(residuals)
  
  1:nrow(residuals) |> 
    purrr::map(
      \(x) {
        ks::vech(residuals[x, ] %*% t(residuals[x, ]))
      }) |> 
    unlist() |> 
    matrix(ncol = p*(p+1)/2)
}