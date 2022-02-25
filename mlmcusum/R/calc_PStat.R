#' Calculate plotting statistic
#'
#' A helper function to calculate the plotting statistic given
#' S and sigma_tau_inv.
#' 
#'
#' @param S A matrix which tracks the MCUSUM through observations 
#' @param sigma_tau_inv The inverse covariance matrix of tau. Estimated from the training data.
#' @return The plotting statistic which determines if the process is considered in-control or out-of-control at a given observation.
#' @export


calc_PStat <- function(S, sigma_tau_inv) {
  N <- nrow(S)
  
  1:N |> 
    purrr::map_dbl(
      \(x) {
        sqrt(t(S[x, ]) %*% sigma_tau_inv %*% S[x, ])
      })
}