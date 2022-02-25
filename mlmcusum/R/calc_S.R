#' Calculate S matrix
#'
#' A helper function to calculate the S vector given tau, k,
#' mu_tau, and sigma_tau_inv. While k is a tuning parameter, the rest
#' are calculated from the data.
#' 
#'
#' @param tau A vector representing covariance.
#' @param k A tuning parameter.
#' @param mu_tau The mean of tau. Estimated from the training data.
#' @param sigma_tau_inv The inverse covariance matrix of tau. Estimated from the training data.
#' @return A vector S which is used to calculate the plotting statistic.
#' @export


calc_S <- function(tau, k, mu_tau, sigma_tau_inv) {
  N <- nrow(tau)
  p <- ncol(tau)
  
  S <- matrix(0, nrow = N, ncol = ncol(tau))
  C <- vector("numeric", N)
  C[1] <- sqrt(t(tau[1, ] - mu_tau) %*% sigma_tau_inv %*% (tau[1, ] - mu_tau))
  
  2:N |> 
    purrr::walk(
      \(x) {
        c <- S[x-1, ] + tau[x, ] - mu_tau
        C[x] <- sqrt(t(c) %*% sigma_tau_inv %*% c)
        
        
        if(C[x] > k) {
          s <- (S[x-1, ] + tau[x, ] - mu_tau)*(1-k/C[x])
          1:p |> 
            purrr::walk(
              \(y) {
                S[x, y] <<- s[y]
              })
        }
      })
  S
}