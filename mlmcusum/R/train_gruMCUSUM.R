#' Train gruMCUSUM method
#'
#' Trains the gruMCUSUM method.
#' 
#'
#' @param data A multivariate time series in dataframe or matrix form. 
#' @param lags The number of lags of each variable to be included in the design matrix.
#' @param k A tuning parameter for the MCUSUM, large k results in shorter memory.
#' @return A named list including the plotting statistic, trained model, residuals, and constants.
#' @export


train_gruMCUSUM <- function(data, lags = 1, k = .9) {
  l <- lags
  p <- ncol(data)
  
  constants <- c(k, l, p)
  names(constants) <- c("k", "lags", "p")
  
  X <- create_X(data, lags = l)
  Y <- create_Y(data, lags = l)
  
  
  fit_gru <- train_gru(X, Y, l) # python
  
  gru_preds <- pred_gru(fit_gru, X) # python
  colnames(gru_preds) <- colnames(data)

  # Get Tau
  tau <- calc_tau(Y - gru_preds)
  
  mu_tau <- colMeans(tau)
  sigma_tau_inv <- solve(cov(tau))

  # Get S
  S <- calc_S(tau, k, mu_tau, sigma_tau_inv)

  # Plotting Statistic
  pstat <- calc_PStat(S, sigma_tau_inv)
  
  # Return
  list(pstat = pstat,
      model = fit_gru,
      residuals = Y - gru_preds,
      mu_tau = mu_tau,
      sigma_tau_inv = sigma_tau_inv,
      constants = constants)
}
