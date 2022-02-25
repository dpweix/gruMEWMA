#' Train mrfMCUSUM method
#'
#' Trains the mrfMCUSUM method.
#' 
#'
#' @param data A multivariate time series in dataframe or matrix form. 
#' @param lags The number of lags of each variable to be included in the design matrix.
#' @param k A tuning parameter for the MCUSUM, large k results in shorter memory.
#' @return A named list including the plotting statistic, trained model, residuals, and constants.
#' @export


train_mrfMCUSUM <- function(data, lags = 1, k = .9) {
  l <- lags
  p <- ncol(data)
  
  constants <- c(k, l, p)
  names(constants) <- c("k", "lags", "p")
    
  X <- create_X(data, lags = l)
  Y <- create_Y(data, lags = l)
  
  
  fit_mrf <-
    MultivariateRandomForest::build_single_tree(X, Y,
                                                m_feature = floor(sqrt(l*p)),
                                                min_leaf = 10,
                                                Inv_Cov_Y = solve(cov(Y)),
                                                Command = 2)
  
  mrf_preds <- MultivariateRandomForest::single_tree_prediction(fit_mrf, X, p)
  colnames(mrf_preds) <- colnames(data)
  
  # Get Tau
  tau <- calc_tau(Y - mrf_preds)
  
  mu_tau <- colMeans(tau)
  sigma_tau_inv <- solve(cov(tau))
  
  # Get S
  S <- calc_S(tau, k, mu_tau, sigma_tau_inv)
  
  # Plotting Statistic
  pstat <- calc_PStat(S, sigma_tau_inv)
  
  # Return
  list(pstat = pstat,
       model = fit_mrf,
       residuals = Y - mrf_preds,
       mu_tau = mu_tau,
       sigma_tau_inv = sigma_tau_inv,
       constants = constants)
}
