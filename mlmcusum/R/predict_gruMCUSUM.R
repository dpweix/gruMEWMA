#' Predicts using gruMCUSUM method
#'
#' Predicts the gruMCUSUM method.
#' 
#'
#' @param model Output from the train_gruMCUSUM function.
#' @param new_data A multivariate time series in dataframe or matrix form. 
#' @return A named list including the plotting statistic and residuals.
#' @export


predict_gruMCUSUM <- function(model, new_data) {
  k <- model$constants[1]
  l <- model$constants[2]
  
  X <- create_X(new_data, lags = l)
  Y <- create_Y(new_data, lags = l)

  
  gru_preds <- pred_gru(model$model, X) # python
  colnames(gru_preds) <- colnames(new_data)
  
  # Get Residuals
  residuals = Y - gru_preds
  
  # Get Tau
  tau <- calc_tau(residuals)
  
  # Get S
  S <- calc_S(tau, k, model$mu_tau, model$sigma_tau_inv)
  
  # Plotting Statistic
  pstat <- calc_PStat(S, model$sigma_tau_inv)
  
  # Return
  list(pstat = pstat,
       residuals = residuals)
}
