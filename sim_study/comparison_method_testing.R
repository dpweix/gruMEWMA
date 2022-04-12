library("tidyverse")
theme_set(theme_classic())
library("MTS")
library("vars")
library("marima")
library("mlmcusum")

dat_lin <- gen_dat_nlr("f1")

### MTS Attempt
model_var <- dat_lin[1:500, -1] |> 
  as.matrix()
  

model_var <- VARMA(dat_lin[1:1000, -1], p = 1, q = 1, include.mean = TRUE)

dat_lin |> 
  pivot_longer(-1) |> 
  ggplot(aes(index, value, color = name)) +
    geom_line()

model_var$residuals |> 
  as_tibble() |> 
  add_column(index = 1:nrow(model_var$residuals), .before = "V1") |> 
  pivot_longer(-1) |> 
  ggplot(aes(index, value, color = name)) +
  geom_line() +
  labs(title = "Exploding Errors - VARMA Training Set", y = "Residuals", color = "Variable") +
  ylim(-1e300, 1e300)

model_var$Ph0

model_var$Phi

model_var$Theta

VARMApred(model_var, h = 1)

model_var$Ph0+model_var$Phi %*% as.numeric(dat_lin[1000, -1])-model_var$Theta %*% model_var$residuals[999, ]

train_varmaMCUSUM <- function(data, k = 0.9) {
  l <- 1
  p <- ncol(data)
  
  constants <- c(k, l, p)
  names(constants) <- c("k", "lags", "p")
  
  X <- create_X(data, lags = l)
  Y <- create_Y(data, lags = l)
  
  
  fit_varma <-
    MTS::VARMACpp(data, p = 1, q = 1, include.mean = TRUE)
  
  residuals <- fit_varma$residuals
  colnames(residuals) <- colnames(data)
  
  # Get Tau
  tau <- calc_tau(residuals)
  
  mu_tau <- colMeans(tau)
  sigma_tau_inv <- solve(cov(tau))
  
  # Get S
  S <- calc_S(tau, k, mu_tau, sigma_tau_inv)
  
  # Plotting Statistic
  pstat <- calc_PStat(S, sigma_tau_inv)
  
  # Return
  list(pstat = pstat,
       model = fit_varma,
       residuals = residuals,
       mu_tau = mu_tau,
       sigma_tau_inv = sigma_tau_inv,
       constants = constants)
}

predict_varmaMCUSUM <- function(model, new_data) {
  # Extract VARMA(1, 1) Model Info
  k <- model$constants[1]
  Ph0 <- model$model$Ph0
  Phi <- model$model$Phi
  Theta <- model$model$Theta
  
  # Convert New Data to Matrix
  new_data <- as.matrix(new_data)
  
  # Prep Residual Matrix
  residuals <- matrix(nrow = nrow(new_data), ncol = ncol(new_data))
  
  # Calc First Row
  pred <- as.numeric(Ph0+Phi %*% as.numeric(tail(model$model$data, 1))-Theta %*% as.numeric(tail(model$model$residuals, 1)))
  
  1:ncol(new_data) |> 
    walk(\(j) {
      residuals[1, j] <<- new_data[1, j] - pred[j]
    })
    
  
  # Calculate Residuals
  2:nrow(new_data) |>
    walk(\(i) {
      
      pred <- as.numeric(Ph0+Phi %*% new_data[i-1, ]-Theta %*% residuals[i-1, ])
      
      1:ncol(new_data) |>
        walk(\(j){
          residuals[i,j] <<- new_data[i, j] - pred[j]
        })
    })
  
  # Get Tau
  tau <- calc_tau(residuals)
  
  # Get S
  S <- calc_S(tau, k, model$mu_tau, model$sigma_tau_inv)
  
  # Plotting Statistic
  pstat <- calc_PStat(S, model$sigma_tau_inv)
  
  # Return
  list(pstat = pstat,
       residuals = residuals,
       Ph0 = Ph0,
       Phi = Phi,
       Theta = Theta)
}

test_varma <- train_varmaMCUSUM(dat_lin[1:1000, -1])
pred_varma <- predict_varmaMCUSUM(test_varma, dat_lin[1001:2000, -1])

VARMApred(test_varma$model, 1)
plot(pred_varma$pstat)


pred_varma$residuals |> 
  as_tibble() |> 
  add_column(index = 1:nrow(pred_varma$residuals), .before = "V1") |> 
  pivot_longer(-1) |> 
  ggplot(aes(index, value, color = name)) +
  geom_line()


plot(test_varma$pstat)

# Comparison of Pstats
tibble(index = 2:2000,
       `No Fault` = c(test_varma$pstat, pred_varma$pstat[1:500], rep(NA, 500)),
       `Fault 1` = c(test_varma$pstat, pred_varma$pstat)) |> 
  pivot_longer(-index) |> 
  mutate(name = as_factor(name)) |> 
  ggplot(aes(index, value)) +
  geom_line() +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  geom_vline(xintercept = 1000, color = "blue") +
  geom_vline(xintercept = 1500, color = "red") +
  geom_hline(yintercept = h, color = "darkgreen") +
  labs(y = "Plotting Statistic", title = "varmaMEWMA Plotting Statistic")

### Try resMEWMA model from Bodnar et al (2017)
train_varmaMEWMA <- function(data, r = 0.3) {
  l <- 1
  p <- ncol(data)
  
  constants <- c(r, l, p)
  names(constants) <- c("r", "lags", "p")
  
  fit_varma <-
    MTS::VARMACpp(data, p = 1, q = 1, include.mean = TRUE)
  
  residuals <- fit_varma$residuals
  colnames(residuals) <- colnames(data)
  
  # Get Tau
  tau <- calc_tau(residuals)
  
  mu_tau <- colMeans(tau)
  sigma_tau_inv <- solve(cov(tau))
  
  # Get D
  D <- calc_D(tau, mu_tau, sigma_tau_inv)
  
  # Plotting Statistic
  pstat <- calc_PStat_MEWMA(r, D, p)
  
  # Return
  list(pstat = pstat,
       D = D,
       model = fit_varma,
       residuals = residuals,
       mu_tau = mu_tau,
       sigma_tau_inv = sigma_tau_inv,
       constants = constants)
}

predict_varmaMEWMA <- function(model, new_data) {
  # Extract VARMA(1, 1) Model Info
  r <- model$constants[1]
  p <- model$constants[3]
  
  Ph0 <- model$model$Ph0
  Phi <- model$model$Phi
  Theta <- model$model$Theta
  
  # Convert New Data to Matrix
  new_data <- as.matrix(new_data)
  
  # Prep Residual Matrix
  residuals <- matrix(nrow = nrow(new_data), ncol = ncol(new_data))
  
  # Calc First Row
  pred <- as.numeric(Ph0+Phi %*% as.numeric(tail(model$model$data, 1))-Theta %*% as.numeric(tail(model$model$residuals, 1)))
  
  1:ncol(new_data) |> 
    purrr::walk(\(j) {
      residuals[1, j] <<- new_data[1, j] - pred[j]
    })
  
  
  # Calculate Residuals
  2:nrow(new_data) |>
    purrr::walk(\(i) {
      
      pred <- as.numeric(Ph0+Phi %*% new_data[i-1, ]-Theta %*% residuals[i-1, ])
      
      1:ncol(new_data) |>
        purrr::walk(\(j){
          residuals[i,j] <<- new_data[i, j] - pred[j]
        })
    })
  
  # Get Tau
  tau <- calc_tau(residuals)
  
  # Get D
  D <- calc_D(tau, model$mu_tau, model$sigma_tau_inv)
  
  # Plotting Statistic
  pstat <- calc_PStat_MEWMA(r, D, p)
  
  # Return
  list(pstat = pstat,
       residuals = residuals,
       Ph0 = Ph0,
       Phi = Phi,
       Theta = Theta)
}

test_varma <- train_varmaMEWMA(dat_lin[1:1000, -1])
pred_varma <- predict_varmaMEWMA(test_varma, dat_lin[1001:2000, -1])

plot(test_varma$pstat)
plot(pred_varma$pstat)
