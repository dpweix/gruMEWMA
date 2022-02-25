library("tidyverse")
theme_set(theme_bw())
library("ks")
library("forecast")
library("MTS")
library("randomForest")
library("MultivariateRandomForest")
library("kableExtra")

### Create Sample Data ###
N <- 1000

ts <- VARMAsim(N, arlags = 1, malags = 0,
              phi = matrix(c(.95, 0, 0,
                             0, .95, 0,
                             0, 0, .95), nrow = 3, byrow = TRUE),
              theta = matrix(c(0, 0, 0,
                               0, 0, 0,
                               0, 0, 0), nrow = 3, byrow = TRUE),
              sigma = diag(3))

colnames(ts$series) <- c("x1", "x2", "x3")

dat <- 
  tibble(index = 1:N,
         ts$series |> as_tibble())




# Create X and Y matrices
l <- 8
p <- 3

create_X <- function(data, lags) {
  N <- nrow(data)
  p <- ncol(data)
  l <- lags
  
  
  1:(N-l) |> 
    map(
      \(x) {
        data[x:(x+l-1), 1:p] |> 
          unlist() |> 
          as.numeric()
      }) |> 
    unlist() |> 
    matrix(ncol = l*p, byrow = TRUE)
}

create_Y <- function(data, lags) {
  N <- nrow(data)
  p <- ncol(data)
  l <- lags
  
  data[(l+1):N, ] |> 
    as.matrix()
}

create_tau <- function(residuals) {
  p <- ncol(residuals)
  
  1:nrow(residuals) |> 
    map(
      \(x) {
        vech(residuals[x, ] %*% t(residuals[x, ]))
      }) |> 
    unlist() |> 
    matrix(ncol = p*(p+1)/2)
}

calc_S <- function(tau, k, mu_tau, sigma_tau_inv) {
  N <- nrow(tau)
  p <- ncol(tau)
  
  S <- matrix(0, nrow = N, ncol = ncol(tau))
  C <- vector("numeric", N)
  C[1] <- sqrt(t(tau[1, ] - mu_tau) %*% sigma_tau_inv %*% (tau[1, ] - mu_tau))
  
  2:N |> 
    walk(
      \(x) {
        c <- S[x-1, ] + tau[x, ] - mu_tau
        C[x] <- sqrt(t(c) %*% sigma_tau_inv %*% c)
        
        
        if(C[x] > k) {
          s <- (S[x-1, ] + tau[x, ] - mu_tau)*(1-k/C[x])
          1:p |> 
            walk(
              \(y) {
                S[x, y] <<- s[y]
              })
        }
      })
  S
}

calc_PStat <- function(S, sigma_tau_inv) {
  N <- nrow(S)
  
  1:N |> 
    map_dbl(
      \(x) {
        sqrt(t(S[x, ]) %*% sigma_tau_inv %*% S[x, ])
      })
}



### mrfMCUSUM ###
X <- create_X(dat[, -1], lags = l)
Y <- create_Y(dat[, -1], lags = l)

# Model and Make Predictions (about 30 seconds)
fit_mrf <- build_single_tree(X, Y,
                             m_feature = floor(sqrt(l*p)),
                             min_leaf = 10,
                             Inv_Cov_Y = solve(cov(Y)),
                             Command = 2)

mrf_preds <- single_tree_prediction(fit_mrf, X, 3)
colnames(mrf_preds) <- c("x1", "x2", "x3")

# Get Tau
tau <- create_tau(Y - mrf_preds)

mu_tau <- colMeans(tau)
sigma_tau_inv <- solve(cov(tau))

# Get S
S <- calc_S(tau, .9, mu_tau, sigma_tau_inv)

# Plotting Statistic
pstat <- calc_PStat(S, sigma_tau_inv)

### gruMCUSUM ###
# Note that the GRU part is done via reticulate
# in a separate file. (5 seconds)

# Attempt to use reticulate here
library("reticulate")

use_condaenv("C:/Users/derek_weix1/Anaconda3/envs/r-reticulate-01")

source_python("gru_functions.py")




gru_preds <- py$gru_preds
colnames(gru_preds) <- c("x1", "x2", "x3")

# Get Tau
tau1 <- create_tau(Y - gru_preds)

mu_tau1 <- colMeans(tau1)
sigma_tau_inv1 <- solve(cov(tau1))

# Get S
S1 <- calc_S(tau1, .9, mu_tau1, sigma_tau_inv1)

# Plotting Statistic
pstat1 <- calc_PStat(S1, sigma_tau_inv1)


### Graphs ##

# Data & Predictions
bind_rows(
  dat[-c(1:l), ],
  tibble(index = (l+1):N,
         mrf_preds |> as_tibble()),
  tibble(index = (l+1):N,
         gru_preds |> as_tibble())
) |> 
  add_column(Data = factor(rep(c("Observed", "MRF", "GRU"), each = N-l),
                           levels = c("Observed", "MRF", "GRU"))) |> 
  pivot_longer(-c("index", "Data")) |> 
  ggplot(aes(index, value, color = name)) +
  geom_line() +
  facet_wrap(~ Data, ncol = 1) +
  labs(y = "", x = "", color = "",
       title = "Observed Data and Model Predictions")

path_w <- "C:/Users/12143/Box/Reports/report_2022_01_19/"

ggsave("predictions.png", width = 20, height = 15, units = "cm", 
       path = path_w)

# Residuals
model_res <-
  bind_rows(
       (Y - mrf_preds) |> as_tibble(),
       (Y - gru_preds) |> as_tibble()
  ) |> 
  add_column(Model = factor(rep(c("MRF", "GRU"), each = N-l),
                            levels = c("MRF", "GRU")),
             index = rep((l+1):N,2)) 

model_res |> 
  pivot_longer(c("x1", "x2", "x3")) |> 
  group_by(Model, name) |> 
  summarise(RMSE = round(sqrt(mean(value^2)), 3)) |> 
  pivot_wider(names_from = Model, values_from = RMSE) |> 
  kbl(format = "latex", booktabs = TRUE)

model_res |> 
  pivot_longer(-c("index", "Model")) |> 
  ggplot(aes(index, value, color = name)) +
  geom_line() +
  facet_wrap(~ Model, ncol = 1) +
  labs(y = "Residuals", x = "", color = "",
       title = "Model Residuals")

ggsave("residuals.png", width = 20, height = 10, units = "cm", 
       path = path_w)

# Plotting Statistics
tibble(index = rep((l+1):N, 2),
       pstat = c(pstat, pstat1),
       Model = factor(rep(c("mfrMCUSUM", "gruMCUSUM"), each = N-l),
                      levels = c("mfrMCUSUM", "gruMCUSUM"))) |> 
  ggplot(aes(index, pstat)) + 
  geom_line() +
  facet_wrap(~ Model, ncol = 1) +
  labs(y = "Plotting Statistic", x = "",
       title = "Sample Plotting Statistics for Proposed Methods")

ggsave("pstats.png", width = 20, height = 10, units = "cm", 
       path = path_w)

# ACF Plots
a <- 20; b <- 10

ggAcf(dat[-1]) + labs(title = "Observed Data ACF") + ylim(-1, 1)
ggsave("acf_obs.png", width = a, height = b, units = "cm", path = path_w)
ggAcf((Y - mrf_preds)) + labs(title = "MRF Residuals ACF") + ylim(-1, 1)
ggsave("acf_mrf.png", width = a, height = b, units = "cm", path = path_w)
ggAcf((Y - gru_preds)) + labs(title = "GRU Residuals ACF") + ylim(-1, 1)
ggsave("acf_gru.png", width = a, height = b, units = "cm", path = path_w)

# PACF Plots
ggPacf(dat[-1]) + labs(title = "Observed Data PACF") + ylim(-1, 1)
ggsave("pacf_obs.png", width = a, height = b, units = "cm", path = path_w)
ggPacf((Y - mrf_preds)) + labs(title = "MRF Residuals PACF") + ylim(-1, 1)
ggsave("pacf_mrf.png", width = a, height = b, units = "cm", path = path_w)
ggPacf((Y - gru_preds)) + labs(title = "GRU Residuals PACF") + ylim(-1, 1)
ggsave("pacf_gru.png", width = a, height = b, units = "cm", path = path_w)
