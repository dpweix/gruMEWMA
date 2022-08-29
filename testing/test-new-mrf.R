library("tidyverse")
theme_set(theme_bw())
library("randomForestSRC")
library("mlmcusum")

### Set up --------------------------------------------------------------------

# Constants
l    <- 2
n_ic <- 1000
n_oc <- 1000

# Make Data
dat <- gen_dat_lin(n_ic + l, n_oc)$f1 |>
  mutate(x4 = lag(x1),
         x5 = lag(x1, n = 2),
         x6 = lag(x2),
         x7 = lag(x2, n = 2),
         x8 = lag(x3),
         x9 = lag(x3, n = 2)) |>
  slice(-c(1:l))

# Fit Model
fit <- rfsrc(formula = Multivar(x1, x2, x3) ~ .,
             data = as.data.frame(dat[1:n_ic, ]),
             ntree = 500,
             mtry = 3,
             splitrule = "mahalanobis")

# Make Predict
pred <- stats::predict(fit, as.data.frame(dat))

# Get Predictions
get.mv.predicted(pred) |> as_tibble()

### mlmcusum Prep -------------------------------------------------------------

# Data as in mlmcusum
X <- create_X(dat[, 1:4], lags = l)
Y <- create_Y(dat[, 1:4], lags = l)

as.formula(paste0("Multivar(`", paste0(colnames(Y), collapse = "` , `"), "`) ~ ."))

# Fit Model
fit <- rfsrc(formula = as.formula(paste0("Multivar(", paste0(colnames(Y), collapse = ","), ") ~ .")),
             data = as.data.frame(cbind(Y, X)),
             ntree = 500,
             mtry = 3,
             splitrule = "mahalanobis")

### Results -------------------------------------------------------------------

# Plot Data
dat |>
  mutate(obs = 1:(n_ic + n_oc)) |>
  pivot_longer(c("x1", "x2", "x3")) |>
  ggplot(aes(obs, value, color = name)) +
  geom_line()

# Plot Predictions
get.mv.predicted(pred) |>
  as_tibble() |>
  mutate(obs = 1:(n_ic+n_oc)) |>
  pivot_longer(c("x1", "x2", "x3")) |>
  ggplot(aes(obs, value, color = name)) +
  geom_line()

# Plot Residuals
(dat[, 1:3] - get.mv.predicted(pred)) |>
  as_tibble() |>
  mutate(obs = 1:(n_ic+n_oc)) |>
  pivot_longer(c("x1", "x2", "x3")) |>
  ggplot(aes(obs, value, color = name)) +
  geom_line()