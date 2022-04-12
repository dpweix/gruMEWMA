### Imports ###
library("here")
library("tidyverse")
theme_set(theme_bw())
library("reticulate")
library("mlmcusum")
library("kableExtra")

use_condaenv("/home/ubuntu/miniconda3/envs/deep-learning-03")
source_python(here("mlmcusum", "inst", "python", "gru_functions.py"))

### Pilot Simulation Study ###
dat_lin <- gen_dat_lin("f1")
dat_nlr <- gen_dat_nlr("f1")
dat_ltm <- gen_dat_ltm("f1")

# Data
bind_rows("linear"     = dat_lin,
          "non-linear" = dat_nlr,
          "long-term"  = dat_ltm, .id = "Group") |> 
  pivot_longer(-c(1,2)) |> 
  mutate(Group = as_factor(Group)) |> 
  ggplot(aes(index, value, color = name)) +
  geom_line() +
  facet_wrap(~ Group, ncol = 1, scales = "free") +
  labs(y = "", x = "", color = "Variable", title = "Simulated Data with Fault 1") +
  geom_vline(xintercept = 1000, color = "blue") +
  geom_vline(xintercept = 1500, color = "red")

### Initial Test
l <- 2
trn_id <- 1:1000
tst_id <- (1000-l+1):2000

### Linear
# Train Model
model_lin_gru <- train_gruMCUSUM(dat_lin[trn_id, -1], lags = l, k = 1.1)
preds_lin_gru1 <- predict_gruMCUSUM(model_lin_gru, dat_lin[tst_id, -1])
preds_lin_gru2 <- predict_gruMCUSUM(model_lin_gru, dat_lin[(1000-l+1):1500, -1])

# Compare Residuals
preds_lin_gru1$residuals |> as_tibble() |> glimpse()
preds_lin_gru2$residuals |> as_tibble() |> glimpse()

(preds_lin_gru2$residuals - preds_lin_gru1$residuals[1:500, ]) |> 
  abs()

# PStat
preds_lin_gru1$pstat
preds_lin_gru2$pstat

tibble(Pstat_1 = preds_lin_gru1$pstat[1:500],
       Pstat_2 = preds_lin_gru2$pstat,
       index = 1:500) |> 
  pivot_longer(-3) |> 
  ggplot(aes(index, value, color = name)) +
  geom_line() + labs(y = "Pstat", color = "")

plot_pstat <- function(pred_trn, pred_tst) {
  plot(c(pred_trn$pstat, pred_tst$pstat),
       ylab = "pstat", main = "gruMCUSUM")
  abline(v = 1000, col = "blue")
  abline(v = 1500, col = "red")
}

plot_pstat(model_lin_gru, preds_lin_gru1)
plot_pstat(model_lin_gru, preds_lin_gru2)

h <- quantile(model_lin_gru$pstat, .975)

# Comparison of Pstats
tibble(index = 3:2000,
       `No Fault` = c(model_lin_gru$pstat, preds_lin_gru2$pstat, rep(NA, 500)),
       `Fault 1` = c(model_lin_gru$pstat, preds_lin_gru1$pstat)) |> 
  pivot_longer(-index) |> 
  mutate(name = as_factor(name)) |> 
  ggplot(aes(index, value)) +
  geom_line() +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  geom_vline(xintercept = 1000, color = "blue") +
  geom_vline(xintercept = 1500, color = "red") +
  geom_hline(yintercept = h, color = "darkgreen") +
  labs(y = "Plotting Statistic", title = "gruMCUSUM Plotting Statistic")


# Tau
preds_lin_gru1$tau |> as_tibble() |> glimpse()
preds_lin_gru2$tau |> as_tibble() |> glimpse()

# S
preds_lin_gru1$S |> as_tibble() |> head(100)
preds_lin_gru2$S |> as_tibble() |> head(100)

# Residuals 700 x 500
bind_rows(as_tibble(model_lin_gru$residuals),
          as_tibble(preds_lin_gru1$residuals)) |> 
  add_column(index = 1:max(tst_id-l)) |> 
  pivot_longer(1:3) |> 
  ggplot(aes(index, value)) +
  geom_point(shape = 1) +
  geom_vline(xintercept = 1000, color = "blue") +
  geom_vline(xintercept = 1500, color = "red") +
  facet_wrap(~ name, ncol = 1) +
  labs(y = "residuals", title = "gruMCUSUM")


### MRF

### Linear
# Train Model
model_lin_mrf <- train_mrfMCUSUM(dat_lin[trn_id, -1], lags = l, k = 5)
preds_lin_mrf1 <- predict_mrfMCUSUM(model_lin_mrf, dat_lin[tst_id, -1])
preds_lin_mrf2 <- predict_mrfMCUSUM(model_lin_mrf, dat_lin[(1000-l+1):1500, -1])

h <- quantile(model_lin_mrf$pstat, 0.975)

# Pstat
tibble(index = 3:2000,
       `No Fault` = c(model_lin_mrf$pstat, preds_lin_mrf2$pstat, rep(NA, 500)),
       `Fault 1` = c(model_lin_mrf$pstat, preds_lin_mrf1$pstat)) |> 
  pivot_longer(-index) |> 
  mutate(name = as_factor(name)) |> 
  ggplot(aes(index, value)) +
  geom_line() +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  geom_vline(xintercept = 1000, color = "blue") +
  geom_vline(xintercept = 1500, color = "red") +
  geom_hline(yintercept = h, color = "darkgreen") +
  labs(y = "Plotting Statistic",
       title = paste("mrfMCUSUM Plotting Statistic, k =",  model_lin_mrf$constants[1]))

# Residuals
# Residuals 700 x 500
bind_rows(as_tibble(model_lin_mrf$residuals),
          as_tibble(preds_lin_mrf1$residuals)) |> 
  add_column(index = 1:max(tst_id-l)) |> 
  pivot_longer(1:3) |> 
  ggplot(aes(index, value)) +
  geom_point(shape = 1) +
  geom_vline(xintercept = 1000, color = "blue") +
  geom_vline(xintercept = 1500, color = "red") +
  facet_wrap(~ name, ncol = 1) +
  labs(y = "residuals", title = "mrfMCUSUM")

# Compare Residuals of MRF and GRU
residuals_all <- 
  bind_rows(
    as_tibble(model_lin_mrf$residuals)              |> add_column(Method = "mrf", Data = "training: IC"),
    as_tibble(preds_lin_mrf1$residuals[1:500, ])    |> add_column(Method = "mrf", Data = "testing: IC"),
    as_tibble(preds_lin_mrf1$residuals[501:1000, ]) |> add_column(Method = "mrf", Data = "testing: OC"),
    as_tibble(model_lin_gru$residuals)              |> add_column(Method = "gru", Data = "training: IC"),
    as_tibble(preds_lin_gru1$residuals[1:500, ])    |> add_column(Method = "gru", Data = "testing: IC"),
    as_tibble(preds_lin_gru1$residuals[501:1000, ]) |> add_column(Method = "gru", Data = "testing: OC")
  ) |> 
    mutate(Method = as_factor(Method),
           Data = as_factor(Data))

# 600 x 300
residuals_all |> 
  group_by(Data, Method) |> 
  summarize(across(where(is.numeric), \(x) sqrt(mean(x^2)))) |> 
  pivot_longer(-c(1, 2)) |> 
  ggplot(aes(name, value, fill = Method)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Data) +
  labs(x = "Variable", y = "Root Mean Squared Error", title = "RMSE: GRU vs. MRF")
  #kbl(format = "latex", booktabs = TRUE, linesep = '')

residuals_all |> 
  group_by(Data, Method) |> 
  summarize(across(where(is.numeric), sd)) |> 
  pivot_longer(-c(1, 2)) |> 
  ggplot(aes(name, value, fill = Method)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Data) +
  labs(x = "Variable", y = "Standard Deviation", title = "Standard Deviation: GRU vs. MRF")
  #kbl(format = "latex", booktabs = TRUE, linesep = '')
