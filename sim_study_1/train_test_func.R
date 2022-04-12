### Imports ###
library("tidyverse")
theme_set(theme_bw())
library("reticulate")
library("mlmcusum")

use_condaenv("/home/ubuntu/miniconda3/envs/deep-learning-03")
path_py <- "~/git/mlmcusum/inst/python/gru_functions.py"
source_python(path_py)

# Parameters
n_ic <- 500
n_oc <- 500

id_trn <- 1:n_ic
id_tst <- (n_ic+1):(n_ic+n_oc)

# Gen Data
dat_lin <- gen_dat_nlr(n_ic, n_oc)

# Test Methods
# GRU
fit_gru <- train(dat_lin$f1[id_trn, ], method = "gruMCUSUM")
pred_gru <- predict(fit_gru, dat_lin$f1[id_tst, ])

# MRF
fit_mrf <- train(dat_lin$f1[id_trn, ], method = "mrfMCUSUM")
pred_mrf <- predict(fit_mrf, dat_lin$f1[id_tst, ])

# VARMA - MCUSUM
fit_var_mc <- train(dat_lin$f1[id_trn, ], method = "varmaMCUSUM")
pred_var_mc <- predict(fit_var_mc, dat_lin$f1[id_tst, ])

# VARMA - MEWMA
fit_var_mw <- train(dat_lin$f1[id_trn, ], method = "varmaMEWMA")
pred_var_mw <- predict(fit_var_mw, dat_lin$f1[id_tst, ])

# Hotelling's T^2
fit_hts <- train(dat_lin$f1[id_trn, ], method = "htsquare")
pred_hts <- predict(fit_hts, dat_lin$f1[id_tst, ])

# Aggragate Pstat
dat_pst <-
  tibble(GRU_MCUSUM = c(fit_gru$pstat, pred_gru$pstat),
         MRF_MCUSUM = c(fit_mrf$pstat, pred_mrf$pstat),
         VAR_MCUSUM = c(fit_var_mc$pstat, pred_var_mc$pstat),
         VAR_MEWMA = c(fit_var_mw$pstat, pred_var_mw$pstat),
         Hotteling_T = c(fit_hts$pstat, pred_hts$pstat)) |> 
  mutate(index = 1:n())

# Visualize Pstat
dat_pst |> 
  pivot_longer(-index) |> 
  mutate(name = as_factor(name)) |> 
  ggplot(aes(index, value)) +
  geom_line() +
  geom_vline(xintercept = 500, col = "red") +
  facet_wrap(~ name, ncol = 2)
