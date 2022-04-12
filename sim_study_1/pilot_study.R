### Imports ###
library("here")
library("tidyverse")
library("reticulate")
library("mlmcusum")
library("kableExtra")

use_condaenv("/home/ubuntu/miniconda3/envs/deep-learning-03")
path_py <- "~/git/mlmcusum/inst/python/gru_functions.py"
source_python(path_py)

set.seed(123)

# Parameters
# Method: GRU, data linear (first past)
gen_sim_study <- function(n_ic_trn = 500, n_ic_tst = 500, n_oc = 500,
                          l = 2, arl = 40) {
  # Constants
  id_trn <- 1:n_ic_trn
  id_tst <- (n_ic_trn - l):(n_ic_trn+n_ic_tst+n_oc) # adjust l for varma and htsquare
  ql <- 1 - 1/arl
  
  # Generate Data
  dat_lin <- gen_dat_lin(n_ic_trn + n_ic_tst, n_oc)
  dat_nlr <- gen_dat_nlr(n_ic_trn + n_ic_tst, n_oc)
  dat_ltm <- gen_dat_ltm(n_ic_trn + n_ic_tst, n_oc)
  
  ### Linear Data
  # Fit Models
  fit_gru_lin <- train(dat_lin$none[id_trn, ], method = "gruMCUSUM", lags = l, k = 1.1)
  
  # Predict: GRU
  pred_gru_lin_nf <- predict(fit_gru_lin, dat_lin$none[id_tst, ])
  pred_gru_lin_f1 <- predict(fit_gru_lin, dat_lin$f1[id_tst, ])
  pred_gru_lin_f2 <- predict(fit_gru_lin, dat_lin$f2[id_tst, ])
  pred_gru_lin_f3 <- predict(fit_gru_lin, dat_lin$f3[id_tst, ])
  
  ### Get h Level ###
  h_gru_lin <- quantile(pred_gru_lin_nf$pstat[(l+1):n_ic_tst], ql)
  
  ### Get Run Length
  rl_gru_lin_nf <- get_run_length(tail(pred_gru_lin_nf$pstat, n_oc), h_gru_lin)
  rl_gru_lin_f1 <- get_first_fault(tail(pred_gru_lin_f1$pstat, n_oc), h_gru_lin)
  rl_gru_lin_f2 <- get_first_fault(tail(pred_gru_lin_f2$pstat, n_oc), h_gru_lin)
  rl_gru_lin_f3 <- get_first_fault(tail(pred_gru_lin_f3$pstat, n_oc), h_gru_lin)
  
  # Construct Tibble
  rl_gru_lin <- tibble(Data = "linear",
                       method = "GRU-MCUSUM",
                       IC = rl_gru_lin_nf,
                       F1 = rl_gru_lin_f1,
                       F2 = rl_gru_lin_f2,
                       F3 = rl_gru_lin_f3)
  # Save Pstat
  pstat_gru_lin <- tibble(Data = "linear",
                          method = "GRU-MCUSUM",
                          IC = c(fit_gru_lin$pstat, pred_gru_lin_nf$pstat),
                          F1 = c(fit_gru_lin$pstat, pred_gru_lin_f1$pstat),
                          F2 = c(fit_gru_lin$pstat, pred_gru_lin_f2$pstat),
                          F3 = c(fit_gru_lin$pstat, pred_gru_lin_f3$pstat))
  
  # Return
  list(dat_pstat = pstat_gru_lin,
       dat_rl = rl_gru_lin)
}

sim_tst <- gen_sim_study()


