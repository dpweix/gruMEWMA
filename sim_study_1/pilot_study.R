### Imports ###
library("here")
library("tidyverse")
theme_set(theme_bw())
library("reticulate")
library("mlmcusum")
library("kableExtra")
library("furrr")

#path_conda <- "/home/ubuntu/miniconda3/envs/deep-learning-03"
path_conda <- "/home/nossimid/miniconda3/envs/deep_learning_v03"
use_condaenv(path_conda)
path_py <- "~/git/mlmcusum/inst/python/gru_functions.py"
source_python(path_py)

#set.seed(123)

# Parameters
# Method: GRU, data linear (first past)
gen_sim_study <- function(n_ic= 500, n_oc = 500,
                          l = 4, arl = 200, h_vals) {
  # Constants
  id_trn <- 1:n_ic
  id_tst <- (n_ic - l):(n_ic+n_oc) # adjust l for varma and htsquare
  ql <- 1 - 1/arl
  
  ### Linear Data ###
  dat_lin <- gen_dat_lin(n_ic, n_oc)
  
  # Fit Models
  fit_gru_lin <- train(dat_lin$none[id_trn, ], method = "gruMCUSUM", lags = l, k = 1.1)
  fit_mrf_lin <- train(dat_lin$none[id_trn, ], method = "mrfMCUSUM", lags = l, k = 5)
  fit_vmc_lin <- train(dat_lin$none[id_trn, ], method = "varmaMCUSUM", lags = 1, k = 0.9)
  fit_vmw_lin <- train(dat_lin$none[id_trn, ], method = "varmaMEWMA", lags = 1, r = 0.3)
  fit_hts_lin <- train(dat_lin$none[id_trn, ], method = "htsquare")
  
  # Predict: GRU
  pred_gru_lin_nf <- predict(fit_gru_lin, dat_lin$none[id_tst, ])
  pred_gru_lin_f1 <- predict(fit_gru_lin, dat_lin$f1[id_tst, ])
  pred_gru_lin_f2 <- predict(fit_gru_lin, dat_lin$f2[id_tst, ])
  pred_gru_lin_f3 <- predict(fit_gru_lin, dat_lin$f3[id_tst, ])
  
  # Predict: MRF
  pred_mrf_lin_nf <- predict(fit_mrf_lin, dat_lin$none[id_tst, ])
  pred_mrf_lin_f1 <- predict(fit_mrf_lin, dat_lin$f1[id_tst, ])
  pred_mrf_lin_f2 <- predict(fit_mrf_lin, dat_lin$f2[id_tst, ])
  pred_mrf_lin_f3 <- predict(fit_mrf_lin, dat_lin$f3[id_tst, ])
  
  # Predict: VARMA - MCUSUM
  pred_vmc_lin_nf <- predict(fit_vmc_lin, dat_lin$none[id_tst, ])
  pred_vmc_lin_f1 <- predict(fit_vmc_lin, dat_lin$f1[id_tst, ])
  pred_vmc_lin_f2 <- predict(fit_vmc_lin, dat_lin$f2[id_tst, ])
  pred_vmc_lin_f3 <- predict(fit_vmc_lin, dat_lin$f3[id_tst, ])
  
  # Predict: VARMA - MEWMA
  pred_vmw_lin_nf <- predict(fit_vmw_lin, dat_lin$none[id_tst, ])
  pred_vmw_lin_f1 <- predict(fit_vmw_lin, dat_lin$f1[id_tst, ])
  pred_vmw_lin_f2 <- predict(fit_vmw_lin, dat_lin$f2[id_tst, ])
  pred_vmw_lin_f3 <- predict(fit_vmw_lin, dat_lin$f3[id_tst, ])
  
  # Predict: Hotelling's T^2
  pred_hts_lin_nf <- predict(fit_hts_lin, dat_lin$none[id_tst, ])
  pred_hts_lin_f1 <- predict(fit_hts_lin, dat_lin$f1[id_tst, ])
  pred_hts_lin_f2 <- predict(fit_hts_lin, dat_lin$f2[id_tst, ])
  pred_hts_lin_f3 <- predict(fit_hts_lin, dat_lin$f3[id_tst, ])
  
  # Get h Level
  h_gru_lin <- h_vals[1]
  h_mrf_lin <- h_vals[2]
  h_vmc_lin <- h_vals[3]
  h_vmw_lin <- h_vals[4]
  h_hts_lin <- h_vals[5]
  
  # Get Run Length: GRU
  rl_gru_lin_nf <- get_run_length(tail(pred_gru_lin_nf$pstat, n_oc), h_gru_lin)
  rl_gru_lin_f1 <- get_first_fault(tail(pred_gru_lin_f1$pstat, n_oc), h_gru_lin)
  rl_gru_lin_f2 <- get_first_fault(tail(pred_gru_lin_f2$pstat, n_oc), h_gru_lin)
  rl_gru_lin_f3 <- get_first_fault(tail(pred_gru_lin_f3$pstat, n_oc), h_gru_lin)
  
  # Get Run Length: MRF
  rl_mrf_lin_nf <- get_run_length(tail(pred_mrf_lin_nf$pstat, n_oc), h_mrf_lin)
  rl_mrf_lin_f1 <- get_first_fault(tail(pred_mrf_lin_f1$pstat, n_oc), h_mrf_lin)
  rl_mrf_lin_f2 <- get_first_fault(tail(pred_mrf_lin_f2$pstat, n_oc), h_mrf_lin)
  rl_mrf_lin_f3 <- get_first_fault(tail(pred_mrf_lin_f3$pstat, n_oc), h_mrf_lin)
  
  # Get Run Length: VARMA - MCUSUM
  rl_vmc_lin_nf <- get_run_length(tail(pred_vmc_lin_nf$pstat, n_oc), h_vmc_lin)
  rl_vmc_lin_f1 <- get_first_fault(tail(pred_vmc_lin_f1$pstat, n_oc), h_vmc_lin)
  rl_vmc_lin_f2 <- get_first_fault(tail(pred_vmc_lin_f2$pstat, n_oc), h_vmc_lin)
  rl_vmc_lin_f3 <- get_first_fault(tail(pred_vmc_lin_f3$pstat, n_oc), h_vmc_lin)
  
  # Get Run Length: VARMA - MEWMA
  rl_vmw_lin_nf <- get_run_length(tail(pred_vmw_lin_nf$pstat, n_oc), h_vmw_lin)
  rl_vmw_lin_f1 <- get_first_fault(tail(pred_vmw_lin_f1$pstat, n_oc), h_vmw_lin)
  rl_vmw_lin_f2 <- get_first_fault(tail(pred_vmw_lin_f2$pstat, n_oc), h_vmw_lin)
  rl_vmw_lin_f3 <- get_first_fault(tail(pred_vmw_lin_f3$pstat, n_oc), h_vmw_lin)
  
  # Get Run Length: Hotelling's T^2
  rl_hts_lin_nf <- get_run_length(tail(pred_hts_lin_nf$pstat, n_oc), h_hts_lin)
  rl_hts_lin_f1 <- get_first_fault(tail(pred_hts_lin_f1$pstat, n_oc), h_hts_lin)
  rl_hts_lin_f2 <- get_first_fault(tail(pred_hts_lin_f2$pstat, n_oc), h_hts_lin)
  rl_hts_lin_f3 <- get_first_fault(tail(pred_hts_lin_f3$pstat, n_oc), h_hts_lin)
  
  # Construct Tibble
  rl_gru_lin <- tibble(Data = "linear", method = "GRU-MCUSUM", IC = rl_gru_lin_nf,
                       F1 = rl_gru_lin_f1, F2 = rl_gru_lin_f2, F3 = rl_gru_lin_f3)
  rl_mrf_lin <- tibble(Data = "linear", method = "MRF-MCUSUM", IC = rl_mrf_lin_nf,
                       F1 = rl_mrf_lin_f1, F2 = rl_mrf_lin_f2, F3 = rl_mrf_lin_f3)
  rl_vmc_lin <- tibble(Data = "linear", method = "VMC-MCUSUM", IC = rl_vmc_lin_nf,
                       F1 = rl_vmc_lin_f1, F2 = rl_vmc_lin_f2, F3 = rl_vmc_lin_f3)
  rl_vmw_lin <- tibble(Data = "linear", method = "VMW-MEWMA", IC = rl_vmw_lin_nf,
                       F1 = rl_vmw_lin_f1, F2 = rl_vmw_lin_f2, F3 = rl_vmw_lin_f3)
  rl_hts_lin <- tibble(Data = "linear", method = "Hotelling-T2", IC = rl_hts_lin_nf,
                       F1 = rl_hts_lin_f1, F2 = rl_hts_lin_f2, F3 = rl_hts_lin_f3)
  # Save Pstat
  pstat_gru_lin <- tibble(Data = "linear", method = "GRU-MCUSUM",
                          IC = c(fit_gru_lin$pstat, pred_gru_lin_nf$pstat),
                          F1 = c(fit_gru_lin$pstat, pred_gru_lin_f1$pstat),
                          F2 = c(fit_gru_lin$pstat, pred_gru_lin_f2$pstat),
                          F3 = c(fit_gru_lin$pstat, pred_gru_lin_f3$pstat))
  
  pstat_mrf_lin <- tibble(Data = "linear", method = "MRF-MCUSUM",
                          IC = c(fit_mrf_lin$pstat, pred_mrf_lin_nf$pstat),
                          F1 = c(fit_mrf_lin$pstat, pred_mrf_lin_f1$pstat),
                          F2 = c(fit_mrf_lin$pstat, pred_mrf_lin_f2$pstat),
                          F3 = c(fit_mrf_lin$pstat, pred_mrf_lin_f3$pstat))
  
  pstat_vmc_lin <- tibble(Data = "linear", method = "VMC-MCUSUM",
                          IC = c(fit_vmc_lin$pstat, pred_vmc_lin_nf$pstat),
                          F1 = c(fit_vmc_lin$pstat, pred_vmc_lin_f1$pstat),
                          F2 = c(fit_vmc_lin$pstat, pred_vmc_lin_f2$pstat),
                          F3 = c(fit_vmc_lin$pstat, pred_vmc_lin_f3$pstat))
  
  pstat_vmw_lin <- tibble(Data = "linear", method = "VMW-MEWMA",
                          IC = c(fit_vmw_lin$pstat, pred_vmw_lin_nf$pstat),
                          F1 = c(fit_vmw_lin$pstat, pred_vmw_lin_f1$pstat),
                          F2 = c(fit_vmw_lin$pstat, pred_vmw_lin_f2$pstat),
                          F3 = c(fit_vmw_lin$pstat, pred_vmw_lin_f3$pstat))
  
  pstat_hts_lin <- tibble(Data = "linear", method = "Hotelling-T2",
                          IC = c(fit_hts_lin$pstat, pred_hts_lin_nf$pstat),
                          F1 = c(fit_hts_lin$pstat, pred_hts_lin_f1$pstat),
                          F2 = c(fit_hts_lin$pstat, pred_hts_lin_f2$pstat),
                          F3 = c(fit_hts_lin$pstat, pred_hts_lin_f3$pstat))
  
  ### Non-Linear Data ###
  dat_nlr <- gen_dat_nlr(n_ic, n_oc)
  
  # Fit Models
  fit_gru_nlr <- train(dat_nlr$none[id_trn, ], method = "gruMCUSUM", lags = l, k = 1.1)
  fit_mrf_nlr <- train(dat_nlr$none[id_trn, ], method = "mrfMCUSUM", lags = l, k = 5)
  fit_vmc_nlr <- train(dat_nlr$none[id_trn, ], method = "varmaMCUSUM", lags = 1, k = 0.9)
  fit_vmw_nlr <- train(dat_nlr$none[id_trn, ], method = "varmaMEWMA", lags = 1, r = 0.3)
  fit_hts_nlr <- train(dat_nlr$none[id_trn, ], method = "htsquare")
  
  # Predict: GRU
  pred_gru_nlr_nf <- predict(fit_gru_nlr, dat_nlr$none[id_tst, ])
  pred_gru_nlr_f1 <- predict(fit_gru_nlr, dat_nlr$f1[id_tst, ])
  pred_gru_nlr_f2 <- predict(fit_gru_nlr, dat_nlr$f2[id_tst, ])
  pred_gru_nlr_f3 <- predict(fit_gru_nlr, dat_nlr$f3[id_tst, ])
  
  # Predict: MRF
  pred_mrf_nlr_nf <- predict(fit_mrf_nlr, dat_nlr$none[id_tst, ])
  pred_mrf_nlr_f1 <- predict(fit_mrf_nlr, dat_nlr$f1[id_tst, ])
  pred_mrf_nlr_f2 <- predict(fit_mrf_nlr, dat_nlr$f2[id_tst, ])
  pred_mrf_nlr_f3 <- predict(fit_mrf_nlr, dat_nlr$f3[id_tst, ])
  
  # Predict: VARMA - MCUSUM
  pred_vmc_nlr_nf <- predict(fit_vmc_nlr, dat_nlr$none[id_tst, ])
  pred_vmc_nlr_f1 <- predict(fit_vmc_nlr, dat_nlr$f1[id_tst, ])
  pred_vmc_nlr_f2 <- predict(fit_vmc_nlr, dat_nlr$f2[id_tst, ])
  pred_vmc_nlr_f3 <- predict(fit_vmc_nlr, dat_nlr$f3[id_tst, ])
  
  # Predict: VARMA - MEWMA
  pred_vmw_nlr_nf <- predict(fit_vmw_nlr, dat_nlr$none[id_tst, ])
  pred_vmw_nlr_f1 <- predict(fit_vmw_nlr, dat_nlr$f1[id_tst, ])
  pred_vmw_nlr_f2 <- predict(fit_vmw_nlr, dat_nlr$f2[id_tst, ])
  pred_vmw_nlr_f3 <- predict(fit_vmw_nlr, dat_nlr$f3[id_tst, ])
  
  # Predict: Hotelling's T^2
  pred_hts_nlr_nf <- predict(fit_hts_nlr, dat_nlr$none[id_tst, ])
  pred_hts_nlr_f1 <- predict(fit_hts_nlr, dat_nlr$f1[id_tst, ])
  pred_hts_nlr_f2 <- predict(fit_hts_nlr, dat_nlr$f2[id_tst, ])
  pred_hts_nlr_f3 <- predict(fit_hts_nlr, dat_nlr$f3[id_tst, ])
  
  # Get h Level
  h_gru_nlr <- h_vals[6]
  h_mrf_nlr <- h_vals[7]
  h_vmc_nlr <- h_vals[8]
  h_vmw_nlr <- h_vals[9]
  h_hts_nlr <- h_vals[10]
  
  # Get Run Length: GRU
  rl_gru_nlr_nf <- get_run_length(tail(pred_gru_nlr_nf$pstat, n_oc), h_gru_nlr)
  rl_gru_nlr_f1 <- get_first_fault(tail(pred_gru_nlr_f1$pstat, n_oc), h_gru_nlr)
  rl_gru_nlr_f2 <- get_first_fault(tail(pred_gru_nlr_f2$pstat, n_oc), h_gru_nlr)
  rl_gru_nlr_f3 <- get_first_fault(tail(pred_gru_nlr_f3$pstat, n_oc), h_gru_nlr)
  
  # Get Run Length: MRF
  rl_mrf_nlr_nf <- get_run_length(tail(pred_mrf_nlr_nf$pstat, n_oc), h_mrf_nlr)
  rl_mrf_nlr_f1 <- get_first_fault(tail(pred_mrf_nlr_f1$pstat, n_oc), h_mrf_nlr)
  rl_mrf_nlr_f2 <- get_first_fault(tail(pred_mrf_nlr_f2$pstat, n_oc), h_mrf_nlr)
  rl_mrf_nlr_f3 <- get_first_fault(tail(pred_mrf_nlr_f3$pstat, n_oc), h_mrf_nlr)
  
  # Get Run Length: VARMA - MCUSUM
  rl_vmc_nlr_nf <- get_run_length(tail(pred_vmc_nlr_nf$pstat, n_oc), h_vmc_nlr)
  rl_vmc_nlr_f1 <- get_first_fault(tail(pred_vmc_nlr_f1$pstat, n_oc), h_vmc_nlr)
  rl_vmc_nlr_f2 <- get_first_fault(tail(pred_vmc_nlr_f2$pstat, n_oc), h_vmc_nlr)
  rl_vmc_nlr_f3 <- get_first_fault(tail(pred_vmc_nlr_f3$pstat, n_oc), h_vmc_nlr)
  
  # Get Run Length: VARMA - MEWMA
  rl_vmw_nlr_nf <- get_run_length(tail(pred_vmw_nlr_nf$pstat, n_oc), h_vmw_nlr)
  rl_vmw_nlr_f1 <- get_first_fault(tail(pred_vmw_nlr_f1$pstat, n_oc), h_vmw_nlr)
  rl_vmw_nlr_f2 <- get_first_fault(tail(pred_vmw_nlr_f2$pstat, n_oc), h_vmw_nlr)
  rl_vmw_nlr_f3 <- get_first_fault(tail(pred_vmw_nlr_f3$pstat, n_oc), h_vmw_nlr)
  
  # Get Run Length: Hotelling's T^2
  rl_hts_nlr_nf <- get_run_length(tail(pred_hts_nlr_nf$pstat, n_oc), h_hts_nlr)
  rl_hts_nlr_f1 <- get_first_fault(tail(pred_hts_nlr_f1$pstat, n_oc), h_hts_nlr)
  rl_hts_nlr_f2 <- get_first_fault(tail(pred_hts_nlr_f2$pstat, n_oc), h_hts_nlr)
  rl_hts_nlr_f3 <- get_first_fault(tail(pred_hts_nlr_f3$pstat, n_oc), h_hts_nlr)
  
  # Construct Tibble
  rl_gru_nlr <- tibble(Data = "non-linear", method = "GRU-MCUSUM", IC = rl_gru_nlr_nf,
                       F1 = rl_gru_nlr_f1, F2 = rl_gru_nlr_f2, F3 = rl_gru_nlr_f3)
  rl_mrf_nlr <- tibble(Data = "non-linear", method = "MRF-MCUSUM", IC = rl_mrf_nlr_nf,
                       F1 = rl_mrf_nlr_f1, F2 = rl_mrf_nlr_f2, F3 = rl_mrf_nlr_f3)
  rl_vmc_nlr <- tibble(Data = "non-linear", method = "VMC-MCUSUM", IC = rl_vmc_nlr_nf,
                       F1 = rl_vmc_nlr_f1, F2 = rl_vmc_nlr_f2, F3 = rl_vmc_nlr_f3)
  rl_vmw_nlr <- tibble(Data = "non-linear", method = "VMW-MEWMA", IC = rl_vmw_nlr_nf,
                       F1 = rl_vmw_nlr_f1, F2 = rl_vmw_nlr_f2, F3 = rl_vmw_nlr_f3)
  rl_hts_nlr <- tibble(Data = "non-linear", method = "Hotelling-T2", IC = rl_hts_nlr_nf,
                       F1 = rl_hts_nlr_f1, F2 = rl_hts_nlr_f2, F3 = rl_hts_nlr_f3)
  # Save Pstat
  pstat_gru_nlr <- tibble(Data = "non-linear", method = "GRU-MCUSUM",
                          IC = c(fit_gru_nlr$pstat, pred_gru_nlr_nf$pstat),
                          F1 = c(fit_gru_nlr$pstat, pred_gru_nlr_f1$pstat),
                          F2 = c(fit_gru_nlr$pstat, pred_gru_nlr_f2$pstat),
                          F3 = c(fit_gru_nlr$pstat, pred_gru_nlr_f3$pstat))
  
  pstat_mrf_nlr <- tibble(Data = "non-linear", method = "MRF-MCUSUM",
                          IC = c(fit_mrf_nlr$pstat, pred_mrf_nlr_nf$pstat),
                          F1 = c(fit_mrf_nlr$pstat, pred_mrf_nlr_f1$pstat),
                          F2 = c(fit_mrf_nlr$pstat, pred_mrf_nlr_f2$pstat),
                          F3 = c(fit_mrf_nlr$pstat, pred_mrf_nlr_f3$pstat))
  
  pstat_vmc_nlr <- tibble(Data = "non-linear", method = "VMC-MCUSUM",
                          IC = c(fit_vmc_nlr$pstat, pred_vmc_nlr_nf$pstat),
                          F1 = c(fit_vmc_nlr$pstat, pred_vmc_nlr_f1$pstat),
                          F2 = c(fit_vmc_nlr$pstat, pred_vmc_nlr_f2$pstat),
                          F3 = c(fit_vmc_nlr$pstat, pred_vmc_nlr_f3$pstat))
  
  pstat_vmw_nlr <- tibble(Data = "non-linear", method = "VMW-MEWMA",
                          IC = c(fit_vmw_nlr$pstat, pred_vmw_nlr_nf$pstat),
                          F1 = c(fit_vmw_nlr$pstat, pred_vmw_nlr_f1$pstat),
                          F2 = c(fit_vmw_nlr$pstat, pred_vmw_nlr_f2$pstat),
                          F3 = c(fit_vmw_nlr$pstat, pred_vmw_nlr_f3$pstat))
  
  pstat_hts_nlr <- tibble(Data = "non-linear", method = "Hotelling-T2",
                          IC = c(fit_hts_nlr$pstat, pred_hts_nlr_nf$pstat),
                          F1 = c(fit_hts_nlr$pstat, pred_hts_nlr_f1$pstat),
                          F2 = c(fit_hts_nlr$pstat, pred_hts_nlr_f2$pstat),
                          F3 = c(fit_hts_nlr$pstat, pred_hts_nlr_f3$pstat))
  
  ### Long-Term Data ###
  dat_ltm <- gen_dat_ltm(n_ic, n_oc)
  
  # Fit Models
  fit_gru_ltm <- train(dat_ltm$none[id_trn, ], method = "gruMCUSUM", lags = l, k = 1.1)
  fit_mrf_ltm <- train(dat_ltm$none[id_trn, ], method = "mrfMCUSUM", lags = l, k = 5)
  fit_vmc_ltm <- train(dat_ltm$none[id_trn, ], method = "varmaMCUSUM", lags = 1, k = 0.9)
  fit_vmw_ltm <- train(dat_ltm$none[id_trn, ], method = "varmaMEWMA", lags = 1, r = 0.3)
  fit_hts_ltm <- train(dat_ltm$none[id_trn, ], method = "htsquare")
  
  # Predict: GRU
  pred_gru_ltm_nf <- predict(fit_gru_ltm, dat_ltm$none[id_tst, ])
  pred_gru_ltm_f1 <- predict(fit_gru_ltm, dat_ltm$f1[id_tst, ])
  pred_gru_ltm_f2 <- predict(fit_gru_ltm, dat_ltm$f2[id_tst, ])
  pred_gru_ltm_f3 <- predict(fit_gru_ltm, dat_ltm$f3[id_tst, ])
  
  # Predict: MRF
  pred_mrf_ltm_nf <- predict(fit_mrf_ltm, dat_ltm$none[id_tst, ])
  pred_mrf_ltm_f1 <- predict(fit_mrf_ltm, dat_ltm$f1[id_tst, ])
  pred_mrf_ltm_f2 <- predict(fit_mrf_ltm, dat_ltm$f2[id_tst, ])
  pred_mrf_ltm_f3 <- predict(fit_mrf_ltm, dat_ltm$f3[id_tst, ])
  
  # Predict: VARMA - MCUSUM
  pred_vmc_ltm_nf <- predict(fit_vmc_ltm, dat_ltm$none[id_tst, ])
  pred_vmc_ltm_f1 <- predict(fit_vmc_ltm, dat_ltm$f1[id_tst, ])
  pred_vmc_ltm_f2 <- predict(fit_vmc_ltm, dat_ltm$f2[id_tst, ])
  pred_vmc_ltm_f3 <- predict(fit_vmc_ltm, dat_ltm$f3[id_tst, ])
  
  # Predict: VARMA - MEWMA
  pred_vmw_ltm_nf <- predict(fit_vmw_ltm, dat_ltm$none[id_tst, ])
  pred_vmw_ltm_f1 <- predict(fit_vmw_ltm, dat_ltm$f1[id_tst, ])
  pred_vmw_ltm_f2 <- predict(fit_vmw_ltm, dat_ltm$f2[id_tst, ])
  pred_vmw_ltm_f3 <- predict(fit_vmw_ltm, dat_ltm$f3[id_tst, ])
  
  # Predict: Hotelling's T^2
  pred_hts_ltm_nf <- predict(fit_hts_ltm, dat_ltm$none[id_tst, ])
  pred_hts_ltm_f1 <- predict(fit_hts_ltm, dat_ltm$f1[id_tst, ])
  pred_hts_ltm_f2 <- predict(fit_hts_ltm, dat_ltm$f2[id_tst, ])
  pred_hts_ltm_f3 <- predict(fit_hts_ltm, dat_ltm$f3[id_tst, ])
  
  # Get h Level
  h_gru_ltm <- h_vals[11]
  h_mrf_ltm <- h_vals[12]
  h_vmc_ltm <- h_vals[13]
  h_vmw_ltm <- h_vals[14]
  h_hts_ltm <- h_vals[15]
  
  # Get Run Length: GRU
  rl_gru_ltm_nf <- get_run_length(tail(pred_gru_ltm_nf$pstat, n_oc), h_gru_ltm)
  rl_gru_ltm_f1 <- get_first_fault(tail(pred_gru_ltm_f1$pstat, n_oc), h_gru_ltm)
  rl_gru_ltm_f2 <- get_first_fault(tail(pred_gru_ltm_f2$pstat, n_oc), h_gru_ltm)
  rl_gru_ltm_f3 <- get_first_fault(tail(pred_gru_ltm_f3$pstat, n_oc), h_gru_ltm)
  
  # Get Run Length: MRF
  rl_mrf_ltm_nf <- get_run_length(tail(pred_mrf_ltm_nf$pstat, n_oc), h_mrf_ltm)
  rl_mrf_ltm_f1 <- get_first_fault(tail(pred_mrf_ltm_f1$pstat, n_oc), h_mrf_ltm)
  rl_mrf_ltm_f2 <- get_first_fault(tail(pred_mrf_ltm_f2$pstat, n_oc), h_mrf_ltm)
  rl_mrf_ltm_f3 <- get_first_fault(tail(pred_mrf_ltm_f3$pstat, n_oc), h_mrf_ltm)
  
  # Get Run Length: VARMA - MCUSUM
  rl_vmc_ltm_nf <- get_run_length(tail(pred_vmc_ltm_nf$pstat, n_oc), h_vmc_ltm)
  rl_vmc_ltm_f1 <- get_first_fault(tail(pred_vmc_ltm_f1$pstat, n_oc), h_vmc_ltm)
  rl_vmc_ltm_f2 <- get_first_fault(tail(pred_vmc_ltm_f2$pstat, n_oc), h_vmc_ltm)
  rl_vmc_ltm_f3 <- get_first_fault(tail(pred_vmc_ltm_f3$pstat, n_oc), h_vmc_ltm)
  
  # Get Run Length: VARMA - MEWMA
  rl_vmw_ltm_nf <- get_run_length(tail(pred_vmw_ltm_nf$pstat, n_oc), h_vmw_ltm)
  rl_vmw_ltm_f1 <- get_first_fault(tail(pred_vmw_ltm_f1$pstat, n_oc), h_vmw_ltm)
  rl_vmw_ltm_f2 <- get_first_fault(tail(pred_vmw_ltm_f2$pstat, n_oc), h_vmw_ltm)
  rl_vmw_ltm_f3 <- get_first_fault(tail(pred_vmw_ltm_f3$pstat, n_oc), h_vmw_ltm)
  
  # Get Run Length: Hotelling's T^2
  rl_hts_ltm_nf <- get_run_length(tail(pred_hts_ltm_nf$pstat, n_oc), h_hts_ltm)
  rl_hts_ltm_f1 <- get_first_fault(tail(pred_hts_ltm_f1$pstat, n_oc), h_hts_ltm)
  rl_hts_ltm_f2 <- get_first_fault(tail(pred_hts_ltm_f2$pstat, n_oc), h_hts_ltm)
  rl_hts_ltm_f3 <- get_first_fault(tail(pred_hts_ltm_f3$pstat, n_oc), h_hts_ltm)
  
  # Construct Tibble
  rl_gru_ltm <- tibble(Data = "long-term", method = "GRU-MCUSUM", IC = rl_gru_ltm_nf,
                       F1 = rl_gru_ltm_f1, F2 = rl_gru_ltm_f2, F3 = rl_gru_ltm_f3)
  rl_mrf_ltm <- tibble(Data = "long-term", method = "MRF-MCUSUM", IC = rl_mrf_ltm_nf,
                       F1 = rl_mrf_ltm_f1, F2 = rl_mrf_ltm_f2, F3 = rl_mrf_ltm_f3)
  rl_vmc_ltm <- tibble(Data = "long-term", method = "VMC-MCUSUM", IC = rl_vmc_ltm_nf,
                       F1 = rl_vmc_ltm_f1, F2 = rl_vmc_ltm_f2, F3 = rl_vmc_ltm_f3)
  rl_vmw_ltm <- tibble(Data = "long-term", method = "VMW-MEWMA", IC = rl_vmw_ltm_nf,
                       F1 = rl_vmw_ltm_f1, F2 = rl_vmw_ltm_f2, F3 = rl_vmw_ltm_f3)
  rl_hts_ltm <- tibble(Data = "long-term", method = "Hotelling-T2", IC = rl_hts_ltm_nf,
                       F1 = rl_hts_ltm_f1, F2 = rl_hts_ltm_f2, F3 = rl_hts_ltm_f3)
  # Save Pstat
  pstat_gru_ltm <- tibble(Data = "long-term", method = "GRU-MCUSUM",
                          IC = c(fit_gru_ltm$pstat, pred_gru_ltm_nf$pstat),
                          F1 = c(fit_gru_ltm$pstat, pred_gru_ltm_f1$pstat),
                          F2 = c(fit_gru_ltm$pstat, pred_gru_ltm_f2$pstat),
                          F3 = c(fit_gru_ltm$pstat, pred_gru_ltm_f3$pstat))
  
  pstat_mrf_ltm <- tibble(Data = "long-term", method = "MRF-MCUSUM",
                          IC = c(fit_mrf_ltm$pstat, pred_mrf_ltm_nf$pstat),
                          F1 = c(fit_mrf_ltm$pstat, pred_mrf_ltm_f1$pstat),
                          F2 = c(fit_mrf_ltm$pstat, pred_mrf_ltm_f2$pstat),
                          F3 = c(fit_mrf_ltm$pstat, pred_mrf_ltm_f3$pstat))
  
  pstat_vmc_ltm <- tibble(Data = "long-term", method = "VMC-MCUSUM",
                          IC = c(fit_vmc_ltm$pstat, pred_vmc_ltm_nf$pstat),
                          F1 = c(fit_vmc_ltm$pstat, pred_vmc_ltm_f1$pstat),
                          F2 = c(fit_vmc_ltm$pstat, pred_vmc_ltm_f2$pstat),
                          F3 = c(fit_vmc_ltm$pstat, pred_vmc_ltm_f3$pstat))
  
  pstat_vmw_ltm <- tibble(Data = "long-term", method = "VMW-MEWMA",
                          IC = c(fit_vmw_ltm$pstat, pred_vmw_ltm_nf$pstat),
                          F1 = c(fit_vmw_ltm$pstat, pred_vmw_ltm_f1$pstat),
                          F2 = c(fit_vmw_ltm$pstat, pred_vmw_ltm_f2$pstat),
                          F3 = c(fit_vmw_ltm$pstat, pred_vmw_ltm_f3$pstat))
  
  pstat_hts_ltm <- tibble(Data = "long-term", method = "Hotelling-T2",
                          IC = c(fit_hts_ltm$pstat, pred_hts_ltm_nf$pstat),
                          F1 = c(fit_hts_ltm$pstat, pred_hts_ltm_f1$pstat),
                          F2 = c(fit_hts_ltm$pstat, pred_hts_ltm_f2$pstat),
                          F3 = c(fit_hts_ltm$pstat, pred_hts_ltm_f3$pstat))
  
  ### Return
  list(dat_pstat = bind_rows(pstat_gru_lin,
                             pstat_mrf_lin,
                             pstat_vmc_lin[-c(1:l), ],
                             pstat_vmw_lin[-c(1:l), ],
                             pstat_hts_lin[-c(1:l), ],
                             pstat_gru_nlr,
                             pstat_mrf_nlr,
                             pstat_vmc_nlr[-c(1:l), ],
                             pstat_vmw_nlr[-c(1:l), ],
                             pstat_hts_nlr[-c(1:l), ],
                             pstat_gru_ltm,
                             pstat_mrf_ltm,
                             pstat_vmc_ltm[-c(1:l), ],
                             pstat_vmw_ltm[-c(1:l), ],
                             pstat_hts_ltm[-c(1:l), ]) |> 
         mutate(Data = as_factor(Data), method = as_factor(method)),
       dat_rl = bind_rows(rl_gru_lin,
                          rl_mrf_lin,
                          rl_vmc_lin,
                          rl_vmw_lin,
                          rl_hts_lin,
                          rl_gru_nlr,
                          rl_mrf_nlr,
                          rl_vmc_nlr,
                          rl_vmw_nlr,
                          rl_hts_nlr,
                          rl_gru_ltm,
                          rl_mrf_ltm,
                          rl_vmc_ltm,
                          rl_vmw_ltm,
                          rl_hts_ltm) |> 
         mutate(Data = as_factor(Data), method = as_factor(method)))
}

### Load Control Limits
load(here("results", "h.rda"))

### Run Single Simulation
sim_tst <- gen_sim_study(n_ic = 1000,
                         n_oc = 1000, 
                         h_vals = pull(dat_h, h))

sim_tst$dat_pstat |> 
  group_by(method) |> 
  summarise(n = n())

# Visualize Simulation
n_categories <- nlevels(sim_tst$dat_pstat$method)*nlevels(sim_tst$dat_pstat$Data)

sim_tst$dat_pstat |> 
  mutate(index = rep(1:(n()/n_categories), n_categories)) |> 
  pivot_longer(-c(index, Data, method)) |> 
  mutate(method = as_factor(method), 
         name = as_factor(name)) |> 
  ggplot(aes(index, value, color = name)) +
  geom_line() +
  geom_vline(xintercept = 500, color = "blue") +
  geom_vline(xintercept = 1000, color = "red") +
  facet_grid(Data ~ method) +
  ylim(0, 30) +
  labs(y = "Plotting Statistic", color = "",
       title = "Single Sample of Plotting Statistic for Each Method")



# ARL Table
sim_tst$dat_rl

### Multiple Simulations
future::plan(multisession)
n_sim <- 100

sim_vals <- 1:n_sim |> 
  map(\(i) gen_sim_study(n_ic = 1000,
                                n_oc = 1000, 
                                h_vals = pull(dat_h, h)))

future::plan(sequential)

save(sim_vals, file = here("results", "sim_vals.rda"))

# ARL Table
sim_vals |>
  map(\(i) i$dat_rl) |> 
  bind_rows() |> 
  group_by(Data, method) |> 
  summarise(across(where(is.numeric), mean)) |> 
  mutate(across(where(is.numeric), round, 3)) |> 
  kbl(format = "latex", booktabs = TRUE)

# Pstat Plot Table
sim_vals |> 
  map(\(i) i$dat_pstat) 
