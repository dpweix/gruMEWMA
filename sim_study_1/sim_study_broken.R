### Imports ###
library("here")
library("tidyverse")
theme_set(theme_bw())
library("reticulate")
library("mlmcusum")
library("kableExtra")

#path_conda <- "/home/ubuntu/miniconda3/envs/deep-learning-03"
#path_conda <- "/home/nossimid/miniconda3/envs/deep_learning_v03"
#use_condaenv(path_conda)
path_py <- "~/git/mlmcusum/inst/python/gru_functions.py"
source_python(path_py)

#set.seed(123)
### Get h ---------------------------------------------------------------------
sim_h_brk <- function(data_type = "lin", n_ic_trn = 500, n_ic_tst = 500, l = 2, arl = 200) {
  # Constants
  id_trn <- 1:n_ic_trn
  id_tst <- (n_ic_trn - l):(n_ic_trn+n_ic_tst) # adjust l for varma and htsquare
  ql <- 1 - 1/arl
  
  ### Gen Data ###
  if(data_type == "lin")      dat <- gen_dat_lin(n_ic_trn + n_ic_tst, 0)
  else if(data_type == "nlr") dat <- gen_dat_nrl(n_ic_trn + n_ic_tst, 0)
  else if(data_type == "ltm") dat <- gen_dat_ltm(n_ic_trn + n_ic_tst, 0)
  
  # Fit Models
  fit_gru <- train(dat$none[id_trn, ], method = "gruMCUSUM", lags = l, k = 1.1)
  fit_mrf <- train(dat$none[id_trn, ], method = "mrfMCUSUM", lags = l, k = 5)
  fit_vmc <- train(dat$none[id_trn, ], method = "varmaMCUSUM", lags = 1, k = 0.9)
  fit_vmw <- train(dat$none[id_trn, ], method = "varmaMEWMA", lags = 1, r = 0.3)
  fit_hts <- train(dat$none[id_trn, ], method = "htsquare")
  
  # Predict: GRU
  pred_gru_nf <- predict(fit_gru, dat$none[id_tst, ])
  
  # Predict: MRF
  pred_mrf_nf <- predict(fit_mrf, dat$none[id_tst, ])
  
  # Predict: VARMA - MCUSUM
  pred_vmc_nf <- predict(fit_vmc, dat$none[id_tst[-c(1:(l-1))], ])
  
  # Predict: VARMA - MEWMA
  pred_vmw_nf <- predict(fit_vmw, dat$none[id_tst[-c(1:(l-1))], ])
  
  # Predict: Hotelling's T^2
  pred_hts_nf <- predict(fit_hts, dat$none[id_tst[-c(1:(l-1))], ])
  
  # Get h Level
  h_gru <- quantile(pred_gru_nf$pstat[(l+1):n_ic_tst], ql)
  h_mrf <- quantile(pred_mrf_nf$pstat[(l+1):n_ic_tst], ql)
  h_vmc <- quantile(pred_vmc_nf$pstat[(l+1):n_ic_tst], ql)
  h_vmw <- quantile(pred_vmw_nf$pstat[(l+1):n_ic_tst], ql)
  h_hts <- quantile(pred_hts_nf$pstat[(l+1):n_ic_tst], ql)
  
  # Construct Tibble
  rl_gru <- tibble(Data = data_type, method = "gruMCUSUM", h = h_gru)
  rl_mrf <- tibble(Data = data_type, method = "mrfMCUSUM", h = h_mrf)
  rl_vmc <- tibble(Data = data_type, method = "varMCUSUM", h = h_vmc)
  rl_vmw <- tibble(Data = data_type, method = "varMEWMA",  h = h_vmw)
  rl_hts <- tibble(Data = data_type, method = "Hotelling-T2", h = h_hts)
  
  
  ### Return
  list(h_vals = bind_rows(rl_gru,
                          rl_mrf,
                          rl_vmc,
                          rl_vmw,
                          rl_hts) |> 
         mutate(Data = as_factor(Data), method = as_factor(method)))
}

### Get ARL -------------------------------------------------------------------
gen_sim_study_brk <- function(data_type = "lin", n_ic = 500, n_oc = 500,
                              l = 2, arl = 200, h_vals) {
  # Constants
  id_trn <- 1:n_ic
  id_tst <- (n_ic - l):(n_ic+n_oc) # adjust l for varma and htsquare
  ql <- 1 - 1/arl
  
  ### Gen Data ###
  if(data_type == "lin")      dat <- gen_dat_lin(n_ic, n_oc)
  else if(data_type == "nlr") dat <- gen_dat_nrl(n_ic, n_oc)
  else if(data_type == "ltm") dat <- gen_dat_ltm(n_ic, n_oc)
  
  
  # Fit Models
  fit_gru <- train(dat$none[id_trn, ], method = "gruMCUSUM", lags = l, k = 1.1)
  fit_mrf <- train(dat$none[id_trn, ], method = "mrfMCUSUM", lags = l, k = 5)
  fit_vmc <- train(dat$none[id_trn, ], method = "varmaMCUSUM", lags = 1, k = 0.9)
  fit_vmw <- train(dat$none[id_trn, ], method = "varmaMEWMA", lags = 1, r = 0.3)
  fit_hts <- train(dat$none[id_trn, ], method = "htsquare")
  
  # Predict: GRU
  pred_gru_nf <- predict(fit_gru, dat$none[id_tst, ])
  pred_gru_f1 <- predict(fit_gru, dat$f1[id_tst, ])
  pred_gru_f2 <- predict(fit_gru, dat$f2[id_tst, ])
  pred_gru_f3 <- predict(fit_gru, dat$f3[id_tst, ])
  
  # Predict: MRF
  pred_mrf_nf <- predict(fit_mrf, dat$none[id_tst, ])
  pred_mrf_f1 <- predict(fit_mrf, dat$f1[id_tst, ])
  pred_mrf_f2 <- predict(fit_mrf, dat$f2[id_tst, ])
  pred_mrf_f3 <- predict(fit_mrf, dat$f3[id_tst, ])
  
  # Predict: VARMA - MCUSUM
  pred_vmc_nf <- predict(fit_vmc, dat$none[id_tst, ])
  pred_vmc_f1 <- predict(fit_vmc, dat$f1[id_tst, ])
  pred_vmc_f2 <- predict(fit_vmc, dat$f2[id_tst, ])
  pred_vmc_f3 <- predict(fit_vmc, dat$f3[id_tst, ])
  
  # Predict: VARMA - MEWMA
  pred_vmw_nf <- predict(fit_vmw, dat$none[id_tst, ])
  pred_vmw_f1 <- predict(fit_vmw, dat$f1[id_tst, ])
  pred_vmw_f2 <- predict(fit_vmw, dat$f2[id_tst, ])
  pred_vmw_f3 <- predict(fit_vmw, dat$f3[id_tst, ])
  
  # Predict: Hotelling's T^2
  pred_hts_nf <- predict(fit_hts, dat$none[id_tst, ])
  pred_hts_f1 <- predict(fit_hts, dat$f1[id_tst, ])
  pred_hts_f2 <- predict(fit_hts, dat$f2[id_tst, ])
  pred_hts_f3 <- predict(fit_hts, dat$f3[id_tst, ])
  
  # Get h Level
  h_gru <- h_vals[1]
  h_mrf <- h_vals[2]
  h_vmc <- h_vals[3]
  h_vmw <- h_vals[4]
  h_hts <- h_vals[5]
  
  # Get Run Length: GRU
  rl_gru_nf <- get_first_fault(tail(pred_gru_nf$pstat, n_oc), h_gru)
  rl_gru_f1 <- get_first_fault(tail(pred_gru_f1$pstat, n_oc), h_gru)
  rl_gru_f2 <- get_first_fault(tail(pred_gru_f2$pstat, n_oc), h_gru)
  rl_gru_f3 <- get_first_fault(tail(pred_gru_f3$pstat, n_oc), h_gru)
  
  # Get Run Length: MRF
  rl_mrf_nf <- get_first_fault(tail(pred_mrf_nf$pstat, n_oc), h_mrf)
  rl_mrf_f1 <- get_first_fault(tail(pred_mrf_f1$pstat, n_oc), h_mrf)
  rl_mrf_f2 <- get_first_fault(tail(pred_mrf_f2$pstat, n_oc), h_mrf)
  rl_mrf_f3 <- get_first_fault(tail(pred_mrf_f3$pstat, n_oc), h_mrf)
  
  # Get Run Length: VARMA - MCUSUM
  rl_vmc_nf <- get_first_fault(tail(pred_vmc_nf$pstat, n_oc), h_vmc)
  rl_vmc_f1 <- get_first_fault(tail(pred_vmc_f1$pstat, n_oc), h_vmc)
  rl_vmc_f2 <- get_first_fault(tail(pred_vmc_f2$pstat, n_oc), h_vmc)
  rl_vmc_f3 <- get_first_fault(tail(pred_vmc_f3$pstat, n_oc), h_vmc)
  
  # Get Run Length: VARMA - MEWMA
  rl_vmw_nf <- get_first_fault(tail(pred_vmw_nf$pstat, n_oc), h_vmw)
  rl_vmw_f1 <- get_first_fault(tail(pred_vmw_f1$pstat, n_oc), h_vmw)
  rl_vmw_f2 <- get_first_fault(tail(pred_vmw_f2$pstat, n_oc), h_vmw)
  rl_vmw_f3 <- get_first_fault(tail(pred_vmw_f3$pstat, n_oc), h_vmw)
  
  # Get Run Length: Hotelling's T^2
  rl_hts_nf <- get_first_fault(tail(pred_hts_nf$pstat, n_oc), h_hts)
  rl_hts_f1 <- get_first_fault(tail(pred_hts_f1$pstat, n_oc), h_hts)
  rl_hts_f2 <- get_first_fault(tail(pred_hts_f2$pstat, n_oc), h_hts)
  rl_hts_f3 <- get_first_fault(tail(pred_hts_f3$pstat, n_oc), h_hts)
  
  # Construct Tibble
  rl_gru <- tibble(Data = data_type, method = "gruMCUSUM", IC = rl_gru_nf,
                       F1 = rl_gru_f1, F2 = rl_gru_f2, F3 = rl_gru_f3)
  rl_mrf <- tibble(Data = data_type, method = "mrfMCUSUM", IC = rl_mrf_nf,
                       F1 = rl_mrf_f1, F2 = rl_mrf_f2, F3 = rl_mrf_f3)
  rl_vmc <- tibble(Data = data_type, method = "varMCUSUM", IC = rl_vmc_nf,
                       F1 = rl_vmc_f1, F2 = rl_vmc_f2, F3 = rl_vmc_f3)
  rl_vmw <- tibble(Data = data_type, method = "varMEWMA", IC = rl_vmw_nf,
                       F1 = rl_vmw_f1, F2 = rl_vmw_f2, F3 = rl_vmw_f3)
  rl_hts <- tibble(Data = data_type, method = "Hotelling-T2", IC = rl_hts_nf,
                       F1 = rl_hts_f1, F2 = rl_hts_f2, F3 = rl_hts_f3)
  # Save Pstat
  pstat_gru <- tibble(Data = data_type, method = "gruMCUSUM",
                          IC = c(fit_gru$pstat, pred_gru_nf$pstat),
                          F1 = c(fit_gru$pstat, pred_gru_f1$pstat),
                          F2 = c(fit_gru$pstat, pred_gru_f2$pstat),
                          F3 = c(fit_gru$pstat, pred_gru_f3$pstat))
  
  pstat_mrf <- tibble(Data = data_type, method = "mrfMCUSUM",
                          IC = c(fit_mrf$pstat, pred_mrf_nf$pstat),
                          F1 = c(fit_mrf$pstat, pred_mrf_f1$pstat),
                          F2 = c(fit_mrf$pstat, pred_mrf_f2$pstat),
                          F3 = c(fit_mrf$pstat, pred_mrf_f3$pstat))
  
  pstat_vmc <- tibble(Data = data_type, method = "varMCUSUM",
                          IC = c(fit_vmc$pstat, pred_vmc_nf$pstat),
                          F1 = c(fit_vmc$pstat, pred_vmc_f1$pstat),
                          F2 = c(fit_vmc$pstat, pred_vmc_f2$pstat),
                          F3 = c(fit_vmc$pstat, pred_vmc_f3$pstat))
  
  pstat_vmw <- tibble(Data = data_type, method = "varMEWMA",
                          IC = c(fit_vmw$pstat, pred_vmw_nf$pstat),
                          F1 = c(fit_vmw$pstat, pred_vmw_f1$pstat),
                          F2 = c(fit_vmw$pstat, pred_vmw_f2$pstat),
                          F3 = c(fit_vmw$pstat, pred_vmw_f3$pstat))
  
  pstat_hts <- tibble(Data = data_type, method = "Hotelling-T2",
                          IC = c(fit_hts$pstat, pred_hts_nf$pstat),
                          F1 = c(fit_hts$pstat, pred_hts_f1$pstat),
                          F2 = c(fit_hts$pstat, pred_hts_f2$pstat),
                          F3 = c(fit_hts$pstat, pred_hts_f3$pstat))
  
  ### Return
  list(dat_pstat = bind_rows(pstat_gru,
                             pstat_mrf,
                             pstat_vmc[-c(1:l), ],
                             pstat_vmw[-c(1:l), ],
                             pstat_hts[-c(1:l), ]) |> 
         mutate(Data = as_factor(Data), method = as_factor(method)),
       dat_rl = bind_rows(rl_gru,
                          rl_mrf,
                          rl_vmc,
                          rl_vmw,
                          rl_hts) |> 
         mutate(Data = as_factor(Data), method = as_factor(method)))
}

# ### Run Single Simulation
# sim_tst <- gen_sim_study_brk(data_type = "lin")
# 
# sim_tst$dat_pstat |> 
#   group_by(method) |> 
#   summarise(n = n())
# 
# # Visualize Simulation
# n_categories <- nlevels(sim_tst$dat_pstat$method)*nlevels(sim_tst$dat_pstat$Data)
# 
# sim_tst$dat_pstat |> 
#   mutate(index = rep(1:(n()/n_categories), n_categories)) |> 
#   pivot_longer(-c(index, Data, method)) |> 
#   mutate(method = as_factor(method), 
#          name = as_factor(name)) |> 
#   ggplot(aes(index, value, color = name)) +
#   geom_line() +
#   geom_vline(xintercept = 500, color = "blue") +
#   geom_vline(xintercept = 1000, color = "red") +
#   facet_grid(Data ~ method) +
#   #ylim(0, 30) +
#   labs(y = "Plotting Statistic", color = "",
#        title = "Single Sample of Plotting Statistic for Each Method")
# 
# 
# 
# # ARL Table
# sim_tst$dat_rl

### Apply Method --------------------------------------------------------------

### h values
n_sim <- 3

h_sim_lin <- 1:n_sim |> 
  map(\(i) {
    sim_h_brk(data_type = "lin", arl = 200, n_ic_trn = 1000, n_ic_tst = 20000)
  })

# h Value Table
h_val_lin <-
  h_sim_lin |>
  map(\(i) i$h_vals) |> 
  bind_rows() |> 
  group_by(Data, method) |> 
  summarise(across(where(is.numeric), mean))

saveRDS(h_val_lin, file = here("results", "h_sim_lin_01.rds"))

### ARLs
n_sim <- 3

# Linear
h_val_lin <- readRDS(here("results", "h_sim_lin_01.rds"))

sim_vals_lin <- 1:n_sim |> 
  map(\(i) {
    gen_sim_study_brk(data_type = "lin", arl = 200, h_vals = h_val_lin$h,
                      n_ic = 1000, n_oc = 1000)
  })

sim_vals_lin[[1]]$dat_pstat |>
  mutate(index = rep(2:2000, 5)) |> 
  pivot_longer(c(IC, F1, F2, F3)) |> 
  ggplot(aes(index, value, color = method)) +
  geom_line() +
  facet_wrap(~ name) +
  lims(x = c(0, 1500), y = c(0, 100))
  
