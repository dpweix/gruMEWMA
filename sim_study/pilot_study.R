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

### Simulation Study Function ###
get_run_lengths <- 
  function(method = "gru", gen = "linear",
           num_sets = 5, n_ic = 100, n_oc = 100, arl = 40, l = 2,
           k = 1.1, r = 0.3) {
    
    # Constants 
    id_trn <- 1:n_ic
    id_tst <- (n_ic+1):(n_ic + n_oc)
    ql <- 1 - 1/arl
    
  1:num_sets |> 
    map_dfr(\(x) {
      
      ### Generate Data ###
      if(gen == "linear") {
        dat_ic <- gen_dat_lin("none", n_ic + n_oc)[, -1]
        dat_f1 <- gen_dat_lin("f1", n_ic, n_oc)[, -1]
        dat_f2 <- gen_dat_lin("f2", n_ic, n_oc)[, -1]
        dat_f3 <- gen_dat_lin("f3", n_ic, n_oc)[, -1]
        
      } else if(gen == "non-linear") {
        dat_ic <- gen_dat_nlr("none", n_ic + n_oc)[, -1]
        dat_f1 <- gen_dat_nlr("f1", n_ic, n_oc)[, -1]
        dat_f2 <- gen_dat_nlr("f2", n_ic, n_oc)[, -1]
        dat_f3 <- gen_dat_nlr("f3", n_ic, n_oc)[, -1]
        
      } else if(gen == "long-term") {
        dat_ic <- gen_dat_ltm("none", n_ic + n_oc)[, -1]
        dat_f1 <- gen_dat_ltm("f1", n_ic, n_oc)[, -1]
        dat_f2 <- gen_dat_ltm("f2", n_ic, n_oc)[, -1]
        dat_f3 <- gen_dat_ltm("f3", n_ic, n_oc)[, -1]
        
      }
      
      ### Train Models ###
      if(method == "gru") {
        model_ic <- train_gruMCUSUM(dat_ic[id_trn, ], lags = l, k = k)
        model_f1 <- train_gruMCUSUM(dat_f1[id_trn, ], lags = l, k = k)
        model_f2 <- train_gruMCUSUM(dat_f2[id_trn, ], lags = l, k = k)
        model_f3 <- train_gruMCUSUM(dat_f3[id_trn, ], lags = l, k = k)
      } else if(method == "mrf") {
        model_ic <- train_mrfMCUSUM(dat_ic[id_trn, ], lags = l, k = k)
        model_f1 <- train_mrfMCUSUM(dat_f1[id_trn, ], lags = l, k = k)
        model_f2 <- train_mrfMCUSUM(dat_f2[id_trn, ], lags = l, k = k)
        model_f3 <- train_mrfMCUSUM(dat_f3[id_trn, ], lags = l, k = k)
      }
      
      ### Make Predictions ###
      if(method == "gru") {
        preds_ic <- predict_gruMCUSUM(model_ic, dat_ic[id_tst, ])
        preds_f1 <- predict_gruMCUSUM(model_f1, dat_f1[id_tst, ])
        preds_f2 <- predict_gruMCUSUM(model_f2, dat_f2[id_tst, ])
        preds_f3 <- predict_gruMCUSUM(model_f3, dat_f3[id_tst, ])
      } else if(method == "mrf") {
        preds_ic <- predict_mrfMCUSUM(model_ic, dat_ic[id_tst, ])
        preds_f1 <- predict_mrfMCUSUM(model_f1, dat_f1[id_tst, ])
        preds_f2 <- predict_mrfMCUSUM(model_f2, dat_f2[id_tst, ])
        preds_f3 <- predict_mrfMCUSUM(model_f3, dat_f3[id_tst, ])
      }
      
      ### Get h Level ###
      h <- quantile(c(model_ic$pstat,
                      model_f1$pstat,
                      model_f2$pstat,
                      model_f3$pstat), ql) # or preds_ic$pstat
      
      ### Get Run Lengths ###
      rl_ic <- get_run_length(preds_ic$pstat, h)
      rl_f1 <- get_run_length(preds_f1$pstat, h)
      rl_f2 <- get_run_length(preds_f2$pstat, h)
      rl_f3 <- get_run_length(preds_f3$pstat, h)
      
      ### Return ###
      tibble(Data = gen,
             Method = method,
             IC = rl_ic,
             F1 = rl_f1,
             F2 = rl_f2,
             F3 = rl_f3)
      
    })
  }

### Sim Study Hyper Parameters ###
l <- 2 # lag number
arl <- 40 # Theoretical ARL
num_sets <- 5 # repetitions of process
n_ic = 500
n_oc = 500


# Linear Data - GRU 
    rl_lin_gru <- get_run_lengths(method = "gru", gen = "linear",
                                  num_sets = num_sets, n_ic = n_ic, 
                                  n_oc = n_oc, arl = arl)
    save(rl_lin_gru, file = here("sim_study", "rl_lin_gru.rda"))

# Linear Data - MRF 
    rl_lin_mrf <- get_run_lengths(method = "mrf", gen = "linear",
                                  num_sets = num_sets, n_ic = n_ic, 
                                  n_oc = n_oc, arl = arl, k = 2)
    save(rl_lin_mrf, file = here("sim_study", "rl_lin_mrf.rda"))

# Non-Linear Data - GRU 
    rl_nlr_gru <- get_run_lengths(method = "gru", gen = "non-linear",
                                  num_sets = num_sets, n_ic = n_ic, 
                                  n_oc = n_oc, arl = arl)
    save(rl_nlr_gru, file = here("sim_study", "rl_nlr_gru.rda"))

# Linear Data - MRF 
    rl_nlr_mrf <- get_run_lengths(method = "mrf", gen = "non-linear",
                                  num_sets = num_sets, n_ic = n_ic, 
                                  n_oc = n_oc, arl = arl, k = 2)
    save(rl_nlr_mrf, file = here("sim_study", "rl_nlr_mrf.rda"))

# Long Term Data - GRU 
    rl_ltm_gru <- get_run_lengths(method = "gru", gen = "long-term",
                                  num_sets = num_sets, n_ic = n_ic, 
                                  n_oc = n_oc, arl = arl)
    save(rl_ltm_gru, file = here("sim_study", "rl_ltm_gru.rda"))
    
# Long Term Data - MRF 
    rl_ltm_mrf <- get_run_lengths(method = "mrf", gen = "long-term",
                                  num_sets = num_sets, n_ic = n_ic, 
                                  n_oc = n_oc, arl = arl, k = 2)
    save(rl_ltm_mrf, file = here("sim_study", "rl_ltm_mrf.rda"))


### Get ARL Table##
load(here("sim_study", "rl_lin_gru.rda"))
load(here("sim_study", "rl_nlr_gru.rda"))
load(here("sim_study", "rl_ltm_gru.rda"))
load(here("sim_study", "rl_lin_mrf.rda"))
load(here("sim_study", "rl_nlr_mrf.rda"))
load(here("sim_study", "rl_ltm_mrf.rda"))

arl_all <-
  bind_rows(rl_lin_gru, rl_nlr_gru, rl_ltm_gru,
          rl_lin_mrf, rl_nlr_mrf, rl_ltm_mrf) |> 
  mutate(Data = as_factor(Data), Method = as_factor(Method)) |> 
  group_by(Data, Method) |> 
  summarise(across(where(is.numeric), mean)) |> 
  mutate(across(where(is.numeric), round, 2))

arl_all |> 
  kbl(format = "latex", booktabs = TRUE, linesep = c(''))

### Sim Study Hyper Parameters ###
l <- 2 # lag number
arl <- 40 # Theoretical ARL
ql <- 1 - 1/arl # quantile limit
num_sets <- 6 # repetitions of process
id_trn <- 1:100 # index of training data
id_tst <- 101:200 # index of testing data
id_ic <- 1:(50-l) # index of in-control obs for testing data
id_oc <- (51-l):(100-l) # index of out-of-control obs for testing data

# Linear, GRU
rl_lin_gru <-
  1:num_sets |> 
    map_dfr(\(x) {
      # Generate Data
      dat_ic <- gen_dat_lin("none", n_ic = 200)[, -1]
      dat_f1 <- gen_dat_lin("f1", n_ic = 100, n_oc = 100)[, -1]
      dat_f2 <- gen_dat_lin("f2", n_ic = 100, n_oc = 100)[, -1]
      dat_f3 <- gen_dat_lin("f3", n_ic = 100, n_oc = 100)[, -1]
      
      # Train Models
      model_ic <- train_gruMCUSUM(dat_ic[id_trn, ], lags = l, k = 1.1)
      model_f1 <- train_gruMCUSUM(dat_f1[id_trn, ], lags = l, k = 1.1)
      model_f2 <- train_gruMCUSUM(dat_f2[id_trn, ], lags = l, k = 1.1)
      model_f3 <- train_gruMCUSUM(dat_f3[id_trn, ], lags = l, k = 1.1)
      
      # Make Predictions
      preds_ic <- predict_gruMCUSUM(model_ic, dat_ic[id_tst, ])
      preds_f1 <- predict_gruMCUSUM(model_f1, dat_f1[id_tst, ])
      preds_f2 <- predict_gruMCUSUM(model_f2, dat_f2[id_tst, ])
      preds_f3 <- predict_gruMCUSUM(model_f3, dat_f3[id_tst, ])
      
      # Get h level
      h_ic <- quantile(model_ic$pstat, ql) # or preds_ic$pstat
      
      # Get Run Lengths
      rl_ic <- get_run_length(preds_ic$pstat, h_ic)
      rl_f1 <- get_run_length(preds_f1$pstat, h_ic)
      rl_f2 <- get_run_length(preds_f2$pstat, h_ic)
      rl_f3 <- get_run_length(preds_f3$pstat, h_ic)
      
      # Return
      tibble(Data = "Linear",
             Method = "GRU",
             IC = rl_ic,
             F1 = rl_f1,
             F2 = rl_f2,
             F3 = rl_f3,)
    })

save(rl_lin_gru, file = here("sim_study", "rl_lin_gru.rda"))

# Non-Linear, GRU
rl_nlr_gru <-
  1:num_sets |> 
  map_dfr(\(x) {
    # Generate Data
    dat_ic <- gen_dat_nlr("none", n_ic = 200)[, -1]
    dat_f1 <- gen_dat_nlr("f1", n_ic = 100, n_oc = 100)[, -1]
    dat_f2 <- gen_dat_nlr("f2", n_ic = 100, n_oc = 100)[, -1]
    dat_f3 <- gen_dat_nlr("f3", n_ic = 100, n_oc = 100)[, -1]
    
    # Train Models
    model_ic <- train_gruMCUSUM(dat_ic[id_trn, ], lags = l, k = 1.1)
    model_f1 <- train_gruMCUSUM(dat_f1[id_trn, ], lags = l, k = 1.1)
    model_f2 <- train_gruMCUSUM(dat_f2[id_trn, ], lags = l, k = 1.1)
    model_f3 <- train_gruMCUSUM(dat_f3[id_trn, ], lags = l, k = 1.1)
    
    # Make Predictions
    preds_ic <- predict_gruMCUSUM(model_ic, dat_ic[id_tst, ])
    preds_f1 <- predict_gruMCUSUM(model_f1, dat_f1[id_tst, ])
    preds_f2 <- predict_gruMCUSUM(model_f2, dat_f2[id_tst, ])
    preds_f3 <- predict_gruMCUSUM(model_f3, dat_f3[id_tst, ])
    
    # Get h level
    h_ic <- quantile(model_ic$pstat, ql) # or preds_ic$pstat
    
    # Get Run Lengths
    rl_ic <- get_run_length(preds_ic$pstat, h_ic)
    rl_f1 <- get_run_length(preds_f1$pstat, h_ic)
    rl_f2 <- get_run_length(preds_f2$pstat, h_ic)
    rl_f3 <- get_run_length(preds_f3$pstat, h_ic)
    
    # Return
    tibble(Data = "Non-Linear",
           Method = "GRU",
           IC = rl_ic,
           F1 = rl_f1,
           F2 = rl_f2,
           F3 = rl_f3,)
  })

# Long-Term, GRU
rl_ltm_gru <-
  1:num_sets |> 
  map_dfr(\(x) {
    # Generate Data
    dat_ic <- gen_dat_ltm("none", n_ic = 200)[, -1]
    dat_f1 <- gen_dat_ltm("f1", n_ic = 100, n_oc = 100)[, -1]
    dat_f2 <- gen_dat_ltm("f2", n_ic = 100, n_oc = 100)[, -1]
    dat_f3 <- gen_dat_ltm("f3", n_ic = 100, n_oc = 100)[, -1]
    
    # Train Models
    model_ic <- train_gruMCUSUM(dat_ic[id_trn, ], lags = l, k = 1.1)
    model_f1 <- train_gruMCUSUM(dat_f1[id_trn, ], lags = l, k = 1.1)
    model_f2 <- train_gruMCUSUM(dat_f2[id_trn, ], lags = l, k = 1.1)
    model_f3 <- train_gruMCUSUM(dat_f3[id_trn, ], lags = l, k = 1.1)
    
    # Make Predictions
    preds_ic <- predict_gruMCUSUM(model_ic, dat_ic[id_tst, ])
    preds_f1 <- predict_gruMCUSUM(model_f1, dat_f1[id_tst, ])
    preds_f2 <- predict_gruMCUSUM(model_f2, dat_f2[id_tst, ])
    preds_f3 <- predict_gruMCUSUM(model_f3, dat_f3[id_tst, ])
    
    # Get h level
    h_ic <- quantile(model_ic$pstat, ql) # or preds_ic$pstat
    
    # Get Run Lengths
    rl_ic <- get_run_length(preds_ic$pstat, h_ic)
    rl_f1 <- get_run_length(preds_f1$pstat, h_ic)
    rl_f2 <- get_run_length(preds_f2$pstat, h_ic)
    rl_f3 <- get_run_length(preds_f3$pstat, h_ic)
    
    # Return
    tibble(Data = "Long-Term",
           Method = "GRU",
           IC = rl_ic,
           F1 = rl_f1,
           F2 = rl_f2,
           F3 = rl_f3,)
  })

save(rl_ltm_gru, file = here("sim_study", "rl_ltm_gru.rda"))

# Get ARL Table
load(here("sim_study", "rl_lin_gru.rda"))
load(here("sim_study", "rl_nlr_gru.rda"))
load(here("sim_study", "rl_ltm_gru.rda"))

bind_rows(rl_lin_gru, rl_nlr_gru, rl_ltm_gru) |> 
  group_by(Data) |> 
  summarise(across(where(is.numeric), mean))
