### Imports ###
library("here")
library("tidyverse")
library("reticulate")
library("mlmcusum")

#path_conda <- "/home/ubuntu/miniconda3/envs/deep-learning-03"
#path_conda <- "/home/nossimid/miniconda3/envs/deep_learning_v03"
#use_condaenv(path_conda)
path_py <- "~/git/mlmcusum/inst/python/gru_functions.py"
source_python(path_py)

#set.seed(123)

### Get ARL -------------------------------------------------------------------
gen_sim_study_brk <- function(data_type = "lin", n_ic_mod = 500, n_ic_h = 500, n_oc = 500,
                              l = 2, arl = 40, ic_arl_first = FALSE) {
  # Constants
  id_trn_mod <- 1:n_ic_mod
  id_trn_h <- (n_ic_mod-l):(n_ic_mod+n_ic_h)
  id_tst <- (n_ic_mod + n_ic_h - l):(n_ic_mod + n_ic_h + n_oc) # adjust l for varma and htsquare
  ql <- 1 - 1/arl
  
  ### Gen Data ###
  if(data_type == "lin")      dat <- gen_dat_lin(n_ic_mod + n_ic_h, n_oc)
  else if(data_type == "ltl") dat <- gen_dat_ltl(n_ic_mod + n_ic_h, n_oc)
  else if(data_type == "nlr") dat <- gen_dat_nlr(n_ic_mod + n_ic_h, n_oc)
  else if(data_type == "ltm") dat <- gen_dat_ltm(n_ic_mod + n_ic_h, n_oc)
  
  # Choice of Methods
  methods <- 
    expand.grid(
      c("gruMEWMA", "varmaMEWMA"), # methods
      c(.3,.5,.7) # choice of r (or k for MCUSUM)
    ) |> 
    as_tibble() |> 
    set_names("method", "r") |> 
    add_row(method = "htsquare", r = 0)
  
  # Method Names
  method_names <-
    methods |> 
    pmap_chr(\(method, r) {
      paste(method, r, sep = '-')
    })
  
  # Fit Models
  fit <-
    methods |> 
    pmap(\(method, r) {
      train(dat$none[id_trn_mod, ], method = method, lags = l, r = r)
    }) |> 
    set_names(method_names)
  
  # Predict: To estimate h
  pred_h <- 
    fit |> 
    map(\(x) {
      predict(x, dat$none[id_trn_h, ])
    })
  
  # Get h Level
  h <- 
    pred_h |> 
    map(\(x) {
      quantile(x$pstat, ql) |> as.numeric()
    })
  
  # Predictions
  pred <-
    fit |> 
    map(\(x) {
      list(
        nf = predict(x, dat$none[id_tst, ]),
        f1 = predict(x, dat$f1[id_tst, ]),
        f2 = predict(x, dat$f2[id_tst, ]),
        f3 = predict(x, dat$f3[id_tst, ])
      )
    })
  
  # Run Length
  rl <-
    map2_dfr(pred, h, .id = "method",
         \(x, y) {
           tibble(
             data = data_type,
             nf = ifelse(ic_arl_first,
                    get_first_fault(tail(x$nf$pstat, n_oc), y),
                    get_run_length(tail(x$nf$pstat, n_oc), y)),
             f1 = get_first_fault(tail(x$f1$pstat, n_oc), y),
             f2 = get_first_fault(tail(x$f2$pstat, n_oc), y),
             f3 = get_first_fault(tail(x$f3$pstat, n_oc), y)
           )
         }) |> 
    mutate(method = as_factor(method),
           data = as_factor(data))
  
  # Plotting statistic
  pstat <- 
    pmap_dfr(list(fit, pred_h, pred), .id = "method",
         \(x, y, z) {
           tibble(
             data = data_type,
             nf = c(x$pstat, y$pstat, z$nf$pstat),
             f1 = c(x$pstat, y$pstat, z$f1$pstat),
             f2 = c(x$pstat, y$pstat, z$f2$pstat),
             f3 = c(x$pstat, y$pstat, z$f3$pstat)
           )
         }) |> 
    mutate(method = as_factor(method),
           data = as_factor(data))

  
  ### Return
  list(pstat = pstat,
       rl = rl,
       h = tibble(method = h |> names() |> as_factor(),
                  h = unlist(h)))
}














