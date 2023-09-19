library("here")
library("tidyverse")
library("reticulate")
library("mlmewma")

path_py <- "~/git/mlmewma/inst/python/gru_functions.py"
source_python(path_py)

### Get ARL -------------------------------------------------------------------
gen_sim_study_brk <- function(data_type = "lin",
                              n_ic_mod = 500, n_ic_h = 500, n_oc = 500,
                              phi = 0.8, l = 2, arl = 40,
                              B = 500,
                              ic_arl_first = FALSE) {
  # Constants
  id_trn_mod <- 1:n_ic_mod
  id_trn_h <- (n_ic_mod-l):(n_ic_mod+n_ic_h)
  id_tst <- (n_ic_mod + n_ic_h - l):(n_ic_mod + n_ic_h + n_oc) # adjust l for varma and htsquare
  ql <- 1 - 1/arl
  p <- 3
  
  ### Gen Data ###
  if(data_type == "lin")      dat <- gen_dat_lin(n_ic_mod + n_ic_h, n_oc, phi)
  else if(data_type == "ltl") dat <- gen_dat_ltl(n_ic_mod + n_ic_h, n_oc, phi)
  else if(data_type == "nlr") dat <- gen_dat_nlr(n_ic_mod + n_ic_h, n_oc, phi)
  else if(data_type == "ltm") dat <- gen_dat_ltm(n_ic_mod + n_ic_h, n_oc, phi)
  
  # Choice of Methods
  methods <- 
    expand.grid(
      c("gruMEWMA", "mrfMEWMA", "varMEWMA", "MEWMA"), # methods
      c(.2,.4,.6, .8) # choice of r (or k for MCUSUM)
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
      train_fd(dat$none[id_trn_mod, ], method = method, lags = l, r = r)
    }) |> 
    set_names(method_names)
  
  # Predict: To estimate h
  pred_h <- 
    fit |> 
    map(\(x) {
      predict_fd(x, dat$none[id_trn_h, ])
    })
  
  # Bootstrap sample of h
  # h_bootstrap <- 
  #   pred_h |> 
  #   map(\(x) {
  #     1:B |> 
  #       map_dbl(\(b) {
  #         quantile(sample(x$pstat, length(x$pstat), replace = TRUE), ql)
  #       })
  #   }) |> 
  #   set_names(method_names)
  
  # Get h Level
  h <- #map(h_bootstrap, mean)
    pred_h |>
    map(\(x) {
      quantile(x$pstat, ql) |> as.numeric()
    })
  
  # Predictions
  pred <-
    map2(fit, pred_h, \(x, y) {
      list(
        nf = predict_fd(x, dat$none[id_tst, ], pstat0 = last(y$pstat)),
        f1 = predict_fd(x, dat$f1[id_tst, ], pstat0 = last(y$pstat)),
        f2 = predict_fd(x, dat$f2[id_tst, ], pstat0 = last(y$pstat)),
        f3 = predict_fd(x, dat$f3[id_tst, ], pstat0 = last(y$pstat))
      )
    })
  
  # Run Length
  rl <-
    map2_dfr(pred, h, .id = "method",
         \(x, y) {
           tibble(
             phi = phi,
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














