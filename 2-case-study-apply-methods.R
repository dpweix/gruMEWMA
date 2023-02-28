library("tidyverse")
library("here")
library("lubridate")
library("forecast")
library("reticulate")
library("kableExtra")
library("zoo")

# Custom package https://github.com/dpweix/mlewma.git
library("mlmewma")

# Load GRU functions
path_py <- "~/git/mlmewma/inst/python/gru_functions.py"
source_python(path_py)

### Fit models ----------------------------------------------------------------
set.seed(3)
py_set_seed(3)

# Load full data, training data for modeling, training data for estimating
# the control limit, and testing data
dat     <- readRDS(here("data", "bw30-navajo.rds"))
dat_mod <- readRDS(here("data", "bw30-navajo-trn-mod.rds"))
dat_ctl <- readRDS(here("data", "bw30-navajo-trn-ctl.rds"))
dat_tst <- readRDS(here("data", "bw30-navajo-tst.rds"))

# Constants
sec_btw_obs <- 3
method_types <- c("gruMEWMA" , "mrfMEWMA"  , "varMEWMA", "htsquare")
method_const <- 2 * c(0.1        , 0.1         , 0.1       , 0)
method_l     <- c(2          , 2           , 1         , 1)

# Train models
fit <-
  pmap(list(method_types, method_const, method_l), 
       \(method, r, l) {
         train_fd(dat_mod, method = method, lags = l, r = r)
       }) |> 
  set_names(method_types)

# Predictions for estimating h
pred_ctl <- map(fit,
                  \(x) {
                    predict_fd(x, dat_ctl)
                  })

# Predictions on testing data
pred_tst <- map(fit,
                \(x) {
                  predict_fd(x, dat_tst)
                })

# Combine pstat from all data sets
pstat <- names(pred_tst) |> 
  map(\(x) {
    bind_rows(fit[[x]]$pstat      |> as_tibble(),
              pred_ctl[[x]]$pstat |> as_tibble(),
              pred_tst[[x]]$pstat |> as_tibble()) |> 
      mutate(Date_Time = pull(dat, Date_Time) |> tail(n()))
  }) |> 
  set_names(names(pred_tst))

# Calculate control limits
arl_ic <- 200
ql <- 1 - 1/arl_ic

h <- pred_ctl |>
  map(\(x) {
      quantile(x$pstat, ql)
  })

# Get location of first occurrence of five OC flags in a row
flags <- 1:4 |> 
  map(\(x) {
    which(pred_tst[[x]]$pstat > h[[x]]) |> 
      as.numeric()
  }) |> 
  set_names(method_types)

diff(flags$gruMEWMA) |> head(20)
diff(flags$mrfMEWMA) |> head(20)
diff(flags$varMEWMA) |> head(20)
diff(flags$htsquare) |> head(20)

flags$gruMEWMA[5]
flags$mrfMEWMA[6]
flags$varMEWMA[5]
flags$htsquare[7]

# The GRU model cannot make new predictions when loaded from save
saveRDS(fit,      here("data", "fit.rds"))
saveRDS(pred_ctl, here("data", "pred-ctl.rds"))
saveRDS(pred_tst, here("data", "pred-tst.rds"))
saveRDS(pstat,    here("data", "pstat.rds"))
saveRDS(h,        here("data", "h.rds"))
