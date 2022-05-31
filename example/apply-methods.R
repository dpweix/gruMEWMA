library("tidyverse")
theme_set(theme_bw())
library("here")
library("lubridate")
library("mlmcusum")
library("forecast")
library("reticulate")
library("kableExtra")


### Load Data -----------------------------------------------------------------
# Load GRU functions
path_py <- "~/git/mlmcusum/inst/python/gru_functions.py"
source_python(path_py)

# Get Data
load(here("example", "bw30_navajo.rda"))

dat <- dat[-c(1:20), ]

# Variables of Interest
var_monitored <- c("Cell 1 Flux (LMH)", "Cell 2 Flux (LMH)",
                   "Perm. Cond. 1 (mS/cm)", "Perm. Cond. 2 (mS/cm)")

# Data Split
end_trn <- ymd_hms("2020-11-01 04:00:00")
end_tst <- ymd_hms("2020-11-01 08:00:00")

# Time Series Plot
dat |> 
  pivot_longer(all_of(var_monitored)) |> 
  ggplot(aes(Date_Time, value)) +
  geom_point(shape = 1) +
  geom_vline(xintercept = end_trn, color = "blue") +
  geom_vline(xintercept = end_tst, color = "red") +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  labs(x = "", y = "")

ggsave("/home/ubuntu/git/reports/gruMCUSUM_paper/example-data.png",
       width = 25, height = 20, units = "cm")

# Count by Set
dat |> filter(Date_Time <= end_trn) |> nrow() # Training
dat |> filter(Date_Time >= end_trn,
              Date_Time <= end_tst) |> nrow() # Testing
dat |> filter(Date_Time >= end_tst) |> nrow() # Application

### Fit Models ----------------------------------------------------------------
# For this application we use data from "2020-10-31 12:00:00" to 
# "2020-11-01 12:00:00" as training data. Everything else is testing. Could 
# mimic actual application of training for a day and application for a day.

# Constants
sec_btw_obs <- 3
l <- 2
method_types <- c("gru_MCUSUM", "mrf_MCUSUM", "var_MCUSUM", "var_MEWMA", "htsquare")
method_const <- c(1.1         , 5           , 1.1         , 0.3        , 0)

# Model Training Data
dat_trn <- dat |> 
  filter(Date_Time <= end_trn) |> 
  select(all_of(var_monitored)) 

# Method Testing Data
dat_tst <- dat |> 
  filter(Date_Time >= end_trn,
         Date_Time <= end_tst) |> 
  select(all_of(var_monitored)) 

# Method Application Data
dat_app <- dat |> 
  filter(Date_Time >= end_tst) |> 
  select(all_of(var_monitored))

# Fit Models
fit_gru <- train(dat_trn, method = "gruMCUSUM", lags = l, k = 1.1)
fit_mrf <- train(tail(dat_trn, 1000), method = "mrfMCUSUM", lags = 2, k = 5) # removed data for memory purposes
fit_vmc <- train(dat_trn, method = "varMCUSUM", lags = 1, k = 1.1)
fit_vmw <- train(dat_trn, method = "varMEWMA", lags = 1, r = 0.3)

# All fitted models
fit <- list(gruMCUSUM = fit_gru, 
            mrfMCUSUM = fit_mrf, 
            varMCUSUM = fit_vmc, 
            varMEWMA = fit_vmw)

saveRDS(fit, here("example", "fitted_models.rds"))



### Apply Methods -------------------------------------------------------------
fit <- loadRDS(here("example", "fitted_models.rds"))

# Predict Testing Data
pred_tst <- map(fit,
                \(x) {
                  predict(x, dat_tst)
                })

# Determine Control Limits
arl_ic <- 200
ql <- 1 - 1/arl_ic

# Get h values
h_lim <- pred_tst |> 
  map(\(x) {
    quantile(x$pstat, ql)
  })

# Predict Application Data
pred_app <- map(fit,
                \(x) {
                  predict(x, dat_app)
                })

### Visualize Pstat -----------------------------------------------------------

# Combine Results from Fit and Prediction
pstat <- c("gruMCUSUM", "mrfMCUSUM", "varMCUSUM", "varMEWMA") |> 
  map(\(x) {
    bind_rows(fit[[x]]$pstat |> as_tibble(),
              pred_tst[[x]]$pstat |> as_tibble(),
              pred_app[[x]]$pstat |> as_tibble()) |> 
      mutate(Date_Time = pull(dat, Date_Time) |> tail(n()))
  })

names(pstat) <- c("gruMCUSUM", "mrfMCUSUM", "varMCUSUM", "varMEWMA")

# y limits for each graph
y_lims <-
  map2(pstat, c(.6, .5, .65, .7), \(x, q) {
    quantile(x$value, q)
  })

# Plot pstat from GRU, MRF, and VAR
pstat_plots <- 1:length(pstat) |> 
  map(\(i) {
    pstat[[i]] |> 
      ggplot(aes(Date_Time, value)) +
      geom_line() +
      geom_vline(xintercept = end_trn, color = "blue") +
      geom_vline(xintercept = end_tst, color = "red") +
      geom_hline(yintercept = h_lim[[i]], color = "darkgreen") +
      labs(x = "", y = "Plotting Statistic",
           title = names(pstat)[i]) +
      lims(y = c(0, y_lims[[i]]))
  })

# Save Plots
1:length(pstat) |> 
  walk(\(i) {
    ggsave(filename = paste0("~/git/reports/gruMCUSUM_paper/pstat-", names(pstat)[i], ".png"),
           plot = pstat_plots[[i]], width = 30, height = 10, units = "cm")
  })


### Visualize ACF ------------------------------------------------------------

### Testing Data - Raw/Residuals
res_tst <-
  c(list(Raw = dat_tst), 
    map(pred_tst, \(x) as_tibble(x$residuals)))

# ACF Plots
acf_tst_plots <-
  1:length(res_tst) |> 
  map(\(i) {
    acf_vals <- res_tst[[i]] |>
      Acf(type = "covariance", plot = FALSE)

    acf_vals$type <- "correlation"

    autoplot(acf_vals) +
      labs(title = paste0(names(res_tst)[i]))
    
    # ggAcf(res_tst[[i]], type = "correlation", lag.max = 50) +
    #   lims(y = c(0, 1)) +
    #   labs(title = paste0(names(res_tst)[i]))
  })

# Save Plots
1:(length(res_tst)-1) |> # -1 will leave out the second var plot 
  walk(\(i) {
    ggsave(filename = paste0("~/git/reports/gruMCUSUM_paper/acf-tst-", names(res_tst)[i], ".png"),
           plot = acf_tst_plots[[i]], width = 30, height = 10, units = "cm")
  })

### Faulty Data - Raw/Residuals
start_fault <- ymd_hms("2020-11-01 21:00:00")
id_fault <- dat |> filter(Date_Time >= start_fault) |> nrow()

res_app <-
  c(list(Raw = tail(dat_app, id_fault)),
    map(pred_app, \(x) tail(as_tibble(x$residuals), id_fault)))

# ACF Plots
acf_app_plots <-
  1:length(res_app) |> 
  map(\(i) {
    acf_vals <- res_app[[i]] |>
      Acf(type = "covariance", plot = FALSE) # switch type to correlation or partial

    acf_vals$type <- "correlation"

    autoplot(acf_vals) +
      labs(title = paste0(names(res_app)[i]))
    
    # ggAcf(res_app[[i]]) +
    #   lims(y = c(0, 1)) +
    #   labs(title = paste0(names(res_app)[i]))
  })

# Save Plots
1:(length(res_app)-1) |> # -1 will leave out the second var plot 
  walk(\(i) {
    ggsave(filename = paste0("~/git/reports/gruMCUSUM_paper/acf-app-", names(res_app)[i], ".png"),
           plot = acf_app_plots[[i]], width = 30, height = 10, units = "cm")
  })

### Visualize Residuals -------------------------------------------------------

# Combine Results from Fit and Prediction
res <- c("gruMCUSUM", "mrfMCUSUM", "varMCUSUM") |> 
  map(\(x) {
    bind_rows(fit[[x]]$residuals |> as_tibble(),
              pred_tst[[x]]$residuals |> as_tibble(),
              pred_app[[x]]$residuals |> as_tibble()) |> 
      mutate(Date_Time = pull(dat, Date_Time) |> tail(n()))
  })

names(res) <- c("gruMCUSUM", "mrfMCUSUM", "varMCUSUM")

# Plot Residuals from GRU, MRF, and VAR
res_plots <- 1:length(res) |> 
  map(\(i) {
    res[[i]] |> 
      pivot_longer(all_of(var_monitored)) |> 
      ggplot(aes(Date_Time, value)) +
      geom_line() +
      geom_vline(xintercept = end_trn, color = "blue") +
      geom_vline(xintercept = end_tst, color = "red") +
      facet_wrap(~ name, scales = "free") +
      labs(x = "", y = "Residuals",
           title = names(res)[i])
  })

# Save Plots
1:length(res) |> 
  walk(\(i) {
    ggsave(filename = paste0("~/git/reports/gruMCUSUM_paper/res-", names(res)[i], ".png"),
           plot = res_plots[[i]], width = 30, height = 10, units = "cm")
  })

