library("tidyverse")
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 20))
library("here")
library("lubridate")
library("mlmcusum")
library("forecast")
library("reticulate")
library("kableExtra")
library("zoo")


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
end_trn <- ymd_hms("2020-11-01 00:45:00")
end_tst <- ymd_hms("2020-11-01 08:00:00")

# Time Series Plot
dat |> 
  pivot_longer(all_of(var_monitored)) |> 
  ggplot(aes(Date_Time, value)) +
  geom_line() + #geom_point(shape = 1) +
  geom_vline(xintercept = end_trn, color = "blue") +
  geom_vline(xintercept = end_tst, color = "red") +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  labs(x = "", y = "")

ggsave("/home/ubuntu/git/reports/gruMCUSUM_paper/example-data.png",
       width = 25, height = 20, units = "cm")

# Time Series Plot Cell 1 and Perm Cond. 1
dat |> 
  pivot_longer(c("Cell 1 Flux (LMH)", "Perm. Cond. 1 (mS/cm)")) |> 
  ggplot(aes(Date_Time, value)) +
  geom_line() + #geom_point(shape = 1) +
  geom_vline(xintercept = end_trn, color = "blue") +
  geom_vline(xintercept = end_tst, color = "red") +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  labs(x = "", y = "")

ggsave("/home/ubuntu/git/reports/ppp-presentation/example-data-1.png",
       width = 25, height = 10, units = "cm")

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
method_types <- c("gruMEWMA" , "mrfMEWMA"  , "varMEWMA", "htsquare")
method_const <- c(0.3         , 0.3          , 0.3          , 0)
method_l     <- c(4           , 2            , 1            , 1)
# method_types <- c("gruMEWMA" , "varMEWMA", "htsquare")
# method_const <- c(0.7         , 0.7          , 0)
# method_l     <- c(4           , 1            , 1)

# Model Training Data
dat_trn_mod <- dat |> 
  filter(Date_Time <= end_trn) |> 
  select(all_of(var_monitored)) 

# h Estimating Data
dat_trn_h <- dat |> 
  filter(Date_Time >= end_trn,
         Date_Time <= end_tst) |> 
  select(all_of(var_monitored)) 

# Method Testing Data
dat_tst <- dat |> 
  filter(Date_Time >= end_tst) |> 
  select(all_of(var_monitored))

# Train Models
fit <-
  pmap(list(method_types, method_const, method_l), 
       \(method, r, l) {
         train(dat_trn_mod, method = method, lags = l, r = r)
  }) |> 
  set_names(method_types)

# Attempt to train MRF
# fit_mrf <- train(dat_trn_h, method = "mrfMEWMA", lags = 2)

# fit_gru <- train(dat_trn, method = "gruMCUSUM", lags = l, k = 1.1)
#fit_mrf <- train(tail(dat_trn_mod, 1000), method = "mrfMCUSUM", lags = 2, k = 5) # removed data for memory purposes
# fit_vmc <- train(dat_trn, method = "varMCUSUM", lags = 1, k = 1.1)
# fit_vmw <- train(dat_trn, method = "varMEWMA", lags = 1, r = 0.3)
# 
# # All fitted models
# fit <- list(gruMCUSUM = fit_gru, 
#             mrfMCUSUM = fit_mrf, 
#             varMCUSUM = fit_vmc, 
#             varMEWMA = fit_vmw)

# Doesn't work for gru methods
saveRDS(fit, here("example", "fitted-models.rds"))



### Apply Methods -------------------------------------------------------------
fit <- readRDS(here("example", "fitted_models.rds"))

# Predict Testing Data
pred_trn_h <- map(fit,
                  \(x) {
                    predict_fd(x, dat_trn_h)
                  })

# Determine Control Limits
arl_ic <- 200
ql <- 1 - 1/arl_ic

# Get h values
h <- pred_trn_h |> 
  map(\(x) {
    quantile(x$pstat, ql)
  })

# Predict Application Data
pred_tst <- map(fit,
                \(x) {
                  predict_fd(x, dat_tst)
                })

# Get location of first flag
flags <- 1:3 |> 
  map(\(x) {
    which(pred_tst[[x]]$pstat > h[[x]]) |> 
      as.numeric()
  }) |> 
  set_names(method_types)

flags

diff(flags$gruMEWMA)
diff(flags$varmaMEWMA)
diff(flags$htsquare)

flags$gruMEWMA[5]
flags$varmaMEWMA[7]
flags$htsquare[7]

### Visualize Pstat -----------------------------------------------------------

# Combine Results from Fit and Prediction
pstat <- names(pred_tst) |> 
  map(\(x) {
    bind_rows(fit[[x]]$pstat |> as_tibble(),
              pred_trn_h[[x]]$pstat |> as_tibble(),
              pred_tst[[x]]$pstat |> as_tibble()) |> 
      mutate(Date_Time = pull(dat, Date_Time) |> tail(n()))
  }) |> 
  set_names(names(pred_tst))

# Save Plotting Statistic
saveRDS(pstat, here("example", "pstat.rds"))

# y limits for each graph
y_lims <-
  map2(pstat, c(.95, .95, .9, .99), \(x, q) {
    quantile(x$value, q)
  })

# Plot Control Statistic
pstat_plots <- 1:3 |> 
  map(\(i) {
    pstat[[i]] |> 
      mutate(smoothed = rollmedian(value, k = 101, align = "left", fill = TRUE)) |> 
      ggplot(aes(Date_Time, value)) +
      geom_point(shape = 1, alpha = .1) +
      geom_line(aes(y = smoothed)) +
      geom_vline(xintercept = end_trn, color = "blue") +
      geom_vline(xintercept = end_tst, color = "red") +
      geom_hline(yintercept = h[[i]], color = "darkgreen") +
      labs(x = "", y = "Plotting Statistic",
           title = names(pstat)[i]) +
      lims(y = c(0, y_lims[[i]]))
  })

pstat_plots[[4]] <- pstat[[4]] |> 
  mutate(smoothed = rollmedian(value, k = 101, align = "left", fill = TRUE)) |> 
  ggplot(aes(Date_Time, value)) +
  geom_point(shape = 1, alpha = .1) +
  geom_line(aes(y = smoothed)) +
  geom_vline(xintercept = end_trn, color = "blue") +
  geom_vline(xintercept = end_tst, color = "red") +
  geom_hline(yintercept = h[[3]], color = "darkgreen") +
  labs(x = "", y = "Plotting Statistic",
       title = expression("Hotelling's" ~ T^2)) +
  lims(y = c(0, y_lims[[3]]))

# Save Plots
1:length(pstat) |> 
  walk(\(i) {
    ggsave(filename = paste0("~/git/reports/gruMCUSUM_paper/pstat-", names(pstat)[i], ".png"),
           plot = pstat_plots[[i]], width = 30, height = 10, units = "cm")
  })

### Scatter Plot ------------------------------------------------------------

# Data for Scatter Plots
dat_scatter <- dat_trn_mod

a <- 0.3
wid <- 13
hei <- 8

# Cell 1 vs. Cell 2
dat_scatter |> 
  ggplot(aes(`Cell 1 Flux (LMH)`, `Cell 2 Flux (LMH)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = "~/git/reports/gruMCUSUM_paper/example-cell1-cell2.png",
       width = wid, height = hei, units = "cm")

# Cell 1 vs. Perm Cond 1
dat_scatter |> 
  ggplot(aes(`Cell 1 Flux (LMH)`, `Perm. Cond. 1 (mS/cm)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = "~/git/reports/gruMCUSUM_paper/example-cell1-cond1.png",
       width = wid, height = hei, units = "cm")

# Cell 1 vs. Perm Cond 2
dat_scatter |> 
  ggplot(aes(`Cell 1 Flux (LMH)`, `Perm. Cond. 2 (mS/cm)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = "~/git/reports/gruMCUSUM_paper/example-cell1-cond2.png",
       width = wid, height = hei, units = "cm")

# Cell 2 vs. Perm Cond 1
dat_scatter |> 
  ggplot(aes(`Cell 2 Flux (LMH)`, `Perm. Cond. 1 (mS/cm)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = "~/git/reports/gruMCUSUM_paper/example-cell2-cond1.png",
       width = wid, height = hei, units = "cm")

# Cell 2 vs. Perm Cond 2
dat_scatter |> 
  ggplot(aes(`Cell 2 Flux (LMH)`, `Perm. Cond. 2 (mS/cm)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = "~/git/reports/gruMCUSUM_paper/example-cell2-cond2.png",
       width = wid, height = hei, units = "cm")

# Perm Cond 1 vs. Perm Cond 2
dat_scatter |> 
  ggplot(aes(`Perm. Cond. 1 (mS/cm)`, `Perm. Cond. 2 (mS/cm)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = "~/git/reports/gruMCUSUM_paper/example-cond1-cond2.png",
       width = wid, height = hei, units = "cm")


### Visualize ACF ------------------------------------------------------------

### Testing Data - Raw/Residuals
res_trn_h <-
  c(list(Raw = dat_trn_h), 
    map(pred_trn_h, \(x) as_tibble(x$residuals)))

# ACF Plots
acf_trn_h_plots <-
  1:length(res_trn_h) |> 
  map(\(i) {
    acf_vals <- res_trn_h[[i]] |>
      Acf(type = "correlation", plot = FALSE) #Acf(type = "covariance", plot = FALSE)

    acf_vals$type <- "correlation"

    autoplot(acf_vals) +
      labs(title = paste0(names(res_trn_h)[i])) +
      lims(y = c(-.1, 1))
    
    # ggAcf(res_tst[[i]], type = "correlation", lag.max = 50) +
    #   lims(y = c(0, 1)) +
    #   labs(title = paste0(names(res_tst)[i]))
  })

# Save Plots
1:(length(res_trn_h)) |> # -1 will leave out the second var plot 
  walk(\(i) {
    ggsave(filename = paste0("~/git/reports/gruMCUSUM_paper/acf-tst-", names(res_trn_h)[i], ".png"),
           plot = acf_trn_h_plots[[i]], width = 30, height = 18, units = "cm")
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
# Combine Results from Fit and Prediction
res <- names(pred_tst) |> 
  map(\(x) {
    bind_rows(fit[[x]]$residuals |> as_tibble(),
              pred_trn_h[[x]]$residuals |> as_tibble(),
              pred_tst[[x]]$residuals |> as_tibble()) |> 
      mutate(Date_Time = pull(dat, Date_Time) |> tail(n()))
  }) |> 
  set_names(c("GRU", "VARMA", "Centered Data"))

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
           title = names(res)[i]) +
      lims(y = c(-3, 6))
  })

# Save Plots
1:length(res) |> 
  walk(\(i) {
    ggsave(filename = paste0("~/git/reports/gruMCUSUM_paper/res-", names(res)[i], ".png"),
           plot = res_plots[[i]], width = 30, height = 10, units = "cm")
  })

