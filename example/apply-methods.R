library("tidyverse")
theme_set(theme_bw())
library("here")
library("lubridate")
library("mlmcusum")
library("reticulate")
library("kableExtra")


### Load Data -----------------------------------------------------------------
# Load GRU functions
path_py <- "~/git/mlmcusum/inst/python/gru_functions.py"
source_python(path_py)

# Get Data
load(here("example", "bw30_navajo.rda"))

# Variables of Interest
var_monitored <- c("Cell 1 Flux (LMH)", "Cell 2 Flux (LMH)",
                   "Perm. Cond. 1 (mS/cm)", "Perm. Cond. 2 (mS/cm)")

# Time Series Plot
dat |> 
  filter(Date_Time >= ymd_hms("2020-10-31 12:00:00")) |> 
  pivot_longer(all_of(var_monitored)) |> 
  ggplot(aes(Date_Time, value)) +
  geom_line() +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  labs(x = "", y = "")

# ggsave("/home/ubuntu/git/reports/gruMCUSUM_paper/example-data.png",
#        width = 25, height = 20, units = "cm")

# Count by day
dat |> 
  group_by(Day) |> 
  summarise(n = n())

### Apply Methods -------------------------------------------------------------
# For this application we use data from "2020-10-31 12:00:00" to 
# "2020-11-01 12:00:00" as training data. Everything else is testing. Could 
# mimic actual application of training for a day and application for a day.

# Constants
sec_btw_obs <- 3
l <- 4

# Training Data
dat_trn <- dat |> 
  filter(Date_Time >= ymd_hms("2020-10-31 12:00:00"),
         Date_Time <= ymd_hms("2020-11-01 12:00:00")) |> 
  select(all_of(var_monitored)) 

# Testing Data
dat_tst <- dat |> 
  filter(Date_Time >= ymd_hms("2020-11-01 11:59:48")) |> 
  select(all_of(var_monitored))

# Fit Models
fit <- train(dat_trn, method = "gruMCUSUM", lags = l, k = 1.1)

# Predict observations
pred <- predict(fit, dat_tst)

# Combine Results from Fit and Prediction
dat_output <- 
  bind_rows(as_tibble(fit$residuals),
            as_tibble(pred$residuals)) |> 
  mutate(pstat = c(fit$pstat, pred$pstat),
         Date_Time = filter(dat, Date_Time >= ymd_hms("2020-10-31 12:00:00"))$Date_Time[-c(1:4)])

# View Residuals
dat_output |> 
  pivot_longer(all_of(var_monitored)) |> 
  ggplot(aes(Date_Time, value)) +
  geom_line() +
  geom_vline(xintercept = ymd_hms("2020-11-01 11:59:48"), color = "red") +
  facet_wrap(~ name, scales = "free") +
  lims(y = c(-2, 2)) +
  labs(y = "Residuals")

ggsave("/home/ubuntu/git/reports/gruMCUSUM_paper/example-residuals.png",
       width = 20, height = 10, units = "cm")

# View Pstat
dat_output |> 
  ggplot(aes(Date_Time, pstat)) +
  geom_line() +
  geom_vline(xintercept = ymd_hms("2020-11-01 11:59:48"), color = "red") +
  labs(x = "", y = "Plotting Statistic")
  # lims(y = c(0, 2000),
  #      x = c(ymd_hms("2020-10-31 12:00:00"),
  #            ymd_hms("2020-11-01 12:00:00")))

ggsave("/home/ubuntu/git/reports/gruMCUSUM_paper/example-pstat.png",
       width = 20, height = 5, units = "cm")
