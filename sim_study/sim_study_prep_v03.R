### Imports ###
library("here")
library("reticulate")
library("mlmcusum")

use_condaenv("/home/ubuntu/miniconda3/envs/deep-learning-03")
source_python(here("mlmcusum", "inst", "python", "gru_functions.py"))

### Helper Functions (to go in package) ###
get_arl <- function(pstat, h) {
  length(pstat)/sum(pstat > h)
}

### Pilot Simulation Study ###
gen_dat_lin("none")
gen_dat_nlr("none")
gen_dat_ltm("none")

### Initial Test
set.seed(123)

fault <- "f3"

dat <- gen_dat_ltm(fault)[, -1]
l <- 2

trn_id <- 1:1000
tst_id <- (1000-l+1):2000


model_gru <- train_gruMCUSUM(dat[trn_id, ], lags = l, k = 1.1)
preds_gru <- predict_gruMCUSUM(model_gru, dat[tst_id, ])

model_mrf <- train_mrfMCUSUM(dat[trn_id, ], lags = l, k = 1.1)
preds_mrf <- predict_mrfMCUSUM(model_mrf, dat[tst_id, ])

h_gru <- quantile(model_gru$pstat, .99)
h_mrf <- quantile(model_mrf$pstat, .99)

get_arl(model_mrf$pstat, h_mrf)
get_arl(model_gru$pstat, h_gru)

get_arl(preds_mrf$pstat[1:500], h_mrf)
get_arl(preds_gru$pstat[1:500], h_gru)

get_arl(preds_mrf$pstat[501:1000], h_mrf)
get_arl(preds_gru$pstat[501:1000], h_gru)


# 800 x 400
plot(model_gru$residuals[, 1], type = "l", ylim = c(-1, 1), ylab = "Residuals")
lines(model_gru$residuals[, 2], col = "red")
lines(model_gru$residuals[, 3], col = "blue")

plot(preds_gru$residuals[, 1], type = "l", ylim = c(-1, 1))
lines(preds_gru$residuals[, 2], col = "red")
lines(preds_gru$residuals[, 3], col = "blue")


plot(model_gru$pstat)
plot(preds_gru$pstat)

plot(model_mrf$pstat)
plot(preds_mrf$pstat)

plot(c(model_gru$pstat, preds_gru$pstat),
     ylab = "gruMCUSUM", main = paste("Plotting Statistic:", fault),
     ylim = c(0, 20))
abline(v = 1000, col = "blue")
abline(v = 1500, col = "red")
abline(h = h_gru, col = "purple")

plot(c(model_mrf$pstat, preds_mrf$pstat),
     ylab = "mrfMCUSUM", main = paste("Plotting Statistic:", fault),
     ylim = c(0, 60))
abline(v = 1000, col = "blue")
abline(v = 1500, col = "red")
abline(h = h_mrf, col = "purple")

