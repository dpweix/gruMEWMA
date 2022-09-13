### Imports ###
library("here")
library("tidyverse")
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 15))
library("reticulate")
library("mlmcusum")

### Make Data -----------------------------------------------------------------

dat <- gen_dat_lin()

### Apply Methods -------------------------------------------------------------

fit_mrf <- train(dat$f1[1:1000, ], method = "mrfMEWMA", lags = 2)
pred_mrf <- predict_ml(model = fit_mrf, dat$f1[1001:2000, ])

pred_mrf

plot(fit_mrf$pstat)
plot(pred_mrf$pstat)

fit_mrf$model
fit_mrf$center_scale
fit_mrf$mean_sd
fit_mrf$constants
fit_mrf$residuals

str(fit_mrf)
list(names(fit_mrf),names(fit_mrf$model))
