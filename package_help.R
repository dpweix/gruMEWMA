#install.packages("devtools")
#install.packages("roxygen2")

#devtools::create("mlmcusum")

devtools::document("~/git/mlmcusum")
devtools::install("~/git/mlmcusum")

library("mlmcusum")

?create_X

dat <- gen_dat_lin()

train_X <- create_X(dat[[1]][1:1000, ], lags = 2)
train_Y <- create_Y(dat[[1]][1:1000, ], lags = 2)

test_X <- create_X(dat[[1]][-c(1:1000), ], lags = 2)
test_Y <- create_Y(dat[[1]][-c(1:1000), ], lags = 2)

fit <- build_mrf(train_X, train_Y, n_tree = 5, m_feature = floor(sqrt(ncol(train_X))),
                 min_leaf = 10)

fit$preds |> 
  colMeans()

fit$constants

preds <- predict_mrf(fit, train_X)

preds |> 
  colMeans()
