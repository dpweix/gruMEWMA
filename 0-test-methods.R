library("tidyverse")
library("reticulate")

# Custom package https://github.com/dpweix/mlewma.git
library("mlmewma")

# Load GRU functions
path_py <- "~/git/mlmewma/inst/python/gru_functions.py"
source_python(path_py)

### Test train_fd with exogenous variables

# data and exogenous variables
df <- gen_dat_lin()[[2]]
df_exog <- tibble(v1 = arima.sim(list(ar = .8), n = 2000),
                  v2 = arima.sim(list(ar = .8), n = 2000))

# train gru and mrf on data set
fit_1 <- train_fd(df, method = "gruMEWMA")
fit_2 <- train_fd(df, method = "mrfMEWMA")
fit_3 <- train_fd(df, method = "gruMEWMA", data_exog = df_exog)
fit_4 <- train_fd(df, method = "mrfMEWMA", data_exog = df_exog)

# visualize fit
df_pstat <-
  tibble(index = 1:length(fit_1$pstat),
         gru_plain = fit_1$pstat,
         mrf_plain = fit_2$pstat,
         gru_exog = fit_3$pstat,
         mrf_exog = fit_4$pstat)

df_pstat |> 
  pivot_longer(-index) |> 
  ggplot(aes(index, value, color = name)) +
  geom_line()

# visualize residuals
df_res <-
  tibble(index = rep(1:nrow(fit_1$residuals), 3),
         gru_plain = as.numeric(fit_1$residuals),
         mrf_plain = as.numeric(fit_2$residuals),
         gru_exog = as.numeric(fit_3$residuals),
         mrf_exog = as.numeric(fit_4$residuals),
         variable = rep(c("x1", "x2", "x3"),
                        each = nrow(fit_1$residuals)) |> as_factor()
         )

df_res |> 
  pivot_longer(-c(index, variable)) |> 
  ggplot(aes(index, value, color = variable)) +
  geom_line() +
  facet_wrap(~ name)

df_res |> 
  transmute(gru_diff = gru_plain - gru_exog,
            mrf_diff = mrf_plain - mrf_exog)

# new data and exogenous variabels
df_2 <- gen_dat_lin()[[2]]
df_2_exog <- tibble(v1 = arima.sim(list(ar = .8), n = 2000),
                    v2 = arima.sim(list(ar = .8), n = 2000))

# apply fault detection to new data
pred_1 <- predict_fd(fit_1, df_2)
pred_2 <- predict_fd(fit_2, df_2)
pred_3 <- predict_fd(fit_3, df_2, df_2_exog)
pred_4 <- predict_fd(fit_4, df_2, df_2_exog)

# visualize pstat of new data
df_pred_pstat <-
  tibble(index = 1:length(pred_1$pstat),
         gru_plain = pred_1$pstat,
         mrf_plain = pred_2$pstat,
         gru_exog = pred_3$pstat,
         mrf_exog = pred_4$pstat)

df_pred_pstat |> 
  pivot_longer(-index) |> 
  ggplot(aes(index, value, color = name)) +
  geom_line()

# visualize residuals of new data
df_pred_res <-
  tibble(index = rep(1:nrow(pred_1$residuals), 3),
         gru_plain = as.numeric(pred_1$residuals),
         mrf_plain = as.numeric(pred_2$residuals),
         gru_exog = as.numeric(pred_3$residuals),
         mrf_exog = as.numeric(pred_4$residuals),
         variable = rep(c("x1", "x2", "x3"),
                        each = nrow(pred_1$residuals)) |> as_factor()
  )

df_pred_res |> 
  pivot_longer(-c(index, variable)) |> 
  ggplot(aes(index, value, color = variable)) +
  geom_line() +
  facet_wrap(~ name)

df_pred_res |> 
  transmute(gru_diff = gru_plain - gru_exog,
            mrf_diff = mrf_plain - mrf_exog)
