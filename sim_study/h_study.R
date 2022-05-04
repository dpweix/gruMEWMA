library("here")
library("tidyverse")
library("reticulate")
library("mlmcusum")
source(here("sim_study", "broken_methods.R"))


# These variables should be passed from sim_study.R, defining them here is
# only for testing purposes.
#data_type <- "lin"
#part <- 1

# Parameters
# n_sim    <- 1
# l        <- 2
# arl      <- 200
# n_ic_trn <- 1000
# n_ic_tst <- 20000

# Simulation
h_sim <- 1:n_sim |> 
  map(\(i) {
    sim_h_brk(data_type = data_type, arl = arl, n_ic_trn = n_ic, n_ic_tst = n_oc, l = l)
  })

# h value table
h_val <-
  h_sim |>
  map(\(i) i$h_vals) |> 
  bind_rows() |> 
  group_by(Data, method) |> 
  summarise(across(where(is.numeric), mean))

# Save results
saveRDS(h_val, file = here("results", paste0("h_sim_", data_type, "_", part, ".rds")))
