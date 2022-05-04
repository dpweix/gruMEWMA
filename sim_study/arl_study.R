library("here")
library("tidyverse")
library("reticulate")
library("mlmcusum")
source(here("sim_study", "broken_methods.R"))

# These variables should be passed from sim_study.R, defining them here is
# only for testing purposes.
# data_type <- "lin"
# part <- 1
# h_val <- readRDS(here("results", paste0("h_sim_", data_type, ".rds")))

# Parameters
# n_sim <- 1
# l     <- 2
# arl   <- 200
# n_ic  <- 1000
# n_oc  <- 20000

# Simulation
arl_sim <- 1:n_sim |> 
  map(\(i) {
    gen_sim_study_brk(data_type = data_type, n_ic = n_ic, n_oc = n_oc, l = l,
                      h_vals = pull(h_val, h))
  })

# Save results
saveRDS(arl_sim, file = here("results", paste0("arl_sim_", data_type, "_", part, ".rds")))
