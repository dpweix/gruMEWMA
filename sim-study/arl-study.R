library("here")
library("tidyverse")
library("reticulate")
library("mlmcusum")
source(here("sim-study", "broken-methods.R"))

# These variables should be passed from sim_study.R, defining them here is
# only for testing purposes.

# Parameters
data_type <- "ltm"
n_sim     <- 3
l         <- 2
arl       <- 40
n_ic_mod  <- 1000
n_ic_h    <- 4000
n_oc      <- 4000

# Simulation
arl_sim <- 1:n_sim |> 
  map(\(i) {
    gen_sim_study_brk(data_type = data_type, n_ic_mod = n_ic_mod, n_ic_h = n_ic_h,
                      n_oc = n_oc, l = l, arl = arl)
  })

# Save results
saveRDS(arl_sim, file = here("results", paste0("arl-sim-", data_type, "-", part, ".rds")))


