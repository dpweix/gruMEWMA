library("here")
library("tidyverse")
library("reticulate")
library("mlmewma")
source(here("4-broken-methods.R"))

# These variables should be passed from 6-sim_study.R, defining them here is
# only for testing purposes.

# Parameters
# data_type <- "lin"
# n_sim     <- 3
# l         <- 2
# arl       <- 40
# phi       <- .8
# n_ic_mod  <- 100
# n_ic_h    <- 400
# n_oc      <- 400

# Simulation
arl_sim <- 
  gen_sim_study_brk(data_type = data_type,
                    n_ic_mod = n_ic_mod,
                    n_ic_h = n_ic_h,
                    n_oc = n_oc,
                    phi = phi,
                    l = l,
                    arl = arl)

# Save results (leaving out pstat for disk space reasons)
saveRDS(arl_sim[2:3],
        file = here("results",
                    paste0("arl-sim-", phi,"-", data_type, "-", part, ".rds")))


