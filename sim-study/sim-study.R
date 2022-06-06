### Set Up --------------------------------------------------------------------
library("here")
library("dplyr")

# Parameters for study
n_cores   <- parallel::detectCores()
data_type <- "lin"
n_sim     <- 1
l         <- 2
arl       <- 40
n_ic_mod  <- 1000
n_ic_h    <- 4000
n_oc      <- 4000

### ARL Simulation ------------------------------------------------------------

# Simulate arl_vals
1:(n_cores-1) |> 
  purrr::walk(\(i) {
    part <<- i
    rstudioapi::jobRunScript(path = here("sim-study", "arl-study.R"), importEnv = TRUE)
  })

# Load simulation results and calculate ARLs
arl_val <- 
  1:(n_cores-1) |> 
  purrr::map(\(i) {
    
    arl_sim <- readRDS(here("results", paste0("arl-sim-", data_type, "-", i, ".rds")))
    
    1:n_sim |> 
      purrr::map(\(j) {
        print(arl_sim[[j]]$dat_rl)
      }) |> 
      bind_rows()
    
  }) |> 
  bind_rows() |> 
  group_by(method) |> 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Save aggregated results
saveRDS(arl_val, file = here("results", paste0("arl-sim-", data_type, ".rds")))
