### Set Up --------------------------------------------------------------------
library("here")
library("dplyr")

# Parameters for study
n_cores   <- parallel::detectCores()
data_type <- "lin"
n_sim     <- 1
l         <- 2
arl       <- 200
n_ic      <- 1000
n_oc      <- 20000

### h Simulation --------------------------------------------------------------

# Simulate h_vals
1:(n_cores-1) |> 
  purrr::walk(\(i) {
    part <<- i
    rstudioapi::jobRunScript(path = here("sim_study", "h_study.R"), importEnv = TRUE)
  })

# Load h_vals
h_val <- 
  1:(n_cores-1) |> 
  purrr::map(\(i) {
    readRDS(here("results", paste0("h_sim_", data_type, "_", i, ".rds")))
  }) |> 
  bind_rows() |> 
  group_by(Data, method) |> 
  summarise(across(where(is.numeric), mean))

# Save aggregated results
saveRDS(h_val, file = here("results", paste0("h_sim_", data_type, ".rds")))

### ARL Simulation ------------------------------------------------------------

# Load simulated h_vals
h_val <- readRDS(here("results", paste0("h_sim_", data_type, ".rds")))

# Simulate arl_vals
1:(n_cores-1) |> 
  purrr::walk(\(i) {
    part <<- i
    rstudioapi::jobRunScript(path = here("sim_study", "arl_study.R"), importEnv = TRUE)
  })

# Load simulation results and calculate ARLs
arl_val <- 
  1:(n_cores-1) |> 
  purrr::map(\(i) {
    
    arl_sim <- readRDS(here("results", paste0("arl_sim_", data_type, "_", i, ".rds")))
    
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
saveRDS(arl_val, file = here("results", paste0("arl_sim_", data_type, ".rds")))
