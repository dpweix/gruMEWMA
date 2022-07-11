### Set Up --------------------------------------------------------------------
library("here")
library("dplyr")

# Parameters for study
n_cores   <- parallel::detectCores()
data_type <- "nlr"
n_sim     <- 5
l         <- 2
arl       <- 200
n_ic_mod  <- 1000
n_ic_h    <- 4000
n_oc      <- 4000

### ARL Simulation ------------------------------------------------------------

# Simulate arl_vals
1:(n_cores-1) |> 
  purrr::walk(\(i) {
    part <<- i
    rstudioapi::jobRunScript(path = here("sim-study-mewma", "arl-study.R"), importEnv = TRUE)
  })

# Load simulation results and calculate ARLs
arl_val <- 
  1:(n_cores-1) |> 
  purrr::map(\(i) {
    
    arl_sim <- readRDS(here("results", paste0("arl-sim-", data_type, "-", i, ".rds")))
    
    1:n_sim |> 
      purrr::map(\(j) {
        print(arl_sim[[j]]$rl)
      }) |> 
      bind_rows()
    
  }) |> 
  bind_rows() |> 
  group_by(method) |> 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Save aggregated results
saveRDS(arl_val, file = here("results", paste0("arl-sim-", data_type, ".rds")))

### Make latex tables ---------------------------------------------------------
library("kableExtra")

`arl-sim-ltm` |> 
  mutate(across(where(is.numeric), round, 3)) |> 
  kbl(format = "latex", booktabs = TRUE, linesep =  c('', '', '\\addlinespace'))
