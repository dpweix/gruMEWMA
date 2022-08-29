### Set Up --------------------------------------------------------------------
library("here")
library("dplyr")

# Parameters for study
n_cores   <- parallel::detectCores()
data_type <- "ltm" #lin, ltl, nlr, ltm
n_sim     <- 3
l         <- 3
arl       <- 200
n_ic_mod  <- 10000
n_ic_h    <- 10000
n_oc      <- 20000

# Seed
#set.seed(1) # lin, ltl, 
#set.seed(2) # nlr, ltm

### ARL Simulation ------------------------------------------------------------

# 1 - 15, 16 - 30, 31 - 45, 46-60
start <- 1
stop <- 15

# Simulate arl_vals
start:(stop)  |> 
  purrr::walk(\(i) {
    part <<- i
    rstudioapi::jobRunScript(path = here("sim-study-mewma", "arl-study.R"), importEnv = TRUE)
  })

# Load simulation results and calculate ARLs
arl_val <- 
  1:(60) |> 
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
library("tidyverse")

### All ARL Tables

# Tibble of Methods
methods <-`arl-sim-lin`$method |> 
  str_split("-") |> 
  map_dfr(\(x) {
    tibble(Method = x[1], `$r$` = x[2])
  })

# Tibble of ARLs
arls <- list(`arl-sim-lin`, `arl-sim-ltl`, `arl-sim-nlr`, `arl-sim-ltm`) |> 
  map(\(x) {
    transmute(x, across(where(is.numeric), round, 2)) |> 
      set_names(c("$ARL_0$", "$ARL_{F1}$", "$ARL_{F2}$", "$ARL_{F3}$"))
  }) |> 
  bind_cols(.name_repair = "minimal")

### Combine into LaTeX table ###
# Linear Data
bind_cols(methods, arls[, 1:8], .name_repair = "minimal") |> 
  kbl(format = "latex", booktabs = TRUE, linesep =  c('', '', '', '\\addlinespace'),
      escape = FALSE) |> 
  add_header_above(c(" " = 2, "Linear Short-Term" = 4, "Linear Long-Term" = 4))

# Non-Linear Data
bind_cols(methods, arls[, 9:16], .name_repair = "minimal") |> 
  kbl(format = "latex", booktabs = TRUE, linesep =  c('', '', '', '\\addlinespace'),
      escape = FALSE) |> 
  add_header_above(c(" " = 2, "Non-Linear Short-Term" = 4, "Non-Linear Long-Term" = 4))
