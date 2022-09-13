### Set Up --------------------------------------------------------------------
library("here")
library("tidyverse")

# Parameters for study
data_type <- "lin" #lin, ltl, nlr, ltm
n_sim     <- 1000
l         <- 2
arl       <- 200
n_ic_mod  <- 10000
n_ic_h    <- 10000
n_oc      <- 20000

# Parameters for application
n_cores  <- parallel::detectCores()
max_jobs <- n_cores
batches  <- ceiling(n_sim/max_jobs)

### ARL Simulation ------------------------------------------------------------

# Results folder for chosen data generation method MUST BE EMPTY!!!

# Organize Batches for Job Submission
job_tib <-
  tibble(sim = 1:n_sim,
         batch = rep(1:batches, each = max_jobs) |> head(n_sim))

# Loop to submit batches of jobs. Only submits new batch once every
# job in the current batch is completed.
1:batches |>
  purrr::walk(\(b) {
    sims <- filter(job_tib, batch == b)$sim 
    
    sims |> 
      purrr::walk(\(i) {
        part <<- i
        rstudioapi::jobRunScript(path = here("arl-study.R"), importEnv = TRUE)
      })
    
    while(!all(file.exists(here("results", paste0("arl-sim-", data_type, "-", sims, ".rds"))))) {
      print(paste("Processing Batch:", b))
      Sys.sleep(60)
    } 

  })

# Load simulation results and calculate ARLs
arl_val <- 
  1:(n_sim) |> 
  purrr::map_dfr(\(i) {
    
    arl_sim <- readRDS(here("results", paste0("arl-sim-", data_type, "-", i, ".rds")))
    arl_sim$rl
    
  }) |> 
  group_by(method) |> 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Save aggregated results
saveRDS(arl_val, file = here("results", paste0("arl-sim-", data_type, ".rds")))

### Make latex tables ---------------------------------------------------------
library("kableExtra")
library("here")
library("tidyverse")

# All ARL Tables
arl_lin <- readRDS(here("results", "arl-sim-lin.rds"))
arl_ltl <- readRDS(here("results", "arl-sim-ltl.rds"))
arl_nlr <- readRDS(here("results", "arl-sim-nlr.rds"))
arl_ltm <- readRDS(here("results", "arl-sim-ltm.rds"))

# Tibble of Methods
methods <- arl_lin$method |> 
  str_split("-") |> 
  map_dfr(\(x) {
    tibble(Method = x[1], `$r$` = x[2])
  })

# Tibble of ARLs
arls <- list(arl_lin, arl_ltl, arl_nlr, arl_ltm) |> 
  map(\(x) {
    transmute(x, across(where(is.numeric), round, 2)) |> 
      set_names(c("$ARL_0$", "$ARL_{F1}$", "$ARL_{F2}$", "$ARL_{F3}$"))
  }) |> 
  bind_cols(.name_repair = "minimal")

# LaTeX Table: Linear Data
bind_cols(methods, arls[, 1:8], .name_repair = "minimal") |> 
  kbl(format = "latex", booktabs = TRUE, linesep =  c('', '', '', '\\addlinespace'),
      escape = FALSE) |> 
  add_header_above(c(" " = 2, "Linear Short-Term" = 4, "Linear Long-Term" = 4))

# LaTeX Table: Non-Linear Data
bind_cols(methods, arls[, 9:16], .name_repair = "minimal") |> 
  kbl(format = "latex", booktabs = TRUE, linesep =  c('', '', '', '\\addlinespace'),
      escape = FALSE) |> 
  add_header_above(c(" " = 2, "Non-Linear Short-Term" = 4, "Non-Linear Long-Term" = 4))
