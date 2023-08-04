### Set Up --------------------------------------------------------------------
library("here")
library("tidyverse")

# Parameters for study
data_type <- "lin" #lin, ltl, nlr, ltm
n_sim     <- 10  # 1000
l         <- 2     # 2
arl       <- 200   # 200
phi       <- 0.8     # 0, .4, .8
n_ic_mod  <- 1000 # 10000
n_ic_h    <- 1000 # 10000
n_oc      <- 2000 # 20000

# Parameters for application
n_cores  <- parallel::detectCores()
max_jobs <- 5
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
        rstudioapi::jobRunScript(path = here("5-arl-study.R"), importEnv = TRUE)
      })
    
    while(!all(file.exists(here("results", paste0("arl-sim-", phi,"-", data_type, "-", sims, ".rds"))))) {
      print(paste("Processing Batch:", b))
      Sys.sleep(60)
    } 
  })

# Load simulation results and calculate ARLs
arl_val <- 
  1:(n_sim) |> 
  purrr::map_dfr(\(i) {
    
    arl_sim <- readRDS(here("results", paste0("arl-sim-", phi,"-", data_type, "-", i, ".rds")))
    arl_sim$rl
    
  }) |> 
  group_by(method) |> 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Save aggregated results
saveRDS(arl_val, file = here("results", paste0("arl-sim-", phi,"-", data_type, ".rds")))

### Make latex tables ---------------------------------------------------------
library("kableExtra")
library("here")
library("tidyverse")

# All ARL Tables
arl_0_lin <- readRDS(here("results", "arl-sim-0-lin.rds"))
arl_0_ltl <- readRDS(here("results", "arl-sim-0-ltl.rds"))
arl_0_nlr <- readRDS(here("results", "arl-sim-0-nlr.rds"))
arl_0_ltm <- readRDS(here("results", "arl-sim-0-ltm.rds"))

arl_0.4_lin <- readRDS(here("results", "arl-sim-0.4-lin.rds"))
arl_0.4_ltl <- readRDS(here("results", "arl-sim-0.4-ltl.rds"))
arl_0.4_nlr <- readRDS(here("results", "arl-sim-0.4-nlr.rds"))
arl_0.4_ltm <- readRDS(here("results", "arl-sim-0.4-ltm.rds"))

arl_0.8_lin <- readRDS(here("results", "arl-sim-0.8-lin.rds"))
arl_0.8_ltl <- readRDS(here("results", "arl-sim-0.8-ltl.rds"))
arl_0.8_nlr <- readRDS(here("results", "arl-sim-0.8-nlr.rds"))
arl_0.8_ltm <- readRDS(here("results", "arl-sim-0.8-ltm.rds"))

# Tibble of Methods
methods <- expand_grid(`$\\phi$` = c(0, .4, .8),
                       data = c("Linear Stationary", "Linear Non-stationary", 
                                "Non-linear Stationary", "Non-linear Non-stationary"),
                       arl_0_lin$method |> 
                         str_split("-") |> 
                         map_dfr(\(x) {
                           tibble(Method = x[1], `$r$` = x[2])
                         }))

df_arl_basic <- bind_rows(list(arl_0_lin, arl_0_ltl, arl_0_nlr, arl_0_ltm,
                               arl_0.4_lin, arl_0.4_ltl, arl_0.4_nlr, arl_0.4_ltm,
                               arl_0.8_lin, arl_0.8_ltl, arl_0.8_nlr, arl_0.8_ltm))

df_arl <- 
  bind_cols(methods,
            df_arl_basic |> select(contains("f"))) |> 
  mutate(across(where(is.numeric), \(x) {round(x, 2)})) |> 
  set_names(c("$\\phi$", "Data Type", "Method", "$\\lambda$",
              "$ARL_0$", "$ARL_{F1}$", "$ARL_{F2}$", "$ARL_{F3}$"))


# Prints ARL for Stationary and Non-stationary 
print_arl <- function(phi, linear = FALSE) {
  if(!linear) {
    df <- df_arl |> filter(`$\\phi$` == phi) |> filter(str_detect(`Data Type`, "Non-linear"))
    
    bind_cols(df[1:17, c(1, 3:8)], df[18:34, 5:8], .name_repair = "minimal") |> 
      kbl(format = "latex", booktabs = TRUE, linesep =  c('', '', '', '\\addlinespace'),
          escape = FALSE) |> 
      add_header_above(c(" " = 3, "Non-linear Stationary" = 4, "Non-linear Non-stationary" = 4)) |> 
      row_spec(c(1:4, 9:12, 17), background = "lightgray")
    
  } else {
    df <- df_arl |> filter(`$\\phi$` == phi) |> filter(!str_detect(`Data Type`, "Non-linear"))
    
    bind_cols(df[1:17, c(1, 3:8)], df[18:34, 5:8], .name_repair = "minimal") |> 
      kbl(format = "latex", booktabs = TRUE, linesep =  c('', '', '', '\\addlinespace'),
          escape = FALSE) |> 
      add_header_above(c(" " = 3, "Linear Stationary" = 4, "Linear Non-stationary" = 4)) |> 
      row_spec(c(1:4, 9:12, 17), background = "lightgray")
  }
}

print_arl(phi = 0, linear = TRUE)
print_arl(phi = 0, linear = FALSE)

print_arl(phi = 0.4, linear = TRUE)
print_arl(phi = 0.4, linear = FALSE)

print_arl(phi = 0.8, linear = TRUE)
print_arl(phi = 0.8, linear = FALSE)