library("tidyverse")
library("here")
library("readxl")
library("lubridate")

### Read excel file -----------------------------------------------------------
dat_raw <- 
  read_excel(here("data", "bw30-navajo-raw-data.xlsx"),
             sheet = "Exp1",
             range = "A2:AF68373")

# Format and save data
dat <-
  dat_raw[-c(1:20), ] |> 
  mutate(Day = as_date(Day),
         Time = hms::as_hms(Time),
         Date_Time = ymd_hms(paste(Day, Time)),
         Iteration = as.integer(Iteration),
         `Batch Exchange Count` = as.integer(`Batch Exchange Count`),
         ...6 = NULL,
         ...8 = NULL) |> 
  mutate(across(where(is.character), as_factor))

saveRDS(dat, file = here("data", "bw30-navajo.rds"))
write_csv(dat, file = here("data", "bw30-navajo.csv"))


### Split data for application ------------------------------------------------
dat <- readRDS(here("data", "bw30-navajo.rds"))

# Variables of interest
var_monitored <- c("Cell 1 Flux (LMH)", "Cell 2 Flux (LMH)",
                   "Perm. Cond. 1 (mS/cm)", "Perm. Cond. 2 (mS/cm)")

# Split points
end_trn <- ymd_hms("2020-11-01 00:45:00")
end_tst <- ymd_hms("2020-11-01 08:00:00")

# Data for model training, estimating h, and testing method
dat_mod <- dat |> filter(Date_Time <= end_trn) |> select(all_of(var_monitored)) 
dat_ctl <- dat |> filter(Date_Time >= end_trn,
                         Date_Time <= end_tst) |> select(all_of(var_monitored)) 
dat_tst <- dat |> filter(Date_Time >= end_tst) |> select(all_of(var_monitored))


saveRDS(dat_mod, file = here("data", "bw30-navajo-trn-mod.rds"))
saveRDS(dat_ctl, file = here("data", "bw30-navajo-trn-ctl.rds"))
saveRDS(dat_tst, file = here("data", "bw30-navajo-tst.rds"))

# Size of data sets
dat_mod |> nrow()
dat_ctl |> nrow()
dat_tst |> nrow()
