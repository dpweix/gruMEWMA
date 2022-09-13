library("tidyverse")
theme_set(theme_bw())
library("here")
library("readxl")
library("lubridate")

### Clean Raw Data ###
dat_raw <- 
  read_excel(here("example", "BW30_Navajo_DataAnalysis_Exp1(Take 3)_good.xlsx"),
             sheet = "Exp1", range = "A2:AF68373")

dat <-
  dat_raw |> 
  mutate(Day = as_date(Day),
         Time = hms::as_hms(Time),
         Date_Time = ymd_hms(paste(Day, Time)),
         Iteration = as.integer(Iteration),
         `Batch Exchange Count` = as.integer(`Batch Exchange Count`)) |> 
  mutate(across(where(is.character), as_factor))

save(dat, file = here("example", "bw30_navajo.rda"))

### Read Cleaned Data ###
load(here("example", "bw30_navajo.rda"))

range(dat$Date_Time)

dat[-c(1:20), ] |> 
  pivot_longer(c(`Cell 1 Flux (LMH)`, `Cell 2 Flux (LMH)`,
                 `Perm. Cond. 1 (mS/cm)`, `Perm. Cond. 2 (mS/cm)`)) |> 
  ggplot(aes(Date_Time, value)) +
  geom_line() +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  labs(x = "", y = "")

