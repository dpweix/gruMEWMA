library("tidyverse")
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 15),
             plot.subtitle = element_text(hjust = 0.5, size = 10),
             strip.placement = "outside",
             strip.background = element_blank())
library("patchwork")
library("here")
library("lubridate")
library("mlmewma")
library("forecast")
library("reticulate")
library("kableExtra")
library("zoo")

fig_path <- here("figures/")

### Time series plots ---------------------------------------------------------

# Read data
dat   <- readRDS(here("data", "bw30-navajo.rds"))

# Variables of interest
var_monitored <- c("Cell 1 Flux (LMH)", "Cell 2 Flux (LMH)",
                   "Perm. Cond. 1 (mS/cm)", "Perm. Cond. 2 (mS/cm)")

# Split points
end_trn <- ymd_hms("2020-11-01 00:45:00")
end_tst <- ymd_hms("2020-11-01 08:00:00")

hr_split <- range(filter(dat, between(Date_Time, end_trn, end_tst))$`Hours Run`)

# x-axis breaks
x_breaks <- seq(0, max(dat$`Hours Run`), 3)

# TS plot of variables of interest with patchwork
p_ts1 <- dat |> 
  pivot_longer(c("Cell 1 Flux (LMH)", "Cell 2 Flux (LMH)")) |> 
  ggplot(aes(`Hours Run`, value)) +
  geom_line() + 
  geom_vline(xintercept = hr_split[1], color = "blue") +
  geom_vline(xintercept = hr_split[2], color = "red") +
  facet_wrap(~ name, ncol = 1, scales = "free_x") +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = x_breaks)

p_ts2 <- dat |> 
  pivot_longer(c("Perm. Cond. 1 (mS/cm)", "Perm. Cond. 2 (mS/cm)")) |> 
  ggplot(aes(`Hours Run`, value)) +
  geom_line() + 
  geom_vline(xintercept = hr_split[1], color = "blue") +
  geom_vline(xintercept = hr_split[2], color = "red") +
  facet_wrap(~ name, ncol = 1, scales = "free_x") +
  labs(y = "") +
  scale_x_continuous(breaks = x_breaks)

p_ts1/p_ts2

ggsave(paste0(fig_path, "example-data-patch.png"),
       width = 25, height = 20, units = "cm")

# Time series plots variables of interest
dat |> 
  pivot_longer(all_of(var_monitored)) |> 
  ggplot(aes(`Hours Run`, value)) +
  geom_line() + #geom_point(shape = 1) +
  geom_vline(xintercept = hr_split[1], color = "blue") +
  geom_vline(xintercept = hr_split[2], color = "red") +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  labs(y = "") +
  scale_x_continuous(breaks = x_breaks)

ggsave(paste0(fig_path, "example-data.png"),
       width = 25, height = 20, units = "cm")

# Time series plots cell 1 and perm cond. 1
dat |> 
  pivot_longer(c("Cell 1 Flux (LMH)", "Perm. Cond. 1 (mS/cm)")) |> 
  ggplot(aes(`Hours Run`, value)) +
  geom_line() + #geom_point(shape = 1) +
  geom_vline(xintercept = hr_split[1], color = "blue") +
  geom_vline(xintercept = hr_split[2], color = "red") +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  labs(y = "") + 
  scale_x_continuous(breaks = x_breaks)

ggsave(paste0(fig_path, "example-data-1.png"),
       width = 25, height = 10, units = "cm")

# Time series plot of feed conductivity vs. time
dat |> 
  ggplot(aes(`Hours Run`, `Feed Cond. (mS/cm)`)) +
  geom_line() +
  geom_vline(xintercept = hr_split[1], color = "blue") +
  geom_vline(xintercept = hr_split[2], color = "red") +
  labs(title = "Feed Conductivity (mS/cm)", y = "") +
  scale_x_continuous(breaks = x_breaks)

ggsave(paste0(fig_path, "feed-cond.png"),
       width = 25, height = 5, units = "cm")


# Combine model residuals for all data
fit      <- readRDS(here("data", "fit.rds"))
pred_ctl <- readRDS(here("data", "pred-ctl.rds"))
pred_tst <- readRDS(here("data", "pred-tst.rds"))

res <- names(pred_tst) |> 
  map(\(x) {
    bind_rows(fit[[x]]$residuals |> as_tibble(),
              pred_ctl[[x]]$residuals |> as_tibble(),
              pred_tst[[x]]$residuals |> as_tibble()) |> 
      mutate(`Hours Run` = pull(dat, `Hours Run`) |> tail(n()))
  }) |> 
  set_names(c("GRU", "MRF", "VAR", "Centered Data"))

# Plot residuals from GRU, MRF, and VAR
res_plots <- 1:length(res) |> 
  map(\(i) {
    res[[i]] |> 
      pivot_longer(all_of(var_monitored)) |> 
      ggplot(aes(`Hours Run`, value)) +
      geom_line() +
      geom_vline(xintercept = hr_split[1], color = "blue") +
      geom_vline(xintercept = hr_split[2], color = "red") +
      facet_wrap(~ name, scales = "free") +
      labs(y = "Residuals", title = names(res)[i]) +
      lims(y = c(-3, 4)) +
      scale_x_continuous(breaks = x_breaks)
  })

# Save Plots
1:length(res) |> 
  walk(\(i) {
    ggsave(filename = paste0(fig_path, "res-", names(res)[i], ".png"),
           plot = res_plots[[i]], width = 30, height = 10, units = "cm")
  })

# Read plotting statistics and control limit
pstat <- readRDS(here("data", "pstat.rds"))
h     <- readRDS(here("data", "h.rds"))

# y limits for each graph
y_lims <-
  map2(pstat, c(.99, .99, .99, .99), \(x, q) {
    quantile(x$value, q)
  })

# Plot control statistic
pstat_plots <- 1:3 |> 
  map(\(i) {
    pstat[[i]] |> 
      mutate(smoothed = rollmedian(value, k = 101, align = "left", fill = TRUE)) |> 
      ggplot(aes(`Hours Run`, value)) +
      geom_point(shape = 1, alpha = .1) +
      geom_line(aes(y = smoothed), color = "coral") +
      geom_vline(xintercept = hr_split[1], color = "blue") +
      geom_vline(xintercept = hr_split[2], color = "red") +
      geom_hline(yintercept = h[[i]], color = "darkgreen") +
      labs(y = "Plotting Statistic", title = names(pstat)[i]) +
      scale_x_continuous(breaks = x_breaks) +
      lims(y = c(0, y_lims[[i]]))
  })

pstat_plots[[4]] <- pstat[[4]] |> 
  mutate(smoothed = rollmedian(value, k = 101, align = "left", fill = TRUE)) |> 
  ggplot(aes(`Hours Run`, value)) +
  geom_point(shape = 1, alpha = .1) +
  geom_line(aes(y = smoothed), color = "coral") +
  geom_vline(xintercept = hr_split[1], color = "blue") +
  geom_vline(xintercept = hr_split[2], color = "red") +
  geom_hline(yintercept = h[[3]], color = "darkgreen") +
  labs(y = "Plotting Statistic", title = expression("Hotelling's" ~ T^2)) +
  scale_x_continuous(breaks = x_breaks) +
  lims(y = c(0, y_lims[[3]]))

# Save Plots
1:length(pstat) |> 
  walk(\(i) {
    ggsave(filename = paste0(fig_path, "pstat-", names(pstat)[i], ".png"),
           plot = pstat_plots[[i]],
           width = 30, height = 10, units = "cm")
  })

#### Scatter plots ------------------------------------------------------------

# Data for Scatter Plots
dat_scatter <- readRDS(here("data", "bw30-navajo-trn-mod.rds"))

a   <- 0.3
wid <- 13
hei <- 8

# Cell 1 vs. Cell 2
dat_scatter |> 
  ggplot(aes(`Cell 1 Flux (LMH)`, `Cell 2 Flux (LMH)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = paste0(fig_path, "example-cell1-cell2.png"),
       width = wid, height = hei, units = "cm")

# Cell 1 vs. Perm Cond 1
dat_scatter |> 
  ggplot(aes(`Cell 1 Flux (LMH)`, `Perm. Cond. 1 (mS/cm)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = paste0(fig_path, "example-cell1-cond1.png"),
       width = wid, height = hei, units = "cm")

# Cell 1 vs. Perm Cond 2
dat_scatter |> 
  ggplot(aes(`Cell 1 Flux (LMH)`, `Perm. Cond. 2 (mS/cm)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = paste0(fig_path, "example-cell1-cond2.png"),
       width = wid, height = hei, units = "cm")

# Cell 2 vs. Perm Cond 1
dat_scatter |> 
  ggplot(aes(`Cell 2 Flux (LMH)`, `Perm. Cond. 1 (mS/cm)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = paste0(fig_path, "example-cell2-cond1.png"),
       width = wid, height = hei, units = "cm")

# Cell 2 vs. Perm Cond 2
dat_scatter |> 
  ggplot(aes(`Cell 2 Flux (LMH)`, `Perm. Cond. 2 (mS/cm)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = paste0(fig_path, "example-cell2-cond2.png"),
       width = wid, height = hei, units = "cm")

# Perm Cond 1 vs. Perm Cond 2
dat_scatter |> 
  ggplot(aes(`Perm. Cond. 1 (mS/cm)`, `Perm. Cond. 2 (mS/cm)`)) +
  geom_point(shape = 1, alpha = a) +
  geom_smooth(method = "lm", se = FALSE)

ggsave(filename = paste0(fig_path, "example-cond1-cond2.png"),
       width = wid, height = hei, units = "cm")


### ACF plots -----------------------------------------------------------------

# Data for Scatter Plots
dat_ctl  <- readRDS(here("data", "bw30-navajo-trn-ctl.rds"))
pred_ctl <- readRDS(here("data", "pred-ctl.rds"))
  
# Testing Data - Raw/Residuals
res_ctl <-
  c(list(Raw = dat_ctl), 
    map(pred_ctl, \(x) as_tibble(x$residuals)))

# ACF Plots
acf_ctl_plots <-
  1:length(res_ctl) |> 
  map(\(i) {
    acf_vals <- res_ctl[[i]] |>
      Acf(type = "correlation", plot = FALSE) #Acf(type = "covariance", plot = FALSE)
    
    acf_vals$type <- "correlation"
    
    autoplot(acf_vals) +
      labs(title = paste0(names(res_ctl)[i])) +
      lims(y = c(-.1, 1))
    
    # ggAcf(res_tst[[i]], type = "correlation", lag.max = 50) +
    #   lims(y = c(0, 1)) +
    #   labs(title = paste0(names(res_tst)[i]))
  })

# Save Plots
1:(length(res_ctl)) |> # -1 will leave out the second var plot 
  walk(\(i) {
    ggsave(filename = paste0(fig_path, "acf-ctl-", names(res_ctl)[i], ".png"),
           plot = acf_ctl_plots[[i]], width = 32, height = 20, units = "cm")
  })
