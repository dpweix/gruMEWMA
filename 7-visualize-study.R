library("here")
library("tidyverse")
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 15))
library("reticulate")
library("mlmewma")
             

# load python
path_py <- "~/git/mlmewma/inst/python/gru_functions.py"
source_python(path_py)


### Data Generation Figures --------------------------------------------------
# Where to save figures
fig_path <- here("figures/")
phi <- 0

# Generate data
set.seed(123)
dat_lin <- gen_dat_lin(n_ic = 1000, n_oc = 1000, phi = phi)
dat_ltl <- gen_dat_ltl(n_ic = 1000, n_oc = 1000, phi = phi)
dat_nlr <- gen_dat_nlr(n_ic = 1000, n_oc = 1000, phi = phi)
dat_ltm <- gen_dat_ltm(n_ic = 1000, n_oc = 1000, phi = phi)

# Generate all labels for plots
titles <- 
  expand.grid(c("Linear Stationary:"    , "Linear Non-stationary:",
                "Non-linear Stationary:", "Non-linear Non-stationary:"),
              c("No Fault", "Fault 1", "Fault 2", "Fault 3")) |> 
  arrange(Var1) |>
  pmap_chr(paste)

# Create all plots
plots <-
  map2(titles, c(dat_lin, dat_ltl, dat_nlr, dat_ltm),
       \(x, y) {
         #list(x, y)
         
         y |> 
           mutate(index = 1:n()) |> 
           pivot_longer(c("x1", "x2", "x3")) |>
           ggplot(aes(index, value, color = name)) +
           geom_line() +
           geom_vline(xintercept = 1000, color = "red") +
           labs(title = x, color = "", y = "", x = "") +
           theme(legend.position = "top") +
           guides(color = "none")
         
       }) |> 
  set_names(titles)

# Create plot with legend
x <- titles[[1]]

dat_lin$none |> 
  mutate(index = 1:n()) |> 
  pivot_longer(c("x1", "x2", "x3")) |>
  ggplot(aes(index, value, color = name)) +
  geom_line() +
  geom_vline(xintercept = 1000, color = "red") +
  labs(title = x, color = "", y = "", x = "") +
  theme(legend.position = "top") +  
  scale_color_discrete(labels=c("x1"=expression(y[1]),
                                "x2"=expression(y[2]),
                                "x3"=expression(y[3]))) +
  scale_fill_discrete(guide="none")+
  guides(color=guide_legend(override.aes=list(fill=c("#F8766D","#00BFC4", "blue"))))


# View each plot (no legend)
plots$`Linear Stationary: No Fault`
plots$`Linear Stationary: Fault 1`
plots$`Linear Stationary: Fault 2`
plots$`Linear Stationary: Fault 3`

plots$`Linear Non-stationary: No Fault`
plots$`Linear Non-stationary: Fault 1`
plots$`Linear Non-stationary: Fault 2`
plots$`Linear Non-stationary: Fault 3`

plots$`Non-linear Stationary: No Fault`
plots$`Non-linear Stationary: Fault 1`
plots$`Non-linear Stationary: Fault 2`
plots$`Non-linear Stationary: Fault 3`

plots$`Non-linear Non-stationary: No Fault`
plots$`Non-linear Non-stationary: Fault 1`
plots$`Non-linear Non-stationary: Fault 2`
plots$`Non-linear Non-stationary: Fault 3`

# Save all plots
1:length(plots) |> 
  walk(\(n) {
    ggsave(paste0(fig_path,
                  str_replace_all(titles[[n]], " ", "-") |> str_remove(":"),
                  "-",
                  phi,
                  ".png"),
           plots[[n]], units = "cm", width = 12, height = 8)
  })

### ARL Results Figures --------------------------------------------------
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 15),
             plot.subtitle = element_text(hjust = 0.5, size = 10),
             strip.text = element_text(size = 13),
             strip.placement = "outside",
             
             strip.background = element_blank())
# Parameters for study
data_type <- c("lin", "ltl", "nlr", "ltm")
phi       <- c(0, 0.4, 0.8)
fault     <- c("nf", "f1", "f2", "f3")
n_sim     <- 1000

data_label <- 
  c("Linear Stationary", "Linear Non-stationary", 
    "Non-linear Stationary", "Non-linear Non-stationary") |> 
  set_names(data_type)

fault_label <- 
  c("Steady State Run Lengths", "Fault 1 Run Lengths",
    "Fault 2 Run Lengths", "Fault 3 Run Lengths") |> 
  set_names(fault)

hist_label <- 
  expand_grid(lambda = c(.2, .4, .6, .8),
              method = c("gruMEWMA", "mrfMEWMA", "varMEWMA", "MEWMA")) |> 
  pmap_chr(\(lambda, method) {
    paste0(method, ", \u03BB = ", lambda)
  }) |> 
  c("Hotelling's T") |> 
  set_names(c("gruMEWMA-0.2", "mrfMEWMA-0.2", "varMEWMA-0.2", "MEWMA-0.2",
              "gruMEWMA-0.4", "mrfMEWMA-0.4", "varMEWMA-0.4", "MEWMA-0.4",
              "gruMEWMA-0.6", "mrfMEWMA-0.6", "varMEWMA-0.6", "MEWMA-0.6",
              "gruMEWMA-0.8", "mrfMEWMA-0.8", "varMEWMA-0.8", "MEWMA-0.8",
              "htsquare-0"))

# Generate all RL histograms
expand_grid(data_type, phi, fault) |> 
  pwalk(\(data_type, phi, fault) {
    # Plot Run Lengths
    df_rl <- 
      1:(n_sim) |> 
      purrr::map_dfr(\(i) {
        
        arl_sim <- readRDS(here("results", paste0("arl-sim-", phi,"-", data_type, "-", i, ".rds")))
        arl_sim$rl
        
      })
    
    if(fault == "nf") {
      x_lim <- c(0, 800)
    } else {
      x_lim <- c(0, 100)
    }
    
    rl_histogram <- 
      df_rl |> 
      mutate(rl = df_rl[[fault]]) |> 
      group_by(method, phi, data) |> 
      mutate(rl_median = median(rl),
             rl_mean = mean(rl)) |> 
      ungroup() |> 
      ggplot(aes(rl)) +
      geom_histogram(bins = 70) +
      geom_vline(aes(xintercept = rl_median), color = "red") +
      geom_vline(aes(xintercept = rl_mean), color = "blue") +
      scale_x_continuous(limits = x_lim) + 
      #scale_y_continuous(limits = c(0, 200)) +
      labs(title = paste0(data_label[[data_type]], ", ",
                          fault_label[[fault]],
                          ": \u03C6 = ", phi),
           x = "Run Length", y = "Count") +
      facet_wrap(vars(method), ncol = 4, scales = "free_x",
                 labeller = labeller(method = hist_label))
    
    ggsave(here("figures", paste0("rl-histogram-", phi, "-", data_type, "-", fault, ".png")),
           plot = rl_histogram,
           width = 5000,
           height = 3000,
           units = "px")
  })

# Generate RL histograms in main paper
selected_methods <- c("MEWMA-0.2", "MEWMA-0.4", "MEWMA-0.6", "MEWMA-0.8", 
                      "htsquare-0")
hist_label_2 <- 
  hist_label |> 
  str_subset("gru|mrf|var", negate = TRUE) |> 
  set_names(selected_methods)

a <- map(data_type, \(data_type) {
  # Plot Run Lengths
  df_rl <- 
    1:(n_sim) |> 
    purrr::map_dfr(\(i) {
      
      arl_sim <- readRDS(here("results", paste0("arl-sim-0.8-", data_type, "-", i, ".rds")))
      arl_sim$rl
      
    }) |>
    filter(str_detect(method, paste(selected_methods, collapse = "|")))
  
  x_lim <- c(0, 800)
  y_lim <- c(0, 150)

  rl_histogram <-
    df_rl |>
    mutate(rl = df_rl[["nf"]]) |>
    group_by(method, data) |>
    mutate(rl_median = median(rl),
           rl_mean = mean(rl)) |>
    ungroup() |>
    ggplot(aes(rl)) +
    geom_histogram(bins = 70) +
    geom_vline(aes(xintercept = rl_median), color = "red") +
    geom_vline(aes(xintercept = rl_mean), color = "blue") +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = y_lim) +
    labs(title = paste0(data_label[[data_type]], ", ",
                        fault_label[["nf"]],
                        ": \u03C6 = 0.8"),
         x = "Run Length", y = "Count") +
    facet_wrap(vars(method), ncol = 5, scales = "free",
               labeller = labeller(method = hist_label_2))

  ggsave(here("figures", paste0("rl-histogram-", data_type, ".png")),
         plot = rl_histogram,
         width = 5000,
         height = 1000,
         units = "px")
})

df_rl <- map_dfr(data_type, \(data_type) {
  # Plot Run Lengths
  1:(n_sim) |> 
    purrr::map_dfr(\(i) {
      
      arl_sim <- readRDS(here("results", paste0("arl-sim-0.8-", data_type, "-", i, ".rds")))
      arl_sim$rl
      
    }) |>
    filter(str_detect(method, paste(selected_methods, collapse = "|")))
})

x_lim <- c(0, 800)
y_lim <- c(0, 150)

rl_histogram <-
  df_rl |>
  mutate(rl = df_rl[["nf"]]) |>
  group_by(method, data) |>
  mutate(rl_median = median(rl),
         rl_mean = mean(rl)) |>
  ungroup() |>
  ggplot(aes(rl)) +
  geom_histogram(bins = 70) +
  geom_vline(aes(xintercept = rl_median), color = "red") +
  geom_vline(aes(xintercept = rl_mean), color = "blue") +
  scale_x_continuous(limits = x_lim) +
  scale_y_continuous(limits = y_lim) +
  labs(title = paste0("Steady State Run Lengths: \u03C6 = 0.8"),
       x = "Run Length", y = "Count") +
  facet_grid(data ~ method, switch = "y",
             labeller = labeller(method = hist_label_2, data = data_label))

ggsave(here("figures", paste0("rl-histogram.png")),
       plot = rl_histogram,
       width = 5000,
       height = 2900,
       units = "px")


