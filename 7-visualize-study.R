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
phi <- 0.8

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
data_label <- 
  c("Linear Stationary:", "Linear Non-stationary:", 
    "Non-linear Stationary:", "Non-linear Non-stationary:") |> 
  set_names(c("lin", "ltl", "nlr", "ltm"))

expand_grid(data_type, phi) |> 
  pmap(\(data_type, phi) {
    # Plot Run Lengths
    df_rl <- 
      1:(n_sim) |> 
      purrr::map_dfr(\(i) {
        
        arl_sim <- readRDS(here("results", paste0("arl-sim-", phi,"-", data_type, "-", i, ".rds")))
        arl_sim$rl
        
      })
    
    rl_histogram <- 
      df_rl |> 
      group_by(method, phi, data) |> 
      mutate(nf_median = median(nf),
             nf_mean = mean(nf)) |> 
      ungroup() |> 
      ggplot(aes(nf)) +
      geom_histogram(bins = 80) +
      geom_vline(aes(xintercept = nf_median), color = "red") +
      geom_vline(aes(xintercept = nf_mean), color = "blue") +
      scale_x_continuous(limits = c(0, 800)) + 
      #scale_y_continuous(limits = c(0, 200)) +
      labs(title = paste0(data_label[[data_type]], " phi = ", phi),
           x = "Run Length", y = "Count") +
      facet_wrap(vars(method), ncol = 4, scales = "free_x")
    
    ggsave(here("figures", paste0("rl-histogram-", phi,"-", data_type, ".png")),
           plot = rl_histogram,
           width = 5000,
           height = 3000,
           units = "px")
  })


