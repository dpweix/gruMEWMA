library("here")
library("tidyverse")
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 15))
library("reticulate")
library("mlmewma")

# load python
path_py <- "~/git/mlmewma/inst/python/gru_functions.py"
source_python(path_py)

# Where to save figures
fig_path <- here("figures/")
phi <- 0

# Generate data
dat_lin <- gen_dat_lin(n_ic = 1000, n_oc = 1000, phi = phi)
dat_ltl <- gen_dat_ltl(n_ic = 1000, n_oc = 1000, phi = phi)
dat_nlr <- gen_dat_nlr(n_ic = 1000, n_oc = 1000, phi = phi)
dat_ltm <- gen_dat_ltm(n_ic = 1000, n_oc = 1000, phi = phi)

# Generate all labels for plots
titles <- 
  expand.grid(c("Linear Stationary:"    , "Linear Nonstationary:",
                "Non-Linear Stationary:", "Non-Linear Nonstationary:"),
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

plots$`Linear Nonstationary: No Fault`
plots$`Linear Nonstationary: Fault 1`
plots$`Linear Nonstationary: Fault 2`
plots$`Linear Nonstationary: Fault 3`

plots$`Non-Linear Stationary: No Fault`
plots$`Non-Linear Stationary: Fault 1`
plots$`Non-Linear Stationary: Fault 2`
plots$`Non-Linear Stationary: Fault 3`

plots$`Non-Linear Nonstationary: No Fault`
plots$`Non-Linear Nonstationary: Fault 1`
plots$`Non-Linear Nonstationary: Fault 2`
plots$`Non-Linear Nonstationary: Fault 3`

# Save all plots
1:length(plots) |> 
  walk(\(n) {
    ggsave(paste0(fig_path,
                  str_replace_all(titles[[n]], " ", "-") |> str_remove(":"),
                  ".png"),
           plots[[n]], units = "cm", width = 12, height = 8)
  })
