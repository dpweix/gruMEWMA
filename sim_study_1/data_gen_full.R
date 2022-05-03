### Imports ###
library("tidyverse")
theme_set(theme_bw())
library("mlmcusum")

# Generate All Data
dat_lin <- gen_dat_lin(n_ic = 1000, n_oc = 1000)
dat_nrl <- gen_dat_nlr(n_ic = 1000, n_oc = 1000)
dat_ltm <- gen_dat_ltm(n_ic = 1000, n_oc = 1000)

# Label Combination of Data and Faults
titles <- 
  expand.grid(c("Linear", "Non-Linear", "Long-Term"),
              c("None", "Fault 1", "Fault 2", "Fault 3")) |> 
    arrange(Var1) |>
    pmap_chr(paste)

# Plot and Save this Combinations
plots <-
  map2(titles, c(dat_lin, dat_nrl, dat_ltm),
       \(x, y) {
         list(x, y)
         
         y |> 
           mutate(index = 1:n()) |> 
           pivot_longer(c("x1", "x2", "x3")) |>
           ggplot(aes(index, value, color = name)) +
           geom_line() +
           geom_vline(xintercept = 1000, color = "red") +
           labs(title = x, color = "", y = "", x = "") +
           theme(legend.position = "top")
           #guides(color = "none")
      
       }) |> 
  set_names(titles)
  
# View All or Select
# plots
plots$`Linear None`
plots$`Linear Fault 1`
plots$`Linear Fault 2`
plots$`Linear Fault 3`

plots$`Non-Linear None`
plots$`Non-Linear Fault 1`
plots$`Non-Linear Fault 2`
plots$`Non-Linear Fault 3`

plots$`Long-Term None`
plots$`Long-Term Fault 1`
plots$`Long-Term Fault 2`
plots$`Long-Term Fault 3`

# Save All Plots
1:length(plots) |> 
  walk(\(n) {
    ggsave(paste0("/home/ubuntu/git/reports/gruMCUSUM_paper/",
                  str_replace_all(titles[[n]], " ", "-"), ".png"),
                  plots[[n]], units = "cm", width = 12, height = 8)
  })
