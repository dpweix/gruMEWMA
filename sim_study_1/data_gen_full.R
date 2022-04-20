### Imports ###
library("tidyverse")
theme_set(theme_bw())
library("mlmcusum")

# Generate All Data
dat_lin <- gen_dat_lin()
dat_nrl <- gen_dat_nlr()
dat_ltm <- gen_dat_ltm()

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
           labs(title = x)
      
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
