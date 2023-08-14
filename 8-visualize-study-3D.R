library("tidyverse")
theme_set(cowplot::theme_cowplot())
theme_update(plot.title = element_text(hjust = 0.5, size = 15),
             plot.subtitle = element_text(hjust = 0.5, size = 10),
             strip.placement = "outside",
             strip.background = element_blank())
library("plotly")
library("reticulate")

library("mlmewma")
path_py <- "~/git/mlmewma/inst/python/gru_functions.py"
source_python(path_py)

# devtools::document("~/git/mlmewma")
# devtools::install("~/git/mlmewma")

# Generate and format data
set.seed(123)

n0 <- 1000

format_df <- function(gen_dat_output) {
  gen_dat_output |> 
    map(\(x) {
      x |> 
        mutate(index = 1:n(),
               state = rep(c("IC", "OC"), each = n0),
               .before = x1)
    })
}

df1 <- gen_dat_lin(n_ic = n0, n_oc = n0, phi = 0.8) |> format_df()
df2 <- gen_dat_nlr(n_ic = n0, n_oc = n0, phi = 0.8) |> format_df()
df3 <- gen_dat_ltl(n_ic = n0, n_oc = n0, phi = 0.8) |> format_df()
df4 <- gen_dat_ltm(n_ic = n0, n_oc = n0, phi = 0.8) |> format_df()

df5 <- gen_dat_lin(n_ic = n0, n_oc = n0, phi = 0.4) |> format_df()
df6 <- gen_dat_nlr(n_ic = n0, n_oc = n0, phi = 0.4) |> format_df()
df7 <- gen_dat_ltl(n_ic = n0, n_oc = n0, phi = 0.4) |> format_df()
df8 <- gen_dat_ltm(n_ic = n0, n_oc = n0, phi = 0.4) |> format_df()

df9  <- gen_dat_lin(n_ic = n0, n_oc = n0, phi = 0) |> format_df()
df10 <- gen_dat_nlr(n_ic = n0, n_oc = n0, phi = 0) |> format_df()
df11 <- gen_dat_ltl(n_ic = n0, n_oc = n0, phi = 0) |> format_df()
df12 <- gen_dat_ltm(n_ic = n0, n_oc = n0, phi = 0) |> format_df()

df_list <- 
  list(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12) |> 
  set_names("Linear Stationary, 0.8",
            "Non-linear Stationary, 0.8",
            "Linear Non-stationary, 0.8",
            "Non-linear Non-stationary, 0.8",
            "Linear Stationary, 0.4",
            "Non-linear Stationary, 0.4",
            "Linear Non-stationary, 0.4",
            "Non-linear Non-stationary, 0.4",
            "Linear Stationary, 0",
            "Non-linear Stationary, 0",
            "Linear Non-stationary, 0",
            "Non-linear Non-stationary, 0")

# Helper Parameters
p1_range <- bind_rows(df1) |> select(matches("x\\d")) |> map(\(x) range(x))
p2_range <- bind_rows(df2) |> select(matches("x\\d")) |> map(\(x) range(x))

# Create basic plots
plot_3d <- function(df) {
  df |> 
    plot_ly(x = ~x1,
            y = ~x2,
            z = ~x3, color =~state)
}

plots <- df_list |> 
  map(\(x) {
    list(plot_3d(x$f1),
         plot_3d(x$f2),
         plot_3d(x$f3)) |> set_names("f1", "f2", "f3")
  })


### Format and save plots
scene1.1 = list(camera = list(eye = list(x = 2.1, y = 0, z = 0.1)),
                xaxis = list(title = 'y<sub>1</sub>', range = p1_range$x1),
                yaxis = list(title = 'y<sub>2</sub>', range = p1_range$x2),
                zaxis = list(title = 'y<sub>3</sub>', range = p1_range$x3),
                aspectmode = 'manual',
                aspectratio = list(x = 1, y = 2, z = 1))
scene1.2 = list(camera = list(eye = list(x = .3, y = 2.95, z = .5)), # x = 2, y = -.75, z = 1.25
                xaxis = list(title = 'y<sub>1</sub>', range = p1_range$x1),
                yaxis = list(title = 'y<sub>2</sub>', range = p1_range$x2),
                zaxis = list(title = 'y<sub>3</sub>', range = p1_range$x3),
                aspectmode = 'manual',
                aspectratio = list(x = 1, y = 2, z = 1))

scene2.1 = list(camera = list(eye = list(x = 2.1, y = 0, z = 0.1)),
                xaxis = list(title = 'y<sub>1</sub>', range = p2_range$x1),
                yaxis = list(title = 'y<sub>2</sub>', range = p2_range$x2),
                zaxis = list(title = 'y<sub>3</sub>', range = p2_range$x3),
                aspectmode = 'manual',
                aspectratio = list(x = 1, y = 2, z = 1))
scene2.2 = list(camera = list(eye = list(x = .3, y = 2.95, z = .5)), #.5, 1.5, .5
                xaxis = list(title = 'y<sub>1</sub>', range = p2_range$x1),
                yaxis = list(title = 'y<sub>2</sub>', range = p2_range$x2),
                zaxis = list(title = 'y<sub>3</sub>', range = p2_range$x3),
                aspectmode = 'manual',
                aspectratio = list(x = 1, y = 2, z = 1))

w = 1000
h = 500
title_height <- .9

# Save Images for all plots
walk2(plots, names(plots), \(x, name) {
  file_name <- name |> str_remove_all(",") |> str_replace_all(" ", "-")
  
  if(!str_detect(name, "Non-linear")) {
    scene1 <- scene1.1
    scene2 <- scene1.2
  } else {
    scene1 <- scene2.1
    scene2 <- scene2.2
  }
  
  walk(c("f1", "f2", "f3"), \(fault) {
    title_name <- paste0(str_split_1(name, ",")[1],
                         ": Fault ", str_sub(fault, 2))
    
    save_image(layout(x[[fault]],
                      title = list(text = paste0(title_name,", Angle 2"), y = title_height),
                      width = w, height = h, scene = scene1),
               paste0("figures/", file_name, "-", fault,  "-angle-2.png"))
    
    save_image(layout(x[[fault]],
                      title = list(text = paste0(title_name,", Angle 1"), y = title_height),
                      width = w, height = h, scene = scene2),
               paste0("figures/", file_name, "-", fault, "-angle-1.png"))
  })
  
})