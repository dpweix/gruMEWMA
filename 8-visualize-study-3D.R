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

p1.1 <- plot_3d(df1$f1)
p1.2 <- plot_3d(df1$f2)
p1.3 <- plot_3d(df1$f3) 

p2.1 <- plot_3d(df2$f1)
p2.2 <- plot_3d(df2$f2)
p2.3 <- plot_3d(df2$f3) 


### Format and save plots
scene1.1 = list(camera = list(eye = list(x = 2, y = 0, z = 0.1)),
                xaxis = list(title = 'y<sub>1</sub>', range = p1_range$x1),
                yaxis = list(title = 'y<sub>2</sub>', range = p1_range$x2),
                zaxis = list(title = 'y<sub>3</sub>', range = p1_range$x3),
                aspectmode = 'manual',
                aspectratio = list(x = 1, y = 2, z = 1))
scene1.2 = list(camera = list(eye = list(x = 2, y = -.75, z = 1.25)),
                xaxis = list(title = 'y<sub>1</sub>', range = p1_range$x1),
                yaxis = list(title = 'y<sub>2</sub>', range = p1_range$x2),
                zaxis = list(title = 'y<sub>3</sub>', range = p1_range$x3),
                aspectmode = 'manual',
                aspectratio = list(x = 1, y = 2, z = 1))

scene2.1 = list(camera = list(eye = list(x = 2, y = 0, z = 0.1)),
                xaxis = list(title = 'y<sub>1</sub>', range = p2_range$x1),
                yaxis = list(title = 'y<sub>2</sub>', range = p2_range$x2),
                zaxis = list(title = 'y<sub>3</sub>', range = p2_range$x3),
                aspectmode = 'manual',
                aspectratio = list(x = 1, y = 2, z = 1))
scene2.2 = list(camera = list(eye = list(x = 2, y = -.75, z = 1.25)),
                xaxis = list(title = 'y<sub>1</sub>', range = p2_range$x1),
                yaxis = list(title = 'y<sub>2</sub>', range = p2_range$x2),
                zaxis = list(title = 'y<sub>3</sub>', range = p2_range$x3),
                aspectmode = 'manual',
                aspectratio = list(x = 1, y = 2, z = 1))

w = 1000
h = 500


# Linear, phi = .8
save_image(p1.1 |>  layout(title = list(text = "Fault 1: Linear, 0.8", y = .8),
                           width = w, height = h, scene = scene1.1),
           "figures/fault-1-linear-0.8-angle-1.png")

save_image(p1.1 |> layout(title = list(text = "Fault 1: Linear, 0.8", y = .8),
                          width = w, height = h, scene = scene1.2),
           "figures/fault-1-linear-0.8-angle-2.png")

save_image(p1.2 |>  layout(title = list(text = "Fault 2: Linear, 0.8", y = .8),
                           width = w, height = h, scene = scene1.1),
           "figures/fault-2-linear-0.8-angle-1.png")

save_image(p1.2 |> layout(title = list(text = "Fault 2: Linear, 0.8", y = .8),
                          width = w, height = h, scene = scene1.2),
           "figures/fault-2-linear-0.8-angle-2.png")

save_image(p1.3 |>  layout(title = list(text = "Fault 3: Linear, 0.8", y = .8),
                           width = w, height = h, scene = scene1.1),
           "figures/fault-3-linear-0.8-angle-1.png")

save_image(p1.3 |> layout(title = list(text = "Fault 3: Linear, 0.8", y = .8),
                          width = w, height = h, scene = scene1.2),
           "figures/fault-3-linear-0.8-angle-2.png")


# Non-linear, phi = .8
save_image(p2.1 |>  layout(title = list(text = "Fault 1: Non-linear, 0.8", y = .8),
                           width = w, height = h, scene = scene2.1),
           "figures/fault-1-non-linear-0.8-angle-1.png")

save_image(p2.1 |> layout(title = list(text = "Fault 1: Non-linear, 0.8", y = .8),
                          width = w, height = h, scene = scene2.2),
           "figures/fault-1-non-linear-0.8-angle-2.png")

save_image(p2.2 |>  layout(title = list(text = "Fault 2: Non-linear, 0.8", y = .8),
                           width = w, height = h, scene = scene2.1),
           "figures/fault-2-non-linear-0.8-angle-1.png")

save_image(p2.2 |> layout(title = list(text = "Fault 2: Non-linear, 0.8", y = .8),
                          width = w, height = h, scene = scene2.2),
           "figures/fault-2-non-linear-0.8-angle-2.png")

save_image(p2.3 |>  layout(title = list(text = "Fault 3: Non-linear, 0.8", y = .8),
                           width = w, height = h, scene = scene2.1),
           "figures/fault-3-non-linear-0.8-angle-1.png")

save_image(p2.3 |> layout(title = list(text = "Fault 3: Non-linear, 0.8", y = .8),
                          width = w, height = h, scene = scene2.2),
           "figures/fault-3-non-linear-0.8-angle-2.png")
