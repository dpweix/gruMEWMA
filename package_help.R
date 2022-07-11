#install.packages("devtools")
#install.packages("roxygen2")

#devtools::create("mlmcusum")

devtools::document("~/git/mlmcusum")
devtools::install("~/git/mlmcusum")

library("mlmcusum")

?create_X

