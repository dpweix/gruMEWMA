#install.packages("devtools")
#install.packages("roxygen2")

#devtools::create("mlmcusum")

devtools::document("mlmcusum")
devtools::install("mlmcusum")

library("mlmcusum")

?create_X
