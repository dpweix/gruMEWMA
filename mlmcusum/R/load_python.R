#' Find python conda location
#'
#' Uses reticulate to load in the appropriate conda enviornment. 
#' Will then use source_python to load in python source code for
#' using the GRU. 
#' 
#'
#' @param path_conda A path for use_condaenv. This conda enviornment requires numpy and tensorflow.
#' @param path_py A path for gru_functions.py. This file contains to necessary python code to run the neural networks.
#' @export

load_python <- function(path_conda, path_py) {
  reticulate::use_condaenv(path_conda)
  reticulate::source_python(path_py)
}