#' Generate Sample Data
#'
#' Generates a three dimensional multivariate time series for use in the
#' simulation study. Functions exists for linearly related data, non-linearly
#' related data, and data with both long term dependencies and non-linear
#' relationships. Further, one of three faults can be specified via "f1", "f2",
#' or "f3".
#' 
#'
#' @param fault A marker for which fault is introduced, "f1", "f2", or "f3".
#' @param n_ic The number of observations which are in-control.
#' @param n_oc The number of observations which are out-of-control, i.e. faulty.
#' @param phi The strength of the autocorrelation in the base time series
#' @return A dataframe of sample data for use in the simulation study.
#' @export
gen_dat_lin <- function(fault, n_ic = 1500, n_oc = 500, phi = .8) {
  t <- arima.sim(model = list(ar = phi), n = (n_ic + n_oc)) |> scale_t()
  
  index_ic <- 1:n_ic
  index_oc <- (n_ic+1):(n_ic+n_oc)
  
  dat <-
    tibble::tibble(index = index_ic,
           x1 = t[index_ic] + rnorm(n_ic, 0, .1),
           x2 = 2*t[index_ic] + rnorm(n_ic, 0, .1),
           x3 = -.5*t[index_ic] + rnorm(n_ic, 0, .1))
  
  if(fault == "f1") {
    dplyr::bind_rows(dat,
                     tibble::tibble(index = index_oc,
                       x1 = t[index_oc] + rnorm(n_oc, 0, .1),
                       x2 = 2*t[index_oc] + rnorm(n_oc, 0, .5), # increased variance
                       x3 = -.5*t[index_oc] + rnorm(n_oc, 0, .1))
    )
  } else if(fault == "f2") {
    dplyr::bind_rows(dat,
                     tibble::tibble(index = index_oc,
                       x1 = t[index_oc] + rnorm(n_oc, 0, .1),
                       x2 = 2*t[index_oc] + rnorm(n_oc, 0, .1), 
                       x3 = -1*t[index_oc] + rnorm(n_oc, 0, .1)) # alter x3
    )
    
  } else if(fault == "f3") {
    dplyr::bind_rows(dat,
                     tibble::tibble(index = index_oc,
                       x1 = t[index_oc] + rnorm(n_oc, 0, .5), # increased variance
                       x2 = 2*t[index_oc] + rnorm(n_oc, 0, .5), # for all
                       x3 = -.5*t[index_oc] + rnorm(n_oc, 0, .5)) # variables
    )
  } else if(fault == "none") {
    dat
  } else print("Command not found, use 'f1', 'f2', 'f3', or 'none'.")
}

#' @rdname gen_dat_lin
#' @export
gen_dat_nlr <- function(fault, n_ic = 1500, n_oc = 500, phi = .8) {
  t <- arima.sim(model = list(ar = phi), n = (n_ic + n_oc)) |> scale_t()
  
  index_ic <- 1:n_ic
  index_oc <- (n_ic+1):(n_ic+n_oc)
  
  dat <-
    tibble::tibble(index = index_ic,
                   x1 = t[index_ic] + rnorm(n_ic, 0, .1),
                   x2 = t[index_ic]^2 - 3*t[index_ic] + rnorm(n_ic, 0, .1),
                   x3 = -t[index_ic]^3 + 3*t[index_ic]^2 + rnorm(n_ic, 0, .1))
  
  if(fault == "f1") {
    dplyr::bind_rows(dat,
                     tibble::tibble(index = index_oc,
                       x1 = t[index_oc] + rnorm(n_oc, 0, .1),
                       x2 = t[index_oc]^2 - 3*t[index_oc] + rnorm(n_oc, 0, .5),  # increase variance
                       x3 = -t[index_oc]^3 + 3*t[index_oc]^2 + rnorm(n_oc, 0, .1))
    )
  } else if(fault == "f2") {
    dplyr::bind_rows(dat,
                     tibble::tibble(index = index_oc,
                       x1 = t[index_oc] + rnorm(n_oc, 0, .1),
                       x2 = t[index_oc]^2 - 3*t[index_oc] + rnorm(n_oc, 0, .1),
                       x3 = -1.5*t[index_oc]^3 + 3*t[index_oc]^2 + rnorm(n_oc, 0, .1)) # alter x3
    )
    
  } else if(fault == "f3") {
    dplyr::bind_rows(dat,
                     tibble::tibble(index = index_oc,
                       x1 = t[index_oc] + rnorm(n_oc, 0, .5), # increased variance
                       x2 = t[index_oc]^2 - 3*t[index_oc] + rnorm(n_oc, 0, .5), # for all
                       x3 = -t[index_oc]^3 + 3*t[index_oc]^2 + rnorm(n_oc, 0, .5)) # variables
    )
  } else if(fault == "none") {
    dat
  } else print("Command not found, use 'f1', 'f2', 'f3', or 'none'.")
}

#' @rdname gen_dat_lin
#' @export
gen_dat_ltm <- function(fault, n_ic = 1500, n_oc = 500, phi = .8) {
  t <- arima.sim(model = list(ar = c(.1, rep(0, 31), .8)), n = (n_ic+n_oc)) |> scale_t()
  
  index_ic <- 1:n_ic
  index_oc <- (n_ic+1):(n_ic+n_oc)
  
  dat <-
    tibble::tibble(index = index_ic,
                   x1 = t[index_ic] + rnorm(n_ic, 0, .1),
                   x2 = t[index_ic]^2 - 3*t[index_ic] + rnorm(n_ic, 0, .1),
                   x3 = -t[index_ic]^3 + 3*t[index_ic]^2 + rnorm(n_ic, 0, .1))
  
  if(fault == "f1") {
    dplyr::bind_rows(dat,
                     tibble::tibble(index = index_oc,
                       x1 = t[index_oc] + rnorm(n_oc, 0, .1),
                       x2 = t[index_oc]^2 - 3*t[index_oc] + rnorm(n_oc, 0, .5),  # increase variance
                       x3 = -t[index_oc]^3 + 3*t[index_oc]^2 + rnorm(n_oc, 0, .1))
    )
  } else if(fault == "f2") {
    dplyr::bind_rows(dat,
                     tibble::tibble(index = index_oc,
                       x1 = t[index_oc] + rnorm(n_oc, 0, .1),
                       x2 = t[index_oc]^2 - 3*t[index_oc] + rnorm(n_oc, 0, .1),
                       x3 = -1.5*t[index_oc]^3 + 3*t[index_oc]^2 + rnorm(n_oc, 0, .1)) # alter x3
    )
    
  } else if(fault == "f3") {
    dplyr::bind_rows(dat,
                     tibble::tibble(index = index_oc,
                       x1 = t[index_oc] + rnorm(n_oc, 0, .5), # increased variance
                       x2 = t[index_oc]^2 - 3*t[index_oc] + rnorm(n_oc, 0, .5), # for all
                       x3 = -t[index_oc]^3 + 3*t[index_oc]^2 + rnorm(n_oc, 0, .5)) # variables
    )
  } else if(fault == "none") {
    dat
  } else print("Command not found, use 'f1', 'f2', 'f3', or 'none'.")
}