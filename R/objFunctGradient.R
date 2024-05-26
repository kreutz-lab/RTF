#' Gradient of log-likelihood optimization function
#'
#' @description Gradient of log-likelihood optimization function
#' @return Named vector with optimization value for each parameter
#' @param par Initial values for the parameters to be optimized over.
#' @param data Data frame containing columns named 't' (time), 'y' (quantitative
#' value) and optionally 'sigmaExp' (standard error of the experimental data)
#' @param optimObject optimObject
#' @param calcGradient Placeholder so that stats::optim is not returning  the
#' error 'Error in gr(par, ...) : unused argument (calcGradient = FALSE)'
#' @export objFunctGradient

objFunctGradient <- function(par, data, optimObject, calcGradient) {
    objFunct(par, data, optimObject, calcGradient = TRUE)
}
