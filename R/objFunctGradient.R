#' Gradient of log likelihood optimization function
#'
#' @description Gradient of log likelihood optimization function
#' @return Named vector with optimization value for each parameter
#' @param par Initial values for the parameters to be optimized over.
#' @param data Data frame containing columns named 't' (time), 'y' (quantitative
#' value) and optionally 'sdExp' (standard deviation of the experimental data)
#' @param optimObject optimObject
#' @export objFunctGradient

objFunctGradient <- function(par, data, optimObject) {
  objFunct(par, data, optimObject, calcGradient = TRUE) 
}