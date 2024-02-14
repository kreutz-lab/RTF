#' Calculate Hill equation
#'
#' @description Calculates Hill equation
#' @return Hill equation result value
#' @param d Dose
#' @param M Maximum value
#' @param h Hill coefficient
#' @param K Half-maximum quantity
#' @export hillEquation
#' @examples
#' hillEquation(d = 20, M = 5, h = 3, K = 4)

hillEquation <- function(d, M, h, K, calcGradient = F){
  result <- M * ((d^h)/(K^h + d^h))
  # if(is.infinite(result)){
  #   warning("hillEquation is infinite.")
  # }
  return(result)
}
