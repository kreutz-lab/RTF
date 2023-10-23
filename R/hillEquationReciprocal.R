#' Calculate Hill equation of reciprocal parameter
#'
#' @description Calculates Hill equation of reciprocal parameter
#' @return Hill equation result value
#' @param d Dose
#' @param M Maximum value
#' @param h Hill coefficient
#' @param K Half-maximum quantity
#' @export hillEquationReciprocal
#' @examples
#' hillEquationReciprocal(d = 20, M = 5, h = 3, K = 4)

hillEquationReciprocal <- function(d, M, h, K){
  result <- M * (1 - ((d^h)/(K^h + d^h)))
  # if(is.infinite(result)){
  #   warning(paste0("hillEquationReciprocal is infinite. Parameters: d=", d, ", M=", M, ", h=", h, ", K=", K))
  # }
  return(result)
}
