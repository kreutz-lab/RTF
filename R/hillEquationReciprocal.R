#' Calculate Hill equation of reciprocal parameter
#'
#' @description Calculates Hill equation of reciprocal parameter
#' @return Hill equation result value
#' @param d Dose
#' @param M Maximum value
#' @param h Hill coefficient
#' @param K Half-maximum quantity
#' @param minval (optional) Numeric, defines the lowest value the result can 
#' acquire
#' @export hillEquationReciprocal
#' @examples
#' hillEquationReciprocal(d = 20, M = 5, h = 3, K = 4)

hillEquationReciprocal <- function(d, M, h, K, minval = NULL){
  result <- M * (1 - ((d^h)/(K^h + d^h)))
  if (!is.null(minval)) {
    result[result < minval] <- minval
  }
  # if(is.infinite(result)){
  #   warning(paste0("hillEquationReciprocal is infinite. Parameters: d=", d, ", M=", M, ", h=", h, ", K=", K))
  # }
  return(result)
}
