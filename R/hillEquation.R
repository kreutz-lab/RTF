#' Calculate Hill equation
#'
#' @description Calculates Hill equation
#' @return Hill equation result value
#' @param d Dose
#' @param M Maximum value
#' @param h Hill coefficient
#' @param K Half-maximum quantity
#' @param reciprocal Boolean indicating if Hill equation is calculated for
#' reciprocal parameter
#' @param calcGradient  Boolean indicating if gradient should be calculated
#' @param minval (optional) Numeric, defines the lowest value the result can 
#' acquire. Only relevant if reciprocal = TRUE.
#' @export hillEquation
#' @examples
#' hillEquation(d = 20, M = 5, h = 3, K = 4)

hillEquation <- function(d, M, h, K, reciprocal = FALSE,
                         calcGradient = FALSE, minval = NULL){
  if (reciprocal) {
    if (calcGradient) {
      
    } else {
      result <- M * (1 - ((d^h)/(K^h + d^h)))
      if (!is.null(minval)) {
        result[result < minval] <- minval
      }
    }
  } else {
    if (calcGradient) {
      
    } else { 
      result <- M * ((d^h)/(K^h + d^h))
    }
  }
  return(result)
}