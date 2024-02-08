#' Take Log10(x) or 10^(x) for a vector x
#'
#' @description Take Log10(x) or 10^(x) for a vector x
#' @return Named vector where the parameters, which are 
#' @param x Named vector with parameter values
#' @param takeLog10 Named vector of booleans indicating if log10 is/should be 
#' taken of the model parameters.
#' @param reverse Boolean indicating if 10^ should be taken instead of log10() 
#' (Default: FALSE).
#' @export applyLog10ForTakeLog10
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(data,
#'                                          modus = 'RetardedTransientDynamics')
#' initialGuess.vec <- optimObject.orig[["initialGuess.vec"]]
#' lb.vec <- optimObject.orig[["lb.vec"]]
#' takeLog10 <- optimObject.orig[["takeLog10"]]
#' applyLog10ForTakeLog10(lb.vec, takeLog10) 

applyLog10ForTakeLog10 <- function(x, takeLog10, reverse = FALSE) {
  
  if (reverse) {
    x[names(x) %in% names(which(takeLog10))] <-
      10^x[names(x) %in% names(which(takeLog10))]
  } else {
    x[names(x) %in% names(which(takeLog10))] <-
      log10(x[names(x) %in% names(which(takeLog10))])
  }
  x
}